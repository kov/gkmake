use std::cell::RefCell;
use std::fmt::Debug;
use std::path::PathBuf;
use std::str::FromStr;
use std::sync::Arc;
use std::{fs, process, time::SystemTime};

use anyhow::{bail, Result};
use log::trace;

#[derive(Debug)]
pub struct Makefile {
    pub working_directory: PathBuf,
    pub explicit: Vec<Arc<dyn Rule>>,
}

pub struct ExplicitRule {
    pub targets: Vec<Target>,
    pub pre_reqs: Vec<PreReq>,
    pub recipe: Recipe,
}

#[derive(Debug)]
pub struct Target {
    pub name: String,
}

#[derive(Debug)]
pub struct PreReq {
    pub name: String,
    pub order_only: bool,
}

pub type Recipe = RefCell<String>;

#[derive(Debug)]
struct ExecutionNode<T> {
    rule: Option<T>,
    children: Vec<ExecutionNode<T>>,
}

impl Makefile {
    fn path_for_goal(&self, goal: &str) -> PathBuf {
        let goal_path = PathBuf::from_str(goal)
            .unwrap_or_else(|e| panic!("fatal: failed to get path for {goal}: {e}"));

        if goal_path.is_absolute() {
            goal_path
        } else {
            self.working_directory.join(goal_path)
        }
    }

    // make_for() will recursively look for all the work that needs to be done
    // and build a tree of ExecutionNodes; nodes will have no rule attached to
    // them if the targets of that rule do not need to be made, but may still
    // have children.
    fn make_for(
        &self,
        goal: &str,
        required_for: Option<&str>,
        mut execution_tree: ExecutionNode<Arc<dyn Rule>>,
    ) -> Result<ExecutionNode<Arc<dyn Rule>>> {
        trace!("Trying to make {goal} required for: {required_for:?}");

        // Start by checking our explicit targets.
        let Some(rule) = self.explicit.iter().find(|rule| rule.can_make(goal)) else {
            if let Some(required_for) = required_for {
                bail!("No rule to make '{goal}', required for {required_for}.")
            } else {
                bail!("No rule to make '{goal}'.")
            }
        };

        let goal_path = self.path_for_goal(goal);
        // Check if the target already exists and is up-to-date.
        let (mut needs_update, goal_mtime) = if fs::exists(&goal_path)? {
            trace!("Goal {goal} exists, may not need update...");
            (false, fs::metadata(&goal_path)?.modified()?)
        } else {
            trace!("Goal {goal} does not exist, will be made.");
            (true, SystemTime::UNIX_EPOCH)
        };

        for preq in rule.pre_reqs().iter() {
            let preq_goal = &preq.name;
            let preq_path = self.path_for_goal(preq_goal);

            if !fs::exists(&preq_path)? {
                if !preq.order_only {
                    trace!(
                        "Prerequisite {preq_goal} does not exist. Must make {preq_goal} and {goal}."
                    );
                    needs_update = true;
                } else {
                    trace!(
                        "Prerequisite {preq_goal} does not exist. Must make order-only {preq_goal}."
                    );
                }
            } else if !preq.order_only && fs::metadata(preq_path)?.modified()? > goal_mtime {
                trace!("Prerequisite {preq_goal} was modified after {goal}, must make {goal}.");
                needs_update = true;
            }

            // This is not only here to make the target if it doesn't exist,
            // but also to check if one of the prerequisites of our prereqs
            // also require the dep to be remade.
            let node = self.make_for(
                preq_goal,
                Some(goal),
                ExecutionNode {
                    rule: None,
                    children: vec![],
                },
            )?;

            if node.rule.is_some() && !preq.order_only {
                trace!("Prerequisite {preq_goal} was made and is not for order only, must make {goal}.");
                needs_update = true;
            }

            execution_tree.children.push(node);
        }

        if needs_update {
            execution_tree.rule = Some(Arc::clone(rule));
        } else {
            // If this is the main goal, report back that we did not need to make it.
            if required_for.is_none() {
                println!("'{goal}' is already up-to-date.");
            }
        }

        Ok(execution_tree)
    }

    // make() will get make_for() to build the execution tree, and then get make_node() to
    // execute it recursively.
    // TODO: we should go through the tree validating we do not have dependency cycles
    // (goals that cannot be made are already handled in the previous step); and structure
    // it in a way that allows for running the jobs with rayon.
    pub fn make(&self, goal: &str) -> Result<()> {
        let tree = self.make_for(
            goal,
            None,
            ExecutionNode {
                rule: None,
                children: vec![],
            },
        )?;

        self.make_node(&tree)
    }

    // make_node() will recursively execute pre requisites followed by the
    // dependant rule;
    fn make_node(&self, node: &ExecutionNode<Arc<dyn Rule>>) -> Result<()> {
        for child in node.children.iter() {
            self.make_node(child)?;
        }
        if let Some(rule) = &node.rule {
            self.run_recipe(rule.recipe())
        } else {
            Ok(())
        }
    }

    fn run_recipe(&self, recipe: &Recipe) -> Result<()> {
        trace!("Running recipe: {recipe:?}");
        let recipe = recipe.borrow();

        // Check for silencing commands.
        let recipe = if let Some(recipe) = recipe.strip_prefix('@') {
            recipe
        } else {
            println!("{recipe}");
            recipe.as_str()
        };

        let (cmd, args) = recipe.split_once(' ').unwrap();
        process::Command::new(cmd)
            .args(args.split(' '))
            .spawn()?
            .wait()?;
        Ok(())
    }
}

pub trait Rule {
    fn can_make(&self, goal: &str) -> bool;
    fn targets(&self) -> &[Target];
    fn pre_reqs(&self) -> &[PreReq];
    fn recipe(&self) -> &Recipe;

    fn push_to_recipe(&self, snippet: &str);
}

impl Rule for ExplicitRule {
    fn can_make(&self, goal: &str) -> bool {
        self.targets
            .iter()
            .find(|target| target.name == goal)
            .is_some()
    }

    fn targets(&self) -> &[Target] {
        self.targets.as_slice()
    }

    fn recipe(&self) -> &Recipe {
        &self.recipe
    }

    fn pre_reqs(&self) -> &[PreReq] {
        &self.pre_reqs
    }

    fn push_to_recipe(&self, snippet: &str) {
        self.recipe.borrow_mut().push_str(snippet.trim());
    }
}

impl Debug for dyn Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Rule(\n    targets = {:?}\n    pre_reqs = {:?}\n    recipe = {:?}\n)",
            self.targets(),
            self.pre_reqs(),
            self.recipe()
        )
    }
}

#[cfg(test)]
mod test {
    use indoc::indoc;
    use temp_dir::TempDir;

    use super::*;

    #[test]
    fn test_execution_plan() {
        let tmp_dir =
            TempDir::with_prefix("gkmake-test-").expect("Failed to create temporary directory");

        let mf = Makefile {
            working_directory: tmp_dir.path().into(),
            explicit: vec![
                Arc::new(ExplicitRule {
                    targets: vec![Target {
                        name: "lala".to_owned(),
                    }],
                    pre_reqs: vec![PreReq {
                        name: "lele".to_owned(),
                        order_only: false,
                    }],
                    recipe: "touch lala".to_owned().into(),
                }),
                Arc::new(ExplicitRule {
                    targets: vec![Target {
                        name: "lele".to_owned(),
                    }],
                    pre_reqs: vec![PreReq {
                        name: "lili".to_owned(),
                        order_only: true,
                    }],
                    recipe: "touch lele".to_owned().into(),
                }),
                Arc::new(ExplicitRule {
                    targets: vec![Target {
                        name: "lili".to_owned(),
                    }],
                    pre_reqs: vec![],
                    recipe: "touch lili".to_owned().into(),
                }),
            ],
        };

        let tree = mf
            .make_for(
                "lala",
                None,
                ExecutionNode {
                    rule: None,
                    children: vec![],
                },
            )
            .expect("Failed building execution plan");

        let expected = indoc! {"
            ExecutionNode { rule: Some(Rule(
                targets = [Target { name: \"lala\" }]
                pre_reqs = [PreReq { name: \"lele\", order_only: false }]
                recipe = RefCell { value: \"touch lala\" }
            )), children: [ExecutionNode { rule: Some(Rule(
                targets = [Target { name: \"lele\" }]
                pre_reqs = [PreReq { name: \"lili\", order_only: true }]
                recipe = RefCell { value: \"touch lele\" }
            )), children: [ExecutionNode { rule: Some(Rule(
                targets = [Target { name: \"lili\" }]
                pre_reqs = []
                recipe = RefCell { value: \"touch lili\" }
            )), children: [] }] }] }"
        };

        assert_eq!(expected, format!("{tree:?}"));
    }
}
