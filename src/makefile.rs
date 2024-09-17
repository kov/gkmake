use std::fmt::Debug;
use std::sync::Arc;
use std::{fs, process, time::SystemTime};

use anyhow::{bail, Result};
use log::trace;

pub struct Makefile {
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

pub struct PreReq {
    pub name: String,
    pub order_only: bool,
}

pub type Recipe = String;

#[derive(Debug)]
struct ExecutionNode<T> {
    rule: Option<T>,
    children: Vec<ExecutionNode<T>>,
}

impl Makefile {
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

        // Check if the target already exists and is up-to-date.
        let (mut needs_update, goal_mtime) = if fs::exists(goal)? {
            trace!("Goal {goal} exists, may not need update...");
            (false, fs::metadata(goal)?.modified()?)
        } else {
            trace!("Goal {goal} does not exist, will be made.");
            (true, SystemTime::UNIX_EPOCH)
        };

        for preq in rule.pre_reqs().iter() {
            let preq_goal = &preq.name;

            if !fs::exists(preq_goal)? {
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
            } else if !preq.order_only && fs::metadata(preq_goal)?.modified()? > goal_mtime {
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

        println!("{:?}", tree);
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
}

impl Debug for dyn Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Rule({:?})", self.targets())
    }
}
