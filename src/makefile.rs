use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Debug;
use std::path::PathBuf;
use std::str::FromStr;
use std::sync::mpsc;
use std::sync::Arc;
use std::{fs, process, time::SystemTime};

use anyhow::{bail, Result};
use log::trace;

use crate::subst::find_and_perform_subst;

#[derive(Debug)]
pub struct Makefile {
    pub working_directory: PathBuf,
    pub explicit: Vec<Arc<dyn Rule>>,
    pub variables: HashMap<String, String>,
}

#[derive(Debug)]
struct ExecutionPlan {
    preq_goal_map: HashMap<String, Vec<String>>,
    goal_preq_map: HashMap<String, Vec<String>>,
    goal_map: HashMap<String, Recipe>,
    runnable_jobs: HashSet<String>,
}

impl ExecutionPlan {
    fn new() -> Self {
        ExecutionPlan {
            preq_goal_map: HashMap::new(),
            goal_preq_map: HashMap::new(),
            goal_map: HashMap::new(),
            runnable_jobs: HashSet::new(),
        }
    }

    fn add_job(
        &mut self,
        goal: &str,
        exists: bool,
        pre_req_for: Option<&str>,
        wait_on: Vec<String>,
        pre_reqs: &[&str],
        mut recipe: Recipe,
    ) {
        if let Some(pre_req_for) = pre_req_for {
            self.preq_goal_map
                .entry(goal.to_string())
                .or_insert(vec![])
                .push(pre_req_for.to_string());
        }

        // We have the recipe with all variables other than the special
        // ones replaced at this point. Replace $@ and $< based on what
        // triggered this job.
        let outdated_preqs: Vec<_> = wait_on.iter().map(|s| s.as_str()).collect();
        self.replace_auto_variables(
            &mut recipe,
            goal,
            exists,
            outdated_preqs.as_slice(),
            pre_reqs,
        );

        self.goal_map.entry(goal.to_string()).or_insert(recipe);

        if wait_on.is_empty() {
            self.runnable_jobs.insert(goal.to_string());
        } else {
            self.goal_preq_map
                .entry(goal.to_string())
                .or_insert(vec![])
                .extend_from_slice(wait_on.as_slice());
        }
    }

    fn replace_auto_variables(
        &self,
        recipe: &mut Recipe,
        target: &str,
        exists: bool,
        outdated_pre_reqs: &[&str],
        pre_reqs: &[&str],
    ) {
        recipe.borrow_mut().iter_mut().for_each(|line| {
            *line = line
                .replace("$@", target)
                .replace(
                    "$<",
                    pre_reqs.first().map(|s| s.to_owned()).unwrap_or_else(|| ""),
                )
                .replace(
                    "$?", // Only outdated preqs, unless target does not exist, then all.
                    pre_reqs
                        .iter()
                        .map(|s| *s)
                        .filter(|preq| !exists || outdated_pre_reqs.contains(preq))
                        .collect::<Vec<&str>>()
                        .join(" ")
                        .as_str(),
                )
        });
    }

    fn queue_waiting_jobs(&mut self, job: String) {
        if let Some(targets) = self.preq_goal_map.remove(&job) {
            for target in targets {
                trace!("notifying {target} that {job} completed...");
                let Some(pre_reqs) = self.goal_preq_map.get_mut(&target) else {
                    panic!("Job was in preq_goal but is not on goal_preq?");
                };
                let index = pre_reqs
                    .iter()
                    .position(|preq| *preq == job)
                    .expect("Job was not a pre-req?");
                pre_reqs.remove(index);

                if pre_reqs.is_empty() {
                    trace!("all dependencies for {target} done, adding to runnable");
                    self.goal_preq_map.remove(&target);
                    self.runnable_jobs.insert(target);
                }
            }
        }
    }

    fn next_job(&mut self) -> Option<String> {
        let Some(job) = self.runnable_jobs.iter().next().cloned() else {
            return None;
        };
        self.runnable_jobs.remove(&job);
        Some(job)
    }

    fn execute(mut self) -> Result<()> {
        let (sender_template, receiver) = mpsc::channel();
        let mut inflight = 0;
        loop {
            trace!("runnable jobs: {:?}", self.runnable_jobs);

            if self.runnable_jobs.is_empty() {
                if self.preq_goal_map.is_empty() && inflight == 0 {
                    trace!("everything is done!");
                    break;
                }

                // Wait for a running job to finish.
                let finished_job = receiver.recv().expect("Poisoned channel");
                self.queue_waiting_jobs(finished_job);
                inflight -= 1;
                continue;
            }

            let Some(job) = self.next_job() else {
                break;
            };

            let sender = sender_template.clone();
            let Some(recipe) = self.goal_map.get(&job) else {
                panic!("Runnable job had no recipe");
            };
            let job = job.clone();
            let recipe = recipe.borrow().clone();

            inflight += 1;
            rayon::spawn(move || {
                for command in recipe.iter() {
                    trace!("Running {command}...");

                    // Check for silencing commands.
                    let command = if let Some(command) = command.strip_prefix('@') {
                        command
                    } else {
                        println!("{command}");
                        command.as_str()
                    };

                    process::Command::new("/bin/sh")
                        .arg("-c")
                        .arg(command)
                        .spawn()
                        .expect("Failed to spawn")
                        .wait()
                        .expect("Failed to wait");
                }
                sender.send(job).expect("Poisoned sender");
            })
        }
        Ok(())
    }
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

pub type Recipe = RefCell<Vec<String>>;

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
        plan: &mut ExecutionPlan,
    ) -> Result<bool> {
        trace!("Trying to make {goal} required for: {required_for:?}");

        let goal_path = self.path_for_goal(goal);

        // Check if the target already exists and is up-to-date.
        let exists = fs::exists(&goal_path)?;
        let (mut needs_update, goal_mtime) = if exists {
            trace!("Goal {goal} exists, may not need update...");
            (false, fs::metadata(&goal_path)?.modified()?)
        } else {
            trace!("Goal {goal} does not exist, will be made.");
            (true, SystemTime::UNIX_EPOCH)
        };

        let mut wait_on = vec![];

        // Start by checking our explicit targets.
        if let Some(rule) = self.explicit.iter().find(|rule| rule.can_make(goal)) {
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
                let remade = self.make_for(preq_goal, Some(goal), plan)?;

                if remade {
                    wait_on.push(preq_goal.to_string());

                    if !preq.order_only {
                        trace!("Prerequisite {preq_goal} was made and is not for order only, must make {goal}.");
                        needs_update = true;
                    }
                }
            }

            if needs_update {
                let recipe = self.pre_process_recipe(rule.recipe());
                plan.add_job(
                    goal,
                    exists,
                    required_for,
                    wait_on,
                    rule.pre_reqs()
                        .iter()
                        .map(|preq| preq.name.as_str())
                        .collect::<Vec<&str>>()
                        .as_slice(),
                    recipe?,
                );
            }
        } else {
            // We have no rule and the dependency does not exist already, nothing we can do.
            if !exists {
                if let Some(required_for) = required_for {
                    bail!("No rule to make '{goal}', required for {required_for}.")
                } else {
                    bail!("No rule to make '{goal}'.")
                }
            }
        }

        Ok(needs_update)
    }

    // Apply any variable and function substitutions to the recipe command lines.
    fn pre_process_recipe(&self, recipe: &Recipe) -> Result<Recipe> {
        Ok(RefCell::new(
            recipe
                .borrow()
                .iter()
                .map(|line| find_and_perform_subst(line, &self.variables))
                .collect::<Result<_>>()?,
        ))
    }

    pub fn make(&self, goal: &str) -> Result<()> {
        let mut plan = ExecutionPlan::new();
        if !self.make_for(goal, None, &mut plan)? {
            println!("'{goal}' is already up-to-date.");
            return Ok(());
        }
        trace!("{:#?}", &plan);
        plan.execute()
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
        self.recipe.borrow_mut().push(snippet.trim().into());
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

    use crate::parser::Parser;

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
                    recipe: vec!["touch lala".to_owned()].into(),
                }),
                Arc::new(ExplicitRule {
                    targets: vec![Target {
                        name: "lele".to_owned(),
                    }],
                    pre_reqs: vec![PreReq {
                        name: "lili".to_owned(),
                        order_only: true,
                    }],
                    recipe: vec!["touch lele".to_owned()].into(),
                }),
                Arc::new(ExplicitRule {
                    targets: vec![Target {
                        name: "lili".to_owned(),
                    }],
                    pre_reqs: vec![],
                    recipe: vec!["touch lili".to_owned()].into(),
                }),
            ],
            variables: HashMap::new(),
        };

        let mut plan = ExecutionPlan::new();
        mf.make_for("lala", None, &mut plan)
            .expect("Failed building execution plan");

        let mut expected = HashMap::new();
        expected.insert("lili".into(), vec!["lele".into()]);
        expected.insert("lele".into(), vec!["lala".into()]);

        assert_eq!(expected, plan.preq_goal_map);

        let mut expected = HashMap::new();
        expected.insert("lala".into(), vec!["lele".into()]);
        expected.insert("lele".into(), vec!["lili".into()]);

        assert_eq!(expected, plan.goal_preq_map);

        let mut expected = HashMap::new();
        expected.insert("lala".into(), RefCell::new(vec!["touch lala".into()]));
        expected.insert("lele".into(), RefCell::new(vec!["touch lele".into()]));
        expected.insert("lili".into(), RefCell::new(vec!["touch lili".into()]));

        assert_eq!(expected, plan.goal_map);

        let mut runnable = HashSet::new();
        runnable.insert("lili".to_string());
        assert_eq!(runnable, plan.runnable_jobs);

        let job = plan.next_job().unwrap();
        assert_eq!("lili", job);
        plan.queue_waiting_jobs(job);

        let mut runnable = HashSet::new();
        runnable.insert("lele".to_string());
        assert_eq!(runnable, plan.runnable_jobs);

        let job = plan.next_job().unwrap();
        assert_eq!("lele", job);
        plan.queue_waiting_jobs(job);

        let mut runnable = HashSet::new();
        runnable.insert("lala".to_string());
        assert_eq!(runnable, plan.runnable_jobs);
    }

    #[test]
    fn test_substitutions() {
        let makefile = indoc! {"
            rsfiles := $(wildcard src/*.rs)
            rsdirs := $(dir Cargo.toml src/ $(wildcard src/*.rs))
            rsnotdirs := $(notdir Cargo.toml src/ $(wildcard src/*.rs))
            all:
            	echo woohoo
        "};

        let mf = Parser::new(makefile, PathBuf::from("."))
            .parse()
            .expect("Failed to parse");

        assert_eq!(
            "src/main.rs src/makefile.rs src/parser.rs src/subst.rs",
            mf.variables
                .get("rsfiles")
                .expect("no rsfiles in variables!")
        );

        assert_eq!(
            "./ src/ src/ src/ src/ src/",
            mf.variables.get("rsdirs").expect("no rsdirs in variables!")
        );

        assert_eq!(
            "Cargo.toml main.rs makefile.rs parser.rs subst.rs",
            mf.variables
                .get("rsnotdirs")
                .expect("no rsdirs in variables!")
        );
    }

    #[test]
    fn test_multilines() {
        let makefile = indoc! {"
            depends = gklog gkdial \
                gksu
            all:
            	echo woohoo \
            	    lala
        "};

        let mf = Parser::new(makefile, PathBuf::from("."))
            .parse()
            .expect("Failed to parse");

        assert_eq!(
            "gklog gkdial gksu",
            mf.variables
                .get("depends")
                .expect("no depends in variables!")
        );
    }

    #[test]
    fn test_recipe_preprocessing() {
        let makefile = indoc! {"
            doesexist = makefiles are wild
            all: a
            	@echo 'test for a very complex string here!!1 ?!  @ 3 %~'
            	touch $(wildcard src/*.rs)
            	echo this should be a shell variable: $$HOME
            	echo this should just disappear: $(nonecziste)
            	echo this should be properly replaced: -$(doesexist)-

            a b c: d e
            	touch $@ $<
            	echo $?

            d:
            	touch d

            e:
            	touch e
        "};

        let mf = Parser::new(makefile, PathBuf::from("."))
            .parse()
            .expect("Failed to parse");

        let mut plan = ExecutionPlan::new();
        mf.make_for("all", None, &mut plan)
            .expect("Failed building execution plan");

        let Some(recipe) = plan.goal_map.get("all".into()) else {
            panic!("Expected recipe for target all, but found none.")
        };

        let recipe = recipe.borrow();

        assert_eq!(
            recipe[0],
            "@echo 'test for a very complex string here!!1 ?!  @ 3 %~'"
        );

        assert_eq!(
            recipe[1],
            "touch src/main.rs src/makefile.rs src/parser.rs src/subst.rs"
        );

        assert_eq!(recipe[2], "echo this should be a shell variable: $HOME");

        assert_eq!(recipe[3], "echo this should just disappear: ");

        assert_eq!(
            recipe[4],
            "echo this should be properly replaced: -makefiles are wild-"
        );

        assert_eq!(plan.goal_map.get("b".into()), None);

        let Some(recipe) = plan.goal_map.get("a".into()) else {
            panic!("Expected recipe for target a, but found none.")
        };

        let recipe = recipe.borrow();

        assert_eq!(recipe[0], "touch a d");

        assert_eq!(recipe[1], "echo d e");
    }

    #[test]
    fn test_filter_out() {
        let makefile = indoc! {"
            sources = gklog.c gksu.c gnome.c kde.cpp
            nongk = gnome.c kde.cpp
            all:
            	@echo $(filter-out $(nongk),$(sources))
        "};

        let mf = Parser::new(makefile, PathBuf::from("."))
            .parse()
            .expect("Failed to parse");

        let mut plan = ExecutionPlan::new();
        mf.make_for("all", None, &mut plan).expect("Failed to make");

        let Some(recipe) = plan.goal_map.get("all".into()) else {
            panic!("Expected recipe for target all, but found none.")
        };

        let recipe = recipe.borrow();

        assert_eq!(recipe[0], "@echo gklog.c gksu.c");
    }

    #[test]
    fn test_shell() {
        let makefile = indoc! {"
            all:
            	@echo $(shell ls /etc/passwd)
        "};

        let mf = Parser::new(makefile, PathBuf::from("."))
            .parse()
            .expect("Failed to parse");

        let mut plan = ExecutionPlan::new();
        mf.make_for("all", None, &mut plan).expect("Failed to make");

        let Some(recipe) = plan.goal_map.get("all".into()) else {
            panic!("Expected recipe for target all, but found none.")
        };

        let recipe = recipe.borrow();

        assert_eq!(recipe[0], "@echo /etc/passwd");
    }

    #[test]
    fn test_patsubst() {
        let makefile = indoc! {"
            sources = gklog.c gksu.c
            all:
            	@echo objects: $(patsubst .c,.o,$(sources))
        "};

        let mf = Parser::new(makefile, PathBuf::from("."))
            .parse()
            .expect("Failed to parse");

        let mut plan = ExecutionPlan::new();
        mf.make_for("all", None, &mut plan).expect("Failed to make");

        let Some(recipe) = plan.goal_map.get("all".into()) else {
            panic!("Expected recipe for target all, but found none.")
        };

        let recipe = recipe.borrow();

        assert_eq!(recipe[0], "@echo objects: gklog.o gksu.o");
    }

    #[test]
    fn test_unsupported_function() {
        let makefile = indoc! {"
            all:
            	@echo this is unsupported: $(call whatever)
        "};

        let mf = Parser::new(makefile, PathBuf::from("."))
            .parse()
            .expect("Failed to parse");

        let mut plan = ExecutionPlan::new();
        let res = mf.make_for("all", None, &mut plan);
        assert_eq!(res.is_err(), true);
        assert_eq!(
            format!("{}", res.unwrap_err()),
            "unimplemented: call whatever"
        );
    }
}
