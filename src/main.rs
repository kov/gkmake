use std::sync::Arc;

use anyhow::Result;
use makefile::{ExplicitRule, Makefile, PreReq, Target};

mod makefile;

fn main() -> Result<()> {
    env_logger::init();

    let mf = Makefile {
        explicit: vec![
            Arc::new(ExplicitRule {
                targets: vec![Target {
                    name: "lala".to_owned(),
                }],
                pre_reqs: vec![PreReq {
                    name: "lele".to_owned(),
                    order_only: false,
                }],
                recipe: "touch lala".to_owned(),
            }),
            Arc::new(ExplicitRule {
                targets: vec![Target {
                    name: "lele".to_owned(),
                }],
                pre_reqs: vec![PreReq {
                    name: "lili".to_owned(),
                    order_only: true,
                }],
                recipe: "touch lele".to_owned(),
            }),
            Arc::new(ExplicitRule {
                targets: vec![Target {
                    name: "lili".to_owned(),
                }],
                pre_reqs: vec![],
                recipe: "touch lili".to_owned(),
            }),
        ],
    };

    mf.make("lala")
}
