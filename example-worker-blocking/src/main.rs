use std::io::Write;

mod elm;

pub fn main() {
    let elm_init_allocator: bumpalo::Bump = bumpalo::Bump::new();
    let (elm_initial_state, elm_initial_cmd) =
        (elm::main_main(&elm_init_allocator).init)(elm::double_ended_iterator_to_list(
            &elm_init_allocator,
            std::env::args()
                .skip(1) // path of the executable
                .map(|argument| elm::StringString::One(elm_init_allocator.alloc(argument))),
        ));
    let mut elm_state = elm_main_state_to_persistent(elm_initial_state);
    elm_perform_cmd(elm_initial_cmd);
    'main_loop: loop {
        let elm_step_allocator: bumpalo::Bump = bumpalo::Bump::new();
        let elm_sub = (elm::main_main(&elm_step_allocator).subscriptions)(
            elm_main_state_from_persistent(&elm_step_allocator, &elm_state),
        );
        match elm_watch_sub(&elm_step_allocator, &elm_sub) {
            Option::Some(elm_event) => {
                let (elm_updated_state, elm_update_cmd) = (elm::main_main(&elm_step_allocator)
                    .update)(elm_event)(
                    elm_main_state_from_persistent(&elm_step_allocator, &elm_state),
                );
                elm_perform_cmd(elm_update_cmd);
                elm_state = elm_main_state_to_persistent(elm_updated_state);
            }
            Option::None => {
                // no more things to watch so we're done!
                break 'main_loop;
            }
        }
    }
}
fn elm_watch_sub<'a, Event>(
    allocator: &'a bumpalo::Bump,
    sub: &elm::PlatformSubSub<'a, Event>,
) -> Option<Event> {
    match sub {
        &elm::PlatformSubSub::PortIncoming(elm_port_name, elm_on_event) => match elm_port_name {
            "portStdInReadLine" => {
                let mut user_input = String::new();
                match std::io::BufRead::read_line(&mut std::io::stdin().lock(), &mut user_input) {
                    Result::Err(error) => {
                        println!("failed to read a line from standard input, code {error}");
                        Option::None
                    }
                    Result::Ok(_) => {
                        Option::Some(elm_on_event(elm::JsonValue::String(allocator.alloc(
                            // by default, the ending \n is part of user_input
                            user_input.replace("\n", ""),
                        ))))
                    }
                }
            }
            unknown_elm_port_name => {
                println!("unknown incoming elm port name {unknown_elm_port_name}");
                Option::None
            }
        },
        elm::PlatformSubSub::Batch(sub_subs) => sub_subs
            .iter()
            .find_map(|sub_sub| elm_watch_sub(allocator, sub_sub)),
    }
}
fn elm_perform_cmd<Event>(cmd: elm::PlatformCmdCmd<Event>) {
    match elm_perform_cmd_tree(&cmd.tree) {
        Option::None => {}
        Option::Some(process_exit_code) => std::process::exit(process_exit_code),
    }
}
fn elm_perform_cmd_tree(cmd_tree: &elm::PlatformCmdTree) -> Option<i32> {
    match cmd_tree {
        elm::PlatformCmdTree::PortOutgoing(port_name, json_value) => match *port_name {
            "portProcessExit" => {
                // you can alternatively just use elm::json_decode_*
                match json_value {
                    &elm::JsonValue::Number(code) if code.trunc() == code => {
                        Option::Some(code as i32)
                    }
                    elm::JsonValue::Null
                    | elm::JsonValue::Bool(_)
                    | elm::JsonValue::Number(_) // Float
                    | elm::JsonValue::String(_)
                    | elm::JsonValue::Array(_)
                    | elm::JsonValue::Object(_) => {
                        println!("elm {port_name} expects an Int as input");
                        Option::None
                    }
                }
            }
            "portStdOutWrite" => match json_value {
                elm::JsonValue::String(message) => {
                    let mut std_out_lock = std::io::stdout().lock();
                    let _ = std_out_lock.write_all(message.as_bytes());
                    // still put it on screen
                    let _ = std_out_lock.flush();
                    Option::None
                }
                elm::JsonValue::Null
                | elm::JsonValue::Bool(_)
                | elm::JsonValue::Number(_)
                | elm::JsonValue::Array(_)
                | elm::JsonValue::Object(_) => {
                    println!("elm {port_name} expects a String as input");
                    Option::None
                }
            },
            unknown_elm_port_name => {
                println!("unknown outgoing elm port name {unknown_elm_port_name}");
                Option::None
            }
        },
        elm::PlatformCmdTree::Batch(subs) => {
            let mut result = Option::None;
            for sub in *subs {
                let sub_result = elm_perform_cmd_tree(sub);
                result = result.or(sub_result);
            }
            result
        }
    }
}

/// This is mostly a mirror of elm::MainState<'a>
/// except that what is references is replaced by persistent alternatives like Box, Vec, String etc.
///
/// While it might be a bit inconvenient to try to keep these
/// 2 representations in sync, rust's (record and enum) exhaustiveness checking
/// will at least remind you :)
///
/// If you plan to only use persistent structures in your elm model
/// (that means no strings, lists or recursive choice `type`s)
/// you can replace all uses of ElmStatePersistent with elm::MainState
/// and remove the conversions
struct ElmMainStatePersistent {
    name: Option<String>,
    title: Option<String>,
}
fn elm_main_state_to_persistent(temporary: elm::MainState) -> ElmMainStatePersistent {
    ElmMainStatePersistent {
        name: temporary.name.map(|name| name.to_string()),
        title: temporary.title.map(|title| title.to_string()),
    }
}
fn elm_main_state_from_persistent<'a>(
    _allocator: &'a bumpalo::Bump, // you will need this one for lists etc
    persistent: &'a ElmMainStatePersistent,
) -> elm::MainState<'a> {
    elm::GeneratedNameTitle {
        name: match persistent.name {
            Option::None => Option::None,
            Option::Some(ref name) => Option::Some(elm::StringString::One(name)),
        },
        title: match persistent.title {
            Option::None => Option::None,
            Option::Some(ref title) => Option::Some(elm::StringString::One(title)),
        },
    }
}
