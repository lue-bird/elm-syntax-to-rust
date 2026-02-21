/*

# High-level overview of how continuously running elm subscriptions works:

we have an unordered collection of Futures that represent individual subscriptions
that want to be resolved.

We race all until we have the next resolved event.
We remove the resolved subscription from the unordered collection.
We associate the resolved event with the subscriptions based on the current state
and update the state accordingly.
We diff the new batch of elm subscriptions with the currently running ones,
cancelling those not present anymore and inserting new ones.
Repeat until subscriptions unordered collection empty.

(I tried implementing this with only std/smol but I gave up, any tips appreciated!)

*/
#![feature(allocator_api)]
mod elm;

#[tokio::main]
pub async fn main() {
    let mut elm_state: ElmMainStatePersistent;
    {
        let elm_init_allocator: bumpalo::Bump = bumpalo::Bump::new();
        let (elm_initial_state, elm_initial_cmd) =
            (elm::main_main(&elm_init_allocator).init)(elm::double_ended_iterator_to_list(
                &elm_init_allocator,
                std::env::args()
                    .skip(1) // path of the executable
                    .map(|argument| elm::StringString::One(elm_init_allocator.alloc(argument))),
            ));
        elm_state = elm_main_state_to_persistent(elm_initial_state);
        elm_perform_cmd(elm_initial_cmd);
    }
    let mut running_subs_handles: futures::stream::FuturesUnordered<RunningSub> =
        futures::stream::FuturesUnordered::new();
    'main_loop: loop {
        let elm_step_allocator: bumpalo::Bump = bumpalo::Bump::new();

        let elm_sub: elm::PlatformSubSub<elm::MainEvent> = (elm::main_main(&elm_step_allocator)
            .subscriptions)(
            elm_main_state_from_persistent(&elm_step_allocator, &elm_state),
        );

        let updated_sub_port_names: std::collections::HashSet<&'static str> =
            elm_watch_sub(std::collections::HashSet::new(), &elm_sub);
        running_subs_handles = running_subs_handles
            .into_iter()
            .filter(|running_sub| {
                if updated_sub_port_names.contains(running_sub.elm_incoming_port_name) {
                    true
                } else {
                    running_sub.handle.abort();
                    false
                }
            })
            .collect();
        let new_subs_port_names =
            updated_sub_port_names
                .into_iter()
                .filter(|&updated_sub_port_name| {
                    !running_subs_handles.iter().any(|running_sub| {
                        running_sub.elm_incoming_port_name == updated_sub_port_name
                    })
                });
        for new_sub_port_name in new_subs_port_names {
            match watch_elm_port_incoming(new_sub_port_name) {
                Option::Some(new_sub_handle) => {
                    running_subs_handles.push(RunningSub {
                        elm_incoming_port_name: new_sub_port_name,
                        handle: new_sub_handle,
                    });
                }
                Option::None => {
                    println!("unknown incoming elm port name {new_sub_port_name}");
                }
            }
        }

        match futures::StreamExt::next(&mut running_subs_handles).await {
            Option::None => {
                break 'main_loop;
            }
            Option::Some(event_from_rust_to_elm_or_join_error) => {
                match event_from_rust_to_elm_or_join_error {
                    Result::Err(error) => {
                        println!("task failed {error}");
                    }
                    Result::Ok(event_from_rust_to_elm) => {
                        match elm_sub_associate_event_from_rust(
                            &elm_step_allocator,
                            &event_from_rust_to_elm,
                            &elm_sub,
                        ) {
                            Option::None => {
                                // failed to associate. This is expected when firing multiple subscriptions
                                // that succeed in short time but where one succeeding "invalidates" the others
                            }
                            Option::Some(elm_event) => {
                                let (elm_updated_state, elm_update_cmd) =
                                    (elm::main_main(&elm_step_allocator).update)(elm_event)(
                                        elm_main_state_from_persistent(
                                            &elm_step_allocator,
                                            &elm_state,
                                        ),
                                    );
                                elm_perform_cmd(elm_update_cmd);
                                elm_state = elm_main_state_to_persistent(elm_updated_state);
                            }
                        }
                    }
                }
            }
        };
    }
}

struct RunningSub {
    elm_incoming_port_name: &'static str,
    handle: tokio::task::JoinHandle<EventFromRustToElm>,
}
impl Future for RunningSub {
    type Output = Result<EventFromRustToElm, tokio::task::JoinError>;

    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context,
    ) -> std::task::Poll<Self::Output> {
        Future::poll(std::pin::Pin::new(&mut self.get_mut().handle), cx)
    }
}

enum EventFromRustToElm {
    StdInLineRead(String),
    Sleep1Second(u128),
}
fn elm_watch_sub<Event>(
    mut so_far: std::collections::HashSet<&'static str>,
    sub: &elm::PlatformSubSub<Event>,
) -> std::collections::HashSet<&'static str> {
    match sub {
        &elm::PlatformSubSub::PortIncoming(elm_port_name, _on_event) => {
            so_far.insert(elm_port_name);
        }
        elm::PlatformSubSub::Batch(sub_subs) => {
            for sub_sub in sub_subs.iter() {
                so_far = elm_watch_sub(so_far, sub_sub);
            }
        }
    }
    so_far
}
fn watch_elm_port_incoming(
    elm_port_name: &'static str,
) -> Option<tokio::task::JoinHandle<EventFromRustToElm>> {
    match elm_port_name {
        "portStdInReadLine" => Option::Some(tokio::task::spawn(async {
            let mut user_input: String = String::new();
            match tokio::io::AsyncBufReadExt::read_line(
                &mut tokio::io::BufReader::new(tokio::io::stdin()),
                &mut user_input,
            )
            .await
            {
                Result::Err(error) => {
                    println!("failed to read a line from standard input, code {error}");
                    todo!()
                }
                Result::Ok(_) => EventFromRustToElm::StdInLineRead(user_input.replace("\n", "")),
            }
        })),
        "portSleep1Second" => Option::Some(tokio::task::spawn(async {
            tokio::time::sleep(tokio::time::Duration::from_millis(1000)).await;
            match std::time::UNIX_EPOCH.elapsed() {
                Result::Err(error) => {
                    println!("couldn't determine system time: {error}");
                    todo!()
                }
                Result::Ok(current_posix_millis) => {
                    EventFromRustToElm::Sleep1Second(current_posix_millis.as_millis())
                }
            }
        })),
        _ => Option::None,
    }
}

fn elm_sub_associate_event_from_rust<'a, Event>(
    allocator: &'a bumpalo::Bump,
    event_from_rust_to_elm: &EventFromRustToElm,
    sub: &elm::PlatformSubSub<'a, Event>,
) -> Option<Event> {
    match sub {
        elm::PlatformSubSub::PortIncoming(port_name, on_event) => {
            if *port_name == event_from_rust_to_elm_to_origin_port_name(event_from_rust_to_elm) {
                Option::Some(on_event(match event_from_rust_to_elm {
                    EventFromRustToElm::StdInLineRead(event) => {
                        elm::JsonValue::String(allocator.alloc(event.clone()))
                    }
                    EventFromRustToElm::Sleep1Second(new_time_posix) => {
                        elm::JsonValue::Number(*new_time_posix as f64)
                    }
                }))
            } else {
                Option::None
            }
        }
        elm::PlatformSubSub::Batch(platform_sub_subs) => {
            platform_sub_subs.iter().find_map(|sub_sub| {
                elm_sub_associate_event_from_rust(allocator, event_from_rust_to_elm, sub_sub)
            })
        }
    }
}
fn event_from_rust_to_elm_to_origin_port_name(
    event_from_rust_to_elm: &EventFromRustToElm,
) -> &'static str {
    match event_from_rust_to_elm {
        EventFromRustToElm::StdInLineRead(_) => "portStdInReadLine",
        EventFromRustToElm::Sleep1Second(_) => "portSleep1Second",
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
                    let _ = std::io::Write::write_all(&mut std_out_lock, message.as_bytes());
                    // still put it on screen
                    let _ = std::io::Write::flush(&mut std_out_lock);
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
/// (that means no strings, collections or recursive choice `type`s)
/// you can replace all uses of ElmStatePersistent with elm::MainState
/// and remove the conversions
#[derive(Clone, PartialEq, Eq)]
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
