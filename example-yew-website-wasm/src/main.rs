#![allow(non_shorthand_field_patterns)]
#![feature(allocator_api, btreemap_alloc)]
mod elm;

fn main() {
    yew::Renderer::<App>::new().render();
}

/// This is mostly a mirror of elm::MainState<'a>
/// except that what is references is replaced by persistent alternatives like Box, Vec, String etc.
///
/// While it might be a bit inconvenient to try to keep these
/// 2 representations in sync, rust's (record and enum) exhaustiveness checking
/// will at least remind you when they do :)
///
/// If you plan to only use persistent structures in your elm model
/// (that means no strings, lists or recursive choice `type`s)
/// you can replace all uses of ElmStatePersistent with elm::MainState
/// and remove the conversions
struct ElmStatePersistent {
    count: i64,
    mouse_x: i64,
    mouse_y: i64,
    mouse_trail: Vec<elm::GeneratedXY<i64, i64>>,
}
fn elm_state_to_persistent(temporary: elm::MainState) -> ElmStatePersistent {
    ElmStatePersistent {
        count: temporary.count,
        mouse_x: temporary.mouse_x,
        mouse_y: temporary.mouse_y,
        mouse_trail: temporary.mouse_trail.into_iter().collect::<Vec<_>>(),
    }
}
fn elm_state_from_persistent<'a>(
    allocator: &'a bumpalo::Bump,
    persistent: &ElmStatePersistent,
) -> elm::MainState<'a> {
    let ElmStatePersistent {
        count: count,
        mouse_x: mouse_x,
        mouse_y: mouse_y,
        mouse_trail: mouse_trail,
    } = persistent;
    elm::GeneratedCountMouseTrailMouseXMouseY {
        count: *count,
        mouse_x: *mouse_x,
        mouse_y: *mouse_y,
        mouse_trail: elm::double_ended_iterator_to_list(allocator, mouse_trail.iter().cloned()),
    }
}

pub struct App {
    elm_state: ElmStatePersistent,
}
pub enum AppEvent {
    DomEventFired {
        name: String,
        dom_path: Vec<usize>,
        web_sys_event: web_sys::Event,
    },
}
impl yew::Component for App {
    type Message = AppEvent;

    type Properties = ();

    fn create(_context: &yew::Context<Self>) -> Self {
        App {
            elm_state: elm_state_to_persistent(elm::main_initial_state(())),
        }
    }

    fn update(&mut self, _context: &yew::Context<Self>, event: Self::Message) -> bool {
        match event {
            AppEvent::DomEventFired {
                name: fired_event_name,
                dom_path: fire_target_dom_path,
                web_sys_event: web_sys_event,
            } => {
                let allocator: bumpalo::Bump = bumpalo::Bump::new();
                // lookup same dom path
                // then call its event handler
                // with the then-created event, create the new state
                let current_interface = elm::main_view(
                    &allocator,
                    elm_state_from_persistent(&allocator, &self.elm_state),
                );

                let maybe_target_modifiers = match elm_virtual_dom_lookup_dom_node_at_path(
                    &current_interface,
                    fire_target_dom_path.into_iter(),
                ) {
                    Option::None => Option::None,
                    Option::Some(dom_node_at_path) => match dom_node_at_path {
                        elm::VirtualDomNode::Text(_) => Option::None,
                        elm::VirtualDomNode::Element {
                            modifiers: modifiers,
                            ..
                        } => Option::Some(modifiers),
                        elm::VirtualDomNode::ElementKeyed {
                            modifiers: modifiers,
                            ..
                        } => Option::Some(modifiers),
                    },
                };
                match maybe_target_modifiers {
                    Option::None => {
                        // lookup _can_ correctly return None sometimes
                        // when it is de-synced. Is expected when rapid-firing events
                        // and can be safely ignored
                        println!("failed to associate dom target")
                    }
                    Option::Some(target_modifiers) => {
                        'associating_element_modifier: for modifier in target_modifiers.iter() {
                            match modifier {
                                elm::VirtualDomAttribute::ModifierEventListener {
                                    name: listened_for_event_name,
                                    handler: handler,
                                } => {
                                    if listened_for_event_name == &fired_event_name {
                                        let new_elm_event_or_error = match handler {
                                            elm::VirtualDomHandler::Custom(decoder) => {
                                                match (decoder.decode)(
                                                    web_sys_js_value_to_elm_json(
                                                        &allocator,
                                                        &web_sys_event,
                                                    ),
                                                ) {
                                                    Result::Ok(response) => {
                                                        if response.stop_propagation {
                                                            web_sys_event.cancel_bubble();
                                                        }
                                                        if response.prevent_default {
                                                            web_sys_event.prevent_default()
                                                        };
                                                        Result::Ok(response.message)
                                                    }
                                                    Result::Err(decode_error) => {
                                                        Result::Err(decode_error)
                                                    }
                                                }
                                            }
                                            elm::VirtualDomHandler::MayStopPropagation(decoder) => {
                                                match (decoder.decode)(
                                                    web_sys_js_value_to_elm_json(
                                                        &allocator,
                                                        &web_sys_event,
                                                    ),
                                                ) {
                                                    Result::Ok((
                                                        decoded_event,
                                                        stop_propagation,
                                                    )) => {
                                                        if stop_propagation {
                                                            web_sys_event.cancel_bubble();
                                                        }
                                                        Result::Ok(decoded_event)
                                                    }
                                                    Result::Err(decode_error) => {
                                                        Result::Err(decode_error)
                                                    }
                                                }
                                            }
                                            elm::VirtualDomHandler::MayPreventDefault(decoder) => {
                                                match (decoder.decode)(
                                                    web_sys_js_value_to_elm_json(
                                                        &allocator,
                                                        &web_sys_event,
                                                    ),
                                                ) {
                                                    Result::Ok((
                                                        decoded_event,
                                                        prevent_default,
                                                    )) => {
                                                        if prevent_default {
                                                            web_sys_event.prevent_default()
                                                        }
                                                        Result::Ok(decoded_event)
                                                    }
                                                    Result::Err(decode_error) => {
                                                        Result::Err(decode_error)
                                                    }
                                                }
                                            }
                                            elm::VirtualDomHandler::Normal(decoder) => {
                                                match (decoder.decode)(
                                                    web_sys_js_value_to_elm_json(
                                                        &allocator,
                                                        &web_sys_event,
                                                    ),
                                                ) {
                                                    Result::Ok(decoded_event) => {
                                                        Result::Ok(decoded_event)
                                                    }
                                                    Result::Err(decode_error) => {
                                                        Result::Err(decode_error)
                                                    }
                                                }
                                            }
                                        };
                                        match new_elm_event_or_error {
                                            Result::Err(decode_error) => {
                                                let mut error_string = String::new();
                                                elm::json_decode_error_to_string_help(
                                                    &decode_error,
                                                    String::new(),
                                                    &mut error_string,
                                                    0,
                                                );
                                                // just for debugging, should be silent
                                                web_sys::console::log_2(
                                                    &web_sys::js_sys::JsString::from(
                                                        "listener returned an error ".to_string()
                                                            + &error_string,
                                                    ),
                                                    &web_sys_event,
                                                );
                                            }
                                            Result::Ok(new_elm_event) => {
                                                let update_allocator = bumpalo::Bump::new();
                                                self.elm_state =
                                                    elm_state_to_persistent(elm::main_update(
                                                        &update_allocator,
                                                        new_elm_event,
                                                        elm_state_from_persistent(
                                                            &allocator,
                                                            &self.elm_state,
                                                        ),
                                                    ));
                                                // uncomment to debug
                                                // web_sys::console::log_1(
                                                //     &web_sys::js_sys::JsString::from(format!(
                                                //         "elm event {:?} → updated state {:?}",
                                                //         new_elm_event, self.elm_state
                                                //     )),
                                                // );
                                            }
                                        }
                                        break 'associating_element_modifier;
                                    }
                                }
                                elm::VirtualDomAttribute::ModifierAttribute { .. } => {}
                                elm::VirtualDomAttribute::ModifierStyle { .. } => {}
                                elm::VirtualDomAttribute::ModifierProperty { .. } => {}
                            }
                        }
                    }
                }
            }
        }
        true
    }

    fn view(&self, context: &yew::Context<Self>) -> yew::Html {
        let allocator: bumpalo::Bump = bumpalo::Bump::new();
        elm_dom_node_to_yew(
            context.link(),
            &mut Vec::new(),
            Option::None,
            &elm::main_view(
                &allocator,
                elm_state_from_persistent(&allocator, &self.elm_state),
            ),
        )
    }
}

fn elm_dom_node_to_yew<Event>(
    yew_scope: &yew::html::Scope<App>,
    dom_path: &mut Vec<usize>,
    maybe_key: Option<&str>,
    elm_dom_node: &elm::VirtualDomNode<Event>,
) -> yew::Html {
    match elm_dom_node {
        elm::VirtualDomNode::Text(text) => yew::Html::VText(yew::virtual_dom::VText::from(text)),
        &elm::VirtualDomNode::Element {
            tag: tag,
            namespace: maybe_namespace,
            subs: subs,
            modifiers: modifiers,
        } => {
            let mut vtag: yew::virtual_dom::VTag = yew::virtual_dom::VTag::new(tag.to_string());
            match maybe_namespace {
                Option::None => {}
                Option::Some(namespace) => {
                    vtag.add_attribute("xmlns", yew::AttrValue::from(namespace.to_string()));
                }
            }
            match maybe_key {
                Option::None => {}
                Option::Some(key) => vtag.key = Option::Some(yew::virtual_dom::Key::from(key)),
            }
            vtag.add_children(subs.into_iter().enumerate().map(|(sub_index, sub)| {
                dom_path.push(sub_index);
                let sub_yew_node = elm_dom_node_to_yew(yew_scope, dom_path, Option::None, sub);
                dom_path.pop();
                sub_yew_node
            }));
            yew_vtag_add_elm_virtual_dom_modifiers(yew_scope, dom_path, &mut vtag, modifiers);
            yew::Html::VTag(Box::new(vtag))
        }
        &elm::VirtualDomNode::ElementKeyed {
            tag: tag,
            namespace: _handled_by_yew,
            subs: subs,
            modifiers: modifiers,
        } => {
            let mut vtag: yew::virtual_dom::VTag = yew::virtual_dom::VTag::new(tag.to_string());
            match maybe_key {
                Option::None => {}
                Option::Some(key) => vtag.key = Option::Some(key.into()),
            }
            vtag.add_children(
                subs.into_iter()
                    .enumerate()
                    .map(|(sub_index, (sub_key, sub))| {
                        dom_path.push(sub_index);
                        let sub_yew_node =
                            elm_dom_node_to_yew(yew_scope, dom_path, Option::Some(sub_key), sub);
                        dom_path.pop();
                        sub_yew_node
                    }),
            );
            yew_vtag_add_elm_virtual_dom_modifiers(yew_scope, dom_path, &mut vtag, modifiers);
            yew::Html::VTag(Box::new(vtag))
        }
    }
}

fn yew_vtag_add_elm_virtual_dom_modifiers<Event>(
    yew_scope: &yew::html::Scope<App>,
    dom_path: &Vec<usize>,
    yew_vtag: &mut yew::virtual_dom::VTag,
    elm_virtual_dom_modifiers: &[elm::VirtualDomAttribute<Event>],
) {
    let styles: Vec<String> = elm_virtual_dom_modifiers
        .into_iter()
        .filter_map(|modifier| match modifier {
            elm::VirtualDomAttribute::ModifierStyle {
                key: key,
                value: value,
            } => Option::Some(format!("{key}:{value}")),
            _ => Option::None,
        })
        .collect::<Vec<_>>();
    if !styles.is_empty() {
        yew_vtag.add_attribute("style", styles.join(";"));
    }
    for modifier in elm_virtual_dom_modifiers.into_iter() {
        yew_vtag_add_elm_virtual_dom_modifier_except_style(yew_scope, dom_path, yew_vtag, modifier)
    }
}
fn yew_vtag_add_elm_virtual_dom_modifier_except_style<Event>(
    yew_scope: &yew::html::Scope<App>,
    dom_path: &Vec<usize>,
    yew_vtag: &mut yew::virtual_dom::VTag,
    elm_virtual_dom_modifier: &elm::VirtualDomAttribute<Event>,
) {
    match elm_virtual_dom_modifier {
        elm::VirtualDomAttribute::ModifierStyle { .. } => {}
        elm::VirtualDomAttribute::ModifierAttribute {
            namespace: _handled_by_yew,
            key: key,
            value: value,
        } => {
            yew_vtag.attributes.get_mut_index_map().insert(
                yew::AttrValue::from(key.to_string()),
                (
                    yew::AttrValue::from(value.to_string()),
                    yew::virtual_dom::ApplyAttributeAs::Attribute,
                ),
            );
        }
        elm::VirtualDomAttribute::ModifierProperty {
            key: key,
            value: value,
        } => {
            yew_vtag.attributes.get_mut_index_map().insert(
                yew::AttrValue::from(key.to_string()),
                (
                    match value {
                        elm::JsonValue::Null => yew::AttrValue::from("null"),
                        elm::JsonValue::Bool(bool) => yew::AttrValue::from(match bool {
                            true => "true",
                            false => "false",
                        }),
                        elm::JsonValue::Number(number) => yew::AttrValue::from(number.to_string()),
                        elm::JsonValue::String(str) => yew::AttrValue::from(str.to_string()),
                        elm::JsonValue::Array(_) => unimplemented!(),
                        elm::JsonValue::Object(_) => unimplemented!(),
                    },
                    yew::virtual_dom::ApplyAttributeAs::Property,
                ),
            );
        }
        elm::VirtualDomAttribute::ModifierEventListener {
            name: name,
            handler: handler,
        } => {
            let listener: std::rc::Rc<dyn yew::virtual_dom::Listener> =
                std::rc::Rc::new(YewRegisteredEventListener {
                    name: name.to_string(),
                    is_passive: match handler {
                        elm::VirtualDomHandler::Custom(_) => false,
                        elm::VirtualDomHandler::MayStopPropagation(_) => false,
                        elm::VirtualDomHandler::MayPreventDefault(_) => true,
                        elm::VirtualDomHandler::Normal(_) => true,
                    },
                    dom_path: dom_path.clone(),
                    yew_scope: yew_scope.clone(),
                });
            yew_vtag.add_listener(listener);
        }
    }
}
struct YewRegisteredEventListener {
    name: String,
    is_passive: bool,
    dom_path: Vec<usize>,
    yew_scope: yew::html::Scope<App>,
}

impl yew::virtual_dom::Listener for YewRegisteredEventListener {
    fn kind(&self) -> yew::virtual_dom::ListenerKind {
        yew::virtual_dom::ListenerKind::other(std::borrow::Cow::Owned(self.name.clone()))
    }
    fn handle(&self, event: web_sys::Event) {
        let name_owned = self.name.clone();
        let dom_path_owned = self.dom_path.clone();
        self.yew_scope
            .callback(move |()| AppEvent::DomEventFired {
                dom_path: dom_path_owned.clone(),
                name: name_owned.clone(),
                web_sys_event: event.clone(),
            })
            .emit(());
    }
    fn passive(&self) -> bool {
        self.is_passive
    }
}

fn elm_virtual_dom_lookup_dom_node_at_path<'a, Event: Clone>(
    elm_virtual_dom_node: &'a elm::VirtualDomNode<'a, Event>,
    mut path: impl Iterator<Item = usize>, // consider &mut iterator
) -> Option<&'a elm::VirtualDomNode<'a, Event>> {
    match path.next() {
        Option::None => Option::Some(elm_virtual_dom_node),
        Option::Some(sub_index) => match elm_virtual_dom_node {
            elm::VirtualDomNode::Text(_) => Option::None,
            elm::VirtualDomNode::Element { subs: subs, .. } => match subs.get(sub_index) {
                Option::None => Option::None,
                Option::Some(sub_node) => elm_virtual_dom_lookup_dom_node_at_path(sub_node, path),
            },
            elm::VirtualDomNode::ElementKeyed { subs: subs, .. } => match subs.get(sub_index) {
                Option::None => Option::None,
                Option::Some((_sub_key, sub_node)) => {
                    elm_virtual_dom_lookup_dom_node_at_path(sub_node, path)
                }
            },
        },
    }
}
fn web_sys_js_value_to_elm_json<'a>(
    allocator: &'a bumpalo::Bump,
    web_sys_js_value: &web_sys::wasm_bindgen::JsValue,
) -> elm::JsonValue<'a> {
    // shows that elm::JsonValue should probably be lazy internally for full array and object
    if web_sys_js_value.is_null() {
        elm::JsonValue::Null
    } else {
        match web_sys_js_value.as_bool() {
            Option::Some(bool) => elm::JsonValue::Bool(bool),
            Option::None => {
                match web_sys_js_value.as_f64() {
                    Option::Some(number) => elm::JsonValue::Number(number),
                    Option::None => {
                        match web_sys_js_value.as_string() {
                            Option::Some(string) => elm::JsonValue::String(allocator.alloc(string)),
                            Option::None => {
                                if web_sys_js_value.is_array() {
                                    elm::JsonValue::Array(
                                        allocator.alloc(
                                            web_sys::js_sys::Array::from(&web_sys_js_value)
                                                .iter()
                                                .map(|element| {
                                                    web_sys_js_value_to_elm_json(
                                                        allocator, &element,
                                                    )
                                                })
                                                .collect::<Vec<_>>(),
                                        ),
                                    )
                                } else {
                                    match web_sys::js_sys::Object::try_from(&web_sys_js_value) {
                                        Option::Some(js_object) => {
                                            elm::JsonValue::Object(
                                                allocator.alloc(
                                                    web_sys::js_sys::Object::keys(
                                                        &web_sys::js_sys::Object::get_prototype_of(
                                                            js_object,
                                                        ),
                                                    )
                                                    // sanity check: all these do _not_ work:
                                                    // Object::entries or
                                                    // Object::keys or
                                                    // Reflect::own_keys or
                                                    // Reflect::apply(
                                                    //     &Function::from(eval("(function(o) { return Object.keys(o); })")?,
                                                    //     &global(),
                                                    //     &Array::from_iter(std::iter::once(js_object))
                                                    // ) or
                                                    // JSON::stringify
                                                    // and even when trying to remove proxies with
                                                    // Object.assign({}, _)
                                                    // They return a weird
                                                    // { __yew_subtree_cache_key, __yew_subtree_id, trusted }
                                                    // I tried a whole bunch of stuff but couldn't work out
                                                    // why this happens (it only doesn't with console.log and getPrototypeOf ??).
                                                    // If you know more, PLEASE tell me :)
                                                    .into_iter()
                                                    .filter_map(|key| {
                                                        let maybe_key = key.as_string();
                                                        let maybe_value =
                                                            web_sys::js_sys::Reflect::get(
                                                                js_object, &key,
                                                            )
                                                            .ok();
                                                        match maybe_key.zip(maybe_value) {
                                                            Option::None => Option::None,
                                                            Option::Some((key, value)) => {
                                                                Option::Some((
                                                                    allocator.alloc(key).as_str(),
                                                                    web_sys_js_value_to_elm_json(
                                                                        allocator, &value,
                                                                    ),
                                                                ))
                                                            }
                                                        }
                                                    })
                                                    .collect::<std::collections::BTreeMap<&str, _>>(
                                                    ),
                                                ),
                                            )
                                        }
                                        Option::None =>
                                        // maybe cleaner to return Option::None
                                        {
                                            elm::JsonValue::Null
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
