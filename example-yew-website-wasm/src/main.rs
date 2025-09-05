#![allow(non_shorthand_field_patterns)]
mod elm;

fn main() {
    yew::Renderer::<App>::new().render();
}

pub struct App {
    elm_state: elm::MainState,
}
pub enum AppEvent {
    EventFired {
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
            elm_state: elm::main_initial_state,
        }
    }

    fn update(&mut self, _context: &yew::Context<Self>, event: Self::Message) -> bool {
        match event {
            AppEvent::EventFired {
                name: fired_event_name,
                dom_path: fire_target_dom_path,
                web_sys_event: web_sys_event,
            } => {
                let allocator: bumpalo::Bump = bumpalo::Bump::new();
                // lookup same dom path
                // then call its event handler
                // with the then-created event, create the new state
                let current_interface = elm::main_view(&allocator, self.elm_state.clone());

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
                                                self.elm_state =
                                                    elm::main_update(new_elm_event, self.elm_state);
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
            Vec::new(),
            Option::None,
            &elm::main_view(&allocator, self.elm_state.clone()),
        )
    }
}

fn elm_dom_node_to_yew<Event>(
    yew_scope: &yew::html::Scope<App>,
    dom_path: Vec<usize>,
    maybe_key: Option<&str>,
    elm_dom_node: &elm::VirtualDomNode<Event>,
) -> yew::Html {
    match elm_dom_node {
        elm::VirtualDomNode::Text(text) => yew::Html::VText(yew::virtual_dom::VText::from(text)),
        &elm::VirtualDomNode::Element {
            tag: tag,
            namespace: namespace,
            subs: subs,
            modifiers: modifiers,
        } => {
            let mut vtag: yew::virtual_dom::VTag =
                yew::virtual_dom::VTag::new(namespaced(namespace, tag));
            match maybe_key {
                Option::None => {}
                Option::Some(key) => vtag.key = Option::Some(key.into()),
            }
            vtag.add_children(subs.into_iter().enumerate().map(|(sub_index, sub)| {
                let mut sub_dom_path =
                    // inefficient
                    dom_path.clone();
                sub_dom_path.push(sub_index);
                elm_dom_node_to_yew(yew_scope, sub_dom_path, Option::None, sub)
            }));
            yew_vtag_add_elm_virtual_dom_modifiers(yew_scope, dom_path, &mut vtag, modifiers);
            yew::Html::VTag(Box::new(vtag))
        }
        &elm::VirtualDomNode::ElementKeyed {
            tag,
            namespace,
            subs,
            modifiers,
        } => {
            let mut vtag: yew::virtual_dom::VTag =
                yew::virtual_dom::VTag::new(namespaced(namespace, tag));
            match maybe_key {
                Option::None => {}
                Option::Some(key) => vtag.key = Option::Some(key.into()),
            }
            vtag.add_children(
                subs.into_iter()
                    .enumerate()
                    .map(|(sub_index, (sub_key, sub))| {
                        let mut sub_dom_path =
                            // inefficient
                            dom_path.clone();
                        sub_dom_path.push(sub_index);
                        elm_dom_node_to_yew(yew_scope, sub_dom_path, Option::Some(sub_key), sub)
                    }),
            );
            yew_vtag_add_elm_virtual_dom_modifiers(
                yew_scope,
                dom_path.clone(),
                &mut vtag,
                modifiers,
            );
            yew::Html::VTag(Box::new(vtag))
        }
    }
}

fn yew_vtag_add_elm_virtual_dom_modifiers<Event>(
    yew_scope: &yew::html::Scope<App>,
    dom_path: Vec<usize>,
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
        yew_vtag_add_elm_virtual_dom_modifier_except_style(
            yew_scope,
            // inefficient
            dom_path.clone(),
            yew_vtag,
            modifier,
        )
    }
}
fn yew_vtag_add_elm_virtual_dom_modifier_except_style<Event>(
    yew_scope: &yew::html::Scope<App>,
    dom_path: Vec<usize>,
    yew_vtag: &mut yew::virtual_dom::VTag,
    elm_virtual_dom_modifier: &elm::VirtualDomAttribute<Event>,
) {
    match elm_virtual_dom_modifier {
        elm::VirtualDomAttribute::ModifierStyle { .. } => {}
        elm::VirtualDomAttribute::ModifierAttribute {
            namespace,
            key,
            value,
        } => {
            yew_vtag.attributes.get_mut_index_map().insert(
                yew::AttrValue::from(namespaced(*namespace, key)),
                (
                    yew::AttrValue::from(value.to_string()),
                    yew::virtual_dom::ApplyAttributeAs::Property,
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
                    yew::AttrValue::from(match value {
                        elm::JsonValue::Null => "null".to_string(),
                        elm::JsonValue::Bool(bool) => bool.to_string(),
                        elm::JsonValue::Number(number) => number.to_string(),
                        elm::JsonValue::String(str) => str.to_string(),
                        elm::JsonValue::Array(_) => unimplemented!(),
                        elm::JsonValue::Object(_) => unimplemented!(),
                    }),
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
                    dom_path: dom_path,
                    yew_scope: yew_scope.clone(),
                });
            yew_vtag.add_listener(listener);
        }
    }
}
struct YewRegisteredEventListener {
    name: String, // consider Rc<String>
    is_passive: bool,
    dom_path: Vec<usize>, // consider Rc<Vec<usize>>
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
            .callback(move |()| AppEvent::EventFired {
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
            elm::VirtualDomNode::Element { subs: subs, .. } => subs.get(sub_index),
            elm::VirtualDomNode::ElementKeyed { subs: subs, .. } => {
                subs.get(sub_index).map(|(_sub_key, sub_node)| sub_node)
            }
        },
    }
}

fn namespaced(namespace: Option<&str>, name: &str) -> String {
    match namespace {
        Option::None => name.to_string(),
        Option::Some(namespace) => format!("{namespace}:{name}"),
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
