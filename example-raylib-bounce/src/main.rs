#![feature(allocator_api, btreemap_alloc)]
mod elm;

fn main() {
    let ((mut raylib_handle, raylib_thread), mut elm_state) = {
        let elm_initialized = elm::main_init_window();
        (
            raylib::init()
                .size(
                    elm_initialized.window_width as i32,
                    elm_initialized.window_height as i32,
                )
                .title(&elm_initialized.window_title.to_string())
                .build(),
            elm_initialized.state,
        )
    };
    // check if this limits the throughput on effects etc, if yes, replace
    raylib::prelude::RaylibHandle::set_target_fps(&mut raylib_handle, 60);
    'main_loop: loop {
        let allocator_for_interface: bumpalo::Bump = bumpalo::Bump::new();
        'interface_handling: for interface in
            elm::main_state_to_interface(&allocator_for_interface, elm_state).into_iter()
        {
            match interface {
                elm::MainInterface::InterfaceOnQuit(construct_new_state) => {
                    if raylib_handle.window_should_close() {
                        elm_state = construct_new_state(());
                        break 'interface_handling;
                    }
                }
                elm::MainInterface::InterfaceQuit => {
                    break 'main_loop;
                }
                elm::MainInterface::InterfaceOnSimulationTick(construct_new_state) => {
                    elm_state = construct_new_state(());
                    break 'interface_handling;
                }
                elm::MainInterface::InterfaceRender(to_render) => {
                    let mut raylib_renderer: raylib::prelude::RaylibDrawHandle =
                        raylib_handle.begin_drawing(&raylib_thread);
                    raylib::prelude::RaylibDraw::clear_background(
                        &mut raylib_renderer,
                        elm_color_to_raylib(to_render.clear_color),
                    );
                    for element_to_render in to_render.elements.into_iter() {
                        match element_to_render {
                            elm::MainElementToRender::FilledRectangleToRender(rectangle) => {
                                raylib::prelude::RaylibDraw::draw_rectangle(
                                    &mut raylib_renderer,
                                    rectangle.left as i32,
                                    rectangle.top as i32,
                                    rectangle.width as i32,
                                    rectangle.height as i32,
                                    elm_color_to_raylib(rectangle.color),
                                );
                            }
                        }
                    }
                }
            }
        }
    }
}

fn elm_color_to_raylib(elm_color: elm::ColorColor) -> raylib::prelude::Color {
    let components = elm::color_to_rgba(elm_color);
    raylib::prelude::Color::new(
        (components.red * 255.0) as u8,
        (components.green * 255.0) as u8,
        (components.blue * 255.0) as u8,
        (components.alpha * 255.0) as u8,
    )
}
