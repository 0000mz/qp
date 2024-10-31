mod qp;

use qp::EditorApp;
use qp::EditorMessage;

fn main() -> Result<(), iced::Error> {
    let window_size = iced::Size::new(1000.0, 400.0);
    let window_settings = iced::window::Settings {
        size: window_size,
        ..iced::window::Settings::default()
    };

    iced::application("qp", EditorApp::update, EditorApp::view)
        .window(window_settings)
        .subscription(EditorApp::subscription)
        .run_with(move || {
            (
                EditorApp::new(),
                iced::Task::done(EditorMessage::OpenPanel(window_size)),
            )
        })
}
