mod qp;

use qp::EditorApp;
use qp::EditorMessage;

fn main() -> Result<(), iced::Error> {
    iced::application("qp", EditorApp::update, EditorApp::view)
        .subscription(EditorApp::subscription)
        .run_with(|| {
            (
                EditorApp::new(),
                iced::Task::done(EditorMessage::OpenBuffer),
            )
        })
}
