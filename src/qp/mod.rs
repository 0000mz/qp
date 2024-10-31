use iced::advanced::layout;
use iced::advanced::renderer;
use iced::advanced::widget::Tree;
use iced::advanced::widget::Widget;
use iced::mouse;
use iced::{Color, Element, Length, Rectangle, Size};

pub struct CodeSpace<'a> {
    width: f32,
    height: f32,
    buffer_iter: BufferContentIterator<'a>,
}

impl<'a> CodeSpace<'a> {
    const CELL_WIDTH: usize = 10;
    const CELL_HEIGHT: usize = 14;
    const TEXT_SIZE: usize = 14;

    fn new(width: f32, height: f32, buffer_iter: BufferContentIterator<'a>) -> Self {
        CodeSpace {
            width,
            height,
            buffer_iter,
        }
    }

    fn make_text(c: char) -> iced::advanced::Text {
        iced::advanced::Text {
            content: String::from(c),
            bounds: Size {
                width: CodeSpace::CELL_WIDTH as f32,
                height: CodeSpace::CELL_HEIGHT as f32,
            },
            size: iced::Pixels(CodeSpace::TEXT_SIZE as f32),
            line_height: iced::advanced::text::LineHeight::default(),
            font: iced::Font::MONOSPACE,
            horizontal_alignment: iced::Center.into(),
            vertical_alignment: iced::Center.into(),
            shaping: iced::advanced::text::Shaping::Basic,
            wrapping: iced::advanced::text::Wrapping::None,
        }
    }

    fn cell_to_pixel_space(row: usize, col: usize) -> (f32, f32) {
        let x: f32 = (col as f32) * (CodeSpace::CELL_WIDTH as f32);
        let y: f32 = (row as f32) * (CodeSpace::CELL_HEIGHT as f32);
        (x, y)
    }

    fn draw_char_cell<Renderer: iced::advanced::text::Renderer<Font = iced::Font>>(
        &self,
        renderer: &mut Renderer,
        row: usize,
        col: usize,
        c: char,
    ) {
        let (x, y) = CodeSpace::cell_to_pixel_space(row, col);
        let cellw: f32 = CodeSpace::CELL_WIDTH as f32;
        let cellh: f32 = CodeSpace::CELL_HEIGHT as f32;
        let bounds = Rectangle {
            x,
            y,
            width: cellw,
            height: cellh,
        };
        renderer.fill_text(
            CodeSpace::make_text(c),
            iced::Point {
                x: x + ((CodeSpace::CELL_WIDTH as f32) / 2.0),
                y: y + ((CodeSpace::CELL_HEIGHT as f32) / 2.0),
            },
            Color::WHITE,
            bounds,
        );
    }
}

impl<Message, Theme, Renderer> Widget<Message, Theme, Renderer> for CodeSpace<'_>
where
    Renderer: renderer::Renderer + iced::advanced::text::Renderer<Font = iced::Font>,
{
    fn size(&self) -> Size<iced::Length> {
        Size {
            width: Length::Shrink,
            height: Length::Shrink,
        }
    }

    fn layout(
        &self,
        _tree: &mut Tree,
        _renderer: &Renderer,
        _limits: &layout::Limits,
    ) -> layout::Node {
        layout::Node::new(Size::new(self.width, self.height))
    }

    fn draw(
        &self,
        _state: &Tree,
        renderer: &mut Renderer,
        _theme: &Theme,
        _stype: &renderer::Style,
        layout: layout::Layout<'_>,
        _cursor: mouse::Cursor,
        _viewport: &Rectangle,
    ) {
        renderer.fill_quad(
            renderer::Quad {
                bounds: layout.bounds(),
                ..renderer::Quad::default()
            },
            Color::from_rgb(0.0, 0.0, 0.0), // Color::BLACK,
        );

        let cellw: f32 = CodeSpace::CELL_WIDTH as f32;
        let cellh: f32 = CodeSpace::CELL_HEIGHT as f32;

        // Draw the line numbers.
        let nb_digits: usize = 3;
        let nb_lines: usize = (self.height / cellh) as usize;
        for curr_line in 0..nb_lines {
            let mut l = curr_line;
            let mut digits = nb_digits;
            while l > 0 {
                let base_10_n = l % 10;
                self.draw_char_cell(
                    renderer,
                    curr_line - 1,
                    digits - 1,
                    char::from_u32((48 + base_10_n) as u32).unwrap(),
                );
                l /= 10;
                digits -= 1;
            }
            while digits > 0 {
                self.draw_char_cell(renderer, curr_line, digits - 1, ' ');
                digits -= 1;
            }
        }

        for cell in self.buffer_iter.into_iter() {
            let col = (cell.col + (nb_digits as u32) + 1) as usize;
            let row = cell.row as usize;
            let (x, y) = CodeSpace::cell_to_pixel_space(row, col);

            let bounds = Rectangle {
                x,
                y,
                width: cellw,
                height: cellh,
            };
            {
                const SPACING: f32 = 1.0;

                let mut debug_bounds = bounds.clone();
                debug_bounds.x += SPACING / 2.0;
                debug_bounds.y += SPACING / 2.0;
                debug_bounds.width -= SPACING;
                debug_bounds.height -= SPACING;

                renderer.fill_quad(
                    renderer::Quad {
                        bounds: debug_bounds,
                        ..renderer::Quad::default()
                    },
                    Color::from_rgb(0.5, 0.0, 0.0),
                );
            }
            self.draw_char_cell(renderer, row, col, cell.c);
        }
    }
}

impl<'a, Message, Theme, Renderer> From<CodeSpace<'a>> for Element<'a, Message, Theme, Renderer>
where
    Renderer: renderer::Renderer + iced::advanced::text::Renderer<Font = iced::Font>,
{
    fn from(cs: CodeSpace<'a>) -> Self {
        Self::new(cs)
    }
}

#[derive(Debug)]
struct Cursor(usize, usize);

struct BufferContent {
    data: Vec<String>,
    cursor: Cursor,
}

// TODO: Rename this to BufferMessage
#[derive(Debug, Clone, Copy)]
enum BufferMessage<AsciiCode = u8> {
    AddCharacter(AsciiCode),
    RemoveCharacter,
    CommitAction(iced::keyboard::key::Named),
}

#[derive(Debug)]
enum KeyModifier {
    SHIFT,
    CTRL,
}

#[derive(Copy, Clone)]
struct BufferContentIterator<'a> {
    row: usize,
    col: usize,
    dev_box_ref: &'a BufferContent,
}

impl<'a> BufferContentIterator<'a> {
    fn new(buffer: &'a BufferContent) -> Self {
        BufferContentIterator {
            row: 0,
            col: 0,
            dev_box_ref: buffer,
        }
    }
}

struct BufferContentCellInfo {
    row: u32,
    col: u32,
    c: char,
}

impl Iterator for BufferContentIterator<'_> {
    type Item = BufferContentCellInfo;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            loop {
                let ch = self.dev_box_ref.char_at(self.row, self.col);
                if ch.is_none() {
                    if self.col == 0 {
                        // No more data left. The first cell in this row is empty.
                        return None;
                    } else {
                        self.col = 0;
                        break;
                    }
                }
                self.col += 1;
                return Some(BufferContentCellInfo {
                    row: self.row as u32,
                    col: (self.col - 1) as u32, // undo increment from above.
                    c: ch.unwrap(),
                });
            }
            self.row += 1
        }
    }
}

impl BufferContent {
    fn new() -> Self {
        BufferContent {
            data: vec![String::new()],
            cursor: Cursor(0, 0),
        }
    }

    fn char_at(&self, row: usize, col: usize) -> Option<char> {
        if row >= self.data.len() {
            return None;
        }
        let line: &String = &self.data[row];
        if col >= line.len() {
            return None;
        }
        line.chars().nth(col)
    }

    fn handle_key_event(
        &self,
        key: iced::keyboard::Key,
        modified_key: iced::keyboard::Key,
        modifiers: iced::keyboard::Modifiers,
    ) -> Option<BufferMessage> {
        let mut modifier_enum: Option<KeyModifier> = None;
        if modifiers.shift() {
            modifier_enum = Some(KeyModifier::SHIFT);
        } else if modifiers.control() {
            modifier_enum = Some(KeyModifier::CTRL);
        }
        BufferContent::handle_keypress(key, modified_key, modifier_enum)
    }

    fn ensure_cursor(&mut self) {
        let Cursor(row_i, col_i) = self.cursor;
        if self.data.len() == 0 {
            self.data.push(String::new());
        }
        let new_row_i = std::cmp::min(row_i, self.data.len() - 1);
        self.cursor = Cursor(new_row_i, col_i);
    }

    fn update(&mut self, message: BufferMessage) -> iced::Task<BufferMessage> {
        match message {
            BufferMessage::AddCharacter(ascii_code) => {
                let Cursor(row_i, col_i) = self.cursor;
                self.ensure_cursor();

                let row = &self.data[row_i];
                let (left, right) = row.split_at(col_i);

                let mut new_row = String::from(left);
                new_row.push(ascii_code as char);
                new_row += right;

                self.data[row_i] = new_row;
                self.cursor = Cursor(row_i, col_i + 1);
            }
            BufferMessage::RemoveCharacter => {
                let Cursor(row_i, col_i) = self.cursor;
                let row = &self.data[row_i];
                if row.len() > 0 {
                    let (new_row_ref, _) = row.split_at(row.len() - 1);
                    self.data[row_i] = String::from(new_row_ref);
                    self.cursor = Cursor(row_i, col_i - 1);
                }
            }
            BufferMessage::CommitAction(action) => match action {
                iced::keyboard::key::Named::Enter => {
                    self.ensure_cursor();
                    let Cursor(row_i, _) = self.cursor;
                    let (upper, lower) = self.data.split_at(row_i + 1);

                    let mut new_data = Vec::from(upper);
                    new_data.push(String::new());
                    new_data.append(&mut Vec::from(lower));

                    self.data = new_data;
                    self.cursor = Cursor(row_i + 1, 0);
                }
                _ => {}
            },
        }
        iced::Task::none()
    }

    fn handle_ascii_keypress(key: char, modifier: Option<KeyModifier>) -> Option<BufferMessage> {
        match (key, modifier) {
            (ch, None) => Some(BufferMessage::AddCharacter(ch as u8)),
            (ch, Some(KeyModifier::SHIFT)) => {
                let ascii_code = ch as u8;
                if ascii_code < 97 || ascii_code > 122 {
                    // add non-alphabet characters as is.
                    Some(BufferMessage::AddCharacter(ascii_code))
                } else {
                    // else, capitalize the alphabet character..
                    Some(BufferMessage::AddCharacter(ascii_code - 32))
                }
            }
            _ => None,
        }
    }

    fn handle_keypress(
        key: iced::keyboard::Key,
        modified_key: iced::keyboard::Key,
        modifier: Option<KeyModifier>,
    ) -> Option<BufferMessage> {
        match (key, modified_key, modifier) {
            (_, iced::keyboard::Key::Character(c), m) => {
                // TODO: Lock the debug logs behind some flag.
                println!("DBG Handle character: {:?} {:?}", c, m);
                let chars = c.chars().next().unwrap();
                BufferContent::handle_ascii_keypress(chars.to_ascii_lowercase(), m)
            }
            (iced::keyboard::Key::Named(named), _, _) => {
                println!("DBG Handle named: {:?}", named);
                match named {
                    iced::keyboard::key::Named::Backspace => Some(BufferMessage::RemoveCharacter),
                    iced::keyboard::key::Named::Enter => Some(BufferMessage::CommitAction(
                        iced::keyboard::key::Named::Enter,
                    )),
                    iced::keyboard::key::Named::Space => {
                        Some(BufferMessage::AddCharacter(' ' as u8))
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn view(&self) -> iced::Element<'_, EditorMessage> {
        let width: f32 = 1000.0;
        let height: f32 = 800.0;

        iced::widget::container(CodeSpace::new(
            width,
            height,
            BufferContentIterator::new(self),
        ))
        .into()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum EditorMessage<Key = iced::keyboard::Key, Modifier = iced::keyboard::Modifiers> {
    // Open an empty buffer in the editor.
    OpenBuffer,
    HandleKeys(Key, Key, Modifier),
    ProcessBufferEvent(BufferMessage),
}

pub struct EditorApp {
    buffers: Vec<BufferContent>,
    active_buffer: Option<usize>,
}

impl EditorApp {
    pub fn new() -> Self {
        EditorApp {
            buffers: vec![],
            active_buffer: None,
        }
    }

    pub fn view(&self) -> iced::Element<'_, EditorMessage> {
        if let Some(buffer_idx) = self.active_buffer {
            self.buffers[buffer_idx].view().into()
        } else {
            iced::widget::text("editor").into()
        }
    }

    pub fn subscription(&self) -> iced::Subscription<EditorMessage> {
        iced::keyboard::on_key_press_ext(|key, modified_key, modifiers| {
            Some(EditorMessage::HandleKeys(key, modified_key, modifiers))
        })
    }

    pub fn update(&mut self, message: EditorMessage) -> iced::Task<EditorMessage> {
        match message {
            EditorMessage::OpenBuffer => {
                self.buffers.push(BufferContent::new());
                if let None = self.active_buffer {
                    self.active_buffer = Some(self.buffers.len() - 1);
                }
                iced::Task::none()
            }
            EditorMessage::HandleKeys(key, modified_key, modifier) => {
                if let Some(buffer_idx) = self.active_buffer {
                    match self.buffers[buffer_idx].handle_key_event(key, modified_key, modifier) {
                        Some(msg) => {
                            return iced::Task::done(EditorMessage::<
                                iced::keyboard::Key,
                                iced::keyboard::Modifiers,
                            >::ProcessBufferEvent(
                                msg
                            ));
                        }
                        _ => {}
                    }
                }
                return iced::Task::none();
            }
            EditorMessage::ProcessBufferEvent(buffer_message) => {
                if let Some(buffer_idx) = self.active_buffer {
                    self.buffers[buffer_idx]
                        .update(buffer_message)
                        .map(|buffer_resp| EditorMessage::<iced::keyboard::Key, iced::keyboard::Modifiers>::ProcessBufferEvent(buffer_resp))
                } else {
                    iced::Task::none()
                }
            }
        }
    }
}
