use iced::advanced::layout;
use iced::advanced::renderer;
use iced::advanced::widget::Tree;
use iced::advanced::widget::Widget;
use iced::mouse;
use iced::{Color, Element, Length, Rectangle, Size};

trait Component<Message> {
    // TODO: Similar to how the iced renderer is able to section out parts of
    // the trait functions based on what type of renderer it is, section out
    // this component based on which one can do what. i.e. which components
    // process key events and so on.
    fn handle_key_event(
        &self,
        key: iced::keyboard::Key,
        modified_key: iced::keyboard::Key,
        modifiers: iced::keyboard::Modifiers,
    ) -> Option<Message>;

    fn update(&mut self, message: Message) -> iced::Task<Message>;

    fn view(&self) -> iced::Element<'_, EditorMessage>;
}

struct GridSpaceUtil {}
impl GridSpaceUtil {
    const CELL_WIDTH: usize = 10;
    const CELL_HEIGHT: usize = 14;
    const TEXT_SIZE: usize = 14;

    fn cell_to_pixel_space(row: usize, col: usize) -> (f32, f32) {
        let x: f32 = (col as f32) * (GridSpaceUtil::CELL_WIDTH as f32);
        let y: f32 = (row as f32) * (GridSpaceUtil::CELL_HEIGHT as f32);
        (x, y)
    }
}

pub struct StatusLineRender {
    x: f32,
    y: f32,
    width: f32,
    height: f32,
    command: String,
}

impl StatusLineRender {
    fn new(x: f32, y: f32, width: f32, height: f32, command: String) -> Self {
        StatusLineRender {
            x,
            y,
            width,
            height,
            command,
        }
    }
}

impl<Message, Theme, Renderer> Widget<Message, Theme, Renderer> for StatusLineRender
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
        _layout: layout::Layout<'_>,
        _cursor: mouse::Cursor,
        _viewport: &Rectangle,
    ) {
        let bounds = Rectangle {
            x: self.x,
            y: self.y,
            width: self.width,
            height: self.height,
        };
        renderer.fill_quad(
            renderer::Quad {
                bounds,
                ..renderer::Quad::default()
            },
            Color::from_rgb(0.0, 0.0, 0.5),
        );

        let mode_str = if self.command.len() == 0 {
            String::from("NORMAL")
        } else {
            self.command.clone()
        };
        let mode_str_len = mode_str.len();
        let mode_text = iced::advanced::Text {
            content: mode_str,
            bounds: Size {
                width: (GridSpaceUtil::CELL_WIDTH * mode_str_len) as f32,
                height: GridSpaceUtil::CELL_HEIGHT as f32,
            },
            size: iced::Pixels(GridSpaceUtil::TEXT_SIZE as f32),
            line_height: iced::advanced::text::LineHeight::default(),
            font: iced::Font::MONOSPACE,
            horizontal_alignment: iced::Left,
            vertical_alignment: iced::Top,
            shaping: iced::advanced::text::Shaping::Basic,
            wrapping: iced::advanced::text::Wrapping::None,
        };

        renderer.fill_text(
            mode_text,
            iced::Point {
                x: self.x,
                y: self.y,
            },
            Color::WHITE,
            bounds,
        );
    }
}

impl<'a, Message, Theme, Renderer> From<StatusLineRender> for Element<'a, Message, Theme, Renderer>
where
    Renderer: renderer::Renderer + iced::advanced::text::Renderer<Font = iced::Font>,
{
    fn from(el: StatusLineRender) -> Self {
        Self::new(el)
    }
}

pub struct CodeSpace<'a> {
    width: f32,
    height: f32,
    buffer_iter: BufferContentIterator<'a>,
    cursor: &'a Cursor,
}

impl<'a> CodeSpace<'a> {
    // The number of columns that should be allocated for the number line.
    const LINE_NB_DIGITS: usize = 3;

    fn new(
        width: f32,
        height: f32,
        buffer_iter: BufferContentIterator<'a>,
        cursor: &'a Cursor,
    ) -> Self {
        CodeSpace {
            width,
            height,
            buffer_iter,
            cursor,
        }
    }

    fn make_text(c: char) -> iced::advanced::Text {
        iced::advanced::Text {
            content: String::from(c),
            bounds: Size {
                width: GridSpaceUtil::CELL_WIDTH as f32,
                height: GridSpaceUtil::CELL_HEIGHT as f32,
            },
            size: iced::Pixels(GridSpaceUtil::TEXT_SIZE as f32),
            line_height: iced::advanced::text::LineHeight::default(),
            font: iced::Font::MONOSPACE,
            horizontal_alignment: iced::Center.into(),
            vertical_alignment: iced::Center.into(),
            shaping: iced::advanced::text::Shaping::Basic,
            wrapping: iced::advanced::text::Wrapping::None,
        }
    }

    fn draw_char_cell<Renderer: iced::advanced::text::Renderer<Font = iced::Font>>(
        &self,
        renderer: &mut Renderer,
        row: usize,
        col: usize,
        c: char,
    ) {
        let (x, y) = GridSpaceUtil::cell_to_pixel_space(row, col);
        let cellw: f32 = GridSpaceUtil::CELL_WIDTH as f32;
        let cellh: f32 = GridSpaceUtil::CELL_HEIGHT as f32;
        let bounds = Rectangle {
            x,
            y,
            width: cellw,
            height: cellh,
        };
        renderer.fill_text(
            CodeSpace::make_text(c),
            iced::Point {
                x: x + ((GridSpaceUtil::CELL_WIDTH as f32) / 2.0),
                y: y + ((GridSpaceUtil::CELL_HEIGHT as f32) / 2.0),
            },
            Color::WHITE,
            bounds,
        );
    }

    // The number line takes up a certain # of columns. This returns the
    // row,col that should be used after taking into consideration the space
    // that the number line takes.
    fn nb_ofsetted_row_cols(row: usize, col: usize) -> (usize, usize) {
        let new_col = col + CodeSpace::LINE_NB_DIGITS + 1;
        (row, new_col)
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
            Color::from_rgb(0.2, 0.2, 0.2),
        );

        let cellw: f32 = GridSpaceUtil::CELL_WIDTH as f32;
        let cellh: f32 = GridSpaceUtil::CELL_HEIGHT as f32;

        // Draw the line numbers.
        let nb_lines: usize = ((self.height / cellh) as usize) + 1;
        for curr_line in 0..nb_lines {
            let mut l = curr_line;
            let mut digits = CodeSpace::LINE_NB_DIGITS;
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
            let (row, col) = CodeSpace::nb_ofsetted_row_cols(cell.row as usize, cell.col as usize);
            let (x, y) = GridSpaceUtil::cell_to_pixel_space(row, col);

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

        // Draw the cursor
        {
            let (row, col) = CodeSpace::nb_ofsetted_row_cols(self.cursor.row, self.cursor.col);
            let (cursor_x, cursor_y) = GridSpaceUtil::cell_to_pixel_space(row, col);
            let cursor_bounds = Rectangle {
                x: cursor_x,
                y: cursor_y,
                width: cellw,
                height: cellh,
            };
            renderer.fill_quad(
                renderer::Quad {
                    bounds: cursor_bounds,
                    ..renderer::Quad::default()
                },
                Color::from_rgb(0.0, 0.5, 0.0),
            );
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

#[derive(Debug, Clone, Copy)]
struct Cursor {
    row: usize,
    col: usize,
}

impl std::fmt::Display for Cursor {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Cursor (row: {}, col: {})", self.row, self.col)
    }
}

struct BufferContent {
    data: Vec<String>,
    cursor: Cursor,
    size: iced::Size,
}

#[derive(Debug, Clone, Copy)]
pub enum BufferMessage<AsciiCode = u8> {
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
                    self.col = 0;
                    break;
                }
                self.col += 1;
                return Some(BufferContentCellInfo {
                    row: self.row as u32,
                    col: (self.col - 1) as u32, // undo increment from above.
                    c: ch.unwrap(),
                });
            }
            self.row += 1;
            if self.row >= self.dev_box_ref.data.len() {
                return None;
            }
        }
    }
}

impl Component<BufferMessage> for BufferContent {
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

    fn update(&mut self, message: BufferMessage) -> iced::Task<BufferMessage> {
        match message {
            BufferMessage::AddCharacter(ascii_code) => {
                println!("-> Adding character: {}", ascii_code);
                self.ensure_cursor();

                let row = &self.data[self.cursor.row];
                let (left, right) = row.split_at(self.cursor.col);

                let mut new_row = String::from(left);
                new_row.push(ascii_code as char);
                new_row += right;

                self.data[self.cursor.row] = new_row;
                self.cursor.col += 1
            }
            BufferMessage::RemoveCharacter => {
                let row = &self.data[self.cursor.row];
                if row.len() > 0 {
                    let (new_row_ref, _) = row.split_at(row.len() - 1);
                    self.data[self.cursor.row] = String::from(new_row_ref);
                    self.cursor.col -= 1;
                }
            }
            BufferMessage::CommitAction(action) => match action {
                iced::keyboard::key::Named::Enter => {
                    self.ensure_cursor();
                    let (upper, lower) = self.data.split_at(self.cursor.row + 1);

                    let mut new_data = Vec::from(upper);
                    new_data.push(String::new());
                    new_data.append(&mut Vec::from(lower));

                    self.data = new_data;
                    self.cursor.row += 1;
                    self.cursor.col = 0;
                }
                _ => {}
            },
        }
        iced::Task::none()
    }

    fn view(&self) -> iced::Element<'_, EditorMessage> {
        iced::widget::container(CodeSpace::new(
            self.size.width,
            self.size.height,
            BufferContentIterator::new(self),
            &self.cursor,
        ))
        .into()
    }
}

impl BufferContent {
    fn new(size: iced::Size) -> Self {
        BufferContent {
            data: vec![String::new()],
            cursor: Cursor { row: 0, col: 0 },
            size,
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

    fn ensure_cursor(&mut self) {
        if self.data.len() == 0 {
            self.data.push(String::new());
        }
        let new_row_i = std::cmp::min(self.cursor.row, self.data.len() - 1);
        self.cursor.row = new_row_i;
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
                    e @ iced::keyboard::key::Named::Enter => Some(BufferMessage::CommitAction(e)),
                    iced::keyboard::key::Named::Space => {
                        Some(BufferMessage::AddCharacter(' ' as u8))
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }
}

struct PanelStatusLine {
    bounds: Rectangle,
    current_command: String,
}

impl PanelStatusLine {
    fn new(bounds: Rectangle) -> Self {
        PanelStatusLine {
            bounds,
            current_command: String::new(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum PanelStatusLineMessage {
    ProcessKeyInput(char),
}

impl Component<PanelStatusLineMessage> for PanelStatusLine {
    fn handle_key_event(
        &self,
        _key: iced::keyboard::Key,
        modified_key: iced::keyboard::Key,
        _modifiers: iced::keyboard::Modifiers,
    ) -> Option<PanelStatusLineMessage> {
        if let iced::keyboard::Key::Character(c) = modified_key {
            let ch = c.chars().nth(0).unwrap();
            Some(PanelStatusLineMessage::ProcessKeyInput(ch))
        } else {
            None
        }
    }

    fn update(&mut self, message: PanelStatusLineMessage) -> iced::Task<PanelStatusLineMessage> {
        match message {
            PanelStatusLineMessage::ProcessKeyInput(c) => {
                self.current_command.push(c);
                iced::Task::none()
            }
        }
    }

    fn view(&self) -> iced::Element<'_, EditorMessage> {
        StatusLineRender::new(
            self.bounds.x,
            self.bounds.y,
            self.bounds.width,
            self.bounds.height,
            // TODO: Pass by reference.
            self.current_command.clone(),
        )
        .into()
    }
}

struct Panel {
    buffer: BufferContent,
    status_line: PanelStatusLine,
    mode: PanelMode,
}

#[derive(Clone, Debug, Copy)]
pub enum PanelMode {
    Normal,
    Insert,
    StatusCommand,
}

#[derive(Debug, Clone, Copy)]
pub enum PanelMessage {
    ProcessKeyInput(char),
    ProcessBufferEvent(BufferMessage),
    ProcessStatusLineEvent(PanelStatusLineMessage),
    ModeTransition(PanelMode, Option<char>),
}

impl Panel {
    fn new(size: iced::Size) -> Self {
        let status_line_height = 20.0;
        let buffer_size = iced::Size {
            height: size.height - status_line_height,
            ..size
        };
        let status_line_bounds = Rectangle {
            x: 0.0,
            y: buffer_size.height,
            width: size.width,
            height: status_line_height,
        };
        Panel {
            buffer: BufferContent::new(buffer_size),
            status_line: PanelStatusLine::new(status_line_bounds),
            mode: PanelMode::Normal,
        }
    }

    fn handle_mode_selected_key_event(
        &self,
        key: iced::keyboard::Key,
        modified_key: iced::keyboard::Key,
        modifiers: iced::keyboard::Modifiers,
    ) -> Option<PanelMessage> {
        match self.mode {
            PanelMode::Insert => match self.buffer.handle_key_event(key, modified_key, modifiers) {
                Some(buffer_msg) => Some(PanelMessage::ProcessBufferEvent(buffer_msg)),
                _ => None,
            },
            PanelMode::StatusCommand => {
                match self
                    .status_line
                    .handle_key_event(key, modified_key, modifiers)
                {
                    Some(cmd) => Some(PanelMessage::ProcessStatusLineEvent(cmd)),
                    _ => None,
                }
            }
            PanelMode::Normal => None,
        }
    }
}

impl Component<PanelMessage> for Panel {
    fn handle_key_event(
        &self,
        key: iced::keyboard::Key,
        modified_key: iced::keyboard::Key,
        modifiers: iced::keyboard::Modifiers,
    ) -> Option<PanelMessage> {
        match modified_key {
            iced::keyboard::key::Key::Character(ref ch_str) => {
                let ch = ch_str.chars().nth(0).unwrap();
                if ch == ':' {
                    match self.mode {
                        PanelMode::Normal => {
                            return Some(PanelMessage::ModeTransition(
                                PanelMode::StatusCommand,
                                Some(':'),
                            ));
                        }
                        _ => {}
                    }
                }
            }
            // TODO: Send a "commit action" message to the status line instead of just
            // transitioning immediately.
            iced::keyboard::key::Key::Named(iced::keyboard::key::Named::Enter) => match self.mode {
                PanelMode::StatusCommand => {
                    return Some(PanelMessage::ModeTransition(PanelMode::Normal, None));
                }
                _ => {}
            },
            _ => {}
        }
        self.handle_mode_selected_key_event(key, modified_key, modifiers)
    }

    fn update(&mut self, message: PanelMessage) -> iced::Task<PanelMessage> {
        match message {
            PanelMessage::ProcessBufferEvent(buffer_message) => self
                .buffer
                .update(buffer_message)
                .map(|panel_response| PanelMessage::ProcessBufferEvent(panel_response)),
            PanelMessage::ProcessStatusLineEvent(status_message) => self
                .status_line
                .update(status_message)
                .map(|response| PanelMessage::ProcessStatusLineEvent(response)),
            PanelMessage::ModeTransition(new_mode, input_buffer_opt) => {
                self.mode = new_mode;
                if let Some(input_buffer) = input_buffer_opt {
                    iced::Task::done(PanelMessage::ProcessKeyInput(input_buffer))
                } else {
                    iced::Task::none()
                }
            }
            PanelMessage::ProcessKeyInput(key) => match self.mode {
                PanelMode::Insert => iced::Task::done(PanelMessage::ProcessBufferEvent(
                    BufferMessage::AddCharacter(key as u8),
                )),
                // TODO: Handle this panel mode.
                PanelMode::Normal => iced::Task::none(),
                PanelMode::StatusCommand => iced::Task::done(PanelMessage::ProcessStatusLineEvent(
                    PanelStatusLineMessage::ProcessKeyInput(key),
                )),
            },
        }
    }

    fn view(&self) -> iced::Element<'_, EditorMessage> {
        iced::widget::column![self.buffer.view(), self.status_line.view()].into()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum EditorMessage<Key = iced::keyboard::Key, Modifier = iced::keyboard::Modifiers> {
    // Open an empty buffer in the editor.
    OpenPanel(iced::Size),
    HandleKeys(Key, Key, Modifier),
    ProcessPanelEvent(PanelMessage),
}

pub struct EditorApp {
    panels: Vec<Panel>,
    active_panel: Option<usize>,
}

impl EditorApp {
    pub fn new() -> Self {
        EditorApp {
            panels: vec![],
            active_panel: None,
        }
    }

    pub fn view(&self) -> iced::Element<'_, EditorMessage> {
        if let Some(buffer_idx) = self.active_panel {
            self.panels[buffer_idx].view().into()
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
            EditorMessage::OpenPanel(size) => {
                self.panels.push(Panel::new(size));
                if let None = self.active_panel {
                    self.active_panel = Some(self.panels.len() - 1);
                }
                iced::Task::none()
            }
            EditorMessage::HandleKeys(key, modified_key, modifier) => {
                if let Some(buffer_idx) = self.active_panel {
                    match self.panels[buffer_idx].handle_key_event(key, modified_key, modifier) {
                        Some(msg) => {
                            return iced::Task::done(EditorMessage::<
                                iced::keyboard::Key,
                                iced::keyboard::Modifiers,
                            >::ProcessPanelEvent(
                                msg
                            ));
                        }
                        _ => {}
                    }
                }
                return iced::Task::none();
            }
            EditorMessage::ProcessPanelEvent(panel_message) => {
                if let Some(panel_idx) = self.active_panel {
                    self.panels[panel_idx]
                        .update(panel_message)
                        .map(|panel_response| EditorMessage::<iced::keyboard::Key, iced::keyboard::Modifiers>::ProcessPanelEvent(panel_response))
                } else {
                    iced::Task::none()
                }
            }
        }
    }
}
