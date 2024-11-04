use core::str;
use std::io::Read;
use std::io::Write;

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

struct GridSpaceUtil;
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

pub struct StatusLineRender<'a> {
    x: f32,
    y: f32,
    width: f32,
    height: f32,
    command: &'a Option<String>,
    mode: &'a String,
    cursor: &'a Cursor,
    shortcut: &'a String,
}

impl<'a> StatusLineRender<'a> {
    fn new(
        x: f32,
        y: f32,
        width: f32,
        height: f32,
        command: &'a Option<String>,
        mode: &'a String,
        cursor: &'a Cursor,
        shortcut: &'a String,
    ) -> Self {
        StatusLineRender {
            x,
            y,
            width,
            height,
            command,
            mode,
            cursor,
            shortcut,
        }
    }
}

impl<Message, Theme, Renderer> Widget<Message, Theme, Renderer> for StatusLineRender<'_>
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

        let mode_str = if let Some(cmd) = &self.command {
            String::from(':') + cmd
        } else {
            self.mode.clone()
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

        // Right side text
        {
            // draw "row:col"
            {
                let right_bounds = Rectangle {
                    x: self.x + self.width / 2.0,
                    y: self.y,
                    width: self.width / 2.0,
                    height: self.height,
                };
                renderer.fill_quad(
                    renderer::Quad {
                        bounds: right_bounds,
                        ..renderer::Quad::default()
                    },
                    Color::from_rgb(0.2, 0.0, 0.0),
                );

                let right_text = iced::advanced::Text {
                    content: self.cursor.row.to_string()
                        + ":"
                        + self.cursor.col.to_string().as_ref(),
                    bounds: Size {
                        width: right_bounds.width,
                        height: GridSpaceUtil::CELL_HEIGHT as f32,
                    },
                    size: iced::Pixels(GridSpaceUtil::TEXT_SIZE as f32),
                    line_height: iced::advanced::text::LineHeight::default(),
                    font: iced::Font::MONOSPACE,
                    horizontal_alignment: iced::Right,
                    vertical_alignment: iced::Top,
                    shaping: iced::advanced::text::Shaping::Basic,
                    wrapping: iced::advanced::text::Wrapping::None,
                };
                renderer.fill_text(
                    right_text,
                    iced::Point {
                        x: right_bounds.x + right_bounds.width,
                        y: right_bounds.y,
                    },
                    Color::WHITE,
                    right_bounds,
                );
            }
            {
                // draw shortcut buffer
                let offset = 60.0;
                let rbounds = Rectangle {
                    x: self.x + self.width / 2.0,
                    y: self.y,
                    width: self.width / 2.0 - offset,
                    height: self.height,
                };
                renderer.fill_quad(
                    renderer::Quad {
                        bounds: rbounds,
                        ..renderer::Quad::default()
                    },
                    Color::from_rgb(0.0, 0.2, 0.0),
                );
                let text = iced::advanced::Text {
                    content: self.shortcut.clone(),
                    bounds: Size {
                        width: rbounds.width,
                        height: GridSpaceUtil::CELL_HEIGHT as f32,
                    },
                    size: iced::Pixels(GridSpaceUtil::TEXT_SIZE as f32),
                    line_height: iced::advanced::text::LineHeight::default(),
                    font: iced::Font::MONOSPACE,
                    horizontal_alignment: iced::Right,
                    vertical_alignment: iced::Top,
                    shaping: iced::advanced::text::Shaping::Basic,
                    wrapping: iced::advanced::text::Wrapping::None,
                };
                renderer.fill_text(
                    text,
                    iced::Point {
                        x: rbounds.x + rbounds.width,
                        y: rbounds.y,
                    },
                    Color::WHITE,
                    rbounds,
                );
            }
        }
    }
}

impl<'a, Message, Theme, Renderer> From<StatusLineRender<'a>>
    for Element<'a, Message, Theme, Renderer>
where
    Renderer: renderer::Renderer + iced::advanced::text::Renderer<Font = iced::Font>,
{
    fn from(el: StatusLineRender<'a>) -> Self {
        Self::new(el)
    }
}

pub struct TabLineRenderer {
    x: f32,
    y: f32,
    width: f32,
    height: f32,
}

impl TabLineRenderer {
    fn new(x: f32, y: f32, width: f32, height: f32) -> Self {
        TabLineRenderer {
            x,
            y,
            width,
            height,
        }
    }
}

impl<Message, Theme, Renderer> Widget<Message, Theme, Renderer> for TabLineRenderer
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
        renderer.fill_quad(
            renderer::Quad {
                bounds: Rectangle {
                    x: self.x,
                    y: self.y,
                    width: self.width,
                    height: self.height,
                },
                ..renderer::Quad::default()
            },
            Color::from_rgb(0.0, 0.2, 0.2),
        );
    }
}

impl<'a, Message, Theme, Renderer> From<TabLineRenderer> for Element<'a, Message, Theme, Renderer>
where
    Renderer: renderer::Renderer + iced::advanced::text::Renderer<Font = iced::Font>,
{
    fn from(el: TabLineRenderer) -> Self {
        Self::new(el)
    }
}

pub struct CodeSpace<'a> {
    x: f32,
    y: f32,
    width: f32,
    height: f32,
    buffer_iter: BufferContentIterator<'a>,
    cursor: &'a Cursor,
}

impl<'a> CodeSpace<'a> {
    // The number of columns that should be allocated for the number line.
    const LINE_NB_DIGITS: usize = 3;

    fn new(
        x: f32,
        y: f32,
        width: f32,
        height: f32,
        buffer_iter: BufferContentIterator<'a>,
        cursor: &'a Cursor,
    ) -> Self {
        CodeSpace {
            x,
            y,
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
        let (x, y) = (x + self.x, y + self.y);
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
        _layout: layout::Layout<'_>,
        _cursor: mouse::Cursor,
        _viewport: &Rectangle,
    ) {
        renderer.fill_quad(
            renderer::Quad {
                bounds: Rectangle {
                    x: self.x,
                    y: self.y,
                    width: self.width,
                    height: self.height,
                },
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

        // Draw the current line
        {
            let (_, cursor_y) =
                GridSpaceUtil::cell_to_pixel_space(self.cursor.row, self.cursor.col);
            let bounds = Rectangle {
                x: self.x,
                y: cursor_y + self.y,
                width: self.width,
                height: cellh,
            };
            renderer.fill_quad(
                renderer::Quad {
                    bounds,
                    ..renderer::Quad::default()
                },
                Color::from_rgb(0.2, 0.0, 0.0),
            );
        }

        // Draw the text individually in each cell.
        for cell in self.buffer_iter.into_iter() {
            let (row, col) = CodeSpace::nb_ofsetted_row_cols(cell.row as usize, cell.col as usize);
            let (x, y) = GridSpaceUtil::cell_to_pixel_space(row, col);

            let bounds = Rectangle {
                x: x + self.x,
                y: y + self.y,
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
                x: cursor_x + self.x,
                y: cursor_y + self.y,
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
pub struct Cursor {
    row: usize,
    col: usize,
}

impl std::fmt::Display for Cursor {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Cursor (row: {}, col: {})", self.row, self.col)
    }
}

#[derive(Debug, Clone)]
struct BufferContent {
    data: Vec<String>,
    cursor: Cursor,
    bounds: Rectangle,
}

#[derive(Debug, Clone)]
pub enum CursorAxisMod {
    // Move the cursor horizontally by the delta, clamped to the current row's limits.
    HorizontalCursorDelta(i32 /* col_delta */),
    // Insert a new row `row_delta` away from the current row and move the cursor
    // to that newly inserted row.
    InsertRowAndMoveDelta(i32 /* row_delta */),
}

#[derive(Debug, Clone)]
pub enum BufferCursorNavigation {
    TopOfBuffer,
    BottomOfBuffer,
    // Not just the beginning of the line. The first character within the line,
    // skipping all whitespaces.
    BeginningCharOfLine,
    EndCharOfLine,
}

#[derive(Debug, Clone)]
pub enum BufferMessage<AsciiCode = u8> {
    AddCharacter(AsciiCode),
    AddCharacters(Vec<AsciiCode>),
    RemoveCharacter,
    CommitAction(iced::keyboard::key::Named),
    CursorAxisMod(CursorAxisMod),
    UpstreamResponse(UpstreamedPanelMessage),
    CursorNavigateComamnd(BufferCursorNavigation),
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
            BufferMessage::UpstreamResponse(_) => unreachable!(),
            BufferMessage::CursorNavigateComamnd(nav) => {
                match nav {
                    BufferCursorNavigation::TopOfBuffer => {
                        self.cursor.row = 0;
                        self.cursor.col = 0;
                    }
                    BufferCursorNavigation::BottomOfBuffer => {
                        self.cursor.row = self.data.len() - 1;
                        self.cursor.col = 0;
                    }
                    BufferCursorNavigation::BeginningCharOfLine => {
                        self.cursor.col = 0;
                        let row = &self.data[self.cursor.row];
                        while self.cursor.col < row.len()
                            && &row[self.cursor.col..self.cursor.col + 1] == " "
                        {
                            self.cursor.col += 1;
                        }
                    }
                    BufferCursorNavigation::EndCharOfLine => {
                        let row = &self.data[self.cursor.row];
                        self.cursor.col = row.len() - 1;
                        while self.cursor.col > 0
                            && &row[self.cursor.col..self.cursor.col + 1] == " "
                        {
                            self.cursor.col -= 1;
                        }
                    }
                }
                iced::Task::done(BufferMessage::UpstreamResponse(
                    UpstreamedPanelMessage::SetStatusLineCursor(self.cursor.clone()),
                ))
            }
            BufferMessage::AddCharacter(ascii_code) => {
                self.add_character(ascii_code);
                return iced::Task::done(BufferMessage::UpstreamResponse(
                    UpstreamedPanelMessage::SetStatusLineCursor(self.cursor.clone()),
                ));
            }
            BufferMessage::AddCharacters(chars) => {
                for ascii_code in chars {
                    self.add_character(ascii_code);
                }
                return iced::Task::done(BufferMessage::UpstreamResponse(
                    UpstreamedPanelMessage::SetStatusLineCursor(self.cursor.clone()),
                ));
            }
            BufferMessage::RemoveCharacter => {
                let row = &self.data[self.cursor.row];
                if row.len() > 0 {
                    let (left_side_ref, right_side_ref) = row.split_at(self.cursor.col);
                    if left_side_ref.len() > 0 {
                        self.data[self.cursor.row] =
                            String::from(&left_side_ref[..left_side_ref.len() - 1])
                                + right_side_ref;
                        self.cursor.col -= 1;
                    } else {
                        // Move the whole line into the previous line and update the cursor.
                        let (upper, lower) = self.data.split_at(self.cursor.row);
                        if upper.len() > 0 && lower.len() > 0 {
                            let line_above = &upper[upper.len() - 1];
                            let curr_line = &lower[0];
                            let new_col = line_above.len();

                            let combined_line = String::from(line_above) + curr_line;

                            let mut new_data = Vec::from(&upper[..upper.len() - 1]);
                            new_data.push(combined_line);
                            new_data.append(&mut Vec::from(&lower[1..]));

                            self.data = new_data;
                            self.cursor.row -= 1;
                            self.cursor.col = new_col;
                        }
                    }
                } else if self.cursor.row > 0 {
                    self.cursor.row -= 1;
                    self.cursor.col = self.data[self.cursor.row].len()
                }
                return iced::Task::done(BufferMessage::UpstreamResponse(
                    UpstreamedPanelMessage::SetStatusLineCursor(self.cursor.clone()),
                ));
            }
            BufferMessage::CommitAction(action) => {
                match action {
                    iced::keyboard::key::Named::Enter => {
                        let (upper, lower) = self.data.split_at(self.cursor.row + 1);

                        let mut new_data = Vec::from(upper);

                        // Split the current row at the column point and move the data to the right of cursor
                        // to the next line.
                        let new_line: String = {
                            let (left, right) = upper[upper.len() - 1].split_at(self.cursor.col);
                            let last_i = new_data.len() - 1;
                            new_data[last_i] = String::from(&left[..]);
                            String::from(&right[..])
                        };

                        new_data.push(new_line);
                        new_data.append(&mut Vec::from(lower));

                        self.data = new_data;
                        self.cursor.row += 1;
                        self.cursor.col = 0;
                    }
                    iced::keyboard::key::Named::ArrowLeft => {
                        if self.cursor.col > 0 {
                            self.cursor.col = std::cmp::max(0, self.cursor.col - 1);
                        }
                    }
                    iced::keyboard::key::Named::ArrowRight => {
                        self.cursor.col =
                            std::cmp::min(self.data[self.cursor.row].len(), self.cursor.col + 1);
                    }
                    iced::keyboard::key::Named::ArrowUp => {
                        if self.cursor.row > 0 {
                            self.cursor.row = std::cmp::max(0, self.cursor.row - 1);
                            let max_col = self.data[self.cursor.row].len();
                            self.cursor.col = std::cmp::min(max_col, self.cursor.col);
                        }
                    }
                    iced::keyboard::key::Named::ArrowDown => {
                        self.cursor.row = std::cmp::min(self.data.len() - 1, self.cursor.row + 1);
                        let max_col = self.data[self.cursor.row].len();
                        self.cursor.col = std::cmp::min(max_col, self.cursor.col);
                    }
                    _ => {}
                }
                return iced::Task::done(BufferMessage::UpstreamResponse(
                    UpstreamedPanelMessage::SetStatusLineCursor(self.cursor.clone()),
                ));
            }
            BufferMessage::CursorAxisMod(modfier) => {
                match modfier {
                    CursorAxisMod::HorizontalCursorDelta(delta) => {
                        let new_col = (self.cursor.col as i32) + delta;
                        if new_col >= 0 {
                            let new_col = new_col as usize;
                            self.cursor.col =
                                std::cmp::min(new_col, self.data[self.cursor.row].len());
                        }
                    }
                    CursorAxisMod::InsertRowAndMoveDelta(delta) => {
                        if delta == 1 {
                            let (upper, lower) = self.data.split_at(self.cursor.row + 1);
                            let mut new_data = Vec::from(upper);
                            new_data.push(String::new());
                            new_data.append(&mut Vec::from(lower));

                            self.data = new_data;
                            self.cursor.row += 1;
                            self.cursor.col = 0;
                        } else if delta == -1 {
                            let (upper, lower) = self.data.split_at(self.cursor.row);
                            let mut new_data = Vec::from(upper);
                            new_data.push(String::new());
                            new_data.append(&mut Vec::from(lower));

                            self.data = new_data;
                            // cursor should stay in same position. The inserted row should account for the delta.
                            self.cursor.col = 0;
                        } else {
                            panic!("Non-singular delta for CursorAxisMod::InsertRowAndMoveDelta not implemented.");
                        }
                    }
                }
                return iced::Task::done(BufferMessage::UpstreamResponse(
                    UpstreamedPanelMessage::SetStatusLineCursor(self.cursor.clone()),
                ));
            }
        }
    }

    fn view(&self) -> iced::Element<'_, EditorMessage> {
        iced::widget::container(CodeSpace::new(
            self.bounds.x,
            self.bounds.y,
            self.bounds.width,
            self.bounds.height,
            BufferContentIterator::new(self),
            &self.cursor,
        ))
        .into()
    }
}

impl BufferContent {
    fn new(bounds: Rectangle) -> Self {
        BufferContent {
            data: vec![String::new()],
            cursor: Cursor { row: 0, col: 0 },
            bounds,
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

    fn add_character(&mut self, ascii_code: u8) {
        println!("-> Adding character: {}", ascii_code);

        let row = &self.data[self.cursor.row];
        let (left, right) = row.split_at(self.cursor.col);

        let mut new_row = String::from(left);
        new_row.push(ascii_code as char);
        new_row += right;

        self.data[self.cursor.row] = new_row;
        self.cursor.col += 1;
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
                    e @ iced::keyboard::key::Named::Enter
                    | e @ iced::keyboard::key::Named::ArrowLeft
                    | e @ iced::keyboard::key::Named::ArrowRight
                    | e @ iced::keyboard::key::Named::ArrowUp
                    | e @ iced::keyboard::key::Named::ArrowDown => {
                        Some(BufferMessage::CommitAction(e))
                    }
                    iced::keyboard::key::Named::Space => {
                        Some(BufferMessage::AddCharacter(' ' as u8))
                    }
                    iced::keyboard::key::Named::Tab => {
                        // Tab resolves to 2 spaces, personal preference :). If it matters, allow this to be configured somewhere.
                        Some(BufferMessage::AddCharacters(vec![' ' as u8, ' ' as u8]))
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
    current_command: Option<String>,
    current_shortcut: String,
    mode_string: String,
    cursor: Cursor,
}

impl PanelStatusLine {
    fn new(bounds: Rectangle) -> Self {
        PanelStatusLine {
            bounds,
            current_command: None,
            current_shortcut: String::new(),
            mode_string: String::from("NORMAL"),
            cursor: Cursor { row: 0, col: 0 },
        }
    }

    fn translate_buffer_command(&self, command: &str) -> iced::Task<PanelStatusLineMessage> {
        println!("translating buffer command: [{}]", command);
        let parts = command
            .split_whitespace()
            .map(|s| String::from(s))
            .collect::<Vec<String>>();
        if parts.len() == 0 {
            return iced::Task::none();
        }
        match &parts[0][..] {
            "o" => {
                return iced::Task::done(PanelStatusLineMessage::UpstreamResponse(Some(
                    UpstreamedPanelMessage::ExecuteCommand(PanelCommand::OpenFile(
                        if parts.len() == 2 {
                            Some(Box::new(String::from(&parts[1][..])))
                        } else {
                            None
                        },
                    )),
                )));
            }
            "w" => {
                println!("Dispatching write-file command.");
                return iced::Task::done(PanelStatusLineMessage::UpstreamResponse(Some(
                    UpstreamedPanelMessage::ExecuteCommand(PanelCommand::SaveCurrentFile),
                )));
            }
            _ => {}
        }
        iced::Task::done(PanelStatusLineMessage::UpstreamResponse(None))
    }
}

#[derive(Debug, Clone)]
pub enum CurrentActionHandelingMode {
    // Commit the current action into its actionable task.
    Commit,
    // Abandon the current action alltogether.
    Abandon,
}

#[derive(Debug, Clone)]
pub enum PanelStatusLineMessage {
    ProcessKeyInput(char),
    RemoveCharacterFromCommand,
    HandleCurrentAction(CurrentActionHandelingMode),
    DisplayMode(PanelMode),
    UpstreamResponse(Option<UpstreamedPanelMessage>),
    SetCursor(Cursor),
    SetShortcut(String),
}

impl Component<PanelStatusLineMessage> for PanelStatusLine {
    fn handle_key_event(
        &self,
        _key: iced::keyboard::Key,
        modified_key: iced::keyboard::Key,
        _modifiers: iced::keyboard::Modifiers,
    ) -> Option<PanelStatusLineMessage> {
        match modified_key {
            iced::keyboard::Key::Character(c) => {
                let ch = c.chars().nth(0).unwrap();
                Some(PanelStatusLineMessage::ProcessKeyInput(ch))
            }
            iced::keyboard::Key::Named(iced::keyboard::key::Named::Space) => {
                Some(PanelStatusLineMessage::ProcessKeyInput(' '))
            }
            iced::keyboard::Key::Named(iced::keyboard::key::Named::Backspace) => {
                Some(PanelStatusLineMessage::RemoveCharacterFromCommand)
            }
            _ => None,
        }
    }

    fn update(&mut self, message: PanelStatusLineMessage) -> iced::Task<PanelStatusLineMessage> {
        match message {
            PanelStatusLineMessage::UpstreamResponse(_) => unreachable!(),
            PanelStatusLineMessage::ProcessKeyInput(c) => {
                if self.current_command.is_none() {
                    self.current_command = Some(String::new());
                } else {
                    self.current_command.as_mut().unwrap().push(c);
                }
                iced::Task::none()
            }
            PanelStatusLineMessage::SetShortcut(shortcut) => {
                self.current_shortcut = shortcut;
                iced::Task::none()
            }
            PanelStatusLineMessage::RemoveCharacterFromCommand => {
                if let Some(cmd) = &self.current_command {
                    if cmd.len() > 0 {
                        self.current_command = Some(String::from(&cmd[0..cmd.len() - 1]));
                    }
                }
                iced::Task::none()
            }
            PanelStatusLineMessage::HandleCurrentAction(handle_mode) => {
                let cmd_option = self.current_command.clone();
                self.current_command = None;
                match handle_mode {
                    CurrentActionHandelingMode::Commit => {
                        if let Some(command) = &cmd_option {
                            return self.translate_buffer_command(&command[..]);
                        }
                    }
                    CurrentActionHandelingMode::Abandon => {}
                }
                iced::Task::done(PanelStatusLineMessage::UpstreamResponse(None))
            }
            PanelStatusLineMessage::DisplayMode(mode) => {
                match mode {
                    PanelMode::Insert => {
                        self.mode_string = String::from("INSERT");
                    }
                    PanelMode::Normal | PanelMode::ShortcutCommand => {
                        self.mode_string = String::from("NORMAL");
                    }
                    PanelMode::StatusCommand => {}
                }
                iced::Task::none()
            }
            PanelStatusLineMessage::SetCursor(cursor) => {
                self.cursor = cursor;
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
            &self.current_command,
            &self.mode_string,
            &self.cursor,
            &self.current_shortcut,
        )
        .into()
    }
}

#[derive(Debug, Clone)]
pub struct BufferSource {
    // The filepath associated with the buffer source.
    // It is guaranteed to exist at the time of creation.
    filepath: std::path::PathBuf,
}

#[derive(Debug, Clone, Copy)]
pub enum FileError {
    NoFileSpecified,
    FileNotFound,
    OpenError,
    ReadError,
    // This should be used to communicate unsupported file types, such as
    // directories. Not the same as `FileNotFound`.
    NotANormalFile,
    SaveError,
}

async fn open_relative_file(filepath: Option<&String>) -> Result<std::path::PathBuf, FileError> {
    match filepath {
        None => Err(FileError::NoFileSpecified),
        Some(filepath) => match std::env::current_dir() {
            Err(_) => {
                return Err(FileError::FileNotFound);
            }
            Ok(cwd) => {
                let path = std::path::Path::new(&filepath);
                let relative_path = cwd.as_path().join(path);
                if relative_path.exists() {
                    if relative_path.is_file() {
                        Ok(relative_path)
                    } else {
                        Err(FileError::NotANormalFile)
                    }
                } else {
                    Err(FileError::FileNotFound)
                }
            }
        },
    }
}

async fn open_file(filepath: Option<&String>) -> Result<std::path::PathBuf, FileError> {
    if filepath.is_none() {
        return Err(FileError::NoFileSpecified);
    }
    let path = std::path::PathBuf::from(filepath.as_ref().unwrap());
    println!("Opening file: {:?}", filepath);
    if !path.exists() {
        return open_relative_file(filepath).await;
    }
    if !path.is_file() {
        return Err(FileError::NotANormalFile);
    }

    match std::fs::File::open(&path) {
        Err(_) => Err(FileError::OpenError),
        Ok(_) => Ok(path),
    }
}

async fn save_file(contents: String, filepath: std::path::PathBuf) -> Result<(), FileError> {
    // NOTE: Assumes the file already exists, hence the opening of the file and not creation.
    match std::fs::OpenOptions::new().write(true).open(filepath) {
        Err(_) => Err(FileError::SaveError),
        Ok(mut file) => match file.write_all(contents.as_bytes()) {
            Err(e) => {
                println!("Failed to save file: {:?}", e);
                Err(FileError::SaveError)
            }
            Ok(_) => Ok(()),
        },
    }
}

async fn read_file(filepath: std::path::PathBuf) -> Result<Vec<String>, FileError> {
    if !filepath.exists() {
        return Err(FileError::FileNotFound);
    }
    let mut file = match std::fs::File::open(&filepath) {
        Err(_) => {
            return Err(FileError::OpenError);
        }
        Ok(file) => file,
    };

    let mut data = vec![];
    match file.read_to_end(&mut data) {
        Err(_) => Err(FileError::ReadError),
        Ok(_) => match str::from_utf8(&data[..]) {
            Err(_) => Err(FileError::OpenError),
            Ok(v) => {
                let data_str = String::from(v);
                Ok(data_str
                    .lines()
                    .map(|e| String::from(e))
                    .collect::<Vec<String>>())
            }
        },
    }
}

impl BufferSource {
    async fn open_file_and_create(filepath: String) -> Option<Self> {
        match open_file(Some(&filepath)).await {
            // TODO: Since we are no longer pre-fetching the data from the file and only
            // requesting the content on a call to read(), this no longer needs to return
            // an option and the open_file also does not need to be performed.
            // Refactor this to reflect that change.
            Ok(filepath_buf) => {
                println!("Setting BufferSource filepath buf: {:?}", filepath_buf);
                Some(BufferSource {
                    filepath: filepath_buf,
                })
            }
            // TODO: Instead of returning an option, maybe forward the error result..
            Err(_) => None,
        }
    }
}

struct TabLine {
    bounds: Rectangle,
}

#[derive(Debug, Clone)]
enum TabLineMessage {}

impl TabLine {
    fn new(bounds: Rectangle) -> Self {
        TabLine { bounds }
    }
}

impl Component<TabLineMessage> for TabLine {
    fn handle_key_event(
        &self,
        _key: iced::keyboard::Key,
        _modified_key: iced::keyboard::Key,
        _modifiers: iced::keyboard::Modifiers,
    ) -> Option<TabLineMessage> {
        None
    }

    fn update(&mut self, _message: TabLineMessage) -> iced::Task<TabLineMessage> {
        iced::Task::none()
    }

    fn view(&self) -> iced::Element<'_, EditorMessage> {
        TabLineRenderer::new(
            self.bounds.x,
            self.bounds.y,
            self.bounds.width,
            self.bounds.height,
        )
        .into()
    }
}

struct Panel {
    buffer: BufferContent,
    buffer_source: Option<BufferSource>,
    status_line: PanelStatusLine,
    tab_line: TabLine,
    mode: PanelMode,
    shortcut_buffer: String,
    shortcut_initiators: std::collections::HashSet<char>,
}

#[derive(Clone, Debug, Copy, PartialEq)]
pub enum PanelMode {
    Normal,
    Insert,
    StatusCommand,
    ShortcutCommand,
}

#[derive(Clone, Debug)]
pub enum PanelCommand {
    OpenFile(Option<Box<String>>),
    // Save the file that is currently open in the active buffer.
    // If no file is open in the buffer, nothing should be done.
    SaveCurrentFile,
}

#[derive(Debug, Clone)]
pub enum UpstreamedPanelMessage {
    ExecuteCommand(PanelCommand),
    SetStatusLineCursor(Cursor),
}

#[derive(Debug, Clone)]
pub enum ModeTransitionPayload {
    Char(char),
    InsertModePayload(CursorAxisMod),
    None,
}

#[derive(Debug, Clone)]
pub enum PanelMessage {
    ProcessKeyInput(char),
    ProcessBufferEvent(BufferMessage),
    ProcessStatusLineEvent(PanelStatusLineMessage),
    ModeTransition(
        PanelMode, /* prev mode */
        PanelMode, /* new mode */
        ModeTransitionPayload,
    ),
    AttachBufferSource(Option<BufferSource>),
    AtttachContentToBuffer(Result<Vec<String>, FileError>),
    ShortcutBuffer(char),
    Empty, // does nothing
}

impl Panel {
    fn new(size: iced::Size) -> Self {
        let status_line_height = 20.0;
        let tab_line_height = 20.0;

        let tab_line_bounds = Rectangle {
            x: 0.0,
            y: 0.0,
            width: size.width,
            height: tab_line_height,
        };
        let buffer_bounds = Rectangle {
            x: 0.0,
            y: tab_line_bounds.height,
            width: size.width,
            height: size.height - status_line_height - tab_line_height,
        };
        let status_line_bounds = Rectangle {
            x: 0.0,
            y: tab_line_bounds.height + buffer_bounds.height,
            width: size.width,
            height: status_line_height,
        };
        Panel {
            buffer: BufferContent::new(buffer_bounds),
            status_line: PanelStatusLine::new(status_line_bounds),
            tab_line: TabLine::new(tab_line_bounds),
            mode: PanelMode::Normal,
            buffer_source: None,
            shortcut_buffer: String::new(),
            shortcut_initiators: std::collections::HashSet::from(['g']),
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
            PanelMode::Normal | PanelMode::ShortcutCommand => None,
        }
    }

    fn handle_normal_mode_key(&self, ch: char) -> Option<PanelMessage> {
        match ch {
            ':' => {
                return Some(PanelMessage::ModeTransition(
                    self.mode,
                    PanelMode::StatusCommand,
                    ModeTransitionPayload::Char(':'),
                ));
            }
            'i' => {
                return Some(PanelMessage::ModeTransition(
                    self.mode,
                    PanelMode::Insert,
                    ModeTransitionPayload::InsertModePayload(CursorAxisMod::HorizontalCursorDelta(
                        0,
                    )),
                ));
            }
            'a' => {
                return Some(PanelMessage::ModeTransition(
                    self.mode,
                    PanelMode::Insert,
                    ModeTransitionPayload::InsertModePayload(CursorAxisMod::HorizontalCursorDelta(
                        1,
                    )),
                ));
            }
            'o' => {
                return Some(PanelMessage::ModeTransition(
                    self.mode,
                    PanelMode::Insert,
                    ModeTransitionPayload::InsertModePayload(CursorAxisMod::InsertRowAndMoveDelta(
                        1,
                    )),
                ));
            }
            'O' => {
                return Some(PanelMessage::ModeTransition(
                    self.mode,
                    PanelMode::Insert,
                    ModeTransitionPayload::InsertModePayload(CursorAxisMod::InsertRowAndMoveDelta(
                        -1,
                    )),
                ));
            }
            'h' => {
                return Some(PanelMessage::ProcessBufferEvent(
                    BufferMessage::CommitAction(iced::keyboard::key::Named::ArrowLeft),
                ))
            }
            'l' => {
                return Some(PanelMessage::ProcessBufferEvent(
                    BufferMessage::CommitAction(iced::keyboard::key::Named::ArrowRight),
                ))
            }
            'j' => {
                return Some(PanelMessage::ProcessBufferEvent(
                    BufferMessage::CommitAction(iced::keyboard::key::Named::ArrowDown),
                ))
            }
            'k' => {
                return Some(PanelMessage::ProcessBufferEvent(
                    BufferMessage::CommitAction(iced::keyboard::key::Named::ArrowUp),
                ))
            }
            c @ _ => {
                if self.shortcut_initiators.contains(&c) {
                    return Some(PanelMessage::ModeTransition(
                        self.mode,
                        PanelMode::ShortcutCommand,
                        ModeTransitionPayload::Char(c),
                    ));
                }
            }
        }
        None
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
                match self.mode {
                    PanelMode::Normal => return self.handle_normal_mode_key(ch),
                    PanelMode::ShortcutCommand => {
                        if self.shortcut_buffer.len() == 0
                            && !self.shortcut_initiators.contains(&ch)
                        {
                            return Some(PanelMessage::ModeTransition(
                                self.mode,
                                PanelMode::Normal,
                                ModeTransitionPayload::Char(ch),
                            ));
                        }
                        return Some(PanelMessage::ShortcutBuffer(ch));
                    }
                    _ => {}
                }
            }
            iced::keyboard::key::Key::Named(e @ iced::keyboard::key::Named::ArrowUp)
            | iced::keyboard::key::Key::Named(e @ iced::keyboard::key::Named::ArrowDown)
            | iced::keyboard::key::Key::Named(e @ iced::keyboard::key::Named::ArrowLeft)
            | iced::keyboard::key::Key::Named(e @ iced::keyboard::key::Named::ArrowRight) => {
                match self.mode {
                    PanelMode::Normal => {
                        return Some(PanelMessage::ProcessBufferEvent(
                            BufferMessage::CommitAction(e),
                        ))
                    }
                    _ => {}
                }
            }
            iced::keyboard::key::Key::Named(iced::keyboard::key::Named::Enter) => match self.mode {
                PanelMode::StatusCommand => {
                    return Some(PanelMessage::ProcessStatusLineEvent(
                        PanelStatusLineMessage::HandleCurrentAction(
                            CurrentActionHandelingMode::Commit,
                        ),
                    ));
                }
                _ => {}
            },
            iced::keyboard::key::Key::Named(iced::keyboard::key::Named::Escape) => {
                match self.mode {
                    PanelMode::Insert | PanelMode::StatusCommand | PanelMode::ShortcutCommand => {
                        return Some(PanelMessage::ModeTransition(
                            self.mode,
                            PanelMode::Normal,
                            ModeTransitionPayload::None,
                        ));
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        self.handle_mode_selected_key_event(key, modified_key, modifiers)
    }

    fn update(&mut self, message: PanelMessage) -> iced::Task<PanelMessage> {
        match message {
            PanelMessage::Empty => iced::Task::none(),
            PanelMessage::ShortcutBuffer(c) => {
                self.shortcut_buffer += &c.to_string();
                let mut tasks = vec![iced::Task::done(PanelMessage::ProcessStatusLineEvent(
                    PanelStatusLineMessage::SetShortcut(self.shortcut_buffer.clone()),
                ))];

                let max_buffer_size = 2;
                let mut should_exit_shortcut_mode = false;
                let nav: Option<BufferCursorNavigation> = match &self.shortcut_buffer[..] {
                    "gg" => Some(BufferCursorNavigation::TopOfBuffer),
                    "ge" => Some(BufferCursorNavigation::BottomOfBuffer),
                    "gs" => Some(BufferCursorNavigation::BeginningCharOfLine),
                    "gl" => Some(BufferCursorNavigation::EndCharOfLine),
                    b @ _ => {
                        if b.len() >= max_buffer_size {
                            // received unidentified shortcut...
                            should_exit_shortcut_mode = true;
                        }
                        None
                    }
                };
                if let Some(n) = nav {
                    self.shortcut_buffer = String::new();
                    tasks.push(iced::Task::done(PanelMessage::ProcessBufferEvent(
                        BufferMessage::CursorNavigateComamnd(n),
                    )));
                } else if should_exit_shortcut_mode {
                    // Empty the buffer
                    tasks.push(iced::Task::done(PanelMessage::ModeTransition(
                        self.mode,
                        PanelMode::Normal,
                        ModeTransitionPayload::None,
                    )));
                }
                iced::Task::batch(tasks)
            }
            PanelMessage::ProcessBufferEvent(buffer_message) => {
                match buffer_message {
                    BufferMessage::UpstreamResponse(upstream_message) => match upstream_message {
                        UpstreamedPanelMessage::ExecuteCommand(_) => unreachable!(),
                        UpstreamedPanelMessage::SetStatusLineCursor(cursor) => {
                            return iced::Task::done(PanelMessage::ProcessStatusLineEvent(
                                PanelStatusLineMessage::SetCursor(cursor),
                            ))
                        }
                    },
                    _ => {}
                }
                self.buffer
                    .update(buffer_message)
                    .map(|panel_response| PanelMessage::ProcessBufferEvent(panel_response))
            }
            PanelMessage::ProcessStatusLineEvent(status_message) => match status_message {
                PanelStatusLineMessage::UpstreamResponse(None) => {
                    iced::Task::done(PanelMessage::ModeTransition(
                        self.mode,
                        PanelMode::Normal,
                        ModeTransitionPayload::None,
                    ))
                }
                PanelStatusLineMessage::UpstreamResponse(Some(upstream_message)) => {
                    let mut tasks = vec![iced::Task::done(PanelMessage::ModeTransition(
                        self.mode,
                        PanelMode::Normal,
                        ModeTransitionPayload::None,
                    ))];
                    match upstream_message {
                        UpstreamedPanelMessage::SetStatusLineCursor(_) => unreachable!(),
                        UpstreamedPanelMessage::ExecuteCommand(command) => match command {
                            PanelCommand::OpenFile(None) => {}
                            PanelCommand::OpenFile(Some(file)) => {
                                tasks.push(iced::Task::perform(
                                    BufferSource::open_file_and_create(*file),
                                    PanelMessage::AttachBufferSource,
                                ));
                            }
                            PanelCommand::SaveCurrentFile => match self.buffer_source.as_ref() {
                                None => {}
                                Some(buffersrc) => {
                                    let file_contents = self.buffer.data.join("\n");
                                    let filepath = buffersrc.filepath.clone();
                                    tasks.push(iced::Task::perform(
                                        save_file(file_contents, filepath),
                                        |_| PanelMessage::Empty,
                                    ));
                                }
                            },
                        },
                    }

                    iced::Task::batch(tasks)
                }
                _ => self
                    .status_line
                    .update(status_message)
                    .map(|response| PanelMessage::ProcessStatusLineEvent(response)),
            },
            PanelMessage::AttachBufferSource(buffer_source) => {
                match buffer_source {
                    None => {
                        println!("Failed to attach a buffer source to the panel.");
                    }
                    Some(buffersrc) => {
                        println!("Attached a buffersrc to the panel.");
                        let filepath = buffersrc.filepath.clone();
                        self.buffer_source = Some(buffersrc);
                        return iced::Task::perform(
                            read_file(filepath),
                            PanelMessage::AtttachContentToBuffer,
                        );
                    }
                }
                iced::Task::none()
            }
            PanelMessage::AtttachContentToBuffer(content) => {
                match content {
                    Err(e) => {
                        println!("Failed to get content: {:?}", e);
                    }
                    Ok(content) => {
                        self.buffer.data = content;
                        self.buffer.cursor = Cursor { row: 0, col: 0 };
                    }
                }
                iced::Task::none()
            }
            PanelMessage::ModeTransition(prev_mode, new_mode, transition_payload) => {
                self.mode = new_mode;
                self.shortcut_buffer = String::new();

                let mut tasks = vec![iced::Task::done(PanelMessage::ProcessStatusLineEvent(
                    PanelStatusLineMessage::DisplayMode(new_mode),
                ))];
                match transition_payload {
                    ModeTransitionPayload::Char(input_buffer) => {
                        tasks.push(iced::Task::done(PanelMessage::ProcessKeyInput(
                            input_buffer,
                        )));
                    }
                    ModeTransitionPayload::InsertModePayload(payload) => match new_mode {
                        PanelMode::Insert => {
                            tasks.push(iced::Task::done(PanelMessage::ProcessBufferEvent(
                                BufferMessage::CursorAxisMod(payload),
                            )));
                        }
                        // As it currently is, InsertModePayload is tightly coupled with PanelMode::Insert.
                        _ => unreachable!(),
                    },
                    ModeTransitionPayload::None => {}
                }
                if prev_mode == PanelMode::StatusCommand {
                    // An extra task needs to be dispatched to clear the status line command buffer.
                    tasks.push(iced::Task::done(PanelMessage::ProcessStatusLineEvent(
                        PanelStatusLineMessage::HandleCurrentAction(
                            CurrentActionHandelingMode::Abandon,
                        ),
                    )));
                }
                iced::Task::batch(tasks)
            }
            PanelMessage::ProcessKeyInput(key) => match self.mode {
                PanelMode::Insert => iced::Task::done(PanelMessage::ProcessBufferEvent(
                    BufferMessage::AddCharacter(key as u8),
                )),
                PanelMode::Normal => match self.handle_normal_mode_key(key) {
                    None => iced::Task::none(),
                    Some(msg) => iced::Task::done(msg),
                },
                PanelMode::StatusCommand => iced::Task::done(PanelMessage::ProcessStatusLineEvent(
                    PanelStatusLineMessage::ProcessKeyInput(key),
                )),
                PanelMode::ShortcutCommand => iced::Task::done(PanelMessage::ShortcutBuffer(key)),
            },
        }
    }

    fn view(&self) -> iced::Element<'_, EditorMessage> {
        iced::widget::column![
            self.tab_line.view(),
            self.buffer.view(),
            self.status_line.view()
        ]
        .into()
    }
}

#[derive(Debug, Clone)]
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
