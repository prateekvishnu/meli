/// A `(x, y)` position on screen.
pub type Pos = (usize, usize);

#[inline(always)]
pub fn get_x(p: Pos) -> usize {
    p.0
}
#[inline(always)]
pub fn get_y(p: Pos) -> usize {
    p.1
}
#[inline(always)]
pub fn set_x(p: Pos, new_x: usize) -> Pos {
    (new_x, p.1)
}
#[inline(always)]
pub fn set_y(p: Pos, new_y: usize) -> Pos {
    (p.0, new_y)
}



/// A `(cols, rows)` size.
pub type Size = (usize, usize);

pub trait HasSize {
    fn size(&self) -> Size;
}

pub trait HasPosition {
    fn origin(&self) -> Pos;
    fn set_origin(&mut self, new_origin: Pos);
}

/// A cursor position.
pub struct Cursor {
    pos: Option<Pos>,
    last_pos: Option<Pos>,
}

impl Cursor {
    pub fn new() -> Cursor {
        Cursor {
            pos: None,
            last_pos: None,
        }
    }

    /// Checks whether the current and last coordinates are sequential and returns `true` if they
    /// are and `false` otherwise.
    pub fn is_seq(&self) -> bool {
        if let Some((cx, cy)) = self.pos {
            if let Some((lx, ly)) = self.last_pos {
                (lx + 1, ly) == (cx, cy)
            } else {
                false
            }
        } else {
            false
        }
    }

    pub fn pos(&self) -> Option<Pos> {
        self.pos
    }

    pub fn set_pos(&mut self, newpos: Option<Pos>) {
        self.last_pos = self.pos;
        self.pos = newpos;
    }

    pub fn invalidate_last_pos(&mut self) {
        self.last_pos = None;
    }
}