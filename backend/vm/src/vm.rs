use std::io::Write;

pub struct Vm<'w, W>
where
    W: Write + 'w,
{
    _writer: &'w mut W,
}

impl<'w, W> Vm<'w, W>
where
    W: Write + 'w,
{
    pub fn new(_writer: &'w mut W) -> Self {
        Self { _writer }
    }
}
