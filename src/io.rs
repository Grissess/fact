use std::str;
use std::fmt::Debug;

#[derive(Debug)]
pub enum BytesToCharsError<E> {
    Reader(E),
    Invalid([u8; 4]),
}

#[derive(Debug)]
pub struct BytesToChars<E, I: Iterator<Item=Result<u8, E>>> {
    iter: I,
    fuse: bool,
}

impl<E, I: Iterator<Item=Result<u8, E>>> BytesToChars<E, I> {
    pub fn new(iter: I) -> BytesToChars<E, I> {
        BytesToChars {
            iter: iter,
            fuse: false,
        }
    }
}

impl<E, I: Iterator<Item=Result<u8, E>>> Iterator for BytesToChars<E, I> {
    type Item = Result<char, BytesToCharsError<E>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.fuse { return None; }

        let mut pos: u8 = 0;
        let mut buf: [u8; 4] = Default::default();

        while pos < 4 {
            let byte = match self.iter.next() {
                None => {
                    self.fuse = true;
                    return None
                },
                Some(Err(e)) => {
                    self.fuse = true;
                    return Some(Err(BytesToCharsError::Reader(e)))
                },
                Some(Ok(b)) => b,
            };

            buf[pos as usize] = byte;
            pos += 1;

            match str::from_utf8(&buf) {
                Ok(s) => return Some(Ok(s.chars().next().expect("empty string while consuming"))),
                Err(e) => (),
            };

        }

        self.fuse = true;
        Some(Err(BytesToCharsError::Invalid(buf)))
    }
}

#[derive(Debug)]
pub struct Unwrapped<T, E: Debug, I: Iterator<Item=Result<T, E>>> {
    iter: I,
}

impl<T, E: Debug, I: Iterator<Item=Result<T, E>>> Unwrapped<T, E, I> {
    pub fn new(iter: I) -> Unwrapped<T, E, I> {
        Unwrapped {
            iter: iter,
        }
    }
}

impl<T, E: Debug, I: Iterator<Item=Result<T, E>>> Iterator for Unwrapped<T, E, I> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(Result::unwrap)
    }
}
