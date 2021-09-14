use std::cmp::{min, max};
use std::rc::Rc;
use std::fmt::{Display, Formatter, Pointer};
use colored::*;

#[derive(Clone, PartialOrd, PartialEq, Ord, Eq, Debug)]
pub struct SourceRef {
    // Represents a section of a larger string
    pub offset: usize,
    pub len: usize,
    pub line: usize,
    src: Rc<String>,
}

impl SourceRef {
    pub fn new(offset: usize, len: usize, line: usize, src: Rc<String>) -> SourceRef {
        SourceRef { offset, len, line, src }
    }
    pub fn merge(&self, other: &SourceRef) -> SourceRef {
        let start = min(other.offset, self.offset);
        let end = max(self.offset + self.len, other.offset + other.len);
        SourceRef { offset: start, len: end - start, line: min(self.line, other.line), src: self.src.clone() }
    }
    pub fn source(&self) -> String {
        self.src.chars().skip(self.offset).take(self.len).collect()
    }
}

impl Display for SourceRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut source = self.src.chars().skip(self.offset).take(self.len).collect::<String>().red().to_string();
        let before = self.src.chars().skip(self.offset-10).take(10).collect::<String>().white().to_string();
        let after = self.src.chars().skip(self.offset+self.len).take(10).collect::<String>().white().to_string();
        write!(f, "{}{}{}", before, source, after)
    }
}

#[test]
fn merging() {
    let src = Rc::new(format!("Hello!"));
    let a = SourceRef::new(0, 2, 0, src.clone());
    let b = SourceRef::new(6, 2, 0, src.clone());
    let c = SourceRef::new(1, 2, 0, src.clone());

    assert_eq!(a.merge(&b), SourceRef::new(0, 8, 0, src.clone()));
    assert_eq!(a.merge(&c), SourceRef::new(0, 3, 0, src.clone()));
    assert_ne!(a.merge(&c), SourceRef::new(0, 4, 0, src.clone()));

    assert_eq!(a.merge(&c), c.merge(&a));
    assert_eq!(a.merge(&b), b.merge(&a));
    assert_eq!(b.merge(&c), c.merge(&b));
}

#[test]
fn source() {
    let src = Rc::new("this is my source".to_string());
    let a = SourceRef::new(0, 4, 0, src.clone());
    let b = SourceRef::new(5, 2, 0, src.clone());
    let c = SourceRef::new(0, src.chars().count(), 0, src.clone());

    assert_eq!(a.source(), "this");
    assert_eq!(b.source(), "is");
    assert_eq!(c.source(), "this is my source");

}