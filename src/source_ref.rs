use std::cmp::{min, max};

#[derive(Clone, PartialOrd, PartialEq, Ord, Eq, Debug)]
pub struct SourceRef {
    // Represents a section of a larger string
    pub offset: usize,
    pub len: usize,
    pub line: usize,
}

impl SourceRef {
    pub fn new(offset: usize, len: usize, line: usize) -> SourceRef {
        SourceRef { offset, len, line }
    }
    pub fn merge(&self, other: &SourceRef) -> SourceRef {
        let start = min(other.offset, self.offset);
        let end = max(self.offset + self.len, other.offset + other.len);
        SourceRef { offset: start, len: end - start, line: min(self.line, other.line) }
    }
    pub fn source(&self, src: &str) -> String {
        src.chars().skip(self.offset).take(self.len).collect()
    }
}

#[test]
fn merging() {
    let a = SourceRef::new(0, 2, 0);
    let b = SourceRef::new(6, 2, 0);
    let c = SourceRef::new(1, 2, 0);

    assert_eq!(a.merge(&b), SourceRef::new(0, 8, 0));
    assert_eq!(a.merge(&c), SourceRef::new(0, 3, 0));
    assert_ne!(a.merge(&c), SourceRef::new(0, 4, 0));

    assert_eq!(a.merge(&c), c.merge(&a));
    assert_eq!(a.merge(&b), b.merge(&a));
    assert_eq!(b.merge(&c), c.merge(&b));
}

#[test]
fn source() {
    let src = "this is my source";
    let a = SourceRef::new(0, 4, 0 );
    let b = SourceRef::new(5, 2, 0);
    let c = SourceRef::new(0, src.chars().count(), 0);

    assert_eq!(a.source(src), "this");
    assert_eq!(b.source(src), "is");
    assert_eq!(c.source(src), "this is my source");

}