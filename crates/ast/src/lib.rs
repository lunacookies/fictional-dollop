mod headers;
pub use headers::*;

use arena::Id;

#[derive(Clone, Copy)]
pub enum Ty {
	Named(Path),
	Pointer(Id<Ty>),
	Missing,
}

#[derive(Clone, Copy)]
pub enum Path {
	Local { item: Id<String> },
	Foreign { module: Id<String>, item: Id<String> },
}
