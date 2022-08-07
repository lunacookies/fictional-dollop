mod headers;
pub use headers::*;

use arena::Id;

pub enum Ty {
	Named(String),
	Pointer(Id<Ty>),
	Missing,
}
