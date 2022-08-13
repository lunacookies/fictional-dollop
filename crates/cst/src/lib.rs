use syntax::{NodeKind, SyntaxNode, SyntaxToken, SyntaxTree, TokenKind};
use text_size::TextRange;

pub trait CstNode: Sized {
	fn cast(syntax: SyntaxNode, tree: &SyntaxTree) -> Option<Self>;
	fn syntax(self) -> SyntaxNode;
	fn text(self, tree: &SyntaxTree) -> &str {
		self.syntax().text(tree)
	}
	fn range(self, tree: &SyntaxTree) -> TextRange {
		self.syntax().range(tree)
	}
}

pub trait CstToken: Sized {
	fn cast(syntax: SyntaxToken, tree: &SyntaxTree) -> Option<Self>;
	fn syntax(self) -> SyntaxToken;
	fn text(self, tree: &SyntaxTree) -> &str {
		self.syntax().text(tree)
	}
	fn range(self, tree: &SyntaxTree) -> TextRange {
		self.syntax().range(tree)
	}
}

macro_rules! define_node {
	($kind:ident) => {
		#[derive(Clone, Copy)]
		pub struct $kind(SyntaxNode);

		impl CstNode for $kind {
			fn cast(syntax: SyntaxNode, tree: &SyntaxTree) -> Option<Self> {
				if syntax.kind(tree) == NodeKind::$kind {
					return Some(Self(syntax));
				}
				None
			}

			fn syntax(self) -> SyntaxNode {
				self.0
			}
		}
	};
}

macro_rules! define_compound_node {
	($name:ident, kinds: [$($kind:ident),+]) => {
		#[derive(Clone, Copy)]
		pub enum $name {
			$( $kind($kind), )+
		}

		impl CstNode for $name {
			fn cast(syntax: SyntaxNode, tree: &SyntaxTree) -> Option<Self> {
				match syntax.kind(tree) {
					$( NodeKind::$kind => Some(Self::$kind($kind(syntax))), )+
					_ => None
				}
			}

			fn syntax(self) -> SyntaxNode {
				match self {
					$( Self::$kind($kind(s)) => s, )+
				}
			}
		}
	};
}

macro_rules! define_token {
	($kind:ident) => {
		#[derive(Clone, Copy)]
		pub struct $kind(SyntaxToken);

		impl CstToken for $kind {
			fn cast(syntax: SyntaxToken, tree: &SyntaxTree) -> Option<Self> {
				if syntax.kind(tree) == TokenKind::$kind {
					return Some(Self(syntax));
				}
				None
			}

			fn syntax(self) -> SyntaxToken {
				self.0
			}
		}
	};
}

define_node!(SourceFile);
define_compound_node!(Item, kinds: [Strukt]);
define_node!(Strukt);
define_node!(Field);
define_compound_node!(Ty, kinds: [NamedTy, PointerTy, PrimitiveTy]);
define_node!(NamedTy);
define_node!(PointerTy);
define_node!(PrimitiveTy);
define_compound_node!(Path, kinds: [ForeignPath, LocalPath]);
define_node!(ForeignPath);
define_node!(LocalPath);
define_token!(Ident);

impl SourceFile {
	pub fn items(self, tree: &SyntaxTree) -> impl Iterator<Item = Item> + '_ {
		nodes(self, tree)
	}
}

impl Strukt {
	pub fn name(self, tree: &SyntaxTree) -> Option<Ident> {
		token(self, tree)
	}

	pub fn fields(
		self,
		tree: &SyntaxTree,
	) -> impl Iterator<Item = Field> + '_ {
		nodes(self, tree)
	}
}

impl Field {
	pub fn name(self, tree: &SyntaxTree) -> Option<Ident> {
		token(self, tree)
	}

	pub fn ty(self, tree: &SyntaxTree) -> Option<Ty> {
		node(self, tree)
	}
}

impl NamedTy {
	pub fn path(self, tree: &SyntaxTree) -> Option<Path> {
		node(self, tree)
	}
}

impl PointerTy {
	pub fn pointee(self, tree: &SyntaxTree) -> Option<Ty> {
		node(self, tree)
	}
}

impl PrimitiveTy {
	pub fn kind(self, tree: &SyntaxTree) -> Option<PrimitiveTyKind> {
		self.0.child_tokens(tree).next().and_then(|t| match t.kind(tree) {
			TokenKind::U32Kw => Some(PrimitiveTyKind::U32),
			_ => None,
		})
	}
}

pub enum PrimitiveTyKind {
	U32,
}

impl ForeignPath {
	pub fn module_name(self, tree: &SyntaxTree) -> Option<Ident> {
		token(self, tree)
	}

	pub fn item_name(self, tree: &SyntaxTree) -> Option<Ident> {
		tokens(self, tree).nth(1)
	}
}

impl LocalPath {
	pub fn item_name(self, tree: &SyntaxTree) -> Option<Ident> {
		token(self, tree)
	}
}

fn node<Parent: CstNode, Child: CstNode>(
	node: Parent,
	tree: &SyntaxTree,
) -> Option<Child> {
	node.syntax().child_nodes(tree).find_map(|c| Child::cast(c, tree))
}

fn token<Parent: CstNode, Child: CstToken>(
	node: Parent,
	tree: &SyntaxTree,
) -> Option<Child> {
	node.syntax().child_tokens(tree).find_map(|c| Child::cast(c, tree))
}

fn nodes<Parent: CstNode, Child: CstNode>(
	node: Parent,
	tree: &SyntaxTree,
) -> impl Iterator<Item = Child> + '_ {
	node.syntax().child_nodes(tree).filter_map(|c| Child::cast(c, tree))
}

fn tokens<Parent: CstNode, Child: CstToken>(
	node: Parent,
	tree: &SyntaxTree,
) -> impl Iterator<Item = Child> + '_ {
	node.syntax().child_tokens(tree).filter_map(|c| Child::cast(c, tree))
}
