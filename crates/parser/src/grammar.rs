use crate::Parser;
use syntax::NodeKind;

pub(crate) fn source_file(p: &mut Parser<'_>) {
	p.start_node(NodeKind::SourceFile);

	p.finish_node();
}
