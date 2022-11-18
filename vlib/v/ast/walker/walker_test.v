import v.ast
import v.ast.walker
import v.parser
import v.pref

fn parse_text(text string) &ast.File {
	tbl := ast.new_table()
	prefs := pref.new_preferences()
	return parser.parse_text(text, '', tbl, .skip_comments, prefs)
}

struct NodeByOffset {
	pos int
mut:
	node ast.Node
}

fn (mut n NodeByOffset) visit(node &ast.Node) ! {
	node_pos := node.pos()
	if n.pos >= node_pos.pos && n.pos <= node_pos.pos + node_pos.len && node !is ast.File {
		n.node = node
		return error('')
	}
	return
}

fn test_walk() {
	source := '
module main
struct Foo {
	name string
}
	'
	file := parse_text(source)
	mut nbo := NodeByOffset{
		pos: 13
	}
	walker.walk(mut nbo, file)
	assert nbo.node is ast.Stmt
	stmt := nbo.node as ast.Stmt
	assert stmt is ast.StructDecl
}

fn test_inspect() {
	source := '
module main
	'
	file := parse_text(source)
	walker.inspect(file, unsafe { nil }, fn (node &ast.Node, data voidptr) bool {
		// Second visit must be ast.Stmt
		if node is ast.Stmt {
			if node !is ast.Module {
				// Proceed to another node
				return false
			}
			assert node is ast.Module
			mod := node as ast.Module
			assert mod.name == 'main'
			return false
		}
		assert node is ast.File
		// True means that the inspector must now
		// inspect the ast.File's children
		return true
	})
}
