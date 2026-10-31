import os

const branch_tail_locals_source = 'struct Doc {
	title string
}

struct Node {
	doc &Doc = unsafe { nil }
}

@[heap]
struct Element {
	Node
}

fn (n &Node) document() &Doc {
	return n.doc
}

fn (d &Doc) name() string {
	return d.title
}

fn (e &Element) as_node() &Node {
	return &e.Node
}

fn dom_node(e &Element) &Node {
	return &e.Node
}

fn find(e &Element, found bool) ?&Element {
	if found {
		return e
	}
	return none
}

struct Options {
	doc   ?&Doc
	title string
}
'

// check_branch_tail_locals_program checks `body` after the declarations of a
// node tree. The call info memo is off, so every typing of a branch value
// against an expected type resolves its calls again, as it does for a call whose
// memo slot another call of a large function took.
fn check_branch_tail_locals_program(name string, body string) os.Result {
	root := os.join_path(os.vtmp_dir(), 'v_branch_tail_locals_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), branch_tail_locals_source + body) or {
		panic(err)
	}
	os.setenv('V3_NO_CALL_INFO_MEMO', '1', true)
	defer {
		os.unsetenv('V3_NO_CALL_INFO_MEMO')
	}
	return os.exec([@VEXE, '-check', root])
}

fn test_if_guard_variable_in_struct_field_values() {
	res := check_branch_tail_locals_program('struct_field', 'fn main() {
	e := &Element{}
	found := true
	o := Options{
		doc:   if element := find(e, found) {
			dom_node(element).document()
		} else {
			none
		}
		title: if element := find(e, found) {
			dom_node(element).document().name()
		} else {
			""
		}
	}
	println(o)
}
')
	assert res.exit_code == 0, res.output
}

fn test_branch_locals_in_returned_match_value() {
	res := check_branch_tail_locals_program('match', 'fn title(e &Element, k int) ?string {
	return match k {
		0 {
			element := find(e, true) or { return none }
			node := element.as_node()
			node.document().name()
		}
		1 {
			n, element := 2, find(e, true) or { return none }
			dom_node(element).document().name().repeat(n)
		}
		else {
			none
		}
	}
}

fn main() {
	println(title(&Element{}, 1))
}
')
	assert res.exit_code == 0, res.output
}

// Names that are not bound, including a guard variable in the `else` branch,
// are still reported.
fn test_undefined_names_in_branch_values_are_still_reported() {
	res := check_branch_tail_locals_program('undefined', 'fn main() {
	e := &Element{}
	mut t := ""
	t = if element := find(e, true) {
		missing.as_node().document().name()
	} else {
		element.as_node().document().name()
	}
	println(t)
}
')
	assert res.exit_code != 0, res.output
	assert res.output.contains('undefined ident: `missing`'), res.output
	assert res.output.contains('undefined ident: `element`'), res.output
}
