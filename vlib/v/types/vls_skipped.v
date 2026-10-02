module types

import v.flat
import v.parser
import v.token

// The parse decides a `$if` on the platform, the backend or a flag that is not
// defined, and leaves out the branch it does not take: no node stands for that
// code, written for another platform or build, and a question about a name
// there had nothing to answer from. Such a question parses the file again,
// keeping every branch as `v fmt` does, and adds those nodes after the others.
// The check never sees them: a question comes after the check, and the child
// of a diagnostics server makes its diagnostics before it answers one. Where
// the code was checked, its own nodes come first; a name that the left-out code
// uses and the checked code declares leads back to that declaration (see
// vls_local_binding), with the type the check gave it.

// vls_add_skipped_branches gives the file `file_id` nodes for the branches that
// its parse left out, once, and reports whether it added them now.
fn (mut tc TypeChecker) vls_add_skipped_branches(file_id int) bool {
	if tc.vls_reparsed_files[file_id] or { false } || isnil(tc.vls_prefs) {
		return false
	}
	tc.vls_reparsed_files[file_id] = true
	file := tc.a.source_files[file_id] or { return false }
	mut prefs := *tc.vls_prefs
	prefs.preserve_comptime_conditionals = true
	mut p := parser.Parser.new(&prefs)
	reparsed := p.parse_file(file.name)
	mut parsed_id := -1
	for id, parsed_file in reparsed.source_files {
		if parsed_file.name == file.name {
			parsed_id = id
			break
		}
	}
	if parsed_id < 0 || reparsed.nodes.len == 0 {
		return false
	}
	// The nodes of the checked code of the file, to find the twin of each added
	// one: the same code, parsed twice.
	mut checked := map[string]flat.NodeId{}
	for idx in tc.a.user_code_start .. tc.a.nodes.len {
		node := tc.a.nodes[idx]
		if node.pos.id == file_id && node.pos.end > node.pos.offset {
			key := vls_twin_key(node)
			if key !in checked {
				checked[key] = flat.NodeId(idx)
			}
		}
	}
	mut a := tc.a
	start := a.nodes.len
	tc.vls_added_start = int_min(tc.vls_added_start, start)
	child_shift := i32(a.children.len)
	for child in reparsed.children {
		a.children << if int(child) >= 0 { flat.NodeId(int(child) + start) } else { child }
	}
	for node in reparsed.nodes {
		mut added := node
		if added.children_count != 0 {
			added.children_start += child_shift
		}
		if added.pos.id == parsed_id {
			added = added.with_pos(token.Pos{
				...added.pos
				id: i32(file_id)
			})
		}
		// Declaration attributes name their declaration by its id.
		if added.kind == .directive && added.value.starts_with('@attributes:') {
			added.value = '@attributes:${added.value['@attributes:'.len..].int() + start}'
		}
		if added.pos.id == file_id && added.pos.end > added.pos.offset {
			if twin := checked[vls_twin_key(added)] {
				tc.vls_twins[a.nodes.len] = twin
			}
		}
		a.nodes << added
	}
	tc.index_rewritten_parents_after(start)
	return true
}

// vls_twin_key tells apart the nodes of one file: the same code gets the same
// key however many times it is parsed.
fn vls_twin_key(node flat.Node) string {
	return '${int(node.kind)} ${node.pos.offset} ${node.pos.end} ${node.value}'
}
