module doc

pub fn (nodes []DocNode) find(symname string) ?DocNode {
	for node in nodes {
		if node.name != symname {
			continue
		}
		return node
	}
	return error('symbol not found')
}

// sort_by_name sorts the array based on the symbol names.
pub fn (mut nodes []DocNode) sort_by_name() {
	nodes.sort_with_compare(compare_nodes_by_name)
}

// sort_by_kind sorts the array based on the symbol kind.
pub fn (mut nodes []DocNode) sort_by_kind() {
	nodes.sort_with_compare(compare_nodes_by_kind)
}

fn compare_nodes_by_kind(a &DocNode, b &DocNode) int {
	ak := int((*a).kind)
	bk := int((*b).kind)
	if ak < bk {
		return -1
	}
	if ak > bk {
		return 1
	}
	return 0
}

fn compare_nodes_by_name(a &DocNode, b &DocNode) int {
	al := a.name.to_lower()
	bl := b.name.to_lower()
	return compare_strings(al, bl)
}

// arr() converts the map into an array of `DocNode`.
pub fn (cnts map[string]DocNode) arr() []DocNode {
	mut contents := cnts.keys().map(cnts[it])
	contents.sort_by_name()
	contents.sort_by_kind()
	return contents
}
