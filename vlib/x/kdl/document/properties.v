module document

// property_exists checks if a property key exists in a node's entries.
pub fn property_exists(node &Node, key string) bool {
	for entry in node.entries {
		if entry is Property && entry.key == key {
			return true
		}
	}
	return false
}

// property_get gets a property value by key (rightmost-wins).
pub fn property_get(node &Node, key string) ?Value {
	mut last_idx := -1
	for i, entry in node.entries {
		if entry is Property && entry.key == key {
			last_idx = i
		}
	}
	if last_idx >= 0 {
		if node.entries[last_idx] is Property {
			return node.entries[last_idx].value
		}
	}
	return none
}

// property_has checks if a node has any properties.
pub fn property_has(node &Node) bool {
	for entry in node.entries {
		if entry is Property {
			return true
		}
	}
	return false
}
