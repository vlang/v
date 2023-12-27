module xml

// get_element_by_id returns the first element with the given id, or none if no
// such element exists in the subtree rooted at this node.
pub fn (node XMLNode) get_element_by_id(id string) ?XMLNode {
	// Is this the node we're looking for?
	if attribute_id := node.attributes['id'] {
		if attribute_id == id {
			return node
		}
	}

	if node.children.len == 0 {
		return none
	}

	// Recurse into children
	for child in node.children {
		match child {
			XMLNode {
				if result := child.get_element_by_id(id) {
					return result
				}
			}
			else {}
		}
	}

	return none
}

// get_elements_by_tag returns all elements with the given tag name in the subtree
// rooted at this node. If there are no such elements, an empty array is returned.
pub fn (node XMLNode) get_elements_by_tag(tag string) []XMLNode {
	mut result := []XMLNode{}

	if node.name == tag {
		result << node
	}

	if node.children.len == 0 {
		return result
	}

	// Recurse into children
	for child in node.children {
		if child is XMLNode {
			result << child.get_elements_by_tag(tag)
		}
	}

	return result
}

// get_elements_by_attribute returns all elements with the given attribute-value pair in
// the subtree rooted at this node. If there are no such elements, an empty array is returned.
pub fn (node XMLNode) get_elements_by_attribute(attribute string, value string) []XMLNode {
	mut result := []XMLNode{}

	if attribute_value := node.attributes[attribute] {
		if attribute_value == value {
			result << node
		}
	}

	if node.children.len == 0 {
		return result
	}

	// Recurse into children
	for child in node.children {
		if child is XMLNode {
			result << child.get_elements_by_attribute(attribute, value)
		}
	}

	return result
}

// get_element_by_id returns the first element with the given id, or none if no
// such element exists.
pub fn (doc XMLDocument) get_element_by_id(id string) ?XMLNode {
	return doc.root.get_element_by_id(id)
}

// get_elements_by_attribute returns all elements with the given attribute-value pair.
// If there are no such elements, an empty array is returned.
pub fn (doc XMLDocument) get_elements_by_attribute(attribute string, value string) []XMLNode {
	return doc.root.get_elements_by_attribute(attribute, value)
}

// get_elements_by_tag returns all elements with the given tag name.
// If there are no such elements, an empty array is returned.
pub fn (doc XMLDocument) get_elements_by_tag(tag string) []XMLNode {
	return doc.root.get_elements_by_tag(tag)
}
