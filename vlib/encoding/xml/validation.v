module xml

fn (node XMLNode) validate(elements map[string]DTDElement, entities map[string]string) !XMLNode {
	mut children := []XMLNodeContents{cap: node.children.len}

	valid_elements := elements[node.name].definition
	mut validate_node_children := node.name in elements

	// Check if the node will match everything
	if valid_elements.len == 1 && valid_elements[0] == '#PCDATA' {
		validate_node_children = false
	}

	for child in node.children {
		match child {
			XMLNode {
				if validate_node_children {
					name := child.name
					if name !in valid_elements {
						return error('Invalid child element ${name} for ${node.name}')
					}
				}
				children << child.validate(elements, entities)!
			}
			string {
				children << unescape_text(child, entities: entities)!
			}
			else {
				// Ignore other nodes
				children << child
			}
		}
	}

	return XMLNode{
		name:       node.name
		attributes: node.attributes
		children:   children
	}
}

// validate checks the document is well-formed and valid. It returns a new
// document with the parsed entities expanded when validation is successful.
// Otherwise it returns an error.
pub fn (doc XMLDocument) validate() !XMLDocument {
	// The document is well-formed because we were able to parse it properly.
	match doc.doctype.dtd {
		DocumentTypeDefinition {
			// Store the element and entity definitions
			mut elements := map[string]DTDElement{}
			mut entities := default_entities.clone()
			mut reverse_entities := default_entities_reverse.clone()

			for item in doc.doctype.dtd.list {
				match item {
					DTDElement {
						name := item.name
						if name in elements {
							return error('Duplicate element definition for ${name}')
						}
						elements[name] = item
					}
					DTDEntity {
						name := item.name
						if name in entities {
							return error('Duplicate entity definition for ${name}')
						}
						entities[name] = item.value
						reverse_entities[item.value] = name
					}
				}
			}

			// Now validate the document against the elements and entities.
			new_root := doc.root.validate(elements, entities)!

			// Check the DOCTYPE name matches the root name
			if doc.doctype.name != '' && doc.doctype.name != new_root.name {
				return error('Root element ${new_root.name} does not match DOCTYPE ${doc.doctype.name}')
			}

			return XMLDocument{
				version:                 doc.version
				encoding:                doc.encoding
				doctype:                 doc.doctype
				comments:                doc.comments
				root:                    new_root
				parsed_reverse_entities: reverse_entities
			}
		}
		string {
			// TODO: Validate the document against the DTD string.
			return doc
		}
	}
}
