module svg

import encoding.xml
import os

// parse_svg parses an SVG document from a string. Well-formed SVG is scanned
// directly into the element tree; the general XML parser handles anything the
// fast scanner cannot.
pub fn parse_svg(input string) !Document {
	if root := fast_parse_svg(input) {
		return document_from_root(root)
	}
	doc := xml.XMLDocument.from_string(input) or { return error('invalid XML: ${err.msg()}') }
	return document_from_root(from_xml_node(doc.root))
}

// parse_svg_file parses an SVG document from a file.
pub fn parse_svg_file(path string) !Document {
	contents := os.read_file(path) or { return error('cannot read SVG file: ${err.msg()}') }
	return parse_svg(contents)
}

// Document.from_string parses SVG markup into a document.
pub fn Document.from_string(input string) !Document {
	return parse_svg(input)
}

// Document.from_file parses an SVG file into a document.
pub fn Document.from_file(path string) !Document {
	return parse_svg_file(path)
}

fn document_from_root(root Element) !Document {
	mut result := Document{ root: root }
	result.width = root.get_attr('width')
	if result.width == '' {
		result.width = '100%'
	}
	result.height = root.get_attr('height')
	if result.height == '' {
		result.height = '100%'
	}
	if root.has_attr('viewBox') {
		result.view_box = parse_viewbox(root.get_attr('viewBox')) or { return err }
		result.has_view_box = true
	}
	if root.has_attr('preserveAspectRatio') {
		result.preserve = parse_aspect_ratio(root.get_attr('preserveAspectRatio')) or { return err }
	}
	result.css = collect_styles(&result.root)
	result.sync_root()
	return result
}

fn from_xml_node(node xml.XMLNode) Element {
	mut e := elem(node.name)
	mut keys := node.attributes.keys()
	keys.sort()
	for k in keys {
		// Normalize xlink:href to href while preserving the value.
		if k == 'xlink:href' && !node.attributes['xlink:href'].starts_with('#') {
			e.set_attr('href', node.attributes[k])
		} else if k == 'xlink:href' {
			e.set_attr(k, node.attributes[k])
		} else {
			e.set_attr(k, node.attributes[k])
		}
	}
	if 'style' in node.attributes {
		e.style = parse_style(node.attributes['style']) or { Style{} }
	}
	for child in node.children {
		if child is xml.XMLNode {
			e.children << from_xml_node(child)
		} else if child is string {
			e.text += child
		} else if child is xml.XMLCData {
			e.text += child.text
		}
	}
	return e
}

fn collect_styles(e &Element) []CssRule {
	mut rules := []CssRule{}
	if e.tag == 'style' && e.text.trim_space() != '' {
		rules << parse_css(e.text)
	}
	for i in 0 .. e.children.len {
		rules << collect_styles(&e.children[i])
	}
	return rules
}
