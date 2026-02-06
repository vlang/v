module main

import os
import encoding.xml

fn test_valid_parsing() {
	// We use a .bin file to avoid stripping the BOM from the XML file
	path := os.join_path(os.dir(@FILE), 'workbook.bin')

	doc := xml.XMLDocument.from_file(path) or {
		assert false, 'Failed to parse workbook.bin'
		exit(1)
	}

	sheets := doc.get_elements_by_tag('sheet')
	assert sheets.len == 1, 'Expected 1 sheet, got ${sheets.len}'
}
