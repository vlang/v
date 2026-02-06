module main

import os
import encoding.xml

// All the XML files in the spec directory obtained recursively
const spec_files = os.walk_ext(os.join_path(os.dir(@FILE), 'local'), 'xml')

fn test_can_parse_all_files() ! {
	assert spec_files.len > 0, 'No XML files found in the spec directory'
	for file in spec_files {
		doc := xml.XMLDocument.from_file(file) or {
			// Parsing failed. Check if this was an expected error.
			parent := os.dir(file)
			error_file := os.join_path(parent, 'expected_error.txt')
			error_text := os.read_file(error_file) or {
				// No expected error. Fail the test.
				return error('Failed to parse XML file: ' + file)
			}
			// Check if the error message matches the expected error.
			assert err.msg().trim_space() == error_text.trim_space()
			continue
		}
	}
}
