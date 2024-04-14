module main

import os
import encoding.xml

fn test_valid_parsing() {
	expected := xml.XMLDocument.from_file(os.join_path(os.dir(@FILE), 'entity.xml'))!.validate()!
	actual := xml.XMLDocument.from_file(os.join_path(os.dir(@FILE), 'entity_expected.xml'))!.validate()!
	assert expected == actual, 'Parsed XML document should be equal to expected XML document'
}
