import os
import encoding.xml

fn test_valid_parsing() ! {
	path := os.join_path(os.dir(@FILE), 'hello_world.xml')

	expected := xml.XMLDocument{
		root: xml.XMLNode{
			name:     'message'
			children: [
				xml.XMLNode{
					name:     'greeting'
					children: [
						'Hello, World!',
					]
				},
			]
		}
	}
	actual := xml.XMLDocument.from_file(path)!

	assert expected == actual, 'Parsed XML document should be equal to expected XML document'
}
