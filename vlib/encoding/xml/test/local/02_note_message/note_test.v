import os
import encoding.xml

fn test_valid_parsing() ! {
	path := os.join_path(os.dir(@FILE), 'note.xml')

	expected := xml.XMLDocument{
		root: xml.XMLNode{
			name:     'note'
			children: [
				xml.XMLNode{
					name:     'to'
					children: [
						'Tove',
					]
				},
				xml.XMLNode{
					name:     'from'
					children: [
						'Jani',
					]
				},
				xml.XMLNode{
					name:     'heading'
					children: [
						'Reminder',
					]
				},
				xml.XMLNode{
					name:     'body'
					children: [
						"Don't forget me this weekend!",
					]
				},
			]
		}
	}
	actual := xml.XMLDocument.from_file(path)!

	assert expected == actual, 'Parsed XML document should be equal to expected XML document'
}
