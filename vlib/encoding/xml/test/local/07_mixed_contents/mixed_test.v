import os
import encoding.xml

fn test_valid_parsing() ! {
	path := os.join_path(os.dir(@FILE), 'mixed.xml')

	expected := xml.XMLDocument{
		root: xml.XMLNode{
			name:     'letter'
			children: [
				'Dear Mr.',
				xml.XMLNode{
					name:     'name'
					children: ['John Smith']
				},
				'.\n  Your order',
				xml.XMLNode{
					name:     'orderid'
					children: ['1032']
				},
				'will be shipped on',
				xml.XMLNode{
					name:     'shipdate'
					children: ['2001-07-13']
				},
				'.',
			]
		}
	}
	actual := xml.XMLDocument.from_file(path)!

	assert expected == actual, 'Parsed XML document should be equal to expected XML document'
}
