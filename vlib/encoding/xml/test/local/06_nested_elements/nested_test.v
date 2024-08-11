import os
import encoding.xml

fn test_valid_parsing() ! {
	path := os.join_path(os.dir(@FILE), 'nested.xml')

	expected := xml.XMLDocument{
		root: xml.XMLNode{
			name:     'level1'
			children: [
				xml.XMLNode{
					name:     'level2'
					children: [
						xml.XMLNode{
							name:     'level3'
							children: [
								xml.XMLNode{
									name:     'level4'
									children: [
										'Deeply nested content.',
									]
								},
							]
						},
					]
				},
				xml.XMLNode{
					name:     'level2'
					children: [
						xml.XMLNode{
							name:     'level3'
							children: [
								'Less deeply nested content.',
							]
						},
					]
				},
			]
		}
	}
	actual := xml.XMLDocument.from_file(path)!

	assert expected == actual, 'Parsed XML document should be equal to expected XML document'
}
