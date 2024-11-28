module main

import os
import encoding.xml

fn test_valid_parsing() {
	path := os.join_path(os.dir(@FILE), 'shared.xml')

	expected := xml.XMLDocument{
		root: xml.XMLNode{
			name:       'sst'
			attributes: {
				'count':       '5'
				'uniqueCount': '5'
			}
			children:   [
				xml.XMLNode{
					name:     'si'
					children: [
						xml.XMLNode{
							name:       't'
							attributes: {
								'a': '1'
							}
							children:   ['Item 1']
						},
					]
				},
				xml.XMLNode{
					name:     'si'
					children: [
						xml.XMLNode{
							name:     't'
							children: ['Item 2']
						},
					]
				},
				xml.XMLNode{
					name:     'si'
					children: [
						xml.XMLNode{
							name:     't'
							children: ['Item 3']
						},
					]
				},
				xml.XMLNode{
					name:     'si'
					children: [
						xml.XMLNode{
							name:     't'
							children: ['Item 4']
						},
					]
				},
				xml.XMLNode{
					name:     'si'
					children: [
						xml.XMLNode{
							name:     't'
							children: ['Item 5']
						},
					]
				},
			]
		}
	}
	actual := xml.XMLDocument.from_file(path)!

	assert expected == actual, 'Parsed XML document should be equal to expected XML document'
}
