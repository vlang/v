module main

import os
import xml

fn test_valid_parsing() {
	path := os.join_path(os.dir(@FILE), 'entity.xml')

	mut reverse_entities := xml.default_entities_reverse.clone()
	reverse_entities['Warning: Something bad happened... please refresh and try again.'] = 'warning'

	expected := xml.XMLDocument{
		parsed_reverse_entities: reverse_entities
		doctype: xml.DocumentType{
			name: 'body'
			dtd: xml.DocumentTypeDefinition{
				name: ''
				list: [
					xml.DTDEntity{
						name: 'warning'
						value: 'Warning: Something bad happened... please refresh and try again.'
					},
				]
			}
		}
		root: xml.XMLNode{
			name: 'body'
			children: [
				xml.XMLNode{
					name: 'message'
					children: [
						'Warning: Something bad happened... please refresh and try again.',
					]
				},
			]
		}
	}
	actual := xml.XMLDocument.from_file(path)!.validate()!

	assert expected == actual, 'Parsed XML document should be equal to expected XML document'
}
