module main

import os
import encoding.xml

fn test_valid_parsing() {
	path := os.join_path(os.dir(@FILE), 'element.xml')

	expected := xml.XMLDocument{
		doctype: xml.DocumentType{
			name: 'note'
			dtd:  xml.DocumentTypeDefinition{
				name: ''
				list: [
					xml.DTDElement{
						name:       'note'
						definition: ['to', 'from', 'heading', 'body']
					},
					xml.DTDElement{
						name:       'to'
						definition: ['#PCDATA']
					},
					xml.DTDElement{
						name:       'from'
						definition: ['#PCDATA']
					},
					xml.DTDElement{
						name:       'heading'
						definition: ['#PCDATA']
					},
					xml.DTDElement{
						name:       'body'
						definition: ['#PCDATA']
					},
				]
			}
		}
		root:    xml.XMLNode{
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
	actual := xml.XMLDocument.from_file(path)!.validate()!

	assert expected == actual, 'Parsed XML document should be equal to expected XML document'
}
