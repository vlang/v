module main

import os
import encoding.xml

fn test_valid_parsing() {
	path := os.join_path(os.dir(@FILE), 'attributes.xml')

	expected := xml.XMLDocument{
		root: xml.XMLNode{
			name:       'book'
			attributes: {
				'category': 'web'
			}
			children:   [
				xml.XMLNode{
					name:       'title'
					attributes: {
						'lang':      'en'
						'code:type': 'const char*'
					}
					children:   ['Learning XML']
				},
				xml.XMLNode{
					name:       'author'
					attributes: {
						'attr': ' surrounding spaces '
					}
					children:   ['Erik T. Ray']
				},
				xml.XMLNode{
					name:     'year'
					children: ['2003']
				},
				xml.XMLNode{
					name:     'price'
					children: ['39.95']
				},
			]
		}
	}
	actual := xml.XMLDocument.from_file(path)!

	assert expected == actual, 'Parsed XML document should be equal to expected XML document'
}
