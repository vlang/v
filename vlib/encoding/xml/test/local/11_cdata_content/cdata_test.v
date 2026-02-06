module main

import os
import encoding.xml

fn test_valid_parsing() {
	path := os.join_path(os.dir(@FILE), 'cdata.xml')

	expected := xml.XMLDocument{
		root: xml.XMLNode{
			name:     'sample'
			children: [
				xml.XMLNode{
					name:     'html'
					children: ['This is &lt;b&gt;bold&lt;/b&gt;']
				},
				xml.XMLNode{
					name:     'html'
					children: [xml.XMLCData{
						text: 'This is <b>bold</b>'
					}]
				},
			]
		}
	}
	actual := xml.XMLDocument.from_file(path)!

	assert expected == actual, 'Parsed XML document should be equal to expected XML document'
}
