import os
import encoding.xml

fn test_valid_parsing() ! {
	path := os.join_path(os.dir(@FILE), 'comment.xml')

	expected := xml.XMLDocument{
		comments: [
			xml.XMLComment{
				text: ' Employee Information'
			},
		]
		root:     xml.XMLNode{
			name:     'address'
			children: [
				xml.XMLComment{
					text: ' Full or first name '
				},
				xml.XMLNode{
					name:     'name'
					children: ['Jones']
				},
				xml.XMLComment{
					text: ' Registered name of the company -> '
				},
				xml.XMLNode{
					name:     'company'
					children: ['ABSystems']
				},
				xml.XMLNode{
					name:     'phone'
					children: [xml.XMLComment{
						text: ' Phone with country code -) '
					}, '(046) 1233-44778']
				},
			]
		}
	}
	actual := xml.XMLDocument.from_file(path)!

	assert expected == actual, 'Parsed XML document should be equal to expected XML document'
}
