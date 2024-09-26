import os
import encoding.xml

fn test_valid_parsing() ! {
	path := os.join_path(os.dir(@FILE), 'cd_catalog.xml')

	expected := xml.XMLDocument{
		root: xml.XMLNode{
			name:     'CATALOG'
			children: [
				xml.XMLNode{
					name:     'CD'
					children: [
						xml.XMLNode{
							name:     'TITLE'
							children: [
								'Empire Burlesque',
							]
						},
						xml.XMLNode{
							name:     'ARTIST'
							children: [
								'Bob Dylan',
							]
						},
						xml.XMLNode{
							name:     'COUNTRY'
							children: [
								'USA',
							]
						},
						xml.XMLNode{
							name:     'COMPANY'
							children: [
								'Columbia',
							]
						},
						xml.XMLNode{
							name:     'PRICE'
							children: [
								'10.90',
							]
						},
						xml.XMLNode{
							name:     'YEAR'
							children: [
								'1985',
							]
						},
					]
				},
				xml.XMLNode{
					name:     'CD'
					children: [
						xml.XMLNode{
							name:     'TITLE'
							children: [
								'Hide your heart',
							]
						},
						xml.XMLNode{
							name:     'ARTIST'
							children: [
								'Bonnie Tyler',
							]
						},
						xml.XMLNode{
							name:     'COUNTRY'
							children: [
								'UK',
							]
						},
						xml.XMLNode{
							name:     'COMPANY'
							children: [
								'CBS Records',
							]
						},
						xml.XMLNode{
							name:     'PRICE'
							children: [
								'9.90',
							]
						},
						xml.XMLNode{
							name:     'YEAR'
							children: [
								'1988',
							]
						},
					]
				},
				xml.XMLNode{
					name:     'CD'
					children: [
						xml.XMLNode{
							name:     'TITLE'
							children: [
								'Greatest Hits',
							]
						},
						xml.XMLNode{
							name:     'ARTIST'
							children: [
								'Dolly Parton',
							]
						},
						xml.XMLNode{
							name:     'COUNTRY'
							children: [
								'USA',
							]
						},
						xml.XMLNode{
							name:     'COMPANY'
							children: [
								'RCA',
							]
						},
						xml.XMLNode{
							name:     'PRICE'
							children: [
								'9.90',
							]
						},
						xml.XMLNode{
							name:     'YEAR'
							children: [
								'1982',
							]
						},
					]
				},
				xml.XMLNode{
					name:     'CD'
					children: [
						xml.XMLNode{
							name:     'TITLE'
							children: [
								'Still got the blues',
							]
						},
						xml.XMLNode{
							name:     'ARTIST'
							children: [
								'Gary Moore',
							]
						},
						xml.XMLNode{
							name:     'COUNTRY'
							children: [
								'UK',
							]
						},
						xml.XMLNode{
							name:     'COMPANY'
							children: [
								'Virgin records',
							]
						},
						xml.XMLNode{
							name:     'PRICE'
							children: [
								'10.20',
							]
						},
						xml.XMLNode{
							name:     'YEAR'
							children: [
								'1990',
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
