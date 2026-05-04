module main

import encoding.xml

fn test_node() {
	nodes := [
		xml.XMLNode{
			name:       'test'
			attributes: {
				'test:key':   ' test_value '
				'test:other': '123456'
			}
			children:   [
				xml.XMLNode{
					name:       'child'
					attributes: {
						'child:key': 'child_value'
					}
				},
				'Sample text',
			]
		},
		xml.XMLNode{
			name:       's'
			attributes: {
				'k': 'v'
			}
			children:   [
				'Hello, world!',
				xml.XMLNode{
					name:       'c'
					attributes: {
						'k2': 'v2'
					}
				},
			]
		},
		xml.XMLNode{
			name:       'ext'
			attributes: {
				'uri':          '{B58B0392-4F1F-4190-BB64-5DF3571DCE5F}'
				'xmlns:xcalcf': 'http://schemas.microsoft.com/office/spreadsheetml/2018/calcfeatures'
			}
			children:   [
				xml.XMLNode{
					name:     'xcalcf:calcFeatures'
					children: [
						xml.XMLNode{
							name:       'xcalcf:feature'
							attributes: {
								'name': 'microsoft.com:RD'
							}
						},
						xml.XMLNode{
							name:       'xcalcf:feature'
							attributes: {
								'name': 'microsoft.com:Single'
							}
						},
						xml.XMLNode{
							name:       'xcalcf:feature'
							attributes: {
								'name': 'microsoft.com:FV'
							}
						},
						xml.XMLNode{
							name:       'xcalcf:feature'
							attributes: {
								'name': 'microsoft.com:CNMTM'
							}
						},
						xml.XMLNode{
							name:       'xcalcf:feature'
							attributes: {
								'name': 'microsoft.com:LET_WF'
							}
						},
						xml.XMLNode{
							name:       'xcalcf:feature'
							attributes: {
								'name': 'microsoft.com:LAMBDA_WF'
							}
						},
						xml.XMLNode{
							name:       'xcalcf:feature'
							attributes: {
								'name': 'microsoft.com:ARRAYTEXT_WF'
							}
						},
					]
				},
			]
		},
	]
	values := [
		'
		<test test:key=" test_value " test:other="123456">
			<child child:key="child_value"/>
			Sample text
		</test>'.trim_indent(),
		'
		<s k="v">
			Hello, world!
			<c k2="v2"/>
		</s>'.trim_indent(),
		'
		<ext uri="{B58B0392-4F1F-4190-BB64-5DF3571DCE5F}" xmlns:xcalcf="http://schemas.microsoft.com/office/spreadsheetml/2018/calcfeatures">
			<xcalcf:calcFeatures>
				<xcalcf:feature name="microsoft.com:RD"/>
				<xcalcf:feature name="microsoft.com:Single"/>
				<xcalcf:feature name="microsoft.com:FV"/>
				<xcalcf:feature name="microsoft.com:CNMTM"/>
				<xcalcf:feature name="microsoft.com:LET_WF"/>
				<xcalcf:feature name="microsoft.com:LAMBDA_WF"/>
				<xcalcf:feature name="microsoft.com:ARRAYTEXT_WF"/>
			</xcalcf:calcFeatures>
		</ext>'.trim_indent(),
	]
	for i, node in nodes {
		assert node.pretty_str('\t', 0, xml.default_entities_reverse) == values[i]
	}
}

fn test_doc() {
	docs := [
		xml.XMLDocument{
			root: xml.XMLNode{
				name:       'test'
				attributes: {
					'test:key':   ' test_value '
					'test:other': '123456'
				}
				children:   [
					xml.XMLNode{
						name:       'child'
						attributes: {
							'child:key': 'child_value'
						}
					},
					'Sample text',
				]
			}
		},
		xml.XMLDocument{
			root: xml.XMLNode{
				name:       's'
				attributes: {
					'k': 'v'
				}
				children:   [
					'Hello, world!',
					xml.XMLNode{
						name:       'c'
						attributes: {
							'k2': 'v2'
						}
					},
				]
			}
		},
	]
	values := [
		'
		<?xml version="1.0" encoding="UTF-8"?>
		<test test:key=" test_value " test:other="123456">
			<child child:key="child_value"/>
			Sample text
		</test>'.trim_indent(),
		'
		<?xml version="1.0" encoding="UTF-8"?>
		<s k="v">
			Hello, world!
			<c k2="v2"/>
		</s>'.trim_indent(),
	]
	for i, doc in docs {
		assert doc.pretty_str('\t') == values[i]
	}
}

fn test_large_doc_str() {
	depth := 1500
	payload := '0123456789abcdef'.repeat(16)
	mut node := xml.XMLNode{
		name: 'leaf'
	}
	for i in 0 .. depth {
		node = xml.XMLNode{
			name:     'n${i}'
			children: [
				payload,
				node,
			]
		}
	}
	doc := xml.XMLDocument{
		root: node
	}

	rendered := doc.str()
	assert rendered.starts_with('<?xml version="1.0" encoding="UTF-8"?>\n<n${depth - 1}>')
	assert rendered.contains('<leaf/>')
	assert rendered.ends_with('</n${depth - 1}>')
	assert rendered.len > 5_000_000
}
