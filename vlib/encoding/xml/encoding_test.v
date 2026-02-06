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
