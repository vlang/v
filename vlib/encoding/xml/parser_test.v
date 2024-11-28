module xml

const sample_doc = '
<root>
	<c id="c1"/>
	<c id="c2">
		Sample Text
	</c>
	<empty/>
	<c id="c3"/>
	<abc id="c4"/>
	<xyz id="c5"/>
	<c id="c6"/>
	<cx id="c7"/>
	<cd id="c8"/>
	<child id="c9">
		More Sample Text
	</child>
	<cz id="c10"/>
</root>'

const xml_elements = [
	XMLNode{
		name:       'c'
		attributes: {
			'id': 'c1'
		}
	},
	XMLNode{
		name:       'c'
		attributes: {
			'id': 'c2'
		}
		children:   [
			'Sample Text',
		]
	},
	XMLNode{
		name:       'empty'
		attributes: {}
	},
	XMLNode{
		name:       'c'
		attributes: {
			'id': 'c3'
		}
	},
	XMLNode{
		name:       'abc'
		attributes: {
			'id': 'c4'
		}
	},
	XMLNode{
		name:       'xyz'
		attributes: {
			'id': 'c5'
		}
	},
	XMLNode{
		name:       'c'
		attributes: {
			'id': 'c6'
		}
	},
	XMLNode{
		name:       'cx'
		attributes: {
			'id': 'c7'
		}
	},
	XMLNode{
		name:       'cd'
		attributes: {
			'id': 'c8'
		}
	},
	XMLNode{
		name:       'child'
		attributes: {
			'id': 'c9'
		}
		children:   [
			'More Sample Text',
		]
	},
	XMLNode{
		name:       'cz'
		attributes: {
			'id': 'c10'
		}
	},
]

fn test_single_element_parsing() ! {
	mut reader := FullBufferReader{
		contents: sample_doc.bytes()
	}
	// Skip the "<root>" tag
	mut skip := []u8{len: 6}
	reader.read(mut skip)!

	mut local_buf := [u8(0)]
	mut ch := next_char(mut reader, mut local_buf)!

	mut count := 0

	for count < xml_elements.len {
		match ch {
			`<` {
				next_ch := next_char(mut reader, mut local_buf)!
				match next_ch {
					`/` {}
					else {
						parsed_element := parse_single_node(next_ch, mut reader)!
						assert xml_elements[count] == parsed_element
						count++
					}
				}
				ch = next_char(mut reader, mut local_buf)!
			}
			else {
				for ch != `<` {
					ch = next_char(mut reader, mut local_buf)!
				}
			}
		}
	}
}
