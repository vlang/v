module main

import encoding.xml

const sample_document = '
<root>
	<a attr="value1">
		<b id="middle-tag" attr="value2">
			<c attr="value3">Text1</c>
			<d attr="value4">Text2</d>
			<e attr="value5">
				<f id="innermost" attr="value6">Text3</f>
				<g attr="value7">Text4</g>
				<h attr="value8">Text5</h>
			</e>
			<i attr="value9">Text6</i>
		</b>
		<j attr="value10">Text7</j>
	</a>
	<k attr="value11">Text8</k>
	<l attr="value12">Text9</l>
</root>
'

fn test_querying() ! {
	doc := xml.XMLDocument.from_string(sample_document)!

	assert doc.root.name == 'root'
	assert doc.root.children.len == 3

	middle_tag := doc.get_element_by_id('middle-tag')?
	assert middle_tag.name == 'b'
	assert middle_tag.attributes['attr'] == 'value2'
	assert middle_tag.children.len == 4

	innermost := middle_tag.get_element_by_id('innermost')?
	assert innermost.name == 'f'
	assert innermost.attributes['attr'] == 'value6'

	for count in 1 .. 13 {
		assert doc.get_elements_by_attribute('attr', 'value${count}').len == 1
	}

	i_tags := doc.get_elements_by_tag('i')
	assert i_tags.len == 1
	assert i_tags[0].name == 'i'
	assert i_tags[0].attributes['attr'] == 'value9'
	assert i_tags[0].children.len == 1
	assert i_tags[0].children[0] as string == 'Text6'
}
