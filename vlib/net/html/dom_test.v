module html

import strings

fn generate_temp_html() string {
	mut temp_html := strings.new_builder(200)
	temp_html.write('<!doctype html><html><head><title>Giant String</title></head><body>')
	for counter := 0; counter < 4; counter++ {
		temp_html.write("<div id='name_$counter' ")
		temp_html.write("class='several-$counter'>Look at $counter</div>")
	}
	temp_html.write('</body></html>')
	return temp_html.str()
}

fn generate_dom(temp_html string) DocumentObjectModel {
	mut parser := Parser{}
	parser.parse_html(temp_html)
	dom := parser.get_dom()
	return dom
}

fn test_search_by_tag_type() {
	dom := generate_dom(generate_temp_html())
	assert dom.get_by_tag('div').len == 4
	assert dom.get_by_tag('head').len == 1
	assert dom.get_by_tag('body').len == 1
}

fn test_search_by_attribute_value() {
	mut dom := generate_dom(generate_temp_html())
	// println(temp_html)
	print('Amount ')
	println(dom.get_by_attribute_value('id', 'name_0'))
	assert dom.get_by_attribute_value('id', 'name_0').len == 1
}

fn test_access_parent() {
	mut dom := generate_dom(generate_temp_html())
	div_tags := dom.get_by_tag('div')
	parent := div_tags[0].parent
	assert parent != 0
	for div_tag in div_tags {
		assert div_tag.parent == parent
	}
}

fn test_search_by_attributes() {
	dom := generate_dom(generate_temp_html())
	assert dom.get_by_attribute('id').len == 4
}

fn test_tags_used() {
	dom := generate_dom(generate_temp_html())
	assert dom.get_all_tags().len == 9
}

fn test_access_tag_fields() {
	dom := generate_dom(generate_temp_html())
	id_tags := dom.get_by_attribute('id')
	assert id_tags[0].name == 'div'
	assert id_tags[1].attributes['class'] == 'several-1'
}
