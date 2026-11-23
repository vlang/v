module html

import strings

fn generate_temp_html() string {
	mut temp_html := strings.new_builder(200)
	temp_html.write_string('<!doctype html><html><head><title>Giant String</title></head><body>')
	for counter := 0; counter < 4; counter++ {
		temp_html.write_string("<div id='name_${counter}' ")
		temp_html.write_string("class='several-${counter}'>Look at ${counter}</div>")
	}
	temp_html.write_string('</body></html>')
	return temp_html.str()
}

fn test_search_by_tag_type() {
	dom := parse(generate_temp_html())
	assert dom.get_tags(name: 'div').len == 4
	assert dom.get_tags(name: 'head').len == 1
	assert dom.get_tags(name: 'body').len == 1
}

fn test_search_by_attribute_value() {
	mut dom := parse(generate_temp_html())
	// println(temp_html)
	print('Amount ')
	println(dom.get_tags_by_attribute_value('id', 'name_0'))
	assert dom.get_tags_by_attribute_value('id', 'name_0').len == 1
}

fn test_access_parent() {
	mut dom := parse(generate_temp_html())
	div_tags := dom.get_tags(name: 'div')
	parent := div_tags[0].parent
	assert unsafe { parent != 0 }
	for div_tag in div_tags {
		assert div_tag.parent == parent
	}
}

fn test_search_by_attributes() {
	dom := parse(generate_temp_html())
	assert dom.get_tags_by_attribute('id').len == 4
}

fn test_tags_used() {
	dom := parse(generate_temp_html())
	assert dom.get_tags().len == 9
}

fn test_access_tag_fields() {
	dom := parse(generate_temp_html())
	id_tags := dom.get_tags_by_attribute('id')
	assert id_tags[0].name == 'div'
	assert id_tags[1].attributes['class'] == 'several-1'
}

fn generate_temp_html_with_classes() string {
	mut temp_html := strings.new_builder(400)
	temp_html.write_string('<!doctype html><html><head><title>Giant String</title></head><body>')
	temp_html.write_string("<div class='single'>Single</div>")
	for counter := 0; counter < 4; counter++ {
		temp_html.write_string("<div id='name_${counter}' ")
		temp_html.write_string("class='common'>Common No. ${counter}</div>")
	}
	temp_html.write_string("<div class='complex-0 complex-1 complex-2'>Complex</div>")
	temp_html.write_string("<div class='complex-0 complex-2'>Partial</div>")
	temp_html.write_string('</body></html>')
	return temp_html.str()
}

fn test_search_by_class() {
	dom := parse(generate_temp_html_with_classes())
	single_class_tags := dom.get_tags_by_class_name('single')
	common_class_tags := dom.get_tags_by_class_name('common')
	complex_class_tags := dom.get_tags_by_class_name('complex-0', 'complex-1', 'complex-2')
	partial_class_tags := dom.get_tags_by_class_name('complex-0', 'complex-2')
	shuffled_class_tags := dom.get_tags_by_class_name('complex-2', 'complex-0', 'complex-1')
	assert single_class_tags.len == 1
	assert common_class_tags.len == 4
	assert complex_class_tags.len == 1
	assert complex_class_tags[0].attributes['class'] == 'complex-0 complex-1 complex-2'
	assert partial_class_tags.len == 2
	assert shuffled_class_tags.len == 1
	assert shuffled_class_tags[0].attributes['class'] == 'complex-0 complex-1 complex-2'
}

fn test_unmatched_close_tag_is_ignored() {
	content := '<!doctype html><html><body><div><a href="#">x</a></a></div><article class="news-post">hello</article></body></html>'
	mut dom := parse(content)
	by_attr := dom.get_tags_by_attribute_value('class', 'news-post')
	by_class := dom.get_tags_by_class_name('news-post')
	assert by_attr.len == 1
	assert by_class.len == 1
	assert by_class[0].name == 'article'
}

fn test_document_root_is_indexed() {
	mut dom := parse('<div id="a" class="x">hi</div>')
	assert dom.get_tags().len == 1
	assert dom.get_tags(name: 'div').len == 1
	assert dom.get_tags_by_attribute('id').len == 1
	assert dom.get_tags_by_attribute_value('id', 'a').len == 1
	assert dom.get_tags_by_class_name('x').len == 1
}

fn test_document_root_matches_its_own_classes() {
	mut dom := parse('<div class="x"><p class="y">hi</p></div>')
	assert dom.get_tags_by_class_name('x').len == 1
	assert dom.get_tags_by_class_name('y').len == 1
	assert dom.get_tags_by_attribute('class').len == 2
}

fn test_document_class_search_handles_empty_and_multiple_names() {
	empty := parse('')
	assert empty.get_tags_by_class_name('x').len == 0
	mut dom := parse('<div class="x y"><p class="x">hi</p><span class="x y"></span></div>')
	assert dom.get_tags_by_class_name().len == 0
	matches := dom.get_tags_by_class_name('x', 'y')
	assert matches.len == 2
	assert matches[0].name == 'div'
	assert matches[1].name == 'span'
	assert dom.get_tags_by_class_name('x').len == 3
	assert dom.get_tags(name: 'div')[0] == dom.get_root()
}

fn test_attribute_value_indexes_keep_first_seen_order() {
	mut dom := parse('<html><body><div id="same"></div><p id="other"></p><span id="same"></span><i id=""></i><b id></b></body></html>')
	same := dom.get_tags_by_attribute_value('id', 'same')
	empty := dom.get_tags_by_attribute_value('id', '')
	assert same.len == 2
	assert same[0].name == 'div'
	assert same[1].name == 'span'
	assert empty.len == 2
	assert empty[0].name == 'i'
	assert empty[1].name == 'b'
}

fn test_unknown_attribute_value_queries_do_not_grow_indexes() {
	mut dom := parse('<div id="known"></div>')
	id_indexes := dom.attribute_value_indexes['id'] or { panic('id index was not built') }
	assert id_indexes.len() == 1
	for index in 0 .. 100 {
		assert dom.get_tags_by_attribute_value('id', 'missing_${index}').len == 0
	}
	assert id_indexes.len() == 1
	assert dom.get_tags_by_attribute_value('missing', 'value').len == 0
	assert 'missing' !in dom.attribute_value_indexes
}

fn test_many_distinct_attribute_values_remain_searchable() {
	mut temp_html := strings.new_builder(200_000)
	temp_html.write_string('<div>')
	for index in 0 .. 4096 {
		temp_html.write_string('<span id="value_${index}"></span>')
	}
	temp_html.write_string('</div>')
	mut dom := parse(temp_html.str())
	assert dom.get_tags_by_attribute('id').len == 4096
	assert dom.get_tags_by_attribute_value('id', 'value_0').len == 1
	assert dom.get_tags_by_attribute_value('id', 'value_4095').len == 1
}

fn test_synthetic_text_nodes_cannot_become_dom_containers() {
	mut dom := DocumentObjectModel{
		source_bytes: 1024
	}
	dom.close_tags['/text'] = true
	mut tags := [&Tag{
		name: 'root'
	}]
	for _ in 0 .. 300 {
		tags << &Tag{
			name:         'text'
			content:      'x'
			is_text_node: true
		}
	}
	dom.construct(tags)
	assert dom.parse_error() == none
	assert dom.get_root().children.len == 300
	assert dom.get_root().content == 'x'.repeat(300)
}
