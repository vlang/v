module html

import strings

const html = '<!doctype html>
<html>
<head></head>
<body>
  <div id="1st">
    <div class="foo bar"></div>
  </div>
  <div id="2nd">
    <div class="foo">
      <a href="https://vlang.io/">V</a>
    </div>
    <div class="bar"></div>
    <a href="https://modules.vlang.io/">vlib</a>
    <div class="bar">
      <div class="bar">
        <p>
        <div>modules</div>
        </p>
        <a href="https://vpm.vlang.io/">vpm</a>
      </div>
    </div>
  </div>
  <div id="3rd"></div>
</body>
</html>'

fn test_search_tag_by_type() {
	mut dom := parse(html)
	tag := dom.get_tags(name: 'body')[0]
	assert tag.get_tag('div')?.attributes['id'] == '1st'
	assert tag.get_tag_by_attribute('href')?.content == 'V'
	// TODO: update after improved parsing to not add trailing white space to attribute values
	assert tag.get_tag_by_attribute_value('id', '3rd')?.str() == '<div id="3rd" ></div>'
	assert tag.get_tag_by_class_name('foo')?.attributes['class'] == 'foo bar'
}

fn test_search_tags_by_type() {
	mut dom := parse(html)
	tag := dom.get_tags_by_attribute_value('id', '2nd')[0]
	assert tag.get_tags('div').len == 5
	assert tag.get_tags_by_attribute('href')[2].content == 'vpm'
	assert tag.get_tags_by_attribute_value('class', 'bar').len == 3
	assert tag.get_tags_by_class_name('bar').len == 3
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
	mut dom := parse(generate_temp_html_with_classes())
	tag := dom.get_tags(name: 'body')[0]
	single_class_tags := tag.get_tags_by_class_name('single')
	common_class_tags := tag.get_tags_by_class_name('common')
	complex_class_tags := tag.get_tags_by_class_name('complex-0', 'complex-1', 'complex-2')
	partial_class_tags := tag.get_tags_by_class_name('complex-0', 'complex-2')
	shuffled_class_tags := tag.get_tags_by_class_name('complex-2', 'complex-0', 'complex-1')
	assert single_class_tags.len == 1
	assert common_class_tags.len == 4
	assert complex_class_tags.len == 1
	assert complex_class_tags[0].attributes['class'] == 'complex-0 complex-1 complex-2'
	assert partial_class_tags.len == 2
	assert shuffled_class_tags.len == 1
	assert shuffled_class_tags[0].attributes['class'] == 'complex-0 complex-1 complex-2'
}
