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
	assert tag.get_tag_by_attribute_value('id', '3rd')?.str() == '<div id="3rd"></div>'
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

fn test_search_by_no_class_name() {
	mut dom := parse(generate_temp_html_with_classes())
	tag := dom.get_tags(name: 'body')[0]
	// no class name means no match, not "every tag"
	assert tag.get_tag_by_class_name() == none
	assert tag.get_tags_by_class_name().len == 0
}

fn test_text_returns_newline_only_for_br() {
	// text() must map exactly the line-break element to a newline: a prefix
	// match also catches custom elements like `bracket` and drops their
	// content and children.
	// See https://github.com/vlang/v/issues/30163
	// NOTE: the tags are read from get_tags() rather than get_tags(name:),
	// because the document root is missing from the type index; that is
	// tracked in https://github.com/vlang/v/issues/30268.
	mut dom := parse('<bracket>inner text</bracket>')
	mut bracket := dom.get_tags()[0]
	assert bracket.name == 'bracket'
	assert bracket.text() == 'inner text'

	mut dom_br := parse('<div><p>a<br/>b</p></div>')
	mut brs := dom_br.get_tags(name: 'br/')
	assert brs.len == 1
	assert brs[0].text() == '\n'
}

fn test_br_prefix_elements_preserve_own_text_and_children() {
	for name in ['b', 'bracket', 'broom', 'br-widget'] {
		tag := Tag{
			name:     name
			content:  'before'
			children: [&Tag{ name: 'text', content: 'after' }]
		}
		assert tag.text() == 'beforeafter'
	}
	for name in ['br', 'br/'] {
		assert Tag{ name: name }.text() == '\n'
	}
	legacy_text := Tag{
		name:    'text'
		content: 'raw text'
	}
	assert legacy_text.text() == 'raw text'
	assert legacy_text.str() == 'raw text'
	assert Tag{}.text() == ''
}

fn test_search_by_valueless_attribute() {
	mut dom := parse('<div><input disabled><input value=""></div>')
	// the div is the document root; reach it without the dom index
	tag := dom.get_root()
	assert tag.name == 'div'
	// a present attribute with an empty value is still present
	assert tag.get_tag_by_attribute('disabled')?.name == 'input'
	assert tag.get_tags_by_attribute('disabled').len == 1
	assert tag.get_tags_by_attribute('value').len == 1
	// an absent attribute never matches, even for an empty value
	assert tag.get_tag_by_attribute('nonexistent') == none
	assert tag.get_tags_by_attribute('nonexistent').len == 0
	assert tag.get_tag_by_attribute_value('nonexistent', '') == none
	assert tag.get_tags_by_attribute_value('nonexistent', '').len == 0
	// an empty value matches only where it is actually present
	assert tag.get_tag_by_attribute_value('value', '')?.name == 'input'
}

fn test_search_by_empty_attribute_value_recurses_in_document_order() {
	mut dom := parse('<div><section><input value="" disabled></section><input disabled></div>')
	tag := dom.get_root()
	matches := tag.get_tags_by_attribute_value('value', '')
	assert matches.len == 1
	assert matches[0] == tag.get_tag_by_attribute_value('value', '')?
	assert tag.get_tags_by_attribute('disabled').len == 2
	assert tag.get_tags_by_attribute_value('disabled', '').len == 2
	assert tag.get_tag_by_attribute('disabled')? == matches[0]
}

fn test_tag_walkers_handle_deep_caller_built_tree() {
	small_leaf := &Tag{
		name:    'leaf'
		content: 'x'
	}
	small_root := &Tag{
		name:     'root'
		children: [small_leaf]
	}
	assert small_root.str() == '<root><leaf>x</leaf></root>'

	depth := 150_000
	mut root := &Tag{
		name:    'root'
		content: 'r'
	}
	mut current := root
	for index in 0 .. depth {
		mut child := &Tag{
			name:    'node'
			content: 'x'
		}
		if index == depth - 1 {
			child.name = 'leaf'
			child.attributes['id'] = 'target'
			child.class_set.add('target')
		}
		current.children << child
		current = child
	}
	assert root.get_tags('node').len == depth - 1
	assert root.get_tag('leaf')?.name == 'leaf'
	assert root.get_tag_by_attribute('id')?.name == 'leaf'
	assert root.get_tags_by_attribute('id').len == 1
	assert root.get_tag_by_attribute_value('id', 'target')?.name == 'leaf'
	assert root.get_tags_by_attribute_value('id', 'target').len == 1
	assert root.get_tag_by_class_name('target')?.name == 'leaf'
	assert root.get_tags_by_class_name('target').len == 1
	assert root.text().len == depth + 1
	leaf := root.get_tag('leaf')?
	assert leaf.str() == '<leaf id="target">x</leaf>'
	serialized := root.str()
	leaf_index := serialized.index('<leaf id="target">') or { -1 }
	assert leaf_index >= 0, 'serialized length ${serialized.len}, tail: ${serialized[serialized.len - 100..]}'
	assert serialized[leaf_index..].starts_with('<leaf id="target">x</leaf>')
}

fn test_tag_walkers_skip_cycles_and_revisit_shared_children() {
	mut cyclic := &Tag{
		name:    'cycle'
		content: 'safe'
	}
	cyclic.children << cyclic
	assert cyclic.get_tag('cycle') == none
	assert cyclic.get_tags('cycle').len == 0
	assert cyclic.text() == 'safe'
	assert cyclic.str() == '<cycle>safe</cycle>'

	shared := &Tag{
		name:    'leaf'
		content: 'x'
	}
	left := &Tag{
		name:     'left'
		content:  'l'
		children: [shared]
	}
	right := &Tag{
		name:     'right'
		content:  'r'
		children: [shared]
	}
	root := &Tag{
		name:     'root'
		children: [left, right]
	}
	shared_matches := root.get_tags('leaf')
	assert shared_matches.len == 2
	assert shared_matches[0].name == 'leaf'
	assert shared_matches[1].name == 'leaf'
	assert root.text() == 'lxrx'
	assert root.str() == '<root><left>l<leaf>x</leaf></left><right>r<leaf>x</leaf></right></root>'
}
