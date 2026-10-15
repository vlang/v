module flat

fn test_type_text_forks_share_immutable_spelling_ids() {
	mut parent := TypeTextBindings{}
	parent.set('value', 'pkg.Sum')
	mut left := parent.fork()
	mut right := parent.fork()
	left.set('value', '?pkg.Left[T]')
	right.set('value', 'map[string]pkg.Right')
	left_id := left.type_id('value') or { panic('missing left binding') }
	right_id := right.type_id('value') or { panic('missing right binding') }
	assert left_id != right_id
	assert parent.texts.text(right_id) == right.value('value')
	assert left.value('value') == '?pkg.Left[T]'
	assert right.value('value') == 'map[string]pkg.Right'
	assert parent.value('value') == 'pkg.Sum'
	parent.set_id('imported', right_id)
	assert parent.value('imported') == 'map[string]pkg.Right'
	assert parent.value_id('imported') == right_id
	assert parent.value_id('absent') == 0
}

fn test_indexed_scope_shadowing_keeps_empty_bindings_present() {
	mut outer := TypeTextBindings{}
	outer.set('placeholder', '')
	outer.set('value', 'pkg.Alias')
	mut inner := outer.clone()
	inner.set('value', '[]T')
	inner.delete('placeholder')
	assert inner.value('value') == '[]T'
	assert outer.value('value') == 'pkg.Alias'
	assert outer.contains('placeholder')
	assert outer.get('placeholder') or { panic('empty binding was lost') } == ''
	assert !inner.contains('placeholder')
	assert outer.value_id('placeholder') == 0
	assert outer.type_id('placeholder') or { panic('empty identity was lost') } == 0
}

fn test_source_type_shape_preserves_exact_spelling_probes() {
	wrapped := source_type_shape(source_type_id('map[string][]pkg.Item'))
	assert wrapped.has(.dotted)
	assert wrapped.has(.bracketed)
	assert !wrapped.has(.wrapped)
	assert source_type_shape(source_type_id('?pkg.Item')).has(.wrapped)
	assert source_type_shape(source_type_id('[]typeof(x)')).has(.typeof_expr)
	assert source_type_shape(source_type_id('typeof_field')).has(.typeof_name)
	assert !source_type_shape(source_type_id('typeof_field')).has(.typeof_expr)
	assert !source_type_shape(source_type_id('typeof (x)')).has(.typeof_expr)
	assert !source_type_shape(0).has(.dotted)
}

fn test_type_text_pointer_probe_validates_reused_input_bytes() {
	mut texts := TypeTextTable{}
	mut bytes := '[]First'.bytes()
	// Borrow one buffer deliberately to exercise reuse of the same input address.
	borrowed := unsafe { tos(bytes.data, bytes.len) }
	first := texts.intern(borrowed)
	for i, value in '[]Other'.bytes() {
		bytes[i] = value
	}
	second := texts.intern(borrowed)
	assert first != second
	assert texts.text(first) == '[]First'
	assert texts.text(second) == '[]Other'
}

fn test_ast_type_ids_use_the_shared_spelling_namespace() {
	mut first := FlatAst.new()
	first.intern_text('ordinary payload')
	second := FlatAst.new()
	id := first.type_text_id('pkg.Source[T]')
	assert id == second.type_text_id('pkg.Source[T]')
	assert source_type_name(id) == 'pkg.Source[T]'
	assert first.node_type_text_id('pkg.Source[T]', id) == id
	changed := first.node_type_text_id('pkg.Other', id)
	assert changed != id
	assert source_type_name(changed) == 'pkg.Other'
	assert first.node_type_text_id('', changed) == 0
	assert first.type_text_id('') == 0
}

fn test_source_type_shapes_and_builtin_ids_share_the_spelling_table() {
	assert source_type_int == source_type_id('int'.clone())
	assert source_type_name(source_type_string) == 'string'
	assert source_type_shape(source_type_int).has(.builtin_alias)
	shape := source_type_shape(source_type_id('?[]pkg.Item'))
	assert shape.has(.wrapped)
	assert shape.has(.bracketed)
	assert shape.has(.dotted)
	assert !shape.has(.typeof_expr)
	assert source_type_shape(source_type_id('typeof(value)')).has(.typeof_expr)
	assert !source_type_shape(0).has(.builtin_alias)
	assert source_type_shape(source_type_id('?')).has(.optional)
	assert source_type_shape(source_type_id('!pkg.Item')).has(.optional)
	assert !source_type_shape(source_type_id('&pkg.Item')).has(.optional)
	assert source_type_shape(source_type_id('&pkg.Item')).has(.pointer)
	assert source_type_shape(source_type_id('shared pkg.Item')).has(.shared_type)
	assert source_type_shape(source_type_id('...pkg.Item')).has(.variadic)
	assert source_type_shape(source_type_id(' pkg.Item\t')).has(.padded)
	assert !source_type_shape(source_type_id('pkg.Item')).has(.padded)
}

fn test_binding_value_ids_preserve_present_empty_bindings() {
	mut bindings := TypeTextBindings{}
	bindings.set('empty', '')
	bindings.set_id('value', source_type_int)
	assert bindings.value_id('missing') == 0
	assert bindings.value_id('empty') == 0
	assert bindings.contains('empty')
	assert bindings.value_id('value') == source_type_int
	mut nodes := NodeTypeTextBindings{}
	nodes.set_id(12, source_type_string)
	assert nodes.value_id(12) == source_type_string
	assert nodes.value_id(13) == 0
	assert nodes.type_id(12) or { panic('missing node type identity') } == source_type_string
	assert nodes.type_id(13) == none
	nodes.set_id(13, 0)
	assert nodes.type_id(13) or { panic('empty node identity was lost') } == 0
}
