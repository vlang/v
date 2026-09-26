module transform

import v.flat

fn test_known_struct_selector_does_not_inherit_unrelated_field_type() {
	for receiver_type in ['Model', '&Model', 'Unregistered'] {
		mut a := flat.FlatAst.new()
		receiver := a.add_val(.ident, 'm')
		start := a.children.len
		a.children << receiver
		selector := a.add_node(flat.Node{
			kind:           .selector
			value:          'on_done'
			children_start: start
			children_count: 1
		})
		t := Transformer{
			a:             &a
			cur_module:    'main'
			var_types:     [
				VarTypeBinding{
					name: 'm'
					typ:  receiver_type
				},
			]
			structs:       {
				'Model': StructInfo{}
			}
			unique_fields: {
				'on_done': '?fn ()'
			}
		}
		// Model has no on_done field: a method selector must not acquire the
		// optional callback type belonging to another struct. Keep the fallback
		// for receivers whose struct information is unavailable.
		expected := if receiver_type == 'Unregistered' { '?fn ()' } else { '' }
		assert t.resolve_selector_type(a.nodes[int(selector)]) == expected
	}
}

fn test_known_struct_selector_resolves_nested_promoted_fields_without_checker() {
	for receiver_type in ['Baz', '&Baz'] {
		for prefix in ['', '&'] {
			for field_type in ['int', '[]int'] {
				mut a := flat.FlatAst.new()
				receiver := a.add_val(.ident, 'baz')
				start := a.children.len
				a.children << receiver
				selector := a.add_node(flat.Node{
					kind:           .selector
					value:          'x'
					children_start: start
					children_count: 1
				})
				foo_embed := FieldInfo{
					name:        'Foo'
					typ:         '${prefix}Foo'
					is_embedded: true
				}
				bar_embed := FieldInfo{
					name:        'Bar'
					typ:         '${prefix}Bar'
					is_embedded: true
				}
				mut t := Transformer{
					a:             &a
					cur_module:    'main'
					var_types:     [
						VarTypeBinding{
							name: 'baz'
							typ:  receiver_type
						},
					]
					structs:       {
						'Foo': StructInfo{
							name:   'Foo'
							fields: [FieldInfo{
								name: 'x'
								typ:  field_type
							}]
						}
						'Bar': StructInfo{
							name:   'Bar'
							fields: [foo_embed]
						}
						'Baz': StructInfo{
							name:   'Baz'
							fields: [bar_embed]
						}
					}
					unique_fields: {
						'x': '?fn ()'
					}
				}
				// No checker or node annotations: Baz -> Bar -> Foo.x must resolve
				// through its owner, not the unrelated field-name fallback.
				assert t.resolve_selector_type(a.nodes[int(selector)]) == field_type

				// A nearer declaration shadows the field in a deeper embed.
				t.structs['Bar'] = StructInfo{
					name:   'Bar'
					fields: [foo_embed, FieldInfo{
						name: 'x'
						typ:  'string'
					}]
				}
				assert t.resolve_selector_type(a.nodes[int(selector)]) == 'string'

				// The receiver's own field still wins over all promoted fields.
				t.structs['Baz'] = StructInfo{
					name:   'Baz'
					fields: [bar_embed, FieldInfo{
						name: 'x'
						typ:  'bool'
					}]
				}
				assert t.resolve_selector_type(a.nodes[int(selector)]) == 'bool'
			}
		}
	}
}
