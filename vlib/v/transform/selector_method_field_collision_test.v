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
		mut type_texts_17 := &flat.TypeTextTable{}
		t := Transformer{
			var_type_texts: type_texts_17
			a:              &a
			cur_module:     'main'
			var_types:      [
				VarTypeBinding{
					name: 'm'
					typ:  type_texts_17.intern(receiver_type)
				},
			]
			structs:        {
				'Model': StructInfo{}
			}
			unique_fields:  flat.TypeTextBindings.from_names({
				'on_done': '?fn ()'
			})
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
			for field_type in ['int', '[]int', '?string'] {
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
					typ:         flat.source_type_id('${prefix}Foo')
					is_embedded: true
				}
				bar_embed := FieldInfo{
					name:        'Bar'
					typ:         flat.source_type_id('${prefix}Bar')
					is_embedded: true
				}
				mut type_texts_65 := &flat.TypeTextTable{}
				mut t := Transformer{
					var_type_texts: type_texts_65
					a:              &a
					cur_module:     'main'
					var_types:      [
						VarTypeBinding{
							name: 'baz'
							typ:  type_texts_65.intern(receiver_type)
						},
					]
					structs:        {
						'Foo': StructInfo{
							name:   flat.source_type_id('Foo')
							fields: [FieldInfo{
								name: 'x'
								typ:  flat.source_type_id(field_type)
							}]
						}
						'Bar': StructInfo{
							name:   flat.source_type_id('Bar')
							fields: [foo_embed]
						}
						'Baz': StructInfo{
							name:   flat.source_type_id('Baz')
							fields: [bar_embed]
						}
					}
					unique_fields:  flat.TypeTextBindings.from_names({
						'x': '?fn ()'
					})
				}
				// No checker or node annotations: Baz -> Bar -> Foo.x must resolve
				// through its owner, not the unrelated field-name fallback.
				assert t.resolve_selector_type(a.nodes[int(selector)]) == field_type
				assert t.raw_expr_type_without_smartcast(selector) == field_type

				// A nearer declaration shadows the field in a deeper embed.
				t.structs['Bar'] = StructInfo{
					name:   flat.source_type_id('Bar')
					fields: [foo_embed, FieldInfo{
						name: 'x'
						typ:  flat.source_type_id('string')
					}]
				}
				assert t.resolve_selector_type(a.nodes[int(selector)]) == 'string'
				assert t.raw_expr_type_without_smartcast(selector) == 'string'

				// The receiver's own field still wins over all promoted fields.
				t.structs['Baz'] = StructInfo{
					name:   flat.source_type_id('Baz')
					fields: [bar_embed, FieldInfo{
						name: 'x'
						typ:  flat.source_type_id('bool')
					}]
				}
				assert t.resolve_selector_type(a.nodes[int(selector)]) == 'bool'
				assert t.raw_expr_type_without_smartcast(selector) == 'bool'
			}
		}
	}
}
