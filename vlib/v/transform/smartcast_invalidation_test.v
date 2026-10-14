module transform

fn test_repeated_smartcast_invalidation_is_new_event() {
	mut t := Transformer{
		smartcast_event_id:                  1
		smartcast_invalidation_event_ids:    {
			'h': 1
		}
		smartcast_reestablishment_event_ids: map[string]int{}
	}
	saved_smartcasts := [SmartcastContext{
		type_texts:    t.var_type_texts
		expr_name:     'h.value'
		variant_name:  t.var_type_text_id('Value')
		sum_type_name: t.var_type_text_id(option_unwrap_marker)
	}]
	assert t.restore_smartcasts_since(1, saved_smartcasts) == saved_smartcasts
	t.smartcast_event_id++
	t.smartcast_invalidation_event_ids['h'] = t.smartcast_event_id
	assert t.restore_smartcasts_since(1, saved_smartcasts).len == 0
	recreated := SmartcastContext{
		type_texts:    t.var_type_texts
		expr_name:     'h.value'
		variant_name:  t.var_type_text_id('Foo')
		sum_type_name: t.var_type_text_id(option_unwrap_marker)
	}
	t.smartcast_stack << recreated
	t.smartcast_event_id++
	t.smartcast_reestablishment_event_ids['h.value'] = t.smartcast_event_id
	assert t.restore_smartcasts_since(1, saved_smartcasts) == [recreated]
}
