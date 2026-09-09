module transform

fn test_repeated_smartcast_invalidation_is_new_event() {
	mut t := Transformer{
		smartcast_invalidation_events: ['h']
	}
	saved_smartcasts := [SmartcastContext{
		expr_name: 'h.value'
		variant_name: 'Value'
		sum_type_name: option_unwrap_marker
	}]
	assert t.non_invalidated_smartcasts_since(1, saved_smartcasts).len == 1
	t.smartcast_invalidation_events << 'h'
	assert t.non_invalidated_smartcasts_since(1, saved_smartcasts).len == 0
}
