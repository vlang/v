module vweb

struct JsonContextRow {
	vals []string
}

fn test_json_with_empty_context_does_not_crash() {
	mut ctx := Context{}
	ctx.json([JsonContextRow{
		vals: ['1', '22']
	}])
	assert ctx.done
}
