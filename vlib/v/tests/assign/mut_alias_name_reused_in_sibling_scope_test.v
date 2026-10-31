// A `mut` copy of an immutable value is marked as an alias by name. A later,
// unrelated binding of the same name (after the first one's scope ended) must
// not inherit that marker.
struct AliasCtx {
mut:
	name  string
	items []int
}

fn first_item(ctx AliasCtx, reuse bool) int {
	if reuse {
		mut c := ctx
		return c.items.len
	}
	mut c := AliasCtx{
		items: [1, 2]
	}
	c.items[0] = 5
	return c.items[0]
}

fn fresh_name(ctx AliasCtx, names []string) []string {
	mut out := []string{}
	for name in names {
		if name == ctx.name {
			mut c := ctx
			out << c.name
			continue
		}
		mut c := &AliasCtx{}
		c.name = name
		out << c.name
	}
	return out
}

fn test_mut_alias_name_reused_in_sibling_scope() {
	ctx := AliasCtx{
		name:  'a'
		items: [7]
	}
	assert first_item(ctx, false) == 5
	assert first_item(ctx, true) == 1
	assert fresh_name(ctx, ['a', 'b']) == ['a', 'b']
	assert ctx.items == [7]
}
