type FunString17799 = FnAlias17799 | string

type FnAlias17799 = fn () string

fn foo_bar_17799(foo FnAlias17799) string {
	return foo()
}

fn test_sumtype_alias_fntype_smartcast() {
	fs := FunString17799(FnAlias17799(fn () string {
		return 'bar'
	}))
	assert foo_bar_17799(fs as FnAlias17799) == 'bar'
	if fs is FnAlias17799 {
		assert foo_bar_17799(fs) == 'bar'
		return
	}
	assert false
}
