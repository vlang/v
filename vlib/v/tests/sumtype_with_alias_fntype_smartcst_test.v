type FunString = FnAlias | string

type FnAlias = fn () string

fn foo_bar(foo FnAlias) string {
	return foo()
}

fn test_sumtype_with_alias_fntype_smartcast() {
	fs := FunString(FnAlias(fn () string {
		return 'bar'
	}))

	if fs is FnAlias {
		println(foo_bar(fs))
	}
}
