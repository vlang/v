module ast

// Regression test for https://github.com/vlang/v/issues/28070
// vfmt used to drop the explicit `&` from a `mut x &T` parameter when `T` was an
// unregistered (placeholder, non-struct) type, e.g. when `T` is declared in
// another file of the same module and vfmt parses a single file in isolation.
// The parser only auto-adds a `&` to `mut` params when the base type is a
// registered struct, so `stringify_fn_decl` must strip a leading `&` only when
// the lowered type actually has more indirection than `orig_typ` (the type as
// the user wrote it).
fn test_stringify_fn_decl_keeps_explicit_ref_on_mut_placeholder_param() {
	mut t := new_table()
	foo_idx := t.register_sym(TypeSymbol{
		kind:  .placeholder
		name:  'Foo'
		cname: 'Foo'
		mod:   'main'
	})
	// `&Foo`, exactly as written in `fn f(mut p &Foo)`. `orig_typ` keeps the same
	// number of `&`, so mut lowering added nothing that vfmt may strip.
	foo_ref := new_type(foo_idx).set_nr_muls(1)
	decl := FnDecl{
		name:        'f'
		mod:         'main'
		return_type: void_type
		params:      [
			Param{
				name:     'p'
				is_mut:   true
				typ:      foo_ref
				orig_typ: foo_ref
			},
		]
	}
	s := t.stringify_fn_decl(&decl, 'main', map[string]string{}, false)
	assert s == 'fn f(mut p &Foo)', 'vfmt dropped the explicit `&`: got `${s}`'
}
