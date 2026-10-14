import fnlist

// A generic method in `fnlist` calls its fn-typed field, specialized with a
// main type whose short name matches `glyph.Item`, imported by `fnlist`.
// The call must stay a fn-field call on `fnlist.List[Item]`, not become an
// undeclared `List_Item__key` method call.
struct Item {
	id int
}

fn test_generic_fn_field_call_with_main_type_named_like_imported_type() {
	items := [Item{
		id: 7
	}]
	l := fnlist.new_list[Item](fn (i Item) string {
		return i.id.str()
	})
	assert l.first_key(items) == '7'
}
