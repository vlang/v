// Regression test for https://github.com/vlang/v/issues/28804: attributes on a
// declaration and attributes on its fields are independent scopes, even when
// their names match.
module main

@[comment: 'struct comment']
@[table: 'foo']
pub struct AttributeScopeTest {
pub:
	id   string @[comment: 'UUID'; primary; sql_type: 'CHAR(36)']
	name string @[comment: 'real user name']
}

fn test_same_named_struct_and_field_attributes() {
	item := AttributeScopeTest{
		id:   '123'
		name: 'v'
	}
	assert item.id == '123'
	assert item.name == 'v'
}
