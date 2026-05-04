struct MethodAttributeHolder {}

@[api_name: 'handler']
@[secured: true]
@[retries: 3]
fn (m MethodAttributeHolder) endpoint() {}

fn (m MethodAttributeHolder) no_attrs() {}

fn find_method_attr(attrs []VAttribute, name string, kind AttributeKind) string {
	for attr in attrs {
		if attr.name == name && attr.kind == kind {
			return attr.arg
		}
	}
	return ''
}

fn test_comptime_method_attributes_are_structured() {
	mut saw_endpoint := false
	mut saw_no_attrs := false
	$for method in MethodAttributeHolder.methods {
		if method.name == 'endpoint' {
			saw_endpoint = true
			assert method.attrs.len == 3
			assert method.attributes.len == 3
			assert find_method_attr(method.attributes, 'api_name', .string) == 'handler'
			assert find_method_attr(method.attributes, 'secured', .bool) == 'true'
			assert find_method_attr(method.attributes, 'retries', .number) == '3'
		}
		if method.name == 'no_attrs' {
			saw_no_attrs = true
			assert method.attrs.len == 0
			assert method.attributes.len == 0
		}
	}
	assert saw_endpoint
	assert saw_no_attrs
}
