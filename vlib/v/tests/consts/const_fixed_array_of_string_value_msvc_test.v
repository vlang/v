// for issue 20287
// msvc does not support the "_SLIT" macro for fixed array of string values immediate initialization:
// Array_fixed_string_5 _const_main__escaped_chars = {(_S("\\b")), (_S("\\f")), (_S("\\n")), (_S("\\r")), (_S("\\t"))};
// error C2099: initializer is not a constant
const escaped_chars = [(r'\b'), (r'\f'), (r'\n'), (r'\r'), (r'\t')]!

fn test_main() {
	assert true
}
