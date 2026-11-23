import toml

// toml has a sum type `Any` and calls `a.str()` on it in its own code; this
// program's interface `Any` does not change what `Any` means there.
interface Any {}

fn test_module_code_keeps_its_own_sum_type_named_like_a_program_interface() {
	doc := toml.parse_text('a = 1')!
	assert doc.value('a').string().contains('1')
	assert doc.value('a').int() == 1
	held := Any(1)
	assert held is int
}
