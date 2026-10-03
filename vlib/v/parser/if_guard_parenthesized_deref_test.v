module parser

import os
import v.pref

fn test_if_guard_accepts_parenthesized_optional_dereferences() {
	path := os.join_path(os.vtmp_dir(), 'if_guard_parenthesized_deref_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	for expr in ['*value', '(*value)', '(((*value)))', '(*holder.get())'] {
		os.write_file(path, 'fn main() { if item := ${expr} { _ = item } }\n')!
		mut p := Parser.new(pref.new_preferences())
		p.parse_file(path)
		assert p.diagnostics.len == 0, '${expr}: ${p.diagnostics}'
	}
}

fn test_if_guard_rejects_other_parenthesized_expression_shapes() {
	path := os.join_path(os.vtmp_dir(), 'if_guard_parenthesized_invalid_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	for expr in ['(values[0])', '(maybe())', '(value)', '(!value)', '(&value)'] {
		os.write_file(path, 'fn main() { if item := ${expr} { _ = item } }\n')!
		mut p := Parser.new(pref.new_preferences())
		p.parse_file(path)
		assert p.diagnostics.any(it.message == 'if guard condition expression is illegal, it should return an Option'), '${expr}: ${p.diagnostics}'
	}
}
