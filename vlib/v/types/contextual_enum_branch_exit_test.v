module types

import os
import v.parser
import v.pref

fn check_contextual_enum_branch_exit(name string, body string) TypeChecker {
	path := os.join_path(os.vtmp_dir(), 'contextual_enum_branch_exit_${name}_${os.getpid()}.v')
	os.write_file(path, 'enum Choice { never auto always }
struct Args { mut: choice Choice }
fn choose(value string, mut args Args) ! {
${body}
}
fn main() {}
') or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	return tc
}

fn test_contextual_enum_match_skips_returning_branches() {
	for i, exit in ['return', 'return error("unrecognized")'] {
		tc := check_contextual_enum_branch_exit('match_${i}', 'args.choice = match value {
			"never" { .never }
			"always" { .always }
			else { ${exit} }
		}')
		assert tc.errors.len == 0, tc.errors.str()
	}
}

fn test_contextual_enum_if_skips_returning_branches() {
	for i, body in [
		'args.choice = if value == "never" { .never } else { return error("unrecognized") }',
		'args.choice = if value == "never" { return } else { .always }',
	] {
		tc := check_contextual_enum_branch_exit('if_${i}', body)
		assert tc.errors.len == 0, tc.errors.str()
	}
}

fn test_contextual_enum_loop_match_skips_break_and_continue() {
	for i, exit in ['break', 'continue'] {
		tc := check_contextual_enum_branch_exit('loop_${i}', 'for value.len > 0 {
			args.choice = match value {
				"always" { .always }
				else { ${exit} }
			}
			break
		}')
		assert tc.errors.len == 0, tc.errors.str()
	}
}

fn test_contextual_enum_match_still_checks_value_tails() {
	for i, tail in ['.missing', '"invalid"'] {
		tc := check_contextual_enum_branch_exit('invalid_${i}', 'args.choice = match value {
			"never" { .never }
			"always" { ${tail} }
			else { return }
		}')
		assert tc.errors.len > 0, 'case ${i}: ${tc.errors}'
		if i == 0 {
			assert tc.errors.any(it.msg.contains('unknown enum field')), tc.errors.str()
		}
	}
}

fn test_contextual_enum_match_still_checks_return_payloads() {
	tc := check_contextual_enum_branch_exit('invalid_return', 'args.choice = match value {
		"never" { .never }
		else { return "invalid" }
	}')
	assert tc.errors.any(it.kind == .return_mismatch), tc.errors.str()
}
