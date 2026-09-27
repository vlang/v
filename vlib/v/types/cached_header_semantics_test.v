module types

import os
import time
import v.parser
import v.pref

struct CachedHeaderCheck {
	errors      []TypeError
	notices     []TypeError
	prototypes  []string
	definitions []string
}

fn check_cached_header_source(source string, extension string, parallel bool, scoped bool) !CachedHeaderCheck {
	old_vjobs := os.getenv_opt('VJOBS')
	os.setenv('VJOBS', '2', true)
	defer {
		if value := old_vjobs {
			os.setenv('VJOBS', value, true)
		} else {
			os.unsetenv('VJOBS')
		}
	}
	root := os.join_path(os.vtmp_dir(), 'v3 header semantics ${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir(root)!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	path := os.join_path(root, 'cached.${extension}')
	mut input := 'module cached\n' + source + '\n'
	// Force the real parallel body-check path rather than its small-input fallback.
	// These are definitions, even in a .vh file, so they must still be checked.
	for i in 0 .. min_parallel_check_items + 8 {
		input += 'pub fn padding_${i}(value int) int { return value }\n'
	}
	os.write_file(path, input)!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut prototypes := []string{}
	mut definitions := []string{}
	for node in a.nodes {
		if node.kind != .fn_decl {
			continue
		}
		if node.is_mut {
			prototypes << node.value
		} else {
			definitions << node.value
		}
	}
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.scope_parallel_check_workers = scoped
	tc.diagnose_unknown_calls = true
	// Deliberately leave diagnostic_files empty: every parsed file is checked.
	// Filtering diagnostics to a selected .v file would hide the regression.
	was_parallel := tc.check_semantics_opt(parallel)
	$if windows {
		assert !was_parallel
	} $else {
		assert was_parallel == parallel
	}
	return CachedHeaderCheck{
		errors:      tc.errors.clone()
		notices:     tc.notices.clone()
		prototypes:  prototypes
		definitions: definitions
	}
}

fn test_cached_header_prototypes_have_no_body_diagnostics() {
	source := 'pub struct Box {
	value int
}
pub fn answer() int
fn text(unused string) string
pub fn pair(unused int) (int, string)
fn nothing(unused int)
pub fn (b Box) lookup(unused int) int
pub fn (left Box) + (right Box) Box
pub fn append(mut values []int) int
pub fn generic[T](unused T) T
@[noreturn]
fn stop(unused int)
'
	for parallel in [false, true] {
		for scoped in [false, true] {
			result := check_cached_header_source(source, 'vh', parallel, scoped)!
			assert result.prototypes == ['answer', 'text', 'pair', 'nothing', 'Box.lookup', 'Box.+',
				'append', 'generic', 'stop']
			assert result.definitions.len == min_parallel_check_items + 8
			assert result.errors.len == 0, result.errors.str()
			assert result.notices.len == 0, result.notices.str()
		}
	}
}

fn test_cached_header_and_source_definitions_still_require_returns() {
	source := 'pub struct Box { value int }
pub fn (left Box) + (right Box) Box {}
fn falls_through(unused int) int {}
pub fn good(value int) int { return value }
pub fn uses_mut(mut values []int) int { values << 1 }
'
	for extension in ['v', 'vh'] {
		for parallel in [false, true] {
			for scoped in [false, true] {
				result := check_cached_header_source(source, extension, parallel, scoped)!
				assert result.prototypes.len == 0
				assert result.definitions.len == min_parallel_check_items + 12
				assert result.errors.any(it.msg == 'missing return at end of function `falls_through`'), result.errors.str()
				// A mutable parameter does not make its function a bodyless prototype.
				assert result.errors.any(it.msg == 'missing return at end of function `uses_mut`'), result.errors.str()
				assert result.notices.any(it.msg == 'unused parameter: `unused`'), result.notices.str()
				assert result.errors.any(it.msg == 'missing return at end of function `+`'), result.errors.str()
				assert result.errors.len == 3, result.errors.str()
			}
		}
	}
}

fn test_cached_header_prototype_parameters_are_still_validated() {
	for parallel in [false, true] {
		for scoped in [false, true] {
			result := check_cached_header_source('pub fn duplicate(value int, value int) int\n',
				'vh', parallel, scoped)!
			assert result.prototypes == ['duplicate']
			assert result.errors.any(it.msg == 'redefinition of parameter `value`'), result.errors.str()
			assert !result.errors.any(it.msg.contains('missing return at end of function')), result.errors.str()
			assert !result.notices.any(it.msg.starts_with('unused parameter:')), result.notices.str()
		}
	}
}

fn test_cached_header_prototypes_still_supply_callable_signatures() {
	for parallel in [false, true] {
		for scoped in [false, true] {
			for argument in ['42', '"wrong"'] {
				source := 'fn cached_value(value int) int\n' +
					'pub fn caller() int { return cached_value(${argument}) }\n'
				result := check_cached_header_source(source, 'vh', parallel, scoped)!
				assert result.prototypes == ['cached_value']
				assert 'caller' in result.definitions
				if argument == '42' {
					assert result.errors.len == 0, result.errors.str()
				} else {
					assert result.errors.any(it.kind == .call_arg_mismatch), result.errors.str()
				}
				assert !result.errors.any(it.msg.contains('missing return at end of function')), result.errors.str()
				assert !result.notices.any(it.msg.starts_with('unused parameter:')), result.notices.str()
			}
		}
	}
}
