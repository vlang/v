module types

import os
import time
import v.parser
import v.pref

enum DuplicateFnCheckMode {
	serial
	scoped_serial
	parallel
	scoped_parallel
	selected
	reachable
}

struct DuplicateFnCase {
	name    string
	sources []string
}

fn duplicate_fn_check_modes() []DuplicateFnCheckMode {
	return [.serial, .scoped_serial, .parallel, .scoped_parallel, .selected, .reachable]
}

fn check_duplicate_fn_source(sources []string, mode DuplicateFnCheckMode, padding int) ![]TypeError {
	old_vjobs := os.getenv_opt('VJOBS')
	os.setenv('VJOBS', '2', true)
	defer {
		if value := old_vjobs {
			os.setenv('VJOBS', value, true)
		} else {
			os.unsetenv('VJOBS')
		}
	}
	root := os.join_path(os.vtmp_dir(), 'v3 duplicate functions ${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir(root)!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	mut paths := []string{cap: sources.len}
	for source_idx, source in sources {
		path := os.join_path(root, 'input_${source_idx}.v')
		mut input := 'module duplicates\n' + source + '\n'
		if source_idx == 0 {
			input += 'fn entry() {}\n'
			// Exercise both the small-input fallback and actual worker dispatch.
			for i in 0 .. padding {
				input += 'fn padding_${i}(value int) int { return value }\n'
			}
		}
		os.write_file(path, input)!
		paths << path
	}
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files(paths)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.scope_parallel_check_workers = mode in [.scoped_serial, .scoped_parallel]
	tc.diagnose_unknown_calls = true
	match mode {
		.selected {
			// Duplicates must be rejected even when their bodies are not selected.
			tc.check_semantics_selected({
				'entry': true
			})
		}
		.reachable {
			tc.check_semantics_reachable({
				'entry': true
			})
		}
		else {
			want_parallel := mode in [.parallel, .scoped_parallel]
			was_parallel := tc.check_semantics_opt(want_parallel)
			$if windows {
				assert !was_parallel
			} $else {
				assert was_parallel == (want_parallel && padding >= min_parallel_check_items)
			}
		}
	}
	return tc.errors.clone()
}

fn test_duplicate_functions_are_rejected_by_all_semantic_paths() {
	cases := [
		DuplicateFnCase{
			name:    'lighten'
			sources: ['fn lighten(value int) int { return value + 1 }\nfn lighten(value int) int { return value + 2 }\n']
		},
		DuplicateFnCase{
			name:    'lighten'
			sources: ['fn lighten(value int) int { return value }\nfn lighten(value string) string { return value }\n']
		},
		DuplicateFnCase{
			name:    'Shade.lighten'
			sources: [
				'struct Shade {}\nfn (s Shade) lighten(value int) int { return value + 1 }\n',
				'fn (s Shade) lighten(value int) int { return value + 2 }\n',
			]
		},
	]
	for test_case in cases {
		for mode in duplicate_fn_check_modes() {
			for padding in [0, min_parallel_check_items + 8] {
				errors := check_duplicate_fn_source(test_case.sources, mode, padding)!
				context := '${test_case.name}, ${mode}, padding=${padding}: ${errors}'
				builders := errors.filter(it.severity == 'builder error:')
				assert builders.len == 1, context
				assert builders[0].kind == .duplicate_decl, context
				assert builders[0].msg == 'redefinition of function `${test_case.name}`', context
				conflicts := errors.filter(it.severity == 'conflicting declaration:')
				assert conflicts.len == 2, context
				assert conflicts.all(it.node_value == test_case.name), context
				if test_case.sources.len == 1 {
					assert conflicts[0].pos.id == conflicts[1].pos.id, context
				} else {
					assert conflicts[0].pos.id != conflicts[1].pos.id, context
				}
				assert conflicts[0].pos.offset != conflicts[1].pos.offset, context
				assert errors.len == 3, context
			}
		}
	}
}

fn test_duplicate_fn_check_keeps_distinct_receivers_and_c_declarations_valid() {
	source := 'struct Shade {}\nstruct OtherShade {}\nfn lighten(value int) int { return value }\nfn (s Shade) lighten(value int) int { return value }\nfn (s OtherShade) lighten(value int) int { return value }\nfn C.duplicate_fn_probe(value int) int\nfn C.duplicate_fn_probe(value int) int\n'
	for mode in duplicate_fn_check_modes() {
		errors := check_duplicate_fn_source([source], mode, min_parallel_check_items + 8)!
		assert errors.len == 0, '${mode}: ${errors}'
	}
}

fn test_duplicate_fn_check_accepts_backend_and_arch_source_overrides() {
	root := os.join_path(os.vtmp_dir(), 'v3 source overrides ${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir(root)!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	files := [
		os.join_path(root, 'sample.v'),
		os.join_path(root, 'sample.arm64.v'),
		os.join_path(root, 'sample.c.v'),
	]
	for i, file in files {
		os.write_file(file, 'module overrides\nfn selected() int { return ${i} }\n')!
	}
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files(files)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
}
