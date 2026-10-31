module types

import os
import v.flat
import v.parser
import v.pref

fn test_function_alias_source_lookup_keeps_declaration_scope() {
	root := os.join_path(os.vtmp_dir(), 'function_alias_scope_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	sources := {
		'alpha.v': 'module alpha
pub type JobFn = fn () !
pub type Chained = JobFn
pub type Generic[T] = fn (T) T
pub struct Job {
pub:
	f JobFn @[required]
}
'
		'beta.v':  'module beta
pub type JobFn = fn (mut int) string
pub struct Job {
pub:
	f JobFn @[required]
}
'
		'main.v':  'module main
import alpha as jobs
type JobFn = fn (bool) bool
struct MainJob {
	f JobFn @[required]
}
'
	}
	mut p := parser.Parser.new(pref.new_preferences())
	for name, source in sources {
		path := os.join_path(root, name)
		os.write_file(path, source)!
		p.parse_file(path)
	}
	a := p.a
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	for indexed in [false, true] {
		if indexed {
			tc.check_decl_indexes = tc.new_check_decl_indexes()
		}
		for module_name in ['alpha', 'beta', 'main'] {
			tc.cur_module = module_name
			tc.cur_file = os.join_path(root, '${module_name}.v')
			for alias_name, expected in {
				'alpha.JobFn':        'fn () !'
				'alpha.Chained':      'fn () !'
				'alpha.Generic[int]': 'fn (int) int'
				'beta.JobFn':         'fn (mut int) string'
				'JobFn':              if module_name == 'alpha' {
					'fn () !'
				} else if module_name == 'beta' {
					'fn (mut int) string'
				} else {
					'fn (bool) bool'
				}
			} {
				actual := tc.source_fn_alias_type_text_id(flat.source_type_id(alias_name)) or {
					panic('missing function alias ${alias_name}')
				}
				assert flat.source_type_name(actual) == expected, '${module_name}: ${alias_name}'
			}
			for struct_name, expected in {
				'alpha.Job': 'fn () !'
				'beta.Job':  'fn (mut int) string'
				'MainJob':   'fn (bool) bool'
			} {
				actual, _ := tc.struct_field_diagnostic_fn_type(flat.source_type_id(struct_name),
					'f', 0) or { panic('missing field ${struct_name}.f') }
				assert flat.source_type_name(actual) == expected, '${module_name}: ${struct_name}.f'
			}
			if module_name == 'main' {
				actual := tc.source_fn_alias_type_text_id(flat.source_type_id('jobs.JobFn')) or {
					panic('missing imported alias')
				}
				assert flat.source_type_name(actual) == 'fn () !'
			}
		}
	}
}
