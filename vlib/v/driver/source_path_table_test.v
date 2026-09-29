module driver

import os
import v.flat
import v.parser
import v.pref
import v.token
import v.types

// The driver stages resolve source paths through the AST's table of resolved
// source paths. Each test records an answer os.real_path could never give (a
// path such as 'recorded.v' for a file that is not there), so a stage that
// resolves a path again instead of asking the table gives a different answer.

fn source_path_table_root(name string) string {
	root := os.join_path(os.vtmp_dir(), 'v3_source_path_table_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	return root
}

// parse_recorded_file parses `source` from `root/file_name` and records
// 'recorded.v' as the resolved path of that file and of the spelling
// 'written.v'. It leaves the table open.
fn parse_recorded_file(root string, file_name string, source string) &flat.FlatAst {
	path := os.join_path(root, file_name)
	os.write_file(path, source) or { panic(err) }
	mut a := parse_unrecorded_files([path])
	a.resolved_source_paths[path] = 'recorded.v'
	a.resolved_source_paths['written.v'] = 'recorded.v'
	return a
}

fn parse_unrecorded_files(paths []string) &flat.FlatAst {
	mut p := parser.Parser.new(pref.new_preferences())
	return p.parse_files(paths)
}

// assert_records_both_scans checks that a scan of the initial files recorded
// the path it was given and the parsed file it did not select by name.
fn assert_records_both_scans(a &flat.FlatAst, parsed_path string) {
	assert a.resolved_source_paths['elsewhere.v'] == os.real_path('elsewhere.v')
	assert a.resolved_source_paths[parsed_path] == os.real_path(parsed_path)
}

fn module_decl_values(a &flat.FlatAst) []string {
	mut values := []string{}
	for node in a.nodes {
		if node.kind == .module_decl {
			values << node.value
		}
	}
	return values
}

fn test_cached_type_diagnostics_resolve_through_the_source_path_table() {
	mut file_set := token.FileSet.new()
	mut a := flat.FlatAst.new()
	a.source_files[1] = file_set.add_file('written.v', 16)
	a.resolved_source_paths['written.v'] = 'recorded.v'
	a.resolve_source_paths()
	cached := cache_v3_type_diagnostics(&a, [
		types.TypeError{
			msg:      'cached notice'
			pos:      token.new_span(1, 0, 1)
			severity: 'notice'
		},
	])
	assert cached.len == 1
	assert cached[0].file == 'recorded.v'
	restored := restore_v3_type_diagnostics(mut a, cached)
	assert restored.len == 1
	assert restored[0].pos.id == 1
}

fn test_watched_sources_resolve_through_the_source_path_table() {
	mut file_set := token.FileSet.new()
	mut a := flat.FlatAst.new()
	a.source_files[1] = file_set.add_file('written.v', 16)
	a.resolved_source_paths['written.v'] = 'recorded.v'
	a.resolved_source_paths['cached.v'] = 'recorded_cached.v'
	a.resolve_source_paths()
	watched := watched_v_source_paths(&a, {
		'cached': ['cached.v']
	})
	assert watched == {
		'recorded.v':        true
		'recorded_cached.v': true
	}
}

fn test_monomorph_cache_inputs_resolve_through_the_source_path_table() {
	root := source_path_table_root('monomorph')
	defer {
		os.rmdir_all(root) or {}
	}
	mut a := parse_recorded_file(root, 'main.v', "module main\n\nfn text() string {\n\treturn 'cached text'\n}\n")
	a.resolve_source_paths()
	assert monomorph_cache_runtime_strings(a, ['written.v']).any(it.contains('cached text'))
	assert monomorph_cache_semantic_signature(a, ['written.v']) != monomorph_cache_semantic_signature(a,
		[])
}

fn test_incremental_snapshot_resolves_through_the_source_path_table() {
	root := source_path_table_root('snapshot')
	defer {
		os.rmdir_all(root) or {}
	}
	mut a := parse_recorded_file(root, 'sample.v', 'module sample\n\npub fn value() int {\n\treturn 1\n}\n')
	a.resolve_source_paths()
	snapshot := incremental_program_snapshot(a, ['written.v'])
	assert snapshot.functions.len == 1
	assert snapshot.functions[0].name == 'sample.value'
}

fn test_reachability_rebuild_check_resolves_through_the_source_path_table() {
	root := source_path_table_root('reachability')
	defer {
		os.rmdir_all(root) or {}
	}
	mut a := parse_recorded_file(root, 'main.v', 'module main\n\nfn main() {}\n')
	a.resolve_source_paths()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	mut changed := {
		'main': true
	}
	mut used := map[string]bool{}
	// `main` is reached but was not used by the cached build, and it is only a
	// program function when 'written.v' resolves to its file.
	assert incremental_changed_functions_require_reachability_rebuild(a, &tc, mut changed, mut
		used, ['written.v'])
}

fn test_checker_fixture_header_check_resolves_through_the_source_path_table() {
	root := source_path_table_root('fixture_header')
	defer {
		os.rmdir_all(root) or {}
	}
	mut a := parse_recorded_file(root, 'fixture.v', 'module main\n\n#include "v3_source_path_table_missing.h"\n\nfn main() {}\n')
	a.resolve_source_paths()
	missing := checker_fixture_missing_header(a, ['written.v'], 'cc', []string{}) or { '' }
	assert missing.contains('v3_source_path_table_missing.h')
}

fn test_native_privacy_scan_resolves_v_sources_through_the_source_path_table() {
	root := source_path_table_root('native_privacy')
	defer {
		os.rmdir_all(root) or {}
	}
	referencing := os.join_path(root, 'referencing.v')
	clean := os.join_path(root, 'clean.v')
	os.write_file(referencing, 'module sibling\n\nfn use() {\n\tC.v3_private_ident()\n}\n')!
	os.write_file(clean, 'module sibling\n\nfn unrelated() {}\n')!
	identifiers := {
		'v3_private_ident': true
	}
	sibling_state := &V3ModuleCacheState{
		module_sources: {
			'owner':   []string{}
			'sibling': ['written_sibling.v']
		}
	}
	// A user file and a sibling module source reach the scan only through the table.
	mut a := flat.FlatAst.new()
	a.resolved_source_paths['written_user.v'] = referencing
	a.resolved_source_paths['written_sibling.v'] = referencing
	a.resolve_source_paths()
	user_state := &V3ModuleCacheState{
		module_sources: {
			'owner': []string{}
		}
	}
	assert !cache_external_identifiers_are_private_to_module(&a, user_state, 'owner', identifiers,
		['written_user.v'], '')
	assert !cache_external_identifiers_are_private_to_module(&a, sibling_state, 'owner',
		identifiers, []string{}, '')
	// A parsed file that resolves to a source the scan already read is not read
	// again.
	mut parsed := parse_unrecorded_files([referencing])
	parsed.resolved_source_paths['written_sibling.v'] = clean
	parsed.resolved_source_paths[referencing] = clean
	parsed.resolve_source_paths()
	assert cache_external_identifiers_are_private_to_module(parsed, sibling_state, 'owner',
		identifiers, []string{}, '')
}

fn test_unsupported_generic_scan_resolves_through_the_source_path_table() {
	root := source_path_table_root('generic_scope')
	defer {
		os.rmdir_all(root) or {}
	}
	mut a := flat.FlatAst.new()
	a.add_node(flat.Node{
		kind:  .file
		value: 'written.v'
	})
	a.resolved_source_paths['written.v'] = os.join_path(os.real_path(root), 'recorded.v')
	a.resolve_source_paths()
	mut tc := types.TypeChecker.new(&a)
	// The root is resolved as well, so any spelling of it works.
	set_unsupported_generic_files(mut tc, &a, true, root + os.path_separator + '.')
	assert tc.diagnostic_files['generic:written.v']
}

fn test_builtin_bundle_check_resolves_through_the_source_path_table() {
	mut a := flat.FlatAst.new()
	a.resolved_source_paths['written_strconv.v'] = 'recorded_strconv.v'
	a.resolve_source_paths()
	state := &V3ModuleCacheState{
		bundle_source_paths: {
			'recorded_strconv.v': true
		}
		module_sources:      {
			'strconv': ['written_strconv.v']
		}
	}
	assert module_is_builtin_bundle(state, &a, 'strconv')
	assert cache_builtin_bundle_roots(state, &a) == ['strconv']
}

// Before the table is frozen, the scans of the initial files record what they
// resolve, so later stages find those answers in the table.
fn test_initial_import_scan_records_through_the_source_path_table() {
	root := source_path_table_root('initial_imports')
	defer {
		os.rmdir_all(root) or {}
	}
	mut a := parse_recorded_file(root, 'main.v', 'module main\n\nimport sample\n\nfn main() {}\n')
	assert imports_from_files(mut a, ['written.v']) == {
		'sample': true
	}
	path := os.join_path(root, 'main.v')
	mut fresh := parse_unrecorded_files([path])
	assert imports_from_files(mut fresh, ['elsewhere.v']).len == 0
	assert_records_both_scans(fresh, path)
}

fn test_initial_module_seeding_records_through_the_source_path_table() {
	root := source_path_table_root('initial_modules')
	defer {
		os.rmdir_all(root) or {}
	}
	alpha := os.join_path(root, 'alpha.v')
	beta := os.join_path(root, 'beta.v')
	os.write_file(alpha, 'module alpha\n\npub fn a() {}\n')!
	os.write_file(beta, 'module beta\n\npub fn b() {}\n')!
	mut a := parse_unrecorded_files([alpha, beta])
	a.resolved_source_paths[alpha] = 'recorded_alpha.v'
	a.resolved_source_paths['written_alpha.v'] = 'recorded_alpha.v'
	a.resolved_source_paths[beta] = 'recorded_beta.v'
	a.resolved_source_paths['written_beta.v'] = 'recorded_beta.v'
	// Both files declare a module, so the explicitly imported `alpha` still stays
	// local: that needs the first scan to find both files too.
	mut parsed_modules := map[string]bool{}
	seed_initial_modules(mut a, ['written_alpha.v', 'written_beta.v'], {
		'alpha': true
	}, mut parsed_modules)
	assert parsed_modules == {
		'alpha': true
		'beta':  true
	}
	mut fresh := parse_unrecorded_files([alpha])
	mut fresh_modules := map[string]bool{}
	seed_initial_modules(mut fresh, ['elsewhere.v'], map[string]bool{}, mut fresh_modules)
	assert fresh_modules.len == 0
	assert_records_both_scans(fresh, alpha)
}

fn test_initial_module_canonicalization_records_through_the_source_path_table() {
	root := source_path_table_root('initial_canonical')
	defer {
		os.rmdir_all(root) or {}
	}
	module_dir := os.join_path(root, 'vlib', 'pkg', 'sample')
	os.mkdir_all(module_dir)!
	mut a := parse_recorded_file(module_dir, 'sample.v', 'module sample\n\npub fn value() {}\n')
	mut prefs := pref.new_preferences()
	prefs.vroot = root
	prefs.module_search_paths = []string{}
	canonicalize_colliding_initial_modules(mut a, prefs, ['written.v'], {
		'sample': true
	})
	assert module_decl_values(a) == ['pkg.sample']
	path := os.join_path(module_dir, 'sample.v')
	mut fresh := parse_unrecorded_files([path])
	canonicalize_colliding_initial_modules(mut fresh, prefs, ['elsewhere.v'], {
		'sample': true
	})
	assert module_decl_values(fresh) == ['sample']
	assert_records_both_scans(fresh, path)
}
