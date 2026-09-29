module driver

import os
import v.flat
import v.modulecache
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

fn test_cgen_cache_input_resolves_through_the_source_path_table() {
	mut a := flat.FlatAst.new()
	// Answers only the table can give, so a direct os.real_path would show,
	// and each file resolves to a DISTINCT answer so a bug that resolves
	// every file in a module's list through that list's first element alone
	// cannot pass by coincidence.
	a.resolved_source_paths['written_user.v'] = 'recorded_user.v'
	a.resolved_source_paths['written_module_a.v'] = 'recorded_module_a.v'
	a.resolved_source_paths['written_module_b.v'] = 'recorded_module_b.v'
	a.resolve_source_paths()
	state := &V3ModuleCacheState{
		module_sources: {
			'sample': ['written_module_a.v', 'written_module_b.v']
		}
	}
	input := v3_cgen_cache_input(state, &a, ['written_user.v'], []string{})
	assert input.source_files == ['recorded_user.v']
	assert input.dependency_inputs['module:sample'] == modulecache.header_signature(['recorded_module_a.v',
		'recorded_module_b.v'].join('\n'))
}

fn test_cache_vlib_source_and_header_paths_resolves_through_the_source_path_table() {
	mut a := flat.FlatAst.new()
	// Each file resolves to a DISTINCT answer only the table can give, so a
	// bug that resolves every file in a module's list through that list's
	// first element alone cannot pass by coincidence.
	a.resolved_source_paths['/vlib/strconv/written_a.v'] = 'recorded_vlib_a.v'
	a.resolved_source_paths['/vlib/strconv/written_b.v'] = 'recorded_vlib_b.v'
	a.resolve_source_paths()
	state := &V3ModuleCacheState{
		module_sources: {
			'strconv': ['/vlib/strconv/written_a.v', '/vlib/strconv/written_b.v']
		}
	}
	paths := cache_vlib_source_and_header_paths(state, &a)
	assert paths['/vlib/strconv/written_a.v']
	assert paths['/vlib/strconv/written_b.v']
	assert paths['recorded_vlib_a.v']
	assert paths['recorded_vlib_b.v']
}

fn test_prune_cache_only_function_prototypes_resolves_through_the_source_path_table() {
	mut a := flat.FlatAst.new()
	// An answer only the table can give, so a direct os.real_path would show.
	a.resolved_source_paths['/vlib/owner_mod/x.v'] = 'recorded_owner.v'
	a.resolved_source_paths['written_fn_file.v'] = 'recorded_owner.v'
	a.resolve_source_paths()
	state := &V3ModuleCacheState{
		module_sources: {
			'owner_mod': ['/vlib/owner_mod/x.v']
		}
	}
	mut tc := types.TypeChecker.new(&a)
	tc.fn_type_files['owner_mod.helper'] = 'written_fn_file.v'
	cache_used_fns := {
		'owner_mod.helper': true
	}
	source := 'int owner_mod__helper(void);\n'
	pruned := prune_cache_only_function_prototypes(source, &cache_used_fns, '', &tc, state)
	// The function's raw source file resolves, through the table, to the
	// same path the vlib scan already recorded -- so it is recognized as
	// vlib and its prototype is left alone rather than pruned as cache-only.
	assert pruned == source
}

fn test_crun_build_identity_resolves_through_the_source_path_table() {
	root := source_path_table_root('crun_identity')
	defer {
		os.rmdir_all(root) or {}
	}
	vsh_path := os.join_path(root, 'script.vsh')
	user_path := os.join_path(root, 'other.v')
	second_user_path := os.join_path(root, 'second.v')
	alias_target := os.join_path(root, 'alias_target.v')
	second_target := os.join_path(root, 'second_target.v')
	os.write_file(vsh_path, 'println(1)\n')!
	os.write_file(user_path, 'println(2)\n')!
	os.write_file(second_user_path, 'println(3)\n')!
	os.write_file(alias_target, 'println(4)\n')!
	os.write_file(second_target, 'println(5)\n')!
	prefs := pref.new_preferences()
	state := &V3ModuleCacheState{
		manager: modulecache.Manager{
			dir: root
		}
	}
	mut plain := flat.FlatAst.new()
	distinct_identity := v3_crun_build_identity(state, &plain, prefs, [user_path, second_user_path],
		[]string{}, []string{}, false, false, vsh_path)

	mut aliased := flat.FlatAst.new()
	// An answer only the table can give: it maps the user file onto the same
	// resolved path as the running script, so it must be excluded from the
	// content-addressed identity exactly like a real alias of the script
	// would be. Both resolve to other REAL files (cached_source_signature
	// needs readable files to produce a non-empty signature at all).
	// second_user_path resolves to its OWN distinct real file, so a bug that
	// resolves every user file through user_files[0] alone (rather than the
	// per-element loop variable) cannot pass by coincidence.
	aliased.resolved_source_paths[user_path] = alias_target
	aliased.resolved_source_paths[vsh_path] = alias_target
	aliased.resolved_source_paths[second_user_path] = second_target
	aliased.resolve_source_paths()
	excluding_identity := v3_crun_build_identity(state, &aliased, prefs, [user_path, second_user_path],
		[]string{}, []string{}, false, false, vsh_path)
	only_second_identity := v3_crun_build_identity(state, &aliased, prefs, [second_user_path],
		[]string{}, []string{}, false, false, vsh_path)
	// A signature computed over zero or unreadable files returns '', which
	// would make both comparisons below hold vacuously; guard against that.
	assert distinct_identity.len > 0
	assert excluding_identity.len > 0
	assert distinct_identity != excluding_identity
	assert excluding_identity == only_second_identity
}

fn test_crun_build_identity_resolves_module_sources_through_the_source_path_table() {
	root := source_path_table_root('crun_identity_modules')
	defer {
		os.rmdir_all(root) or {}
	}
	vsh_path := os.join_path(root, 'script.vsh')
	module_a := os.join_path(root, 'module_a.v')
	module_b := os.join_path(root, 'module_b.v')
	shared_target := os.join_path(root, 'shared_target.v')
	os.write_file(vsh_path, 'println(1)\n')!
	os.write_file(module_a, 'println(2)\n')!
	os.write_file(module_b, 'println(3)\n')!
	os.write_file(shared_target, 'println(4)\n')!
	prefs := pref.new_preferences()

	mut two_files := flat.FlatAst.new()
	// An answer only the table can give: it collapses two DIFFERENT real
	// files onto the SAME resolved path, so the identity must treat them as
	// one source, not two -- a direct os.real_path call could never produce
	// this collapse, since module_a and module_b are genuinely distinct
	// files on disk.
	two_files.resolved_source_paths[module_a] = shared_target
	two_files.resolved_source_paths[module_b] = shared_target
	two_files.resolve_source_paths()
	two_files_state := &V3ModuleCacheState{
		manager:        modulecache.Manager{
			dir: root
		}
		module_sources: {
			'a': [module_a, module_b]
		}
	}
	two_files_identity := v3_crun_build_identity(two_files_state, &two_files, prefs, []string{},
		[]string{}, []string{}, false, false, vsh_path)

	mut one_file := flat.FlatAst.new()
	one_file.resolved_source_paths[module_a] = shared_target
	one_file.resolve_source_paths()
	one_file_state := &V3ModuleCacheState{
		manager:        modulecache.Manager{
			dir: root
		}
		module_sources: {
			'a': [module_a]
		}
	}
	one_file_identity := v3_crun_build_identity(one_file_state, &one_file, prefs, []string{},
		[]string{}, []string{}, false, false, vsh_path)
	// A signature computed over zero or unreadable files returns '', which
	// would make the comparison below hold vacuously; guard against that.
	assert two_files_identity.len > 0
	assert one_file_identity.len > 0
	assert two_files_identity == one_file_identity
}
