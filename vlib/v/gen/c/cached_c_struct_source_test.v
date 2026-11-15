module c

import os
import v.cmdexec
import v.modulecache
import v.parser
import v.pref
import v.types

fn test_cached_c_structs_keep_their_individual_source_ownership() ! {
	root := os.join_path(os.vtmp_dir(), 'cached_c_struct_source_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	native_source := os.join_path(root, 'native.c.v')
	owned_source := os.join_path(root, 'owned.v')
	main_source := os.join_path(root, 'main.v')
	native_header := os.join_path(root, 'native.h')
	header_path := os.join_path(root, 'bindings.vh')
	os.write_file(native_source, 'module bindings\npub struct C.cache_native { value i32 }\n')!
	os.write_file(owned_source, 'module bindings\npub struct C.cache_owned { value i32 }\npub struct Holder { native C.cache_native owned C.cache_owned }\n')!
	os.write_file(native_header, 'typedef struct cache_native { int value; } cache_native;\n')!
	// The include belongs to a different module, as sokol.c supplies the headers
	// for sokol.gfx. Sharing a cached module must not change either struct's owner.
	os.write_file(main_source, 'module main\n#include "${native_header}"\nfn main() {}\n')!
	cc := os.find_abs_path_of_executable('cc') or { '' }
	mut header := ''
	for warm in [false, true] {
		mut p := parser.Parser.new(pref.new_preferences())
		mut a := p.parse_files(if warm {
			[header_path, main_source]
		} else {
			[native_source, owned_source, main_source]
		})
		assert p.diagnostics.len == 0, p.diagnostics.str()
		if warm {
			a.cached_header_sources[header_path] = owned_source
			a.cached_header_module_sources[header_path] = [native_source, owned_source]
		}
		mut tc := types.TypeChecker.new(a)
		tc.collect(a)
		mut g := FlatGen.new()
		g.compiler_vroot = root
		output := g.gen_with_used_options(a, map[string]bool{}, &tc, true)
		assert !output.contains('struct cache_native {'), 'warm=${warm}\n${output}'
		owned := 'struct cache_owned {'
		assert output.contains(owned), 'warm=${warm}\n${output}'
		// Compile the actual generated V-owned body beside the opaque native
		// header. Native code remains responsible for cache_native's definition.
		declaration := owned + output.all_after(owned).all_before('};') + '};\n'
		probe := os.join_path(root, 'ownership.c')
		os.write_file(probe, '#include <stdint.h>\ntypedef int32_t i32;\n#include "${native_header}"\n' + declaration +
			'void check(void) { cache_native native = {1}; struct cache_owned owned = {2}; (void)native; (void)owned; }\n')!
		if cc.len > 0 {
			compiled := cmdexec.run(cc, ['-std=c11', '-Werror', '-fsyntax-only', probe])
			assert compiled.exit_code == 0, compiled.output
		}
		regenerated := modulecache.module_header_with_const_order(a, &tc, 'bindings', root,
			map[string]string{}, []string{})
		if warm {
			assert regenerated == header
		} else {
			header = regenerated
			os.write_file(header_path, header)!
		}
	}
}
