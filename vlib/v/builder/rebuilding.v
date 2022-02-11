module builder

import os
import hash
import strings
import v.util
import v.pref
import v.vcache

pub fn (mut b Builder) rebuild_modules() {
	if !b.pref.use_cache || b.pref.build_mode == .build_module {
		return
	}
	util.timing_start('${@METHOD} source_hashing')
	mut new_hashes := map[string]string{}
	mut old_hashes := map[string]string{}
	mut sb_new_hashes := strings.new_builder(1024)
	all_files := b.parsed_files.map(it.path)
	//
	mut cm := vcache.new_cache_manager(all_files)
	sold_hashes := cm.load('.hashes', 'all_files') or { ' ' }
	// eprintln(sold_hashes)
	sold_hashes_lines := sold_hashes.split('\n')
	for line in sold_hashes_lines {
		if line.len == 0 {
			continue
		}
		x := line.split(' ')
		chash := x[0]
		cpath := x[1]
		old_hashes[cpath] = chash
	}
	// eprintln('old_hashes: $old_hashes')
	for p in b.parsed_files {
		cpath := p.path
		ccontent := util.read_file(cpath) or { '' }
		chash := hash.sum64_string(ccontent, 7).hex_full()
		new_hashes[cpath] = chash
		sb_new_hashes.write_string(chash)
		sb_new_hashes.write_byte(` `)
		sb_new_hashes.write_string(cpath)
		sb_new_hashes.write_byte(`\n`)
	}
	snew_hashes := sb_new_hashes.str()
	// eprintln('new_hashes: $new_hashes')
	// eprintln('> new_hashes != old_hashes: ' + ( old_hashes != new_hashes ).str())
	// eprintln(snew_hashes)
	cm.save('.hashes', 'all_files', snew_hashes) or {}
	util.timing_measure('${@METHOD} source_hashing')

	if new_hashes != old_hashes {
		util.timing_start('${@METHOD} rebuilding')
		// eprintln('> b.mod_invalidates_paths: $b.mod_invalidates_paths')
		// eprintln('> b.mod_invalidates_mods: $b.mod_invalidates_mods')
		// eprintln('> b.path_invalidates_mods: $b.path_invalidates_mods')
		$if trace_invalidations ? {
			for k, v in b.mod_invalidates_paths {
				mut m := map[string]bool{}
				for mm in b.mod_invalidates_mods[k] {
					m[mm] = true
				}
				eprintln('> module `$k` invalidates: $m.keys()')
				for fpath in v {
					eprintln('         $fpath')
				}
			}
		}
		mut invalidated_paths := map[string]int{}
		mut invalidated_mod_paths := map[string]int{}
		for npath, nhash in new_hashes {
			if npath !in old_hashes {
				invalidated_paths[npath]++
				continue
			}
			if old_hashes[npath] != nhash {
				invalidated_paths[npath]++
				continue
			}
		}
		for opath, ohash in old_hashes {
			if opath !in new_hashes {
				invalidated_paths[opath]++
				continue
			}
			if new_hashes[opath] != ohash {
				invalidated_paths[opath]++
				continue
			}
		}
		$if trace_invalidations ? {
			eprintln('invalidated_paths: $invalidated_paths')
		}
		mut rebuild_everything := false
		for cycle := 0; true; cycle++ {
			$if trace_invalidations ? {
				eprintln('> cycle: $cycle | invalidated_paths: $invalidated_paths')
			}
			mut new_invalidated_paths := map[string]int{}
			for npath, _ in invalidated_paths {
				invalidated_mods := b.path_invalidates_mods[npath]
				if invalidated_mods == ['main'] {
					continue
				}
				if 'builtin' in invalidated_mods {
					// When `builtin` is invalid, there is no point in
					// extracting a finer grained dependency resolution
					// of the dependencies any more. Instead, just rebuild
					// every module.
					rebuild_everything = true
					break
				}
				for imod in invalidated_mods {
					if imod == 'main' {
						continue
					}
					for np in b.mod_invalidates_paths[imod] {
						new_invalidated_paths[np]++
					}
				}
				$if trace_invalidations ? {
					eprintln('> npath -> invalidated_mods | $npath -> $invalidated_mods')
				}
				mpath := os.dir(npath)
				invalidated_mod_paths[mpath]++
			}
			if rebuild_everything {
				break
			}
			if new_invalidated_paths.len == 0 {
				break
			}
			invalidated_paths = new_invalidated_paths.clone()
		}
		if rebuild_everything {
			invalidated_mod_paths = {}
			for npath, _ in new_hashes {
				mpath := os.dir(npath)
				pimods := b.path_invalidates_mods[npath]
				if pimods == ['main'] {
					continue
				}
				invalidated_mod_paths[mpath]++
			}
		}
		$if trace_invalidations ? {
			eprintln('invalidated_mod_paths: $invalidated_mod_paths')
			eprintln('rebuild_everything: $rebuild_everything')
		}
		if invalidated_mod_paths.len > 0 {
			impaths := invalidated_mod_paths.keys()
			vexe := pref.vexe_path()
			for imp in impaths {
				b.v_build_module(vexe, imp)
			}
		}
		util.timing_measure('${@METHOD} rebuilding')
	}
}

fn (mut b Builder) v_build_module(vexe string, imp_path string) {
	pwd := os.getwd()
	defer {
		os.chdir(pwd) or {}
	}
	// do run `v build-module x` always in main vfolder; x can be a relative path
	vroot := os.dir(vexe)
	os.chdir(vroot) or {}
	boptions := b.pref.build_options.join(' ')
	rebuild_cmd := '${os.quoted_path(vexe)} $boptions build-module ${os.quoted_path(imp_path)}'
	vcache.dlog('| Builder.' + @FN, 'vexe: $vexe | imp_path: $imp_path | rebuild_cmd: $rebuild_cmd')
	$if trace_v_build_module ? {
		eprintln('> Builder.v_build_module: $rebuild_cmd')
	}
	os.system(rebuild_cmd)
}

fn (mut b Builder) rebuild_cached_module(vexe string, imp_path string) string {
	res := b.pref.cache_manager.exists('.o', imp_path) or {
		if b.pref.is_verbose {
			println('Cached $imp_path .o file not found... Building .o file for $imp_path')
		}
		b.v_build_module(vexe, imp_path)
		rebuilded_o := b.pref.cache_manager.exists('.o', imp_path) or {
			panic('could not rebuild cache module for $imp_path, error: $err.msg()')
		}
		return rebuilded_o
	}
	return res
}

fn (mut b Builder) handle_usecache(vexe string) {
	if !b.pref.use_cache || b.pref.build_mode == .build_module {
		return
	}
	mut libs := []string{} // builtin.o os.o http.o etc
	mut built_modules := []string{}
	builtin_obj_path := b.rebuild_cached_module(vexe, 'vlib/builtin')
	libs << builtin_obj_path
	for ast_file in b.parsed_files {
		if b.pref.is_test && ast_file.mod.name != 'main' {
			imp_path := b.find_module_path(ast_file.mod.name, ast_file.path) or {
				verror('cannot import module "$ast_file.mod.name" (not found)')
				break
			}
			obj_path := b.rebuild_cached_module(vexe, imp_path)
			libs << obj_path
			built_modules << ast_file.mod.name
		}
		for imp_stmt in ast_file.imports {
			imp := imp_stmt.mod
			// strconv is already imported inside builtin, so skip generating its object file
			// TODO: incase we have other modules with the same name, make sure they are vlib
			// is this even doign anything?
			if imp in ['strconv', 'strings', 'dlmalloc'] {
				continue
			}
			if imp in built_modules {
				continue
			}
			if util.should_bundle_module(imp) {
				continue
			}
			// The problem is cmd/v is in module main and imports
			// the relative module named help, which is built as cmd.v.help not help
			// currently this got this workign by building into main, see ast.FnDecl in cgen
			if imp == 'help' {
				continue
			}
			imp_path := b.find_module_path(imp, ast_file.path) or {
				verror('cannot import module "$imp" (not found)')
				break
			}
			obj_path := b.rebuild_cached_module(vexe, imp_path)
			libs << obj_path
			built_modules << imp
		}
	}
	b.ccoptions.post_args << libs
}
