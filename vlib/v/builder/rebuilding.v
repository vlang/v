module builder

import hash
import strings
import v.util
import v.vcache

pub fn (mut b Builder) rebuild_modules() {
	$if !usecache_invalidate ? {
		return
	}
	if !b.pref.use_cache {
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
		sb_new_hashes.write_b(` `)
		sb_new_hashes.write_string(cpath)
		sb_new_hashes.write_b(`\n`)
	}
	snew_hashes := sb_new_hashes.str()
	// eprintln('new_hashes: $new_hashes')
	// eprintln('> new_hashes != old_hashes: ' + ( old_hashes != new_hashes ).str())
	// eprintln(snew_hashes)
	cm.save('.hashes', 'all_files', snew_hashes) or {}
	util.timing_measure('${@METHOD} source_hashing')

	if new_hashes != old_hashes {
		util.timing_start('${@METHOD} rebuilding')
		eprintln('> b.mod_invalidates_paths: $b.mod_invalidates_paths')
		eprintln('> b.mod_invalidates_mods: $b.mod_invalidates_mods')
		eprintln('> b.path_invalidates_mods: $b.path_invalidates_mods')
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
		util.timing_measure('${@METHOD} rebuilding')
	}
}
