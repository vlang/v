module main

import os
import v.help

fn vpm_search(keywords []string) {
	search_keys := keywords.map(it.replace('_', '-'))
	if settings.is_help {
		help.print_and_exit('search')
	}
	if search_keys.len == 0 {
		vpm_error('specify at least one keyword to search for.')
		exit(2)
	}
	modules := get_all_modules()
	installed_modules := get_installed_modules()
	joined := search_keys.join(', ')
	mut index := 0
	for mod in modules {
		for k in search_keys {
			if !mod.contains(k) {
				continue
			}
			if index == 0 {
				println('Search results for `${joined}`:\n')
			}
			index++
			mut parts := mod.split('.')
			// in case the author isn't present
			if parts.len == 1 {
				parts << parts[0]
				parts[0] = ' '
			} else {
				parts[0] = ' by ${parts[0]} '
			}
			installed := if mod in installed_modules { ' (installed)' } else { '' }
			println('${index}. ${parts[1]}${parts[0]}[${mod}]${installed}')
			break
		}
	}
	if index == 0 {
		vroot := os.real_path(os.dir(vexe))
		mut messages := ['No module(s) found for `${joined}` .']
		for vlibmod in search_keys {
			if os.is_dir(os.join_path(vroot, 'vlib', vlibmod)) {
				messages << 'There is already an existing `${vlibmod}` module in vlib, so you can just `import ${vlibmod}` .'
			}
		}
		for m in messages {
			println(m)
		}
	} else {
		eprintln('\nUse `v install author_name.module_name` to install the module.')
	}
}
