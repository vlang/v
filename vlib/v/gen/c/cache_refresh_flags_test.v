module c

import os
import v.flat
import v.pref
import v.util.cache_refresh

fn test_refresh_keeps_updated_module_flags_and_project_group_order() {
	mut a := flat.FlatAst.new()
	a.add_node(flat.Node{ kind: .file, value: os.join_path(@VEXEROOT, 'vlib', 'sample', 'sample.v') })
	a.add_node(flat.Node{ kind: .module_decl, value: 'sample' })
	a.add_node(flat.Node{ kind: .directive, value: 'flag', typ: flat.source_type_id('-DMODULE=new') })
	a.add_node(flat.Node{ kind: .directive, value: 'flag', typ: flat.source_type_id('-DADDED') })
	project := [
		cache_refresh.FlagGroup{ flags: ['-DSTART'], at_start: true, in_main: true },
		cache_refresh.FlagGroup{ flags: ['-DPROJECT', '-include', 'config.h'], in_main: true },
	]
	flags := cache_directive_flags_with_project(&a, @VEXEROOT, pref.target_from('linux', 'amd64') or { panic(err) }, map[string]string{}, project)
	assert flags == ['-DSTART', '-DMODULE=new', '-DADDED', '-DPROJECT', '-include', 'config.h']
}

fn test_refresh_deduplicates_project_groups_before_linker_filtering() {
	fresh := CFlagDirectiveGroup{ flags: ['-DDUP', '-L', 'library'], in_main: false }
	recorded := CFlagDirectiveGroup{ flags: ['-DDUP'], in_main: true, identity: fresh.flags.join('\x00') }
	assert ordered_c_flag_directive_groups([fresh, recorded]) == fresh.flags
}

fn test_project_flag_inventory_excludes_vlib_and_keeps_vlib_entry_files() {
	mut a := flat.FlatAst.new()
	module_file := os.join_path(@VEXEROOT, 'vlib', 'sample', 'sample.v')
	entry_file := os.join_path(@VEXEROOT, 'vlib', 'sample', 'sample_test.v')
	project_file := os.join_path(os.vtmp_dir(), 'project', 'main.v')
	for file in [module_file, entry_file, project_file] {
		a.add_node(flat.Node{ kind: .file, value: file })
		a.add_node(flat.Node{
			kind:  .module_decl
			value: if file == module_file {
				'sample'
			} else {
				'main'
			}
		})
		a.add_node(flat.Node{
			kind:  .directive
			value: 'flag'
			typ:   flat.source_type_id(if file == module_file {
				'-DMODULE'
			} else {
				'-DPROJECT'
			})
		})
	}
	groups := cache_project_flag_groups(&a, @VEXEROOT, pref.target_from('linux', 'amd64') or { panic(err) }, map[string]string{}, {
		entry_file: true
	})
	assert groups.len == 2
	assert groups.all(it.flags == ['-DPROJECT'] && it.in_main)
}
