module checker

import v.ast
import v.table
import v.util
import v.checker.mark_used_walker

// mark_used walks the AST, starting at main() and marks all used fns transitively
fn (mut c Checker) mark_used(ast_files []ast.File) {
	util.timing_start(@METHOD)
	mut walker := mark_used_walker.Walker{
		files: ast_files
	}

	mut allfns := map[string]ast.FnDecl{}
	for i in 0 .. ast_files.len {
		file := unsafe { &ast_files[i] }
		for node in file.stmts {
			if node is ast.FnDecl {
				fkey := if node.is_method {
					'${int(node.receiver.typ)}.$node.name'
				} else {
					node.name
				}
				allfns[fkey] = node
			}
		}
	}
	all_fn_root_names := [
		'main.main',
		'__new_array',
		'__new_array_with_default',
		'new_array_from_c_array',
		'panic',
		'memdup',
		'vstrlen',
		'tos2',
		'tos',
		'isnil',
		'utf8_char_len',
		'utf8_str_visible_length',
		'builtin_init',
		'print_backtrace_skipping_top_frames',
		'print_backtrace_skipping_top_frames_mac',
		'print_backtrace_skipping_top_frames_linux',
		'print_backtrace_skipping_top_frames_freebsd',
		'print_backtrace_skipping_top_frames_windows',
		'print_backtrace_skipping_top_frames_mingw',
		'print_backtrace_skipping_top_frames_msvc',
		'print_backtrace_skipping_top_frames_tcc',
		/* byteptr and charptr */
		'3.vstring',
		'3.vstring_with_len',
		'4.vstring',
		'4.vstring_with_len',
		/* string. methods */
		'18.add',
		'18.all_after',
		'18.all_before',
		'18.trim_space',
		'18.replace',
		'18.clone',
		'18.trim',
		'18.substr',
		'18.at',
		'18.index_kmp',
		/* string. ==, !=, etc... */
		'18.eq',
		'18.ne',
		'18.lt',
		'18.gt',
		'18.le',
		'18.ge',
		/* ustring. ==, !=, etc... */
		'19.eq',
		'19.ne',
		'19.lt',
		'19.gt',
		'19.le',
		'19.ge',
		'19.add',
		/* other array methods */
		'21.get',
		'21.get_unsafe',
		'59.get',
		'65557.free',
		'65557.push',
		'65557.set',
		'65557.set_unsafe',
		/* TODO: process the _vinit const initializations automatically too */
		'os.getwd',
		'os.init_os_args',
	]
	// println( allfns.keys() )
	for fn_name in all_fn_root_names {
		walker.fn_decl(mut allfns[fn_name])
	}

	$if trace_skip_unused ? {
		for key, _ in walker.used_fns {
			println('> used fn key: $key')
		}
	}

	c.table.used_fns = walker.used_fns
	//
	c.table.used_fns['term.can_show_color_on_stdin'] = true
	c.table.used_fns['term.can_show_color_on_stdout'] = true
	c.table.used_fns['term.can_show_color_on_stderr'] = true
	//
	c.table.used_fns['main.can_use_relative_paths'] = true
	//
	// eprintln('>>> c.table.used_fns: $c.table.used_fns')
	util.timing_measure(@METHOD)

	// println(walker.used_fns)
	// c.walk(ast_files)
}
