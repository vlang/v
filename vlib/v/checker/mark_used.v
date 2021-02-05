module checker

import v.ast
import v.table
import v.util
import v.checker.mark_used_walker

// mark_used walks the AST, starting at main() and marks all used fns transitively
fn (mut c Checker) mark_used(ast_files []ast.File) {
	// println('walking the ast')
	// c.is_recursive = true
	// c.fn_decl(mut c.table2.main_fn_decl_node)

	util.timing_start(@METHOD)
	mut walker := mark_used_walker.Walker{
		files: ast_files
	}

	// TODO: walking over the AST using roots != main.main, like `panic`
	// for example, will potentially eliminate many cases where manual
	// whitelisting is needed.
	// TODO: use the full name of the functions in the map. For now the
	// receiver type is skipped, so we can not distinguish between
	// array.trim and string.trim etc.
	for stmt in c.main_fn_decl_node.stmts {
		walker.stmt(stmt)
	}
	// walker.fn_decl(mut c.table2.main_fn_decl_node)
	// println('time = ${time.ticks() - t}ms, nr used fns=$walker.used_fns.len')

	/*
	for key, _ in walker.used_fns {
		println(key)
	}
	*/
	c.table.used_fns = walker.used_fns

	// Whitelist some functions that are used by cgen directly:
	c.table.used_fns['tos'] = true
	c.table.used_fns['tos2'] = true
	c.table.used_fns['v_panic'] = true
	c.table.used_fns['panic'] = true
	c.table.used_fns['eprintln'] = true
	c.table.used_fns['print_backtrace_skipping_top_frames'] = true
	c.table.used_fns['print_backtrace_skipping_top_frames_linux'] = true
	c.table.used_fns['new_array_from_c_array'] = true
	c.table.used_fns['__new_array_with_default'] = true
	c.table.used_fns['__new_array'] = true
	c.table.used_fns['vcalloc'] = true
	c.table.used_fns['set_unsafe'] = true
	c.table.used_fns['get_unsafe'] = true
	c.table.used_fns['push'] = true
	c.table.used_fns['ensure_cap'] = true
	c.table.used_fns['v_realloc'] = true
	c.table.used_fns['all_before'] = true
	c.table.used_fns['all_after'] = true
	c.table.used_fns['add'] = true
	c.table.used_fns['isnil'] = true
	c.table.used_fns['vstrlen'] = true
	c.table.used_fns['trim_space'] = true
	c.table.used_fns['malloc'] = true
	c.table.used_fns['trim'] = true
	c.table.used_fns['at'] = true
	c.table.used_fns['replace'] = true
	c.table.used_fns['index_'] = true
	c.table.used_fns['index_after'] = true
	c.table.used_fns['index_kmp'] = true
	c.table.used_fns['substr'] = true
	c.table.used_fns['clone'] = true
	c.table.used_fns['free'] = true
	c.table.used_fns['has_index'] = true
	c.table.used_fns['key'] = true
	c.table.used_fns['set'] = true
	c.table.used_fns['get'] = true
	c.table.used_fns['new_node'] = true
	c.table.used_fns['eq'] = true
	c.table.used_fns['ne'] = true
	c.table.used_fns['lt'] = true
	c.table.used_fns['gt'] = true
	c.table.used_fns['le'] = true
	c.table.used_fns['ge'] = true
	c.table.used_fns['split_child'] = true
	c.table.used_fns['bytes'] = true
	c.table.used_fns['utf8_char_len'] = true
	c.table.used_fns['utf8_str_visible_length'] = true
	c.table.used_fns['main.main'] = true
	c.table.used_fns['builtin_init'] = true
	c.table.used_fns['memdup'] = true
	c.table.used_fns['vstring'] = true
	c.table.used_fns['vstring_with_len'] = true
	c.table.used_fns['string'] = true // array.string
	// c.table.used_fns['str'] = true // builtin .str() methods; They use strings.builder and strconv.ftoa_64 etc.
	// whitelist common modules const initializers too:
	c.table.used_fns['os.getwd'] = true
	c.table.used_fns['os.init_os_args'] = true
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
