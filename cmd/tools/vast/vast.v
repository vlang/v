module main

import flag
import os
import time
import v.flat
import v.parser
import v.pref
import v.token

struct Context {
mut:
	is_watch         bool
	is_compile       bool
	is_print         bool
	is_terse         bool
	is_skip_defaults bool
	check            bool
	hide_names       map[string]bool
}

fn main() {
	if os.args.len < 2 {
		eprintln('not enough parameters')
		exit(1)
	}
	mut ctx := Context{}
	mut fp := flag.new_flag_parser(os.args[2..])
	fp.application('v ast')
	fp.usage_example('demo.v       generate demo.json file.')
	fp.usage_example('-w demo.v    generate demo.json file, and watch for changes.')
	fp.usage_example('-c demo.v    generate demo.json *and* a demo.c file, and watch for changes.')
	fp.usage_example('-p demo.v    print the json output to stdout.')
	fp.usage_example('-s demo.v    do NOT show properties having default values.')
	fp.description('Dump a JSON representation of the V AST for a given .v or .vsh file.')
	fp.description('By default, `v ast` saves JSON next to the input file.')
	ctx.is_watch = fp.bool('watch', `w`, false, 'watch a V file and rewrite its JSON when it changes')
	ctx.is_print = fp.bool('print', `p`, false, 'print the AST to stdout')
	ctx.is_compile = fp.bool('compile', `c`, false, 'watch a V file, rewrite its JSON, and generate C whenever it changes')
	ctx.is_terse = fp.bool('terse', `t`, false, 'show only AST node names and structure')
	ctx.is_skip_defaults = fp.bool('skip-defaults', `s`, false, 'skip properties that have default values')
	ctx.check = fp.bool('check', `k`, false, 'type check the input before dumping its AST')
	hfields := fp.string_multi('hide', 0, 'hide fields; specify several by separating them with commas').join(',')
	for field in hfields.split(',') {
		ctx.hide_names[field] = true
	}
	fp.limit_free_args_to_at_least(1)!
	for vfile in fp.remaining_parameters() {
		file := absolute_path(vfile)
		check_file(file)
		ctx.write_file_or_print(file)
		if ctx.is_watch || ctx.is_compile {
			ctx.watch_for_changes(file)
		}
	}
}

fn (ctx Context) write_file_or_print(file string) {
	if ctx.check {
		compiler := os.getenv_opt('VEXE') or { 'v' }
		result := os.execute('${os.quoted_path(compiler)} -check ${os.quoted_path(file)}')
		if result.exit_code != 0 {
			eprint(result.output)
			exit(result.exit_code)
		}
	}
	ast_json := ctx.json(file)
	if ctx.is_print {
		println(ast_json)
	} else {
		out_file := file[..file.len - os.file_ext(file).len] + '.json'
		os.write_file(out_file, ast_json) or { panic(err) }
		println('${time.now()}: AST written to: ${out_file}')
	}
}

fn (ctx Context) watch_for_changes(file string) {
	println('start watching...')
	mut timestamp := i64(0)
	for {
		new_timestamp := os.file_last_mod_unix(file)
		if timestamp != new_timestamp {
			ctx.write_file_or_print(file)
			if ctx.is_compile {
				compiler := os.getenv_opt('VEXE') or { 'v' }
				file_name := file[..file.len - os.file_ext(file).len]
				os.system('${os.quoted_path(compiler)} -o ${os.quoted_path(file_name + '.c')} ${os.quoted_path(file)}')
			}
		}
		timestamp = new_timestamp
		time.sleep(500 * time.millisecond)
	}
}

fn absolute_path(path string) string {
	if os.is_abs_path(path) {
		return path
	}
	if path.starts_with('./') {
		return os.join_path(os.getwd(), path[2..])
	}
	return os.join_path(os.getwd(), path)
}

fn check_file(file string) {
	if os.file_ext(file) !in ['.v', '.vv', '.vsh'] {
		eprintln('the file `${file}` must be a v file or vsh file')
		exit(1)
	}
	if !os.exists(file) {
		eprintln('the v file `${file}` does not exist')
		exit(1)
	}
}

fn (ctx Context) json(file string) string {
	mut prefs := pref.new_preferences()
	prefs.enable_globals = true
	prefs.is_fmt = true
	mut p := parser.Parser.new(prefs)
	a := p.parse_file(file)
	mut root := create_object()
	mut files := create_array()
	for raw_id in a.file_node_ids {
		id := flat.NodeId(raw_id)
		node := a.node(id)
		if node.kind == .file && node.children_count > 0 {
			files.add_item(ctx.ast_node(a, id))
		}
	}
	ctx.add_field(mut root, 'files', files, false)
	if a.comments.len > 0 {
		mut comments := create_array()
		for comment in a.comments {
			mut item := create_object()
			ctx.add_field(mut item, 'text', create_string(comment.text), false)
			ctx.add_field(mut item, 'pos', position_node(comment.pos), false)
			comments.add_item(item)
		}
		ctx.add_field(mut root, 'comments', comments, false)
	}
	return json_print(mut root)
}

fn (ctx Context) ast_node(a &flat.FlatAst, id flat.NodeId) &Node {
	node := a.node(id)
	mut result := create_object()
	ctx.add_field(mut result, 'kind', create_string('${node.kind}'), false)
	ctx.add_field(mut result, 'value', create_string(node.value), node.value == '')
	ctx.add_field(mut result, 'type', create_string(node.typ), node.typ == '')
	ctx.add_field(mut result, 'op', create_string('${node.op}'), node.op == .none)
	ctx.add_field(mut result, 'is_mut', if node.is_mut { create_true() } else { create_false() }, !node.is_mut)
	ctx.add_field(mut result, 'pos', position_node(node.pos), !node.pos.is_valid())
	if node.children_count > 0 {
		mut children := create_array()
		for child in a.children_of(node) {
			children.add_item(ctx.ast_node(a, child))
		}
		ctx.add_field(mut result, 'children', children, false)
	}
	return result
}

fn position_node(pos token.Pos) &Node {
	mut result := create_object()
	add_item_to_object(mut result, 'file_id', create_number(pos.id))
	add_item_to_object(mut result, 'offset', create_number(pos.offset))
	add_item_to_object(mut result, 'end', create_number(pos.end))
	return result
}

fn (ctx Context) add_field(mut node Node, key string, child &Node, is_default bool) {
	if key in ctx.hide_names || ctx.is_terse && key !in ['kind', 'files', 'children']
		|| ctx.is_skip_defaults && is_default {
		return
	}
	add_item_to_object(mut node, key, child)
}

fn (mut node Node) add_item(child &Node) {
	add_item_to_array(mut node, child)
}
