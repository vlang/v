module main

import flag
import os
import time
import v.astjson

struct Context {
mut:
	is_watch   bool
	is_compile bool
	is_print   bool
	opts       astjson.Options
	// hide_names keeps the raw `--hide` names so a field can also be listed for
	// its own sake; `opts.hidden` is what the renderer consults.
	hide_names map[string]bool
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
	terse := fp.bool('terse', `t`, false, 'show only AST node names and structure')
	skip_defaults := fp.bool('skip-defaults', `s`, false, 'skip properties that have default values')
	check := fp.bool('check', `k`, false, 'type check the input before dumping its AST')
	hfields := fp.string_multi('hide', 0, 'hide fields; specify several by separating them with commas').join(',')
	for field in hfields.split(',') {
		if field != '' {
			ctx.hide_names[field] = true
		}
	}
	ctx.opts = astjson.Options{
		terse:         terse
		skip_defaults: skip_defaults
		hidden:        ctx.hide_names.keys()
	}
	fp.limit_free_args_to_at_least(1)!
	for vfile in fp.remaining_parameters() {
		file := absolute_path(vfile)
		check_file(file)
		ctx.write_file_or_print(file, check)
		if ctx.is_watch || ctx.is_compile {
			ctx.watch_for_changes(file)
		}
	}
}

fn (ctx Context) write_file_or_print(file string, check bool) {
	if check {
		compiler := os.getenv_opt('VEXE') or { 'v' }
		result := os.exec([compiler, '-check', file])
		if result.exit_code != 0 {
			eprint(result.output)
			exit(result.exit_code)
		}
	}
	a := astjson.parse(file)
	ast_json := astjson.dump(a, ctx.opts)
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
			ctx.write_file_or_print(file, false)
			if ctx.is_compile {
				compiler := os.getenv_opt('VEXE') or { 'v' }
				file_name := file[..file.len - os.file_ext(file).len]
				os.system_args([compiler, '-o', '${file_name + '.c'}', file])
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
