import os
import term
import time

struct Context {
mut:
	cut_time     time.Time
	deprecations int
}

fn (mut ctx Context) analyze_line(line string, position_file string, position_line int) {
	blame_for_time := os.execute('git blame -L${position_line} --porcelain -- ${position_file}')
	if blame_for_time.exit_code != 0 {
		return
	}
	ts := blame_for_time.output.all_after('committer-time').all_before('\n').trim_space().int()
	t := time.unix(ts)
	if ctx.cut_time < t {
		println(term.colorize(term.gray, '>>> SKIPPING since t: ${t} > ${ctx.cut_time}, ${position_file}:${position_line}: ${line}'))
		return
	}
	ctx.deprecations++
	blame_for_context := os.execute('git blame -L${position_line},+5 -- ${position_file}')
	context := blame_for_context.output.trim_space().split_into_lines()
	println(term.colorize(term.red, '${position_file}:${position_line}: deprecation: ${ctx.deprecations}, timestamp: ${ts} - ${t}'))
	for cline in context {
		println('    ${cline}')
	}
}

fn main() {
	if os.args.len < 2 {
		eprintln('Usage: v run cmd/tools/show_ancient_deprecations.v [DAYS]')
		exit(1)
	}
	cut_months := os.args[1].int()
	cut_time := time.now().add(-cut_months * 24 * time.hour)
	mut ctx := Context{
		cut_time: cut_time
	}
	println('> Deprecations that happened before ${cut_time}')
	all_v_files := os.walk_ext('.', '.v')
	for v_file in all_v_files {
		if v_file == './vlib/v/fmt/tests/attrs_keep.vv' {
			println(term.colorize(term.gray, '>>> SKIPPING deprecations attrs formatting test file ${v_file}'))
			continue
		}
		if v_file.starts_with('./vlib/v/checker/tests') && v_file.contains('deprec') {
			println(term.colorize(term.gray, '>>> SKIPPING deprecations test file ${v_file}'))
			continue
		}
		file_content := os.read_file(v_file)!
		if !file_content.contains('[deprecated') {
			continue
		}
		lines := file_content.split_into_lines()
		for line_num := lines.len - 1; line_num > 0; line_num-- {
			line := lines[line_num].all_before('//')
			mut is_deprecation_line := false
			if line.contains('\tif ') {
				continue
			}
			if line.contains('[deprecated:') {
				is_deprecation_line = true
			}
			if line.contains('[deprecated]') {
				is_deprecation_line = true
			}
			if !is_deprecation_line {
				continue
			}
			ctx.analyze_line(line, v_file, line_num + 1)
		}
	}
	println('> Summary: there were ${term.colorize(term.bright_yellow, ctx.deprecations.str())} deprecations found, done before ${term.colorize(term.magenta,
		cut_time.str())}.')
	if ctx.deprecations > 0 {
		exit(1)
	}
}
