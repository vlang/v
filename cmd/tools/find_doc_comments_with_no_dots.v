import os

fn main() {
	args := arguments()
	if args.len < 2 {
		println('Usage: v run doc_comment_checker.v <file1.v> <file2.v>...')
		return
	}
	mut fpaths := []string{}
	for filepath in args[1..] {
		if os.is_file(filepath) {
			fpaths << filepath
		}
		if os.is_dir(filepath) {
			fpaths << os.walk_ext(filepath, '.v').filter(!it.ends_with('_test.v')
				&& !it.ends_with('_test.c.v'))
		}
	}
	fpaths.sort()
	mut ctx := Context{}
	for filepath in fpaths {
		ctx.process_fpath(filepath) or {
			eprintln('error with ${filepath}: ${err}')
			continue
		}
	}
	println('> Processed ${fpaths.len} .v files, found errors: ${ctx.errors} , in ${ctx.pub_symbols} `pub` declarations, and ${ctx.pub_comment_lines} pub comment lines.')
	if ctx.errors > 0 {
		exit(1)
	}
}

struct Context {
mut:
	errors            int
	pub_symbols       int
	pub_comment_lines int
	comments          int
}

fn (mut ctx Context) process_fpath(filepath string) ! {
	lines := os.read_lines(filepath)!
	mut prev := 0
	for iline, line in lines {
		if line.starts_with('pub ') {
			ctx.pub_symbols++
			mut comments := []CommentLine{}
			mut i := 0
			for i = int_max(0, iline - 1); i >= prev; i-- {
				pline := lines[i]
				if pline.starts_with('// ') {
					comments << CommentLine{pline, i + 1}
				} else {
					prev = iline + 1
					break
				}
			}
			if comments.len > 0 {
				cline := comments.last()
				fword := cline.comment.all_after('// ').all_before(' ')
				if !line.contains(fword) {
					continue
				}
				ctx.pub_comment_lines += comments.len
				if !cline.comment.ends_with('.') {
					println('${filepath}:${cline.line}: ${cline.comment}')
					ctx.errors++
				}
			}
		}
	}
}

struct CommentLine {
	comment string
	line    int
}
