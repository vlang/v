module cbuilder

import os
import time
import v.util
import v.builder
import sync.pool
import v.gen.c

const cc_compiler = os.getenv_opt('CC') or { 'cc' }
const cc_ldflags = os.getenv_opt('LDFLAGS') or { '' }
const cc_cflags = os.getenv_opt('CFLAGS') or { '' }
const cc_cflags_opt = os.getenv_opt('CFLAGS_OPT') or { '' } // '-O3' }

fn parallel_cc(mut b builder.Builder, result c.GenOutput) {
	tmp_dir := os.vtmp_dir()
	sw_total := time.new_stopwatch()
	defer {
		eprint_time(sw_total, @METHOD)
	}
	c_files := int_max(1, util.nr_jobs)
	eprintln('> c_files: ${c_files} | util.nr_jobs: ${util.nr_jobs}')

	// Write generated stuff in `g.out` before and after the `out_fn_start_pos` locations,
	// like the `int main()` to "out_0.c" and "out_x.c"

	// out.h
	os.write_file('${tmp_dir}/out.h', result.header) or { panic(err) }

	// out_0.c
	out0 := '//out0\n' + result.out_str[..result.out_fn_start_pos[0]]
	os.write_file('${tmp_dir}/out_0.c', '#include "out.h"\n' + out0 + '\n//X:\n' + result.out0_str) or {
		panic(err)
	}

	// out_x.c
	os.write_file('${tmp_dir}/out_x.c', '#include "out.h"\n\n' + result.extern_str + '\n' +
		result.out_str[result.out_fn_start_pos.last()..]) or { panic(err) }

	mut prev_fn_pos := 0
	mut out_files := []os.File{len: c_files}
	mut fnames := []string{}

	for i in 0 .. c_files {
		fname := '${tmp_dir}/out_${i + 1}.c'
		fnames << fname
		out_files[i] = os.create(fname) or { panic(err) }

		// Common .c file code
		out_files[i].writeln('#include "out.h"\n') or { panic(err) }
		out_files[i].writeln(result.extern_str) or { panic(err) }
	}

	for i, fn_pos in result.out_fn_start_pos {
		if prev_fn_pos >= result.out_str.len || fn_pos >= result.out_str.len || prev_fn_pos > fn_pos {
			eprintln('> EXITING i=${i} out of ${result.out_fn_start_pos.len} prev_pos=${prev_fn_pos} fn_pos=${fn_pos}')
			break
		}
		if i == 0 {
			// Skip typeof etc stuff that's been added to out_0.c
			prev_fn_pos = fn_pos
			continue
		}
		fn_text := result.out_str[prev_fn_pos..fn_pos]
		out_files[i % c_files].writeln(fn_text) or { panic(err) }
		prev_fn_pos = fn_pos
	}
	for i in 0 .. c_files {
		out_files[i].close()
	}

	mut cc_path := cc_compiler
	explicit_cc_flag_passed := b.pref.build_options.any(it.starts_with('-cc '))
	if explicit_cc_flag_passed {
		// do not guess, just use the user's preference
		cc_path = b.pref.ccompiler
	}
	cc := os.quoted_path(cc_path)
	mut compile_args := b.get_compile_args()
	mut linker_args := b.get_linker_args()
	if !explicit_cc_flag_passed {
		compile_args = compile_args.filter(it != '-bt25')
		linker_args = linker_args.filter(it != '-bt25')
	}
	scompile_args := compile_args.filter(it != '-flto').join(' ') // always remove link time optimization, it slows down linking 10x
	slinker_args := linker_args.join(' ')

	mut o_postfixes := ['0', 'x']
	mut cmds := []string{}
	for i in 0 .. c_files {
		o_postfixes << (i + 1).str()
	}
	for postfix in o_postfixes {
		cmds << '${cc} ${cc_cflags} ${cc_cflags_opt} ${scompile_args} -w -o ${tmp_dir}/out_${postfix}.o -c ${tmp_dir}/out_${postfix}.c'
	}
	sw := time.new_stopwatch()
	mut pp := pool.new_pool_processor(callback: build_parallel_o_cb)
	pp.set_max_jobs(util.nr_jobs)
	pp.work_on_items(cmds)
	eprint_time(sw, 'C compilation on ${util.nr_jobs} thread(s), processing ${cmds.len} commands')

	obj_files := fnames.map(it.replace('.c', '.o')).join(' ')
	link_cmd := '${cc} ${scompile_args} -o ${os.quoted_path(b.pref.out_name)} ${tmp_dir}/out_0.o ${obj_files} ${tmp_dir}/out_x.o ${slinker_args} ${cc_ldflags}'
	sw_link := time.new_stopwatch()
	link_res := os.execute(link_cmd)
	eprint_result_time(sw_link, 'link_cmd', link_cmd, link_res)
}

fn build_parallel_o_cb(mut p pool.PoolProcessor, idx int, _wid int) voidptr {
	cmd := p.get_item[string](idx)
	sw := time.new_stopwatch()
	res := os.execute(cmd)
	eprint_result_time(sw, 'cc_cmd', cmd, res)
	return unsafe { nil }
}

fn eprint_result_time(sw time.StopWatch, label string, cmd string, res os.Result) {
	eprint_time(sw, '${label}: `${cmd}` => ${res.exit_code}')
	if res.exit_code != 0 {
		eprintln(res.output)
	}
}

fn eprint_time(sw time.StopWatch, label string) {
	eprintln('> ${sw.elapsed().milliseconds():5} ms, ${label}')
}
