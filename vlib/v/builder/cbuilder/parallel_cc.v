module cbuilder

import os
import time
import v.util
import v.builder
import sync.pool

fn parallel_cc(mut b builder.Builder, header string, _res string, out_str string, out_fn_start_pos []int) {
	c_files := util.nr_jobs - 1
	println('> c_files: ${c_files} | util.nr_jobs: ${util.nr_jobs}')
	out_h := header.replace_once('static char * v_typeof_interface_IError', 'char * v_typeof_interface_IError')
	os.write_file('out_str.txt', out_str) or { panic(err) }
	os.write_file('out_res.txt', _res) or { panic(err) }
	// Write generated stuff in `g.out` before and after the `out_fn_start_pos` locations,
	// like the `int main()` to "out_0.c" and "out_x.c"
	x := _res.find_between('// ZULUL1', '// ZULUL2')
	os.write_file('out_x.txt', x) or { panic(err) }
	out0 := '//out0\n' +
		out_str[..out_fn_start_pos[0]].replace_once('static char * v_typeof_interface_IError', 'char * v_typeof_interface_IError')
	// out_str[..out0_start].replace_once('static char * v_typeof_interface_IError', 'char * v_typeof_interface_IError')
	// os.write_file('out.h', out_h + '\n//out0:\n' + out0) or { panic(err) }
	os.write_file('out.h', out_h) or { panic(err) }
	os.write_file('out_0.c', '#include "out.h"\n' + out0 + '\n//X:\n' + x) or { panic(err) }
	// os.write_file('out_0.c', out0) or { panic(err) }
	os.write_file('out_x.c', '#include "out.h"\n' + out_str[out_fn_start_pos.last()..]) or {
		panic(err)
	}

	mut prev_fn_pos := 0
	mut out_files := []os.File{len: c_files}
	mut fnames := []string{}
	for i in 0 .. c_files {
		fname := 'out_${i + 1}.c'
		fnames << fname
		out_files[i] = os.create(fname) or { panic(err) }
		out_files[i].writeln('#include "out.h"\n') or { panic(err) }
	}
	// out_fn_start_pos.sort()
	for i, fn_pos in out_fn_start_pos {
		if prev_fn_pos >= out_str.len || fn_pos >= out_str.len || prev_fn_pos > fn_pos {
			println('> EXITING i=${i} out of ${out_fn_start_pos.len} prev_pos=${prev_fn_pos} fn_pos=${fn_pos}')
			break
		}
		if i == 0 {
			// Skip typeof etc stuff that's been added to out_0.c
			prev_fn_pos = fn_pos
			continue
		}
		fn_text := out_str[prev_fn_pos..fn_pos]
		out_files[i % c_files].writeln(fn_text + '\n///////////////////////////\n\n') or {
			panic(err)
		}
		prev_fn_pos = fn_pos
	}
	for i in 0 .. c_files {
		out_files[i].close()
	}

	sw := time.new_stopwatch()
	mut o_postfixes := ['0', 'x']
	for i in 0 .. c_files {
		o_postfixes << (i + 1).str()
	}
	mut pp := pool.new_pool_processor(callback: build_parallel_o_cb)
	nr_threads := c_files + 2
	pp.set_max_jobs(nr_threads)
	pp.work_on_items(o_postfixes)
	eprintln('> C compilation on ${nr_threads} threads, working on ${o_postfixes.len} files took: ${sw.elapsed().milliseconds()} ms')
	// cc := os.quoted_path(cc_compiler)
	link_cmd := '${cc} -o ${os.quoted_path(b.pref.out_name)} out_0.o ${fnames.map(it.replace('.c',
		'.o')).join(' ')} out_x.o -lpthread ${cc_ldflags}'
	sw_link := time.new_stopwatch()
	link_res := os.execute(link_cmd)
	eprint_time('link_cmd', link_cmd, link_res, sw_link)
}

fn build_parallel_o_cb(mut p pool.PoolProcessor, idx int, _wid int) voidptr {
	postfix := p.get_item[string](idx)
	sw := time.new_stopwatch()
	// cc := os.quoted_path(cc_compiler)
	cmd := '${cc} ${cc_cflags} -O3 -c -w -o out_${postfix}.o out_${postfix}.c'
	res := os.execute(cmd)
	eprint_time('c cmd2', cmd, res, sw)
	return unsafe { nil }
}

fn eprint_time(label string, cmd string, res os.Result, sw time.StopWatch) {
	eprintln('> ${label}: `${cmd}` => ${res.exit_code} , ${sw.elapsed().milliseconds()} ms')
	if res.exit_code != 0 {
		eprintln(res.output)
	}
}

// const cc = '/Users/work5/code/v/thirdparty/tcc/tcc.exe'
const cc = os.quoted_path(cc_compiler)
const cc_compiler = os.getenv_opt('CC') or { 'cc' }

const cc_ldflags = os.getenv_opt('LDFLAGS') or { '' }

const cc_cflags = os.getenv_opt('CFLAGS') or { '' }
