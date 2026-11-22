module fastc

import os
import v.cmdexec

// A method call on an indexed receiver inside arithmetic (`y - edges[i].edge.top()`) is
// lowered before its index, whose rewrite would otherwise hide the raw call.
fn test_fastc_method_on_indexed_receiver_in_arithmetic() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_method_on_indexed_receiver_in_arithmetic_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

import math

struct Edge {
mut:
	y0      f64
	y1      f64
	x_start f64
	x_inc   f64
}

fn (edge &Edge) top() f64 {
	return math.min(edge.y0, edge.y1)
}

struct WorkingEdge {
mut:
	edge Edge
	x    int
}

fn sort_filter(mut edges []WorkingEdge, y int) {
	y_mid := f64(y) + 0.5
	for i in 0 .. edges.len {
		edges[i].x = int(math.round(edges[i].edge.x_start + edges[i].edge.x_inc * (y_mid - edges[i].edge.top())))
	}
}

fn main() {
	mut edges := [WorkingEdge{
		edge: Edge{
			y0:      1
			y1:      3
			x_start: 2
			x_inc:   0.5
		}
	}]
	sort_filter(mut edges, 2)
	println(edges[0].x)
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-nocache', '-gc', 'none', '-o',
		program, root], os.real_path(os.join_path(@DIR, '../../../..')), {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'VOSARGS':                ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '3', run.output
}
