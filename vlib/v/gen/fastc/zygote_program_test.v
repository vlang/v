module fastc

import os
import v.cmdexec

// These programs are built without the unit cache. A compiler that holds TinyCC then
// compiles their units from one parse of the shared declarations (see FastcZygote);
// any other compiler gives each unit to a TinyCC process. Both must agree.
fn fastc_uncached_program(name string, files map[string]string, options []string) (os.Result, string) {
	root := os.join_path(os.vtmp_dir(), 'fastc_zygote_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'shapes')) or { panic(err) }
	for path, source in files {
		os.write_file(os.join_path(root, path), source) or { panic(err) }
	}
	program := os.join_path(root, 'program')
	mut args := ['-b', 'fastc', '-nocache', '-enable-globals']
	args << options
	args << ['-o', program, root]
	return cmdexec.run(@VEXE, args), program
}

fn fastc_remove_uncached_program(program string) {
	os.rmdir_all(os.dir(program)) or {}
}

const fastc_zygote_program_files = {
	'main.v':          'module main

import shapes

__global visits = 0

const scale = shapes.unit * 3

fn main() {
	mut total := 0
	for side in 1 .. 4 {
		square := shapes.Square{side * scale}
		total += shapes.area(square)
		visits++
	}
	println("\${total} \${visits} \${shapes.Kind.square}")
}
'
	'shapes/shapes.v': 'module shapes

pub const unit = 2

pub enum Kind {
	square
	circle
}

pub struct Square {
pub:
	side int
}

pub fn area(square Square) int {
	return square.side * square.side
}
'
}

fn test_fastc_uncached_program_links_the_definitions_of_all_units() {
	$if !macos && !linux {
		return
	}
	for options in [[]string{}, ['-g']] {
		build, program := fastc_uncached_program('links${options.len}', fastc_zygote_program_files,
			options)
		defer {
			fastc_remove_uncached_program(program)
		}
		assert build.exit_code == 0, build.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == '504 3 square'
	}
}

fn test_fastc_uncached_program_reports_tinycc_errors() {
	$if !macos && !linux {
		return
	}
	// The missing header fails in the declarations that all units share.
	head, head_program := fastc_uncached_program('head_error', {
		'main.v': 'module main

#include "fastc_zygote_missing_header.h"

fn main() {
	println(1)
}
'
	}, [])
	defer {
		fastc_remove_uncached_program(head_program)
	}
	assert head.exit_code != 0
	assert head.output.contains("include file 'fastc_zygote_missing_header.h' not found"), head.output
	assert !os.exists(head_program)
	// The macro fails where one unit's function uses it.
	body, body_program := fastc_uncached_program('body_error', {
		'main.v': 'module main

#define FASTC_ZYGOTE_BROKEN_VALUE (1 +)

fn broken() int {
	return C.FASTC_ZYGOTE_BROKEN_VALUE
}

fn main() {
	println(broken())
}
'
	}, [])
	defer {
		fastc_remove_uncached_program(body_program)
	}
	assert body.exit_code != 0
	assert body.output.contains("error: expression expected before ')'"), body.output
	assert !os.exists(body_program)
}
