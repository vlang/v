module main

import os
import v.cmdexec

// These programs build with `-b arm64` and must print what the C backend
// prints. Each one is a project of several modules, because the SSA builder's
// name and layout lookups go wrong when modules share type, enum or function
// names, or size arrays with their own constants.

// run_arm64 writes `files` into a new project, builds its `main.v` with
// `-b arm64` and `args`, and returns the program's output.
fn run_arm64(name string, files map[string]string, args []string) string {
	root := os.join_path(os.vtmp_dir(), 'arm64_ssa_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: '${name}' }") or { panic(err) }
	for path, source in files {
		full := os.join_path(root, path)
		os.mkdir_all(os.dir(full)) or { panic(err) }
		os.write_file(full, source) or { panic(err) }
	}
	output := os.join_path(root, 'main')
	mut build := ['-b', 'arm64']
	build << args
	build << ['-o', output, os.join_path(root, 'main.v')]
	compiled := cmdexec.run(@VEXE, build)
	assert compiled.exit_code == 0, compiled.output
	run := cmdexec.run(output, []string{})
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

// gg's `App` must not take over the layout of the program's own `App`.
fn test_module_struct_keeps_program_struct_name() {
	$if macos && arm64 {
		output := run_arm64('struct_names', {
			'widgets/widgets.v': "module widgets

pub struct App {
pub mut:
	backend int = 7
	queue   [8]u64
	title   string = 'widgets'
}

pub fn new_app() App {
	return App{}
}
"
			'main.v':            "module main

import widgets

struct Ctx {
mut:
	width int
}

struct App {
mut:
	ctx   &Ctx = unsafe { nil }
	image int
}

fn main() {
	mut app := &App{}
	app.ctx = &Ctx{
		width: 600
	}
	app.image = 3
	app.ctx.width += app.image
	println('\${app.ctx.width} \${app.image} \${widgets.new_app().title}')
}
"
		}, []string{})
		assert output == '603 3 widgets', output
	}
}

// `views.TextStyle` is a struct even though `styles.TextStyle` is an enum; taking
// it for one gave it the `i32` layout and then overwrote `i32` with the struct.
fn test_qualified_struct_is_not_another_modules_enum() {
	$if macos && arm64 {
		output := run_arm64('enum_names', {
			'styles/styles.v': 'module styles

pub enum TextStyle {
	plain
	bold
}
'
			'views/views.v':   "module views

import styles

pub struct TextStyle {
pub:
	size  f64 = 15.0
	name  string
	lines int = 1
}

fn styled(base TextStyle, bold bool) TextStyle {
	if bold {
		return TextStyle{
			...base
			lines: base.lines + 1
		}
	}
	return base
}

pub fn heading(size f64) TextStyle {
	return styled(TextStyle{
		size: size
		name: 'heading'
	}, true)
}

pub fn plain() styles.TextStyle {
	return .bold
}
"
			'main.v':          "module main

import views

fn scaled(v i32, by i32) i32 {
	return v * by
}

fn main() {
	style := views.heading(24.0)
	println('\${style.name} \${style.size} \${style.lines} \${int(views.plain())} \${scaled(i32(7), i32(6))}')
}
"
		}, []string{})
		assert output == 'heading 24.0 2 1 42', output
	}
}

// `sappish.Desc{}` builds `C.big_desc`, not `render.Desc`, which shares its short name.
fn test_alias_literal_uses_alias_target() {
	$if macos && arm64 {
		output := run_arm64('alias_literals', {
			'desc.h':            '#pragma once
typedef struct item_desc { int a; int b; } item_desc;
typedef struct big_desc { int start; item_desc items[4][3]; int end; } big_desc;
static inline int big_end(const big_desc* d) { return d->end + d->items[3][2].b; }
static inline int big_size(void) { return (int)sizeof(big_desc); }
'
			'sappish/sappish.v': 'module sappish

#include "@VMODROOT/desc.h"

pub struct C.item_desc {
pub mut:
	a int
	b int
}

pub struct C.big_desc {
pub mut:
	start int
	items [4][3]C.item_desc
	end   int
}

pub type Desc = C.big_desc

fn C.big_end(d &C.big_desc) int

pub fn check(d &Desc) int {
	return C.big_end(d)
}
'
			'render/render.v':   'module render

pub struct Desc {
pub mut:
	max_vertices int
	sample_count int
}

pub fn samples(d Desc) int {
	return d.sample_count
}
'
			'main.v':            "module main

import render
import sappish

fn make(end int) sappish.Desc {
	return sappish.Desc{
		start: 1
		end:   end
	}
}

fn main() {
	d := make(12)
	r := render.Desc{
		sample_count: 4
	}
	println('\${d.start} \${d.end} \${sappish.check(&d)} \${render.samples(r)}')
}
"
		}, ['-d', 'arm64_external_link'])
		assert output == '1 12 12 4', output
	}
}

// Inside `outer`, `Context` is `outer.Context`, not the alias `fonts.Context`.
fn test_module_struct_wins_over_bare_alias() {
	$if macos && arm64 {
		output := run_arm64('alias_names', {
			'fonts/fonts.v': 'module fonts

pub struct C.FONScontext {}

pub type Context = C.FONScontext
'
			'outer/outer.v': 'module outer

import fonts

@[heap]
pub struct Context {
pub mut:
	flag  bool = true
	fons  &fonts.Context = unsafe { nil }
	width int
	count int
}

pub fn new_context(width int) &Context {
	return &Context{
		width: width
		count: 3
	}
}

pub fn read_back(user_data voidptr) int {
	ctx := unsafe { &Context(user_data) }
	return ctx.width * 10 + ctx.count
}
'
			'main.v':        'module main

import outer

fn main() {
	ctx := outer.new_context(60)
	println(outer.read_back(voidptr(ctx)))
}
'
		}, []string{})
		assert output == '603', output
	}
}

// `frame` names the program's function, not the `.frame` member of an enum
// in another module.
fn test_function_wins_over_enum_member() {
	$if macos && arm64 {
		output := run_arm64('fn_names', {
			'phases/phases.v': 'module phases

enum Phase {
	init
	frame
	cleanup
}

pub fn count() int {
	return int(Phase.cleanup) + 1
}
'
			'main.v':          "module main

import phases

struct Config {
	frame_fn fn (int) int = unsafe { nil }
}

fn frame(n int) int {
	return n * 2
}

fn main() {
	cfg := Config{
		frame_fn: frame
	}
	println('\${cfg.frame_fn(21)} \${phases.count()}')
}
"
		}, []string{})
		assert output == '42 3', output
	}
}

// `[rows][cols]C.item_desc` in a C struct uses `cdesc`'s constants, so the
// struct has the size C gives it.
fn test_c_struct_arrays_sized_by_module_constants() {
	$if macos && arm64 {
		output := run_arm64('c_struct_arrays', {
			'desc.h':        '#pragma once
typedef struct item_desc { int a; int b; } item_desc;
typedef struct big_desc { int start; item_desc items[4][3]; int end; } big_desc;
static inline int big_end(const big_desc* d) { return d->end + d->items[3][2].b; }
static inline int big_size(void) { return (int)sizeof(big_desc); }
'
			'cdesc/cdesc.v': 'module cdesc

#include "@VMODROOT/desc.h"

pub const rows = 4
pub const cols = 3

pub struct C.item_desc {
pub mut:
	a int
	b int
}

pub struct C.big_desc {
pub mut:
	start int
	items [rows][cols]C.item_desc
	end   int
}

pub type BigDesc = C.big_desc

fn C.big_end(d &C.big_desc) int
fn C.big_size() int

pub fn check(d &BigDesc) int {
	return C.big_end(d)
}

pub fn c_size() int {
	return C.big_size()
}
'
			'main.v':        "module main

import cdesc

fn main() {
	mut d := cdesc.BigDesc{}
	d.end = 9
	println('\${sizeof(cdesc.BigDesc)} \${cdesc.c_size()} \${cdesc.check(&d)}')
}
"
		}, ['-d', 'arm64_external_link'])
		assert output == '104 104 9', output
	}
}

// Indexing `[rows][cols]Item` from `main`, where `rows` and `cols` mean nothing,
// still steps over whole rows. Through an alias of a C struct too.
fn test_fixed_array_index_strides_from_other_module() {
	$if macos && arm64 {
		output := run_arm64('fixed_array_index', {
			'grid/grid.v': 'module grid

pub const rows = 4
pub const cols = 3

pub struct Item {
pub mut:
	a i32
	b i32
}

pub struct Big {
pub mut:
	start i32
	items [rows][cols]Item
	end   i32
}
'
			'main.v':      'module main

import grid

fn main() {
	mut d := grid.Big{}
	d.items[3][2].b = 30
	base := unsafe { &u8(&d) }
	mut set := []int{}
	for o := 0; o < int(sizeof(grid.Big)); o += 4 {
		if unsafe { *(&i32(base + o)) } != 0 {
			set << o
		}
	}
	println(set)
}
'
		}, []string{})
		assert output == '[96]', output
	}
}

fn test_c_struct_array_index_through_alias() {
	$if macos && arm64 {
		output := run_arm64('c_struct_index', {
			'desc.h':        '#pragma once
typedef struct item_desc { int a; int b; } item_desc;
typedef struct big_desc { int start; item_desc items[4][3]; int end; } big_desc;
static inline int big_end(const big_desc* d) { return d->end + d->items[3][2].b; }
static inline int big_size(void) { return (int)sizeof(big_desc); }
'
			'cdesc/cdesc.v': 'module cdesc

#include "@VMODROOT/desc.h"

pub const rows = 4
pub const cols = 3

pub struct C.item_desc {
pub mut:
	a int
	b int
}

pub struct C.big_desc {
pub mut:
	start int
	items [rows][cols]C.item_desc
	end   int
}

pub type BigDesc = C.big_desc

fn C.big_end(d &C.big_desc) int
fn C.big_size() int

pub fn check(d &BigDesc) int {
	return C.big_end(d)
}

pub fn c_size() int {
	return C.big_size()
}
'
			'main.v':        "module main

import cdesc

fn main() {
	mut d := cdesc.BigDesc{}
	d.end = 9
	d.items[3][2].b = 30
	println('\${sizeof(cdesc.BigDesc)} \${cdesc.c_size()} \${cdesc.check(&d)}')
}
"
		}, ['-d', 'arm64_external_link'])
		assert output == '104 104 39', output
	}
}
