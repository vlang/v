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

// The backend rejects `**` only in reachable code, whatever a quick look at all the
// nodes of the program finds.
fn test_unreachable_power_operator_does_not_stop_arm64() {
	$if macos && arm64 {
		output := run_arm64('unused_power', {
			'main.v': 'fn unused(a int) int {
	return 2 ** a
}

fn main() {
	println(7)
}
'
		}, [])
		assert output == '7'
	}
}

// A plain arm64 build checks only the library bodies that the program reaches.
fn test_arm64_leaves_unreached_library_bodies_unchecked() {
	$if macos && arm64 {
		root := os.join_path(os.vtmp_dir(), 'arm64_ssa_lazy_bodies_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'main.v')
		os.write_file(source, "import os

fn main() {
	println(os.file_name('a/b.txt'))
}
") or {
			panic(err)
		}
		output := os.join_path(root, 'main')
		compiled := cmdexec.run(@VEXE, ['-b', 'arm64', '-nocache', '-v', '-o', output, source])
		assert compiled.exit_code == 0, compiled.output
		assert compiled.output.contains('left unchecked'), compiled.output
		assert compiled.output.contains(' 0 left unchecked') == false, compiled.output
		ran := cmdexec.run(output, [])
		assert ran.exit_code == 0, ran.output
		assert ran.output.trim_space() == 'b.txt'
	}
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
		}, []string{})
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
		}, []string{})
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
		}, []string{})
		assert output == '104 104 39', output
	}
}

// Storing a 12-byte struct, or zeroing it, writes 12 bytes: the next array
// element keeps its first field.
fn test_aggregate_tail_store_stays_in_bounds() {
	$if macos && arm64 {
		output := run_arm64('aggregate_tail', {
			'main.v': "module main

struct Tail {
mut:
	a u32
	b u32
	c u32
}

struct Outer {
mut:
	head [26]u32
	tail Tail
}

fn main() {
	mut items := [Outer{}, Outer{}]
	items[1].head[0] = 77
	items[0].tail = Tail{1, 2, 3}
	println('\${items[1].head[0]} \${items[0].tail.c}')
	items[1].head[0] = 78
	items[0].tail = Tail{}
	println('\${items[1].head[0]} \${items[0].tail.c}')
}
"
		}, []string{})
		assert output == '77 3
78 0', output
	}
}

// `tex: 0` in a C struct literal stores a null pointer, not a pointer to a 0.
fn test_integer_in_pointer_field_is_an_address() {
	$if macos && arm64 {
		output := run_arm64('int_pointer', {
			'handle.h': '#pragma once
typedef struct handle_desc { int id; void* tex; } handle_desc;
static inline int handle_has_tex(const handle_desc* d) { return d->tex != 0; }
'
			'main.v':   'module main

#include "@VMODROOT/handle.h"

pub struct C.handle_desc {
pub mut:
	id  int
	tex voidptr
}

fn C.handle_has_tex(d &C.handle_desc) int

fn main() {
	d := C.handle_desc{
		id:  1
		tex: 0
	}
	println(\'\${d.id} \${d.tex == unsafe { nil }} \${C.handle_has_tex(&d)}\')
}
'
		}, []string{})
		assert output == '1 true 0', output
	}
}

// `[]thread T.wait()` joins every thread and returns their results in order.
fn test_thread_array_wait() {
	$if macos && arm64 {
		output := run_arm64('thread_array_wait', {
			'main.v': "fn sq(n int) int {
	return n * n
}

fn name(n int) string {
	return 'w\${n}'
}

fn work(n int) {
	_ = n * 2
}

fn main() {
	mut ints := []thread int{}
	for i in 1 .. 5 {
		ints << spawn sq(i)
	}
	println(ints.wait())
	strs := [spawn name(1), spawn name(2), spawn name(3)]
	println(strs.wait())
	voids := [spawn work(1), spawn work(2)]
	voids.wait()
	empty := []thread int{}
	println(empty.wait().len)
	println('done')
}
"
		}, []string{})
		assert output == "[1, 4, 9, 16]
['w1', 'w2', 'w3']
0
done", output
	}
}

// A sum type field starts as its first variant's zero value, and `= ''` or an
// assignment of a variant value stores it with that variant's tag.
fn test_sum_type_zero_value_and_variant_stores() {
	$if macos && arm64 {
		output := run_arm64('sum_values', {
			'ui/ui.v': 'module ui

pub type DisplayText = string | int | f64

pub fn (text DisplayText) kind() string {
	return if text is string {
		\'string "\${text}"\'
	} else if text is int {
		\'int \${text}\'
	} else if text is f64 {
		\'f64 \${text}\'
	} else {
		\'no variant\'
	}
}

@[params]
pub struct ButtonProps {
pub mut:
	id    string
	text  DisplayText = \'\'
	other DisplayText
}
'
			'main.v':  "module main

import ui

fn main() {
	p := ui.ButtonProps{
		id: 'a'
	}
	mut q := ui.ButtonProps{
		text: 42
	}
	q.other = 2.5
	println('\${p.text.kind()} | \${p.other.kind()} | \${q.text.kind()} | \${q.other.kind()}')
}
"
		}, []string{})
		assert output == 'string "" | string "" | int 42 | f64 2.5', output
	}
}

// `controller.build(...)` calls the function in the `build` field of a generic
// instance, not some type's `build` method.
fn test_function_field_call_on_generic_instance() {
	$if macos && arm64 {
		output := run_arm64('generic_fn_field', {
			'lib/lib.v': "module lib

pub struct Props {
pub:
	variant int = 2
}

pub fn (p Props) build(n int) string {
	return 'Props.build \${p.variant} \${n}'
}

pub struct Config[T] {
pub:
	build fn (mut T) string = unsafe { nil }
	model T
}

struct Controller[T] {
mut:
	build fn (mut T) string = unsafe { nil }
	model T
}

pub fn start[T](config Config[T]) string {
	mut controller := &Controller[T]{
		build: config.build
		model: config.model
	}
	return controller.build(mut controller.model)
}
"
			'main.v':    "module main

import lib

struct Calc {
mut:
	n int
}

fn build_calc(mut c Calc) string {
	c.n++
	return 'build_calc \${c.n}'
}

fn main() {
	println(lib.start(lib.Config[Calc]{
		build: build_calc
		model: Calc{
			n: 41
		}
	}))
	println(lib.Props{}.build(1))
}
"
		}, []string{})
		assert output == 'build_calc 42
Props.build 2 1', output
	}
}

// `node.id` on a `Node[P]` that embeds `Core` writes and reads `Core.id`.
fn test_embedded_fields_of_generic_instance() {
	$if macos && arm64 {
		output := run_arm64('generic_embed', {
			'lib/lib.v': "module lib

pub struct Comp {
pub mut:
	name string
}

struct ChildList {
mut:
	children []voidptr
}

struct Core {
	ChildList
mut:
	owner &Comp = unsafe { nil }
	id    string
}

pub struct Node[P] {
	Core
pub mut:
	props P
}

pub fn (n &Node[P]) owner_name() string {
	return n.owner.name
}

pub fn new_node[P](mut owner Comp, props P, id string) &Node[P] {
	mut node := &Node[P]{
		props: props
	}
	node.owner = &owner
	node.id = id
	return node
}

pub fn describe[P](n &Node[P]) string {
	return '\${n.id} \${n.owner_name()} \${n.props}'
}
"
			'main.v':    "module main

import lib

fn main() {
	mut comp := lib.Comp{
		name: 'root'
	}
	a := lib.new_node(mut comp, 7, 'seven')
	b := lib.new_node(mut comp, 'text', 'label')
	println('\${lib.describe(a)} | \${lib.describe(b)}')
}
"
		}, []string{})
		assert output == 'seven root 7 | label root text', output
	}
}

// `fn C.x() Rect` in module `geo` names `geo.Rect`, an alias of a C struct.
// Resolved outside its module, the result came out as an integer.
fn test_c_function_returns_alias_of_its_module() {
	$if macos && arm64 {
		output := run_arm64('c_fn_alias', {
			'geo/geo.h': '#pragma once
typedef struct geo_rect { double x; double y; double w; double h; } geo_rect;
static inline geo_rect geo_make(double s) { geo_rect r = { s, s * 2, s * 3, s * 4 }; return r; }
'
			'geo/geo.v': 'module geo

#include "@VMODROOT/geo/geo.h"

pub type Rect = C.geo_rect

pub struct C.geo_rect {
pub mut:
	x f64
	y f64
	w f64
	h f64
}

fn C.geo_make(s f64) Rect

pub fn make(s f64) Rect {
	return C.geo_make(s)
}
'
			'main.v':    "module main

import geo

fn main() {
	r := geo.make(1.5)
	println('\${r.x} \${r.y} \${r.w} \${r.h}')
}
"
		}, []string{})
		assert output == '1.5 3.0 4.5 6.0', output
	}
}

// `is_empty` is lowered with the other methods of `@[flag]` enums, for a local
// value, a field of an imported struct and a function result.
fn test_flag_enum_is_empty() {
	$if macos && arm64 {
		output := run_arm64('flag_is_empty', {
			'jobs/jobs.v': 'module jobs

@[flag]
pub enum Reason {
	paint
	layout
}

pub struct Queue {
pub mut:
	pending Reason
}

pub fn (mut q Queue) add(reason Reason) bool {
	was_empty := q.pending.is_empty()
	q.pending.set(reason)
	return was_empty && !q.pending.is_empty()
}
'
			'main.v':      "module main

import jobs

@[flag]
enum Local {
	a
	b
}

@[flag]
enum Small as u8 {
	x
	y
}

fn local(value Local) Local {
	return value
}

fn main() {
	mut flags := Local.zero()
	empty := flags.is_empty()
	flags.set(.b)
	mut small := Small.zero()
	small.set_all()
	mut queue := jobs.Queue{}
	println('\${small.all(.x | .y)} \${empty} \${flags.is_empty()} \${local(Local.zero()).is_empty()} \${queue.add(.layout)} \${queue.pending.is_empty()}')
}
"
		}, []string{})
		assert output == 'true true false true true false', output
	}
}

// `return match` over a value no branch names, such as an out-of-range cast,
// stops the program. It used to run into the next block and spin there.
fn test_return_match_without_a_matching_branch_stops() {
	$if macos && arm64 {
		root := os.join_path(os.vtmp_dir(), 'arm64_ssa_no_branch_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'main.v')
		os.write_file(source, "enum Color {
	red
	green
}

fn name(c Color) string {
	return match c {
		.red { 'red' }
		.green { 'green' }
	}
}

fn main() {
	println(name(.green))
	println(name(unsafe { Color(99) }))
}
") or { panic(err) }
		output := os.join_path(root, 'main')
		compiled := cmdexec.run(@VEXE, ['-w', '-b', 'arm64', '-o', output, source])
		assert compiled.exit_code == 0, compiled.output
		run := cmdexec.run_with_timeout(output, []string{}, 20000)
		assert run.exit_code == 1, run.output
		assert run.output.contains('green'), run.output
		assert run.output.contains('fn name: no branch for the value'), run.output
	}
}
