module main

import os
import v.cmdexec

// Programs that the V compiler itself needs to build and run with `-b arm64`.
// Each one must print what the C backend prints.

// run_arm64 builds `source` as a single-file program with `-b arm64` and returns
// what it prints.
fn run_arm64(name string, source string) string {
	root := os.join_path(os.vtmp_dir(), 'arm64_self_host_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	path := os.join_path(root, 'main.v')
	os.write_file(path, source) or { panic(err) }
	output := os.join_path(root, 'main')
	compiled := cmdexec.run(@VEXE, ['-b', 'arm64', '-o', output, path])
	assert compiled.exit_code == 0, compiled.output
	run := cmdexec.run(output, []string{})
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

// u128/i128 multiply, divide, remainder, shifts by a run-time count, negation,
// complement, increments, `match`/`in` and conversions to and from scalars.
fn test_128_bit_arithmetic_and_conversions() {
	$if macos && arm64 {
		output := run_arm64('wide_integers', "module main

fn mix(a u128, b u128, s int) []string {
	return [
		(a * b).str(),
		(a / b).str(),
		(a % b).str(),
		(a << s).str(),
		(a >> s).str(),
	]
}

fn smix(a i128, b i128, s int) []string {
	return [
		(a * b).str(),
		(a / b).str(),
		(a % b).str(),
		(a >> s).str(),
	]
}

fn main() {
	big := (u128(0xFFFF_FFFF_FFFF_FFFF) << 32) + u128(12345)
	println(mix(big, u128(1_000_000_007), 7))
	println(mix(u128(10), u128(3), 100))
	println(smix(i128(-1) - (i128(1) << 70), i128(-97), 5))
	println(smix(i128(123456789), i128(1000), 3))
}

fn conv(a u128, s i128, n int, f f64) []string {
	return [
		u64(a).str(),
		u32(a).str(),
		i64(s).str(),
		u128(n).str(),
		i128(n).str(),
		i128(-n).str(),
		f64(a).str(),
		f64(s).str(),
		u128(f).str(),
		i128(-f).str(),
	]
}

fn init() {
	println(conv((u128(7) << 64) + u128(0x1_0000_0005), i128(-12345678901234), 42, 1.0e20))
}

fn ops(a i128, u u128) []string {
	mut x := a
	x++
	mut y := u
	y--
	kind := match u {
		u128(0) { 'zero' }
		u128(5) { 'five' }
		else { 'other' }
	}
	return [(-a).str(), (~u).str(), x.str(), y.str(), kind, (a in i128(-10)..i128(10)).str()]
}

fn init() {
	println(ops(i128(-7), u128(5)))
	println(ops((i128(1) << 80), u128(0)))
}
")
		assert output == "['4294967301', '5', '-12345678901234', '42', '42', '-42', '1.2912720852026183e+20', '-1.2345678901234e+13', '100000000000000000000', '-100000000000000000000']
['79228163068861475189099358509742967695', '79228161959667203871', '578568288', '10141204801825835211423871409280', '618970019642690137416007776']
['30', '3', '1', '12676506002282294014967032053760', '0']
['114517387209588896432225', '12171047636261972200', '-25', '-36893488147419103233']
['123456789000', '123456', '789', '15432098']", output
	}
}

// `C.FIONREAD` and `C.KERN_SUCCESS` come from headers the C backend always
// includes, without an `#include` in the program.
fn test_c_constants_of_the_c_backend_headers() {
	$if macos && arm64 {
		output := run_arm64('c_constants', "module main

// No `#include`: the C backend's generated C always includes these headers.
fn main() {
	println('\${int(C.FIONREAD) > 0} \${int(C.KERN_SUCCESS)} \${int(C.VM_FLAGS_ANYWHERE)}')
}
")
		assert output == 'true 0 1', output
	}
}

// `x in names` over a constant array of strings compares every element.
fn test_in_a_constant_string_array() {
	$if macos && arm64 {
		output := run_arm64('const_in', "const external_commands = [
	'ast',
	'bin2v',
	'bug',
	'bug-report',
	'bug-report-send',
	'build-examples',
	'build-tools',
	'build-vbinaries',
	'bump',
	'check-md',
	'clean',
	'complete',
	'compress',
	'cover',
	'diff',
	'doc',
	'doctor',
	'download',
	'env',
	'fmt',
	'generate',
	'git-fmt-hook',
	'gret',
	'init',
	'install',
	'link',
	'list',
	'ls',
	'mcp',
	'missdoc',
	'mod',
	'new',
	'outdated',
	'pbgen',
	'quest',
	'reduce',
	'remove',
	'repl',
	'repeat',
	'retry',
	'search',
	'self',
	'setup-freetype',
	'shader',
	'share',
	'skills',
	'should-compile-all',
	'show',
	'sqlite',
	'symlink',
	'scan',
	'test',
	'test-all',
	'test-cleancode',
	'test-fmt',
	'test-parser',
	'test-self',
	'time',
	'timeout',
	'tool',
	'tracev',
	'translate',
	'unlink',
	'up',
	'update',
	'upgrade',
	'vendor',
	'vet',
	'vlib-docs',
	'watch',
	'where',
	'why',
	'wipe-cache',
]

fn check(s string) bool {
	return s in external_commands
}

fn main() {
	println('\${check('')} \${check('ast')} \${check('as')} \${check('zzz')} \${check('vet')} \${external_commands.len}')
}
")
		assert output == 'false true false false true 73', output
	}
}

// Functions return fixed arrays and options of fixed arrays by value.
fn test_fixed_array_returns() {
	$if macos && arm64 {
		output := run_arm64('fixed_returns', "fn plain(x string) [3]string {
	return ['a', x, 'c']!
}

fn opt(x string) ?[3]string {
	return ['a', x, 'c']!
}

fn opt_none(f bool) ?[3]string {
	if f {
		return none
	}
	return ['p', 'q', 'r']!
}

fn main() {
	p := plain('B')
	println('\${p[0]}\${p[1]}\${p[2]}')
	o := opt('B') or { ['x', 'x', 'x']! }
	println('\${o[0]}\${o[1]}\${o[2]}')
	n := opt_none(false) or { ['x', 'x', 'x']! }
	println('\${n[0]}\${n[1]}\${n[2]}')
	for s in n {
		print(s)
	}
	println('')
}
")
		assert output == 'aBc
aBc
pqr
pqr', output
	}
}

fn test_fixed_array_option_unwrap() {
	$if macos && arm64 {
		output := run_arm64('fixed_option', "enum Kind {
	qualified
	pointer
	array
}

fn name_of(n u32) string {
	return ['main', 'Foo', 'Bar'][n]
}

fn pieces(kind Kind, parts []u32) ?[5]string {
	match kind {
		.qualified {
			return ['', name_of(parts[0]), '.', name_of(parts[1]), '']!
		}
		else {}
	}
	prefix := match kind {
		.pointer { '&' }
		.array { '[]' }
		else { return none }
	}
	return [prefix, name_of(parts[0]), '', '', '']!
}

fn small(kind Kind, parts []u32) ?string {
	ps := pieces(kind, parts) or { return none }
	mut len := 0
	for piece in ps {
		if piece.len > 255 - len {
			return none
		}
		len += piece.len
	}
	mut buffer := [256]u8{}
	mut offset := 0
	for piece in ps {
		if piece.len > 0 {
			unsafe { vmemcpy(&buffer[offset], piece.str, piece.len) }
			offset += piece.len
		}
	}
	return unsafe { tos(&buffer[0], len) }.clone()
}

fn main() {
	println(small(.qualified, [u32(0), 1]) or { 'none' })
	println(small(.pointer, [u32(2)]) or { 'none' })
	println(small(.array, [u32(1)]) or { 'none' })
}
")
		assert output == 'main.Foo
&Bar
[]Foo', output
	}
}

fn test_fixed_array_if_guard() {
	$if macos && arm64 {
		output := run_arm64('fixed_guard', "@[noinline]
fn opt(x int) ?[3]int {
	if x < 0 {
		return none
	}
	return [1, x, 3]!
}

fn main() {
	r := opt(7)
	println(r != none)
	if o := opt(5) {
		println('ok \${o[1]}')
	} else {
		println('none')
	}
}
")
		assert output == 'true
ok 5', output
	}
}

// A value passed to a `&T` parameter is passed by the address of a temporary.
fn test_rvalue_to_reference_parameter() {
	$if macos && arm64 {
		output := run_arm64('rvalue_ref', "struct Iface {
	name_id u32
}

struct Ptr {
	base &Kind
}

type Kind = Iface | Ptr | int

fn describe(t &Kind) string {
	return match t {
		Iface { 'iface \${t.name_id}' }
		Ptr { 'ptr' }
		int { 'int \${t}' }
	}
}

fn intern(t &Kind) &Kind {
	return unsafe { t }
}

fn main() {
	println(describe(Kind(Iface{
		name_id: 7
	})))
	println(describe(Kind(42)))
	k := Kind(Ptr{
		base: intern(Kind(Iface{
			name_id: 9
		}))
	})
	println(describe(k))
}
")
		assert output == 'iface 7
int 42
ptr', output
	}
}

// `m[k] = make_ref()` stores the `Kind` that `make_ref() &Kind` points to: the
// transformer declares the staging value as `Kind`.
fn test_implicit_deref_in_typed_declaration() {
	$if macos && arm64 {
		output := run_arm64('map_deref', "struct Leaf {
	n int
}

struct Ptr {
	base &Kind
}

type Kind = Leaf | Ptr

const leaf = Kind(Leaf{5})

fn make_ref() &Kind {
	return &Kind(Ptr{
		base: &leaf
	})
}

fn clobber(n int) int {
	mut buf := [64]int{}
	for i in 0 .. 64 {
		buf[i] = n * i + 7
	}
	return buf[n % 64]
}

fn describe(k Kind) string {
	return match k {
		Leaf { 'leaf \${k.n}' }
		Ptr { '&' + describe(*k.base) }
	}
}

fn main() {
	mut m := map[string]Kind{}
	m['a'] = make_ref()
	_ = clobber(3)
	println(describe(m['a'] or { Kind(Leaf{0}) }))
	println(voidptr((m['a'] or { Kind(Leaf{0}) } as Ptr).base) == voidptr(&leaf))
}
")
		assert output == '&leaf 5
true', output
	}
}

// `for k, mut v in m { v = x }` writes the map's own entries.
fn test_map_iteration_by_reference() {
	$if macos && arm64 {
		output := run_arm64('map_mut_iter', "fn main() {
	mut n := {
		'k': 1
		'j': 2
	}
	for _, mut v in n {
		v = v * 10
	}
	println(n)
	mut m := {
		'a': ['1']
	}
	for _, mut values in m {
		println(values.len)
		values = ['z']
	}
	println(m)
}
")
		assert output == "{'k': 10, 'j': 20}
1
{'a': ['z']}", output
	}
}

// A new string key is copied into the map, so reusing its buffer keeps the entry.
fn test_map_owns_string_keys() {
	$if macos && arm64 {
		output := run_arm64('map_string_key_clone', "import strings

fn main() {
	mut m := map[string]int{}
	mut sb := strings.new_builder(16)
	sb.write_string('changed')
	m[unsafe { tos(sb.data, sb.len) }] = 3
	sb.go_back_to(0)
	sb.write_string('xxxxxxx')
	println('changed' in m)
	mut buf := []u8{len: 3, init: `a`}
	m[unsafe { tos(buf.data, 3) }] = 4
	buf[0] = `z`
	println(m)
}
")
		assert output == "true
{'changed': 3, 'aaa': 4}", output
	}
}

// `clear()` drops deleted entries, so keys added afterwards are found again.
fn test_map_clear_after_delete() {
	$if macos && arm64 {
		output := run_arm64('map_clear_after_delete', "fn main() {
	mut m := map[string]int{}
	m['a'] = 1
	m['b'] = 2
	m.delete('a')
	m.clear()
	m['c'] = 3
	m['d'] = 4
	println('c' in m)
	println(m)
	println(m.len)
}
")
		assert output == "true
{'c': 3, 'd': 4}
2", output
	}
}

// A fixed array sized by a constant expression gets its full storage, so filling
// it does not overwrite the locals next to it.
fn test_fixed_array_length_expression() {
	$if macos && arm64 {
		output := run_arm64('fixed_array_len_expr', "const kib = 1024

struct F {
	cfile voidptr
	fd    int
}

fn fill(p &u8, n int) {
	unsafe { C.memset(p, 0x41, n) }
}

fn main() {
	f := F{voidptr(123), 7}
	mut buffer := [64 * kib]u8{}
	mut shifted := [(1 << 4) + 2]u16{}
	fill(&buffer[0], buffer.len)
	shifted[17] = 9
	println('\${f.cfile} \${f.fd} \${buffer.len} \${buffer[65535]} \${shifted.len} \${shifted[17]}')
}
")
		assert output == '7b 7 65536 65 18 9', output
	}
}

// Lookups do not write to the map, so threads can read a shared map at once.
fn test_concurrent_map_reads() {
	$if macos && arm64 {
		output := run_arm64('map_concurrent_reads', "fn count_found(m map[string]int, n int) int {
	mut found := 0
	for _ in 0 .. 20 {
		for i in 0 .. n {
			if 'key_\${i}' in m {
				found++
			}
		}
	}
	return found
}

fn main() {
	mut ok := true
	for _ in 0 .. 10 {
		mut m := map[string]int{}
		for i in 0 .. 2000 {
			m['key_\${i}'] = i
		}
		m.delete('key_0')
		mut c := m.clone()
		c['extra'] = 1
		mut threads := []thread int{}
		for _ in 0 .. 8 {
			threads << spawn count_found(c, 2000)
		}
		ok = ok && threads.wait().all(it == 1999 * 20)
	}
	println(ok)
}
")
		assert output == 'true', output
	}
}
