// vtest build: tinyc && !self_sandboxed_packaging? && !sanitized_job?
import os
import time
import term
import v.util.diff
import v.util.vtest

const vexe = @VEXE

const vroot = os.real_path(@VMODROOT)

const testdata_folder = os.join_path(vroot, 'vlib/v/tests/testdata/builtin_overflow')

fn mm(s string) string {
	return term.colorize(term.magenta, s)
}

fn mj(input ...string) string {
	return mm(input.filter(it.len > 0).join(' '))
}

fn test_out_files() {
	println(term.colorize(term.green, '> testing whether .out files match:'))
	os.chdir(vroot) or {}
	output_path := os.join_path(os.vtmp_dir(), 'overflow_outs')
	os.mkdir_all(output_path)!
	defer {
		os.rmdir_all(output_path) or {}
	}
	files := os.ls(testdata_folder) or { [] }
	tests := files.filter(it.ends_with('.out'))
	if tests.len == 0 {
		eprintln('no `.out` tests found in ${testdata_folder}')
		return
	}
	paths := vtest.filter_vtest_only(tests, basepath: testdata_folder).sorted()
	mut total_errors := 0
	for out_path in paths {
		basename, path, relpath, out_relpath := target2paths(out_path, '.out')
		pexe := os.join_path(output_path, '${basename}.exe')
		//
		file_options := '-g -check-overflow'
		alloptions := '-o ${os.quoted_path(pexe)} ${file_options}'
		label := mj('v', file_options, 'run', relpath) + ' == ${mm(out_relpath)} '
		//
		compile_cmd := '${os.quoted_path(vexe)} ${alloptions} ${os.quoted_path(path)}'
		sw_compile := time.new_stopwatch()
		compilation := os.exec([vexe, ...(os.split_args(alloptions) or { panic(err) }), path])
		compile_ms := sw_compile.elapsed().milliseconds()
		ensure_compilation_succeeded(compilation, compile_cmd)
		//
		sw_run := time.new_stopwatch()
		res := os.exec([pexe])
		run_ms := sw_run.elapsed().milliseconds()
		//
		if res.exit_code < 0 {
			println('nope')
			panic(res.output)
		}
		mut found := res.output.trim_right('\r\n').replace('\r\n', '\n')
		mut expected := os.read_file(out_path)!
		expected = expected.trim_right('\r\n').replace('\r\n', '\n')
		if expected.contains('================ V panic ================') {
			// panic include backtraces and absolute file paths, so can't do char by char comparison
			n_found := normalize_panic_message(found, vroot)
			n_expected := normalize_panic_message(expected, vroot)
			if found.contains('================ V panic ================') {
				if n_found.starts_with(n_expected) {
					println('${term.green('OK (panic)')} C:${compile_ms:6}ms, R:${run_ms:2}ms ${label}')
					continue
				} else {
					// Both have panics, but there was a difference...
					// Pass the normalized strings for further reporting.
					// There is no point in comparing the backtraces too.
					found = n_found
					expected = n_expected
				}
			}
		}
		if expected != found {
			println('${term.red('FAIL')} C:${compile_ms:6}ms, R:${run_ms:2}ms ${label}')
			if diff_ := diff.compare_text(expected, found) {
				println(term.header('difference:', '-'))
				println(diff_)
			} else {
				println(term.header('expected:', '-'))
				println(expected)
				println(term.header('found:', '-'))
				println(found)
			}
			println(term.h_divider('-'))
			total_errors++
		} else {
			println('${term.green('OK  ')} C:${compile_ms:6}ms, R:${run_ms:2}ms ${label}')
		}
	}
	assert total_errors == 0
}

fn normalize_panic_message(message string, vroot string) string {
	mut msg := message.all_before('=========================================')
	// change windows to nix path
	s := vroot.replace(os.path_separator, '/')
	msg = msg.replace(s + '/', '')
	msg = msg.trim_space()
	return msg
}

fn vroot_relative(opath string) string {
	nvroot := vroot.replace(os.path_separator, '/') + '/'
	npath := opath.replace(os.path_separator, '/')
	return npath.replace(nvroot, '')
}

fn ensure_compilation_succeeded(compilation os.Result, cmd string) {
	if compilation.exit_code < 0 {
		eprintln('> cmd exit_code < 0, cmd: ${cmd}')
		panic(compilation.output)
	}
	if compilation.exit_code != 0 {
		eprintln('> cmd exit_code != 0, cmd: ${cmd}')
		panic('compilation failed: ${compilation.output}')
	}
}

fn target2paths(target_path string, postfix string) (string, string, string, string) {
	basename := os.file_name(target_path).replace(postfix, '')
	target_dir := os.dir(target_path)
	path := os.join_path(target_dir, '${basename}.vv')
	relpath := vroot_relative(path)
	target_relpath := vroot_relative(target_path)
	return basename, path, relpath, target_relpath
}

// The cases below cover the `-check-overflow` checks for unary negation, `/` and `%`
// (`min / -1`), and shift counts, and the opt-in `-check-casts` checks. Each program
// is compiled once with and once without its flag, then run once per case.

const signed_int_types = ['i8', 'i16', 'i32', 'i64', 'int', 'isize']!

const all_int_types = ['i8', 'u8', 'i16', 'u16', 'i32', 'u32', 'i64', 'u64', 'int', 'isize', 'usize']!

struct OpCase {
	args   []string
	output string // the whole output, or the panic message when `panics` is set
	panics bool
}

fn ok_case(output string, args ...string) OpCase {
	return OpCase{
		args:   args
		output: output
	}
}

fn panic_case(message string, args ...string) OpCase {
	return OpCase{
		args:   args
		output: message
		panics: true
	}
}

fn int_type_bits(typ string) int {
	return match typ {
		'i8', 'u8' { 8 }
		'i16', 'u16' { 16 }
		'i32', 'u32' { 32 }
		'int', 'isize', 'usize' { int(sizeof(isize)) * 8 }
		else { 64 }
	}
}

// helper_type is the type that a `builtin.overflow` helper reports for `typ`.
fn helper_type(typ string) string {
	prefix := if typ.starts_with('u') { 'u' } else { 'i' }
	return '${prefix}${int_type_bits(typ)}'
}

fn signed_min(bits int) string {
	return if bits == 64 { '-9223372036854775808' } else { '-${u64(1) << (bits - 1)}' }
}

fn signed_min_plus_one(bits int) string {
	return '-${(u64(1) << (bits - 1)) - 1}'
}

fn signed_max(bits int) string {
	return '${(u64(1) << (bits - 1)) - 1}'
}

// printable prints an `isize` result through `i64`: printing an `isize` goes
// through `strconv.format_int`, which does not handle `min_i64` yet.
fn printable(typ string, expr string) string {
	return if typ == 'isize' { 'i64(${expr})' } else { expr }
}

fn overflow_ops_source() string {
	mut sb := []string{}
	sb << 'import os\nimport math\nimport math.bits\n'
	for typ in all_int_types {
		parse := if typ.starts_with('u') { 'u64' } else { 'i64' }
		sb << 'fn parse_${typ}(s string) ${typ} {\n\treturn ${typ}(s.${parse}())\n}\n'
		sb << 'fn shl_${typ}(x ${typ}, n int) ${typ} {\n\treturn x << n\n}\n'
		sb << 'fn shr_${typ}(x ${typ}, n int) ${typ} {\n\treturn x >> n\n}\n'
		sb << 'fn ushr_${typ}(x ${typ}, n int) ${typ} {\n\treturn ${typ}(x >>> n)\n}\n'
		sb << 'fn shl_assign_${typ}(x ${typ}, n int) ${typ} {\n\tmut r := x\n\tr <<= n\n\treturn r\n}\n'
		sb << 'fn shr_assign_${typ}(x ${typ}, n int) ${typ} {\n\tmut r := x\n\tr >>= n\n\treturn r\n}\n'
		sb << 'fn div_assign_${typ}(x ${typ}, y ${typ}) ${typ} {\n\tmut r := x\n\tr /= y\n\treturn r\n}\n'
		sb << 'fn div_into_${typ}(mut p &${typ}, y ${typ}) {\n\tunsafe {\n\t\t*p /= y\n\t}\n}\n'
		sb << 'fn div_ptr_${typ}(x ${typ}, y ${typ}) ${typ} {\n\tmut r := x\n\tdiv_into_${typ}(mut &r, y)\n\treturn r\n}\n'
	}
	for typ in signed_int_types {
		sb << 'fn neg_${typ}(x ${typ}) ${typ} {\n\treturn -x\n}\n'
		sb << 'fn div_${typ}(x ${typ}, y ${typ}) ${typ} {\n\treturn x / y\n}\n'
		sb << 'fn mod_${typ}(x ${typ}, y ${typ}) ${typ} {\n\treturn x % y\n}\n'
		sb << 'fn mod_assign_${typ}(x ${typ}, y ${typ}) ${typ} {\n\tmut r := x\n\tr %= y\n\treturn r\n}\n'
		sb << 'fn mod_into_${typ}(mut p &${typ}, y ${typ}) {\n\tunsafe {\n\t\t*p %= y\n\t}\n}\n'
		sb << 'fn mod_ptr_${typ}(x ${typ}, y ${typ}) ${typ} {\n\tmut r := x\n\tmod_into_${typ}(mut &r, y)\n\treturn r\n}\n'
	}
	sb << '@[ignore_overflow]\nfn ignored_ops(x i8, n int) string {\n\treturn "\${-x} \${u64(1) << n}"\n}\n'
	sb << 'fn main() {\n\top := os.args[1]\n\ttyp := os.args[2]\n\ta := os.args[3]'
	sb << '\tb := if os.args.len > 4 { os.args[4] } else { "0" }\n\tmatch op + "_" + typ {'
	for typ in all_int_types {
		for op in ['shl', 'shr', 'ushr', 'shl_assign', 'shr_assign'] {
			sb << '\t\t"${op}_${typ}" { println(${printable(typ, '${op}_${typ}(parse_${typ}(a), b.int())')}) }'
		}
		for op in ['div_assign', 'div_ptr'] {
			sb << '\t\t"${op}_${typ}" { println(${printable(typ, '${op}_${typ}(parse_${typ}(a), parse_${typ}(b))')}) }'
		}
	}
	for typ in signed_int_types {
		sb << '\t\t"neg_${typ}" { println(${printable(typ, 'neg_${typ}(parse_${typ}(a))')}) }'
		for op in ['div', 'mod', 'mod_assign', 'mod_ptr'] {
			sb << '\t\t"${op}_${typ}" { println(${printable(typ, '${op}_${typ}(parse_${typ}(a), parse_${typ}(b))')}) }'
		}
	}
	sb << '\t\t"ignored_i8" { println(ignored_ops(parse_i8(a), b.int())) }'
	sb << '\t\t"vlib_u64" { println("\${bits.rotate_left_64(parse_u64(a), b.int())} \${math.abs(min_i64)}") }'
	sb << '\t\telse { panic("unknown case") }\n\t}\n}\n'
	return sb.join('\n')
}

fn overflow_ops_cases(checked bool) []OpCase {
	mut cases := []OpCase{}
	for typ in all_int_types {
		bits := int_type_bits(typ)
		h := helper_type(typ)
		w := bits.str()
		top := if typ.starts_with('u') { (u64(1) << (bits - 1)).str() } else { signed_min(bits) }
		cases << ok_case(top, 'shl', typ, '1', '${bits - 1}')
		cases << ok_case('8', 'shr', typ, '64', '3')
		cases << ok_case('1', 'ushr', typ, '1', '0')
		cases << ok_case('16', 'shl_assign', typ, '1', '4')
		cases << ok_case('0', 'shr_assign', typ, '1', '${bits - 1}')
		cases << ok_case('3', 'div_assign', typ, '7', '2')
		cases << ok_case('14', 'div_ptr', typ, '100', '7')
		if checked {
			cases << panic_case('attempt to shl with overflow(${h}(1) << ${w})', 'shl', typ, '1',
				w)
			cases << panic_case('attempt to shl with overflow(${h}(1) << -1)', 'shl', typ, '1',
				'-1')
			cases << panic_case('attempt to shr with overflow(${h}(1) >> ${w})', 'shr', typ, '1',
				w)
			cases << panic_case('attempt to shr with overflow(${h}(1) >> -3)', 'shr', typ, '1',
				'-3')
			cases << panic_case('attempt to shr with overflow(u${w}(1) >> ${w})', 'ushr', typ,
				'1', w)
			cases << panic_case('attempt to shl with overflow(${h}(1) << ${bits + 1})',
				'shl_assign', typ, '1', '${bits + 1}')
			cases << panic_case('attempt to shr with overflow(${h}(1) >> ${w})', 'shr_assign',
				typ, '1', w)
			cases << panic_case('division by zero', 'div_assign', typ, '7', '0')
		} else {
			// V defines the result of a shift by the width or more.
			cases << ok_case('0', 'shl', typ, '1', w)
			cases << ok_case('0', 'shl', typ, '1', '-1')
			cases << ok_case('0', 'shr', typ, '1', w)
			cases << ok_case('0', 'ushr', typ, '1', w)
			cases << ok_case('0', 'shl_assign', typ, '1', '${bits + 1}')
		}
	}
	for typ in signed_int_types {
		bits := int_type_bits(typ)
		h := helper_type(typ)
		min := signed_min(bits)
		cases << ok_case(signed_max(bits), 'neg', typ, signed_min_plus_one(bits))
		cases << ok_case(min, 'div', typ, min, '1')
		cases << ok_case('0', 'mod', typ, min, '1')
		cases << ok_case('-${u64(1) << (bits - 2)}', 'div', typ, min, '2')
		cases << ok_case('-1', 'mod_assign', typ, '-7', '-2')
		cases << ok_case(signed_min_plus_one(bits), 'div_assign', typ, signed_max(bits), '-1')
		cases << ok_case('-50', 'div_ptr', typ, '-100', '2')
		cases << ok_case('-2', 'mod_ptr', typ, '-100', '7')
		if checked {
			cases << panic_case('attempt to neg with overflow(-${h}(${min}))', 'neg', typ, min)
			cases << panic_case('attempt to div with overflow(${h}(${min}) / ${h}(-1))', 'div',
				typ, min, '-1')
			cases << panic_case('attempt to mod with overflow(${h}(${min}) % ${h}(-1))', 'mod',
				typ, min, '-1')
			cases << panic_case('attempt to div with overflow(${h}(${min}) / ${h}(-1))',
				'div_assign', typ, min, '-1')
			cases << panic_case('attempt to mod with overflow(${h}(${min}) % ${h}(-1))',
				'mod_assign', typ, min, '-1')
			cases << panic_case('attempt to div with overflow(${h}(${min}) / ${h}(-1))',
				'div_ptr', typ, min, '-1')
			cases << panic_case('attempt to mod with overflow(${h}(${min}) % ${h}(-1))',
				'mod_ptr', typ, min, '-1')
			cases << panic_case('division by zero', 'div', typ, min, '0')
			cases << panic_case('modulo by zero', 'mod_assign', typ, '5', '0')
		} else {
			cases << ok_case(min, 'neg', typ, min)
		}
	}
	// `@[ignore_overflow]` functions and vlib keep the wrapping negation and V's
	// defined shift results.
	cases << ok_case('-128 0', 'ignored', 'i8', '-128', '64')
	cases << ok_case('5 -9223372036854775808', 'vlib', 'u64', '5', '0')
	return cases
}

const cast_pairs = [
	['i64', 'i8']!,
	['i64', 'u8']!,
	['i64', 'u32']!,
	['int', 'i32']!,
	['int', 'u16']!,
	['u64', 'i64']!,
	['i64', 'u64']!,
	['u32', 'i32']!,
	['u16', 'u8']!,
	['i16', 'u16']!,
	['i32', 'i16']!,
	['u8', 'i8']!,
	['int', 'usize']!,
	['usize', 'u32']!,
	['isize', 'i8']!,
	['i8', 'i64']!,
	['u32', 'i64']!,
	['u8', 'u16']!,
]!

fn casts_source() string {
	mut sb := []string{}
	sb << 'import os\nimport math.bits\nimport strconv\n\ntype Byte = u8\n'
	for pair in cast_pairs {
		src, dst := pair[0], pair[1]
		sb << 'fn cast_${src}_${dst}(x ${src}) ${dst} {\n\treturn ${dst}(x)\n}\n'
	}
	sb << 'fn literal_cast() u8 {\n\treturn u8(-1)\n}\n'
	sb << 'fn literal_not_cast() u64 {\n\treturn u64(~0)\n}\n'
	sb << 'fn big_literal_casts() string {\n\treturn "\${u64(0x8000000000000000)} \${u64(18446744073709551615)} \${u64(0xcbf29ce484222325)} \${u64((0xffff_ffff_ffff_ffff))} \${u32(0xffffffff)} \${usize(0xffffffff)} \${i8(0x7f)} \${u16(0b1111111111111111)}"\n}\n'
	sb << 'fn alias_cast(x i64) Byte {\n\treturn Byte(x)\n}\n'
	sb << '@[ignore_overflow]\nfn ignored_cast(x i64) u8 {\n\treturn u8(x)\n}\n'
	sb << 'fn main() {\n\tkind := os.args[1]\n\ta := os.args[2]\n\tmatch kind {'
	for pair in cast_pairs {
		src, dst := pair[0], pair[1]
		parse := if src.starts_with('u') { 'u64' } else { 'i64' }
		sb << '\t\t"${src}_${dst}" { println(cast_${src}_${dst}(${src}(a.${parse}()))) }'
	}
	sb << '\t\t"literal" { println(literal_cast()) }'
	sb << '\t\t"literal_not" { println(literal_not_cast()) }'
	sb << '\t\t"big_literals" { println(big_literal_casts()) }'
	sb << '\t\t"alias" { println(alias_cast(a.i64())) }'
	sb << '\t\t"ignored" { println(ignored_cast(a.i64())) }'
	sb << '\t\t"vlib" { println("\${bits.rotate_left_64(u64(5), 0)} \${strconv.format_int(-255, 16)} \${f64(1.5)} \${u32(0xdeadbeef).hex()}") }'
	sb << '\t\telse { panic("unknown case") }\n\t}\n}\n'
	return sb.join('\n')
}

fn casts_cases(checked bool) []OpCase {
	// [src, dst, value, truncated result without the flag]
	failing := [
		['i64', 'i8', '300', '44'],
		['i64', 'i8', '-129', '127'],
		['i64', 'u8', '-1', '255'],
		['i64', 'u8', '256', '0'],
		['i64', 'u32', '-5', '4294967291'],
		['i64', 'u32', '4294967296', '0'],
		['int', 'i32', '2147483648', '-2147483648'],
		['int', 'u16', '65536', '0'],
		['u64', 'i64', '9223372036854775808', '-9223372036854775808'],
		['i64', 'u64', '-1', '18446744073709551615'],
		['u32', 'i32', '2147483648', '-2147483648'],
		['u16', 'u8', '256', '0'],
		['i16', 'u16', '-1', '65535'],
		['i32', 'i16', '32768', '-32768'],
		['u8', 'i8', '128', '-128'],
		['isize', 'i8', '-200', '56'],
	]
	fitting := [
		['i64', 'i8', '127'],
		['i64', 'i8', '-128'],
		['i64', 'u8', '255'],
		['i64', 'u32', '4294967295'],
		['int', 'i32', '-2147483648'],
		['int', 'u16', '65535'],
		['u64', 'i64', '9223372036854775807'],
		['i64', 'u64', '9223372036854775807'],
		['u32', 'i32', '2147483647'],
		['u16', 'u8', '255'],
		['i16', 'u16', '32767'],
		['i32', 'i16', '-32768'],
		['u8', 'i8', '127'],
		['int', 'usize', '0'],
		['i8', 'i64', '-128'],
		['u32', 'i64', '4294967295'],
		['u8', 'u16', '255'],
	]
	mut cases := []OpCase{}
	for c in fitting {
		cases << ok_case(c[2], '${c[0]}_${c[1]}', c[2])
	}
	for c in failing {
		if checked {
			cases << panic_case('attempt to cast with overflow(${c[1]}(${c[0]}(${c[2]})))',
				'${c[0]}_${c[1]}', c[2])
		} else {
			cases << ok_case(c[3], '${c[0]}_${c[1]}', c[2])
		}
	}
	if int_type_bits('usize') == 64 {
		if checked {
			cases << panic_case('attempt to cast with overflow(usize(int(-1)))', 'int_usize',
				'-1')
			cases << panic_case('attempt to cast with overflow(u32(usize(4294967296)))',
				'usize_u32', '4294967296')
		} else {
			cases << ok_case('18446744073709551615', 'int_usize', '-1')
			cases << ok_case('0', 'usize_u32', '4294967296')
		}
	}
	if checked {
		cases << panic_case('attempt to cast with overflow(u8(int(-1)))', 'literal', '0')
		cases << panic_case('attempt to cast with overflow(u64(int(-1)))', 'literal_not', '0')
		cases << panic_case('attempt to cast with overflow(u8(i64(256)))', 'alias', '256')
	} else {
		cases << ok_case('255', 'literal', '0')
		cases << ok_case('18446744073709551615', 'literal_not', '0')
		cases << ok_case('0', 'alias', '256')
	}
	// Untyped literals that fit the target type are not reported, even when they do
	// not fit in `int`.
	cases << ok_case('9223372036854775808 18446744073709551615 14695981039346656037 18446744073709551615 4294967295 4294967295 127 65535',
		'big_literals', '0')
	cases << ok_case('44', 'ignored', '300')
	cases << ok_case('5 -ff 1.5 deadbeef', 'vlib', '0')
	return cases
}

fn compile_case_program(source_path string, exe_path string, flags []string) {
	mut args := ['-o', exe_path]
	args << flags
	args << source_path
	compilation := os.exec([vexe, ...args])
	ensure_compilation_succeeded(compilation, '${vexe} ${args.join(' ')}')
}

fn run_cases(label string, exe_path string, cases []OpCase) int {
	mut errors := 0
	for c in cases {
		res := os.exec([exe_path, ...c.args])
		output := res.output.trim_space().replace('\r\n', '\n')
		ok := if c.panics {
			res.exit_code != 0 && output.contains(c.output)
		} else {
			res.exit_code == 0 && output == c.output
		}
		if !ok {
			errors++
			eprintln('${term.red('FAIL')} ${label} ${c.args.join(' ')}: expected ${if c.panics {
				'panic'
			} else {
				'output'
			}} `${c.output}`, got exit code ${res.exit_code} and output:\n${output}')
		}
	}
	println('${if errors == 0 { term.green('OK  ') } else { term.red('FAIL') }} ${label}: ${cases.len - errors}/${cases.len} cases')
	return errors
}

fn test_checked_negation_division_shifts_and_casts() {
	work_dir := os.join_path(os.vtmp_dir(), 'overflow_ops_${os.getpid()}')
	os.mkdir_all(work_dir)!
	defer {
		os.rmdir_all(work_dir) or {}
	}
	ops_source := os.join_path(work_dir, 'overflow_ops.v')
	os.write_file(ops_source, overflow_ops_source())!
	casts_source_path := os.join_path(work_dir, 'casts.v')
	os.write_file(casts_source_path, casts_source())!
	mut errors := 0
	for checked in [true, false] {
		suffix := if checked { 'checked' } else { 'plain' }
		ops_exe := os.join_path(work_dir, 'overflow_ops_${suffix}.exe')
		compile_case_program(ops_source, ops_exe, if checked { ['-check-overflow'] } else { [] })
		errors += run_cases('overflow ops (${suffix})', ops_exe, overflow_ops_cases(checked))
		casts_exe := os.join_path(work_dir, 'casts_${suffix}.exe')
		compile_case_program(casts_source_path, casts_exe, if checked {
			['-check-casts']
		} else {
			[]
		})
		errors += run_cases('casts (${suffix})', casts_exe, casts_cases(checked))
	}
	assert errors == 0
}
