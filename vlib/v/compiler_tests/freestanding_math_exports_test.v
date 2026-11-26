import os

const freestanding_math_exports_program = '
module main
import math

fn C.pow(f64, f64) f64
fn C.floor(f64) f64
fn C.fabs(f64) f64

struct PowRow { x u64 y u64 expected u64 }

fn near(actual f64, expected f64) bool {
    if math.is_nan(expected) { return math.is_nan(actual) }
    if math.is_inf(expected, 0) || expected == 0 {
        return math.f64_bits(actual) == math.f64_bits(expected)
    }
    if math.is_nan(actual) || math.is_inf(actual, 0) || math.signbit(actual) != math.signbit(expected) { return false }
    a := math.f64_bits(actual) & 0x7fffffffffffffff
    b := math.f64_bits(expected) & 0x7fffffffffffffff
    return if a > b { a-b <= 4 } else { b-a <= 4 }
}

fn operator_power(x f64, y f64) f64 { return x ** y }

fn main() {
    rows := [@POW_ROWS@]!
    for index, row in rows {
        x := math.f64_from_bits(row.x)
        y := math.f64_from_bits(row.y)
        expected := math.f64_from_bits(row.expected)
        result := C.pow(x, y)
        if !near(result, expected) {
            println(index)
            println(math.f64_bits(result).hex())
            println(row.expected.hex())
            exit(1)
        }
        assert near(operator_power(x, y), expected), "row \${index}: power operator"
    }
    negative_zero := math.copysign(0, -1)
    assert math.f64_bits(C.fabs(negative_zero)) == 0
    assert math.f64_bits(C.floor(negative_zero)) == math.f64_bits(negative_zero)
    for x in [math.inf(1), math.inf(-1), math.max_f64, -math.max_f64, 9223372036854775808.0, -9223372036854775808.0] {
        assert C.floor(x) == x
        assert C.fabs(x) == math.abs(x)
    }
    assert math.is_nan(C.floor(math.nan()))
    assert math.is_nan(C.fabs(math.nan()))
    tiny := math.f64_from_bits(1)
    assert C.floor(tiny) == 0
    assert C.floor(-tiny) == -1
    assert C.fabs(-tiny) == tiny
    assert C.floor(-0.5) == -1
    assert C.floor(-1.25) == -2
    assert C.floor(1.25) == 1
    assert C.fabs(-1.25) == 1.25
    assert "\${1.25:.2f}" == "1.25"
    assert "\${-1.25:.2f}" == "-1.25"
    println("freestanding math exports passed")
}
'

fn test_freestanding_c_math_exports_and_general_power_operator() {
	$if linux || macos {
		root := os.join_path(os.vtmp_dir(), 'freestanding_math_exports_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer { os.rmdir_all(root) or {} }
		fixture := os.read_file(os.join_path(@VMODROOT, 'vlib', 'math', 'testdata', 'pow_bits.tsv')) or { panic(err) }
		mut rows := []string{}
		for line in fixture.split_into_lines() {
			if line.len == 0 || line[0] == `#` { continue }
			fields := line.split('\t')
			assert fields.len == 3
			rows << 'PowRow{u64(0x${fields[0]}), u64(0x${fields[1]}), u64(0x${fields[2]})}'
		}
		assert rows.len == 854
		source := os.join_path(root, 'main.c.v')
		os.write_file(source, freestanding_math_exports_program.replace('@POW_ROWS@', rows.join(','))) or { panic(err) }
		for mode in ['', '-prod'] {
			binary := os.join_path(root, 'probe')
			build := os.exec([@VEXE, '-no-retry-compilation', '-freestanding', '-gc', 'none', mode,
				'-o', binary, source])
			assert build.exit_code == 0, build.output
			run := os.exec([binary])
			assert run.exit_code == 0, '${mode}: ${run.output}'
			assert run.output.trim_space() == 'freestanding math exports passed', run.output
		}
	}
}
