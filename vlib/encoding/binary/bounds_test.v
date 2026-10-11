import os

const vexe = @VEXE

const panic_message = 'encoding.binary: index out of range'

const helper_source = "import os
import encoding.binary

fn main() {
	name := os.args[1]
	n := os.args[2].int()
	o := os.args[3].int()
	mut parent := []u8{len: 16, init: u8(0xa0 + index)}
	mut b := if n < 0 { []u8{} } else { unsafe { parent[..n] } }
	value := match name {
		'big_endian_u16' { u64(binary.big_endian_u16(b)) }
		'big_endian_u16_at' { u64(binary.big_endian_u16_at(b, o)) }
		'big_endian_u16_end' { u64(binary.big_endian_u16_end(b)) }
		'big_endian_u32' { u64(binary.big_endian_u32(b)) }
		'big_endian_u32_at' { u64(binary.big_endian_u32_at(b, o)) }
		'big_endian_u32_end' { u64(binary.big_endian_u32_end(b)) }
		'big_endian_u64' { binary.big_endian_u64(b) }
		'big_endian_u64_at' { binary.big_endian_u64_at(b, o) }
		'big_endian_u64_end' { binary.big_endian_u64_end(b) }
		'little_endian_u16' { u64(binary.little_endian_u16(b)) }
		'little_endian_u16_at' { u64(binary.little_endian_u16_at(b, o)) }
		'little_endian_u16_end' { u64(binary.little_endian_u16_end(b)) }
		'little_endian_u32' { u64(binary.little_endian_u32(b)) }
		'little_endian_u32_at' { u64(binary.little_endian_u32_at(b, o)) }
		'little_endian_u32_end' { u64(binary.little_endian_u32_end(b)) }
		'little_endian_u64' { binary.little_endian_u64(b) }
		'little_endian_u64_at' { binary.little_endian_u64_at(b, o) }
		'little_endian_u64_end' { binary.little_endian_u64_end(b) }
		'little_endian_f32_at' { u64(binary.little_endian_f32_at(b, o)) }
		'big_endian_put_u16' {
			binary.big_endian_put_u16(mut b, 0)
			u64(0)
		}
		'big_endian_put_u16_at' {
			binary.big_endian_put_u16_at(mut b, 0, o)
			u64(0)
		}
		'big_endian_put_u16_end' {
			binary.big_endian_put_u16_end(mut b, 0)
			u64(0)
		}
		'big_endian_put_u32' {
			binary.big_endian_put_u32(mut b, 0)
			u64(0)
		}
		'big_endian_put_u32_at' {
			binary.big_endian_put_u32_at(mut b, 0, o)
			u64(0)
		}
		'big_endian_put_u32_end' {
			binary.big_endian_put_u32_end(mut b, 0)
			u64(0)
		}
		'big_endian_put_u64' {
			binary.big_endian_put_u64(mut b, 0)
			u64(0)
		}
		'big_endian_put_u64_at' {
			binary.big_endian_put_u64_at(mut b, 0, o)
			u64(0)
		}
		'big_endian_put_u64_end' {
			binary.big_endian_put_u64_end(mut b, 0)
			u64(0)
		}
		'little_endian_put_u16' {
			binary.little_endian_put_u16(mut b, 0)
			u64(0)
		}
		'little_endian_put_u16_at' {
			binary.little_endian_put_u16_at(mut b, 0, o)
			u64(0)
		}
		'little_endian_put_u16_end' {
			binary.little_endian_put_u16_end(mut b, 0)
			u64(0)
		}
		'little_endian_put_u32' {
			binary.little_endian_put_u32(mut b, 0)
			u64(0)
		}
		'little_endian_put_u32_at' {
			binary.little_endian_put_u32_at(mut b, 0, o)
			u64(0)
		}
		'little_endian_put_u32_end' {
			binary.little_endian_put_u32_end(mut b, 0)
			u64(0)
		}
		'little_endian_put_u64' {
			binary.little_endian_put_u64(mut b, 0)
			u64(0)
		}
		'little_endian_put_u64_at' {
			binary.little_endian_put_u64_at(mut b, 0, o)
			u64(0)
		}
		'little_endian_put_u64_end' {
			binary.little_endian_put_u64_end(mut b, 0)
			u64(0)
		}
		else { panic('unknown case ' + name) }
	}
	println('ok \${value}')
}
"

struct Form {
	name string
	size int
}

fn forms() []Form {
	mut res := []Form{}
	for prefix in ['big_endian_', 'little_endian_', 'big_endian_put_', 'little_endian_put_'] {
		for width, size in {
			'u16': 2
			'u32': 4
			'u64': 8
		} {
			for suffix in ['', '_at', '_end'] {
				res << Form{'${prefix}${width}${suffix}', size}
			}
		}
	}
	res << Form{'little_endian_f32_at', 4}
	return res
}

fn run_case(exe string, name string, n int, o int) os.Result {
	return os.exec([exe, name, n.str(), o.str()])
}

fn test_out_of_range_access_panics_and_exact_fit_works() {
	dir := os.join_path(os.vtmp_dir(), 'binary_bounds_test_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	src := os.join_path(dir, 'helper.v')
	exe := os.join_path(dir, 'helper' + $if windows { '.exe' } $else { '' })
	os.write_file(src, helper_source) or { panic(err) }
	compile := os.exec([vexe, '-o', exe, src])
	assert compile.exit_code == 0, compile.output
	for form in forms() {
		mut bad := [][]int{}
		if form.name.ends_with('_at') {
			bad << [form.size, 1]
			bad << [form.size, -1]
			bad << [form.size, 2147483647]
			bad << [form.size, -2147483647 - 1]
			bad << [-1, 0]
		} else {
			bad << [form.size - 1, 0]
			bad << [-1, 0]
		}
		for c in bad {
			res := run_case(exe, form.name, c[0], c[1])
			assert res.exit_code != 0, '${form.name} n=${c[0]} o=${c[1]} did not panic: ${res.output}'
			assert res.output.contains(panic_message), '${form.name} n=${c[0]} o=${c[1]}: ${res.output}'
		}
		res := run_case(exe, form.name, form.size, 0)
		assert res.exit_code == 0, '${form.name} exact fit: ${res.output}'
		assert res.output.starts_with('ok '), res.output
	}
}
