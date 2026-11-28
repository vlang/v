import encoding.binary
import os

const panic_message = 'encoding.binary: index out of range'

const bounds_probe = '--vtest-binary-bounds'

fn testsuite_begin() {
	if os.args.len == 5 && os.args[1] == bounds_probe {
		probe_bounds(os.args[2], os.args[3].int(), os.args[4].int())
		exit(0)
	}
}

fn probe_bounds(name string, n int, o int) u64 {
	mut parent := []u8{len: 16, init: u8(0xa0 + index)}
	mut b := if n < 0 { []u8{} } else { unsafe { parent[..n] } }
	return match name {
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
		'big_endian_f32_at' { u64(binary.big_endian_f32_at(b, o)) }
		'little_endian_f64_at' { u64(binary.little_endian_f64_at(b, o)) }
		'big_endian_f64_at' { u64(binary.big_endian_f64_at(b, o)) }
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
		'little_endian_put_f32_at' {
			binary.little_endian_put_f32_at(mut b, 0, o)
			u64(0)
		}
		'little_endian_put_f64_at' {
			binary.little_endian_put_f64_at(mut b, 0, o)
			u64(0)
		}
		'big_endian_put_f32_at' {
			binary.big_endian_put_f32_at(mut b, 0, o)
			u64(0)
		}
		'big_endian_put_f64_at' {
			binary.big_endian_put_f64_at(mut b, 0, o)
			u64(0)
		}
		else { panic('unknown case ' + name) }
	}
}

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
	res << Form{'little_endian_put_f32_at', 4}
	res << Form{'little_endian_f64_at', 8}
	res << Form{'little_endian_put_f64_at', 8}
	res << Form{'big_endian_f32_at', 4}
	res << Form{'big_endian_put_f32_at', 4}
	res << Form{'big_endian_f64_at', 8}
	res << Form{'big_endian_put_f64_at', 8}
	return res
}

fn test_out_of_range_access_panics_and_exact_fit_works() {
	exe := os.executable()
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
			res := os.exec([exe, bounds_probe, form.name, c[0].str(), c[1].str()])
			assert res.exit_code != 0, '${form.name} n=${c[0]} o=${c[1]} did not panic: ${res.output}'
			assert res.output.contains(panic_message), '${form.name} n=${c[0]} o=${c[1]}: ${res.output}'
		}
		// An exact fit must succeed. Only the panic cases need a child process.
		probe_bounds(form.name, form.size, 0)
	}
}
