module main

import os
import strings

struct OverflowStruct {
	name string
	max  [2]string // add by 1 every time, should overflow
	min  [2]string // sub by 1 every time, should overflow
	mid  [2]string // mult by 10 every time, should overflow
}

// TODO: add tests for `rune`,`int`,`isize`,`usize`.
const overflows = [
	OverflowStruct{
		name: 'u8'
		max:  ['254', '255']!
		min:  ['1', '0']!
		mid:  ['25', '250']!
	},
	OverflowStruct{
		name: 'u16'
		max:  ['65534', '65535']!
		min:  ['1', '0']!
		mid:  ['6553', '65530']!
	},
	OverflowStruct{
		name: 'u32'
		max:  ['4294967294', '4294967295']!
		min:  ['1', '0']!
		mid:  ['429496729', '4294967290']!
	},
	OverflowStruct{
		name: 'u64'
		max:  ['18446744073709551614', '18446744073709551615']!
		min:  ['1', '0']!
		mid:  ['1844674407370955161', '18446744073709551610']!
	},
	OverflowStruct{
		name: 'i8'
		max:  ['126', '127']!
		min:  ['-127', '-128']!
		mid:  ['12', '120']!
	},
	OverflowStruct{
		name: 'i16'
		max:  ['32766', '32767']!
		min:  ['-32767', '-32768']!
		mid:  ['3276', '32760']!
	},
	OverflowStruct{
		name: 'i32'
		max:  ['2147483646', '2147483647']!
		min:  ['-2147483647', '-2147483648']!
		mid:  ['214748364', '2147483640']!
	},
	OverflowStruct{
		name: 'i64'
		max:  ['9223372036854775806', '9223372036854775807']!
		min:  ['-9223372036854775807', '-9223372036854775808']!
		mid:  ['922337203685477580', '9223372036854775800']!
	},
]

const ops = ['add', 'add_assign', 'inc', 'sub', 'sub_assign', 'dec', 'mul', 'mul_assign']

fn main() {
	mut sb := strings.new_builder(1024)
	for o in overflows {
		for op in ops {
			match op {
				'add' {
					// vv file
					sb.writeln('// vtest vflags: -g -check-overflow\n')
					sb.writeln('mut x := ${o.name}(${o.max[0]})')
					sb.writeln('println(x)')
					sb.writeln('x = x + 1')
					sb.writeln('println(x)')
					sb.writeln('x = x + 1')
					sb.writeln('println(x)')
					os.write_file('panic_on_${o.name}_${op}_overflow.vv', sb.str())!

					// out file
					sb.writeln('${o.max[0]}')
					sb.writeln('${o.max[1]}')
					sb.writeln('================ V panic ================')
					sb.writeln('   module: builtin.overflow')
					sb.writeln(' function: add_${o.name}()')
					sb.writeln('  message: attempt to add with overflow(${o.name}(${o.max[1]}) + ${o.name}(1))')
					sb.writeln('     file: vlib/builtin/overflow/overflow.v\n')
					os.write_file('panic_on_${o.name}_${op}_overflow.out', sb.str())!
				}
				'add_assign' {
					// vv file
					sb.writeln('// vtest vflags: -g -check-overflow\n')
					sb.writeln('mut x := ${o.name}(${o.max[0]})')
					sb.writeln('println(x)')
					sb.writeln('x += 1')
					sb.writeln('println(x)')
					sb.writeln('x += 1')
					sb.writeln('println(x)')
					os.write_file('panic_on_${o.name}_${op}_overflow.vv', sb.str())!

					// out file
					sb.writeln('${o.max[0]}')
					sb.writeln('${o.max[1]}')
					sb.writeln('================ V panic ================')
					sb.writeln('   module: builtin.overflow')
					sb.writeln(' function: add_${o.name}()')
					sb.writeln('  message: attempt to add with overflow(${o.name}(${o.max[1]}) + ${o.name}(1))')
					sb.writeln('     file: vlib/builtin/overflow/overflow.v\n')
					os.write_file('panic_on_${o.name}_${op}_overflow.out', sb.str())!
				}
				'inc' {
					// vv file
					sb.writeln('// vtest vflags: -g -check-overflow\n')
					sb.writeln('mut x := ${o.name}(${o.max[0]})')
					sb.writeln('println(x)')
					sb.writeln('x ++')
					sb.writeln('println(x)')
					sb.writeln('x ++')
					sb.writeln('println(x)')
					os.write_file('panic_on_${o.name}_${op}_overflow.vv', sb.str())!

					// out file
					sb.writeln('${o.max[0]}')
					sb.writeln('${o.max[1]}')
					sb.writeln('================ V panic ================')
					sb.writeln('   module: builtin.overflow')
					sb.writeln(' function: add_${o.name}()')
					sb.writeln('  message: attempt to add with overflow(${o.name}(${o.max[1]}) + ${o.name}(1))')
					sb.writeln('     file: vlib/builtin/overflow/overflow.v\n')
					os.write_file('panic_on_${o.name}_${op}_overflow.out', sb.str())!
				}
				'sub' {
					// vv file
					sb.writeln('// vtest vflags: -g -check-overflow\n')
					sb.writeln('mut x := ${o.name}(${o.min[0]})')
					sb.writeln('println(x)')
					sb.writeln('x = x - 1')
					sb.writeln('println(x)')
					sb.writeln('x = x - 1')
					sb.writeln('println(x)')
					os.write_file('panic_on_${o.name}_${op}_overflow.vv', sb.str())!

					// out file
					sb.writeln('${o.min[0]}')
					sb.writeln('${o.min[1]}')
					sb.writeln('================ V panic ================')
					sb.writeln('   module: builtin.overflow')
					sb.writeln(' function: sub_${o.name}()')
					sb.writeln('  message: attempt to sub with overflow(${o.name}(${o.min[1]}) - ${o.name}(1))')
					sb.writeln('     file: vlib/builtin/overflow/overflow.v\n')
					os.write_file('panic_on_${o.name}_${op}_overflow.out', sb.str())!
				}
				'sub_assign' {
					// vv file
					sb.writeln('// vtest vflags: -g -check-overflow\n')
					sb.writeln('mut x := ${o.name}(${o.min[0]})')
					sb.writeln('println(x)')
					sb.writeln('x -= 1')
					sb.writeln('println(x)')
					sb.writeln('x -= 1')
					sb.writeln('println(x)')
					os.write_file('panic_on_${o.name}_${op}_overflow.vv', sb.str())!

					// out file
					sb.writeln('${o.min[0]}')
					sb.writeln('${o.min[1]}')
					sb.writeln('================ V panic ================')
					sb.writeln('   module: builtin.overflow')
					sb.writeln(' function: sub_${o.name}()')
					sb.writeln('  message: attempt to sub with overflow(${o.name}(${o.min[1]}) - ${o.name}(1))')
					sb.writeln('     file: vlib/builtin/overflow/overflow.v\n')
					os.write_file('panic_on_${o.name}_${op}_overflow.out', sb.str())!
				}
				'dec' {
					// vv file
					sb.writeln('// vtest vflags: -g -check-overflow\n')
					sb.writeln('mut x := ${o.name}(${o.min[0]})')
					sb.writeln('println(x)')
					sb.writeln('x --')
					sb.writeln('println(x)')
					sb.writeln('x --')
					sb.writeln('println(x)')
					os.write_file('panic_on_${o.name}_${op}_overflow.vv', sb.str())!

					// out file
					sb.writeln('${o.min[0]}')
					sb.writeln('${o.min[1]}')
					sb.writeln('================ V panic ================')
					sb.writeln('   module: builtin.overflow')
					sb.writeln(' function: sub_${o.name}()')
					sb.writeln('  message: attempt to sub with overflow(${o.name}(${o.min[1]}) - ${o.name}(1))')
					sb.writeln('     file: vlib/builtin/overflow/overflow.v\n')
					os.write_file('panic_on_${o.name}_${op}_overflow.out', sb.str())!
				}
				'mul' {
					// vv file
					sb.writeln('// vtest vflags: -g -check-overflow\n')
					sb.writeln('mut x := ${o.name}(${o.mid[0]})')
					sb.writeln('println(x)')
					sb.writeln('x = x * 10')
					sb.writeln('println(x)')
					sb.writeln('x = x * 10')
					sb.writeln('println(x)')
					os.write_file('panic_on_${o.name}_${op}_overflow.vv', sb.str())!

					// out file
					sb.writeln('${o.mid[0]}')
					sb.writeln('${o.mid[1]}')
					sb.writeln('================ V panic ================')
					sb.writeln('   module: builtin.overflow')
					sb.writeln(' function: mul_${o.name}()')
					sb.writeln('  message: attempt to mul with overflow(${o.name}(${o.mid[1]}) * ${o.name}(10))')
					sb.writeln('     file: vlib/builtin/overflow/overflow.v\n')
					os.write_file('panic_on_${o.name}_${op}_overflow.out', sb.str())!
				}
				'mul_assign' {
					// vv file
					sb.writeln('// vtest vflags: -g -check-overflow\n')
					sb.writeln('mut x := ${o.name}(${o.mid[0]})')
					sb.writeln('println(x)')
					sb.writeln('x *= 10')
					sb.writeln('println(x)')
					sb.writeln('x *= 10')
					sb.writeln('println(x)')
					os.write_file('panic_on_${o.name}_${op}_overflow.vv', sb.str())!

					// out file
					sb.writeln('${o.mid[0]}')
					sb.writeln('${o.mid[1]}')
					sb.writeln('================ V panic ================')
					sb.writeln('   module: builtin.overflow')
					sb.writeln(' function: mul_${o.name}()')
					sb.writeln('  message: attempt to mul with overflow(${o.name}(${o.mid[1]}) * ${o.name}(10))')
					sb.writeln('     file: vlib/builtin/overflow/overflow.v\n')
					os.write_file('panic_on_${o.name}_${op}_overflow.out', sb.str())!
				}
				else {}
			}
		}
	}
}
