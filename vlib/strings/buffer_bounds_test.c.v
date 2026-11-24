import os

const buffer_bounds_source = r'
import os
import strings

fn main() {
	mut buffer := strings.Buffer{}
	match os.args[1] {
		"negative_capacity" { _ = strings.new_buffer(-1) }
		"overflow_capacity" { _ = strings.new_buffer(max_int) }
		"negative_reserve" { buffer.reserve(-1) }
		"overflow_reserve" { buffer.reserve(max_int) }
		"overflow_append" {
			buffer.write_u8(`a`)
			buffer.reserve(max_int - 1)
		}
		"negative_pointer_count" { unsafe { buffer.write_ptr(nil, -1) } }
		"negative_index" {
			buffer.write_string("abc")
			buffer.set_byte(-1, `x`)
		}
		"past_end_index" {
			buffer.write_string("abc")
			buffer.set_byte(3, `x`)
		}
		else { panic("unknown test case") }
	}
}
'

fn test_buffer_rejects_invalid_sizes_and_indices_before_memory_access() ! {
	work_dir := os.join_path(os.vtmp_dir(), 'strings_buffer_bounds_${os.getpid()}')
	os.mkdir_all(work_dir)!
	defer { os.rmdir_all(work_dir) or {} }
	source := os.join_path(work_dir, 'bounds.v')
	binary := os.join_path(work_dir, 'bounds' + $if windows { '.exe' } $else { '' })
	os.write_file(source, buffer_bounds_source)!
	compile := os.exec([@VEXE, '-gc', 'none', '-o', binary, source])
	assert compile.exit_code == 0, compile.output
	for case_name, message in {
		'negative_capacity':      'capacity exceeds addressable length'
		'overflow_capacity':      'capacity exceeds addressable length'
		'negative_reserve':       'length exceeds addressable length'
		'overflow_reserve':       'length exceeds addressable length'
		'overflow_append':        'length exceeds addressable length'
		'negative_pointer_count': 'length exceeds addressable length'
		'negative_index':         'index out of range'
		'past_end_index':         'index out of range'
	} {
		result := os.exec([binary, case_name])
		assert result.exit_code != 0, '${case_name}: ${result.output}'
		assert result.output.contains('strings.Buffer ${message}'), '${case_name}: ${result.output}'
	}
}
