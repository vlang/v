import os

interface InterpolationNamed {
	name() string
}

struct InterpolationRecord {
	label string
}

fn (r &InterpolationRecord) name() string {
	return r.label
}

struct InterpolationCustom {
	label string
}

fn (r &InterpolationCustom) name() string {
	return r.label
}

fn (r &InterpolationCustom) str() string {
	return 'custom(${r.label})'
}

interface InterpolationAny {}

fn interpolate_explicit_pointer(item InterpolationNamed) string {
	if item is &InterpolationRecord {
		return '${item}'
	}
	return ''
}

fn interpolate_match(item InterpolationNamed) string {
	return match item {
		InterpolationRecord { '${item}' }
		else { '' }
	}
}

fn interpolate_after_assert(item InterpolationNamed) string {
	assert item is InterpolationRecord
	return '${item}'
}

fn test_interface_struct_smartcast_interpolation() {
	for item in [InterpolationNamed(&InterpolationRecord{ label: 'pointer' }),
		InterpolationNamed(InterpolationRecord{ label: 'value' })] {
		expected := "&InterpolationRecord{\n    label: '${item.name()}'\n}"
		assert interpolate_explicit_pointer(item) == expected
		assert interpolate_match(item) == expected
		assert interpolate_after_assert(item) == expected
		if item is InterpolationRecord {
			assert '${item}' == expected
			assert '${item:s}' == expected
		} else {
			assert false
		}
	}
}

fn test_interface_smartcast_pointer_format_uses_original_address() {
	record := &InterpolationRecord{ label: 'pointer' }
	item := InterpolationNamed(record)
	if item is InterpolationRecord {
		assert '${item:p}' == '${record:p}'
	} else {
		assert false
	}
}

fn test_interface_smartcast_interpolation_with_custom_str() {
	for item in [InterpolationNamed(&InterpolationCustom{ label: 'pointer' }),
		InterpolationNamed(InterpolationCustom{ label: 'value' })] {
		expected := '&custom(${item.name()})'
		if item is InterpolationCustom {
			assert '${item}' == expected
			assert '${item:s}' == expected
		} else {
			assert false
		}
	}
}

fn test_interface_scalar_smartcast_interpolation() {
	item := InterpolationAny('value')
	if item is string {
		assert '${item}' == 'value'
		assert '${item:s}' == 'value'
	} else {
		assert false
	}
}

fn test_interface_smartcast_interpolation_keeps_single_pointer_in_c() {
	tmp := os.join_path(os.vtmp_dir(), 'interface_smartcast_interpolation_${os.getpid()}')
	os.mkdir_all(tmp)!
	defer { os.rmdir_all(tmp) or {} }
	source := os.join_path(tmp, 'main.v')
	output := os.join_path(tmp, 'main.c')
	os.write_file(source, 'interface Named { name() string }
struct Record { label string }
fn (r &Record) name() string { return r.label }
fn render_if(item Named) string {
	if item is Record { return "\${item}" }
	return ""
}
fn render_explicit(item Named) string {
	if item is &Record { return "\${item}" }
	return ""
}
fn render_assert(item Named) string {
	assert item is Record
	return "\${item}"
}
fn render_match(item Named) string {
	return match item {
		Record { "\${item}" }
		else { "" }
	}
}
fn main() {
	item := Named(&Record{ label: "record" })
	println(render_if(item))
	println(render_explicit(item))
	println(render_assert(item))
	println(render_match(item))
}
')!
	result := os.exec([@VEXE, '-o', output, source])
	assert result.exit_code == 0, result.output
	generated := os.read_file(output)!
	// The stringifier needs the stored object pointer, not an address of that pointer.
	assert generated.contains('(main__Record*)(item._object)')
	assert !generated.contains('(main__Record**)')
}
