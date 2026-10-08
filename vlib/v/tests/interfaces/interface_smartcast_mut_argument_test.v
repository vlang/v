import os

interface SmartcastStorage {
	label string
}

struct SmartcastCell {
mut:
	label string
}

fn update_smartcast_cell(mut cell SmartcastCell) {
	cell.label = 'updated'
}

fn rebind_smartcast_cell(mut cell &SmartcastCell) {
	cell = &SmartcastCell{ label: 'replacement' }
}

fn recover_smartcast_cell(item SmartcastStorage) ?&SmartcastCell {
	return if item is SmartcastCell { item } else { none }
}

fn test_mut_interface_smartcast_argument_keeps_boxed_storage() {
	mut items := [SmartcastStorage(SmartcastCell{ label: 'value' })]
	mut item := items[0]
	alias := recover_smartcast_cell(item) or { panic('missing cell') }
	if mut item is SmartcastCell {
		update_smartcast_cell(mut item)
		assert item.label == 'updated'
	} else {
		assert false
	}
	assert alias.label == 'updated'
	assert items[0].label == 'updated'
}

fn test_mut_interface_smartcast_argument_keeps_pointer_storage() {
	mut original := &SmartcastCell{ label: 'pointer' }
	mut item := SmartcastStorage(original)
	alias := recover_smartcast_cell(item) or { panic('missing cell') }
	if mut item is SmartcastCell {
		update_smartcast_cell(mut item)
		assert item.label == 'updated'
	} else {
		assert false
	}
	assert alias == original
	assert original.label == 'updated'
	assert alias.label == 'updated'
}

fn test_mut_explicit_interface_pointer_smartcast_argument_keeps_storage() {
	mut original := &SmartcastCell{ label: 'pointer' }
	mut item := SmartcastStorage(original)
	if mut item is &SmartcastCell {
		update_smartcast_cell(mut item)
	} else {
		assert false
	}
	assert original.label == 'updated'
}

fn test_mut_pointer_parameter_still_rebinds_an_actual_pointer_slot() {
	mut original := &SmartcastCell{ label: 'original' }
	alias := original
	rebind_smartcast_cell(mut original)
	assert original.label == 'replacement'
	assert alias.label == 'original'
}

fn test_mut_interface_smartcast_is_not_a_rebindable_pointer_slot() {
	tmp := os.join_path(os.vtmp_dir(), 'interface_smartcast_mut_pointer_${os.getpid()}')
	os.mkdir_all(tmp)!
	defer { os.rmdir_all(tmp) or {} }
	source := os.join_path(tmp, 'main.v')
	for pattern in ['Cell', '&Cell'] {
		os.write_file(source, 'interface Storage { label string }
struct Cell { label string }
fn replace(mut cell &Cell) { cell = &Cell{ label: "replacement" } }
fn main() {
	mut item := Storage(&Cell{ label: "original" })
	if mut item is ${pattern} { replace(mut item) }
}
')!
		result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-check',
			source])
		assert result.exit_code != 0, result.output
		assert result.output.contains('cannot use'), result.output
		assert result.output.contains('argument 1 to `replace`'), result.output
	}
}
