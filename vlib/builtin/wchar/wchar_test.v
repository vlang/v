import builtin.wchar

const little_serial_number = [u16(67), 0, 76, 0, 52, 0, 54, 0, 73, 0, 49, 0, 65, 0, 48, 0, 48,
	0, 54, 0, 52, 0, 57, 0, 0, 0, 0]
const big_serial_number = [u16(0), 67, 0, 76, 0, 52, 0, 54, 0, 73, 0, 49, 0, 65, 0, 48, 0, 48,
	0, 54, 0, 52, 0, 57, 0, 0, 0, 0]

const wide_serial_number_windows = little_serial_number.map(u8(it))

const swide_serial_number = 'CL46I1A00649'

fn test_from_to_rune() {
	for r in swide_serial_number.runes() {
		c := wchar.from_rune(r)
		assert c.to_rune() == r
	}
	assert wchar.from_rune(0).to_rune() == 0
}

fn test_to_string() {
	mut p := voidptr(little_serial_number.data)
	$if big_endian {
		p = big_serial_number.data
	}
	$if windows {
		p = wide_serial_number_windows.data
	}
	assert unsafe { wchar.length_in_characters(p) } == swide_serial_number.len
	s := unsafe { wchar.to_string(p) }
	dump(s)
	assert s == swide_serial_number
}

fn test_from_string() {
	x := wchar.from_string(swide_serial_number)
	assert unsafe { x[0] } == wchar.from_rune(`C`)
	assert unsafe { x[1] } == wchar.from_rune(`L`)
	assert unsafe { x[2] } == wchar.from_rune(`4`)
	assert unsafe { x[11] } == wchar.from_rune(`9`)
	assert unsafe { x[12] } == wchar.zero
}
