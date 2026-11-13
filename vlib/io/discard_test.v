module io

fn test_discard() {
	mut d := Discard{}
	n := d.write('hello'.bytes())!
	assert n == 5
}

fn test_discard_empty_and_repeated_writes() {
	mut d := discard
	assert d.write([]u8{})! == 0
	assert d.write('first'.bytes())! == 5
	assert d.write([]u8{})! == 0
	assert d.write('second'.bytes())! == 6
}

fn test_discard_implements_writer_and_preserves_input() {
	mut writer := Writer(discard)
	data := [u8(0), 127, 128, 255]
	expected := data.clone()
	assert writer.write(data[..2])! == 2
	assert writer.write(data[2..])! == 2
	assert writer.write(data)! == 4
	assert data == expected
}

fn test_discard_with_multi_writer() {
	mut writer := new_multi_writer(discard, Discard{})
	assert writer.write('output'.bytes())! == 6
	assert writer.write([]u8{})! == 0
}
