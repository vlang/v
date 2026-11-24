import io
import strings

fn test_buffer_as_io_writer() ! {
	mut buffer := strings.new_buffer(1)
	first_count := io.write_string(mut buffer, 'first\x00')!
	second_count := io.write_string(mut buffer, 'λ😀')!
	assert first_count == 6
	assert second_count == 6
	text := buffer.take_string()
	assert text == 'first\x00λ😀'
	assert unsafe { text.str[text.len] } == 0
	assert buffer.len() == 0
	unsafe {
		text.free()
		buffer.free()
	}
}
