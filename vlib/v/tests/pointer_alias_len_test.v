// A `len` field read through a parameter whose type is an alias of a pointer
// (`type BufferPtr = &Buffer`) in another module is a pointer access.
import pointer_alias_len_module as pal

fn test_len_field_through_pointer_alias_parameter() {
	mut storage := [4]u8{}
	mut buf := pal.Buffer{
		data: unsafe { &storage[0] }
		cap:  4
	}
	pal.append(&buf, `a`)
	pal.append(&buf, `b`)
	assert buf.len == 2
	assert storage[0] == `a`
	assert storage[1] == `b`
}
