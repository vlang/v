module pointer_alias_len_module

pub struct Buffer {
pub mut:
	data &u8 = unsafe { nil }
	len  int
	cap  int
}

pub type BufferPtr = &Buffer

// append writes `byte` through a parameter whose type is an alias of a pointer.
pub fn append(buf BufferPtr, byte u8) {
	if buf.len < buf.cap {
		unsafe {
			buf.data[buf.len] = byte
			buf.len++
		}
	}
}
