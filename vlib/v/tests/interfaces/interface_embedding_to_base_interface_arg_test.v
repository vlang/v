interface Reader {
	read(p []u8) !int
}

interface Writer {
	write(p []u8) !int
}

interface ReaderWriter {
	Reader
	Writer
}

struct Thing {}

fn (t Thing) read(p []u8) !int {
	return p.len
}

fn (t Thing) write(p []u8) !int {
	return p.len
}

fn read_at_least(r Reader, buf []u8, min int) !int {
	_ = min
	return r.read(buf)
}

fn use_reader_writer(rw ReaderWriter) !int {
	return read_at_least(rw, [u8(1), 2], 1)
}

fn test_interface_embedding_allows_passing_embed_to_base_interface_arg() {
	got := use_reader_writer(Thing{}) or { panic(err) }
	assert got == 2
}
