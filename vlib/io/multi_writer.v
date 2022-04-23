module io

// new_multi_writer returns a Writer that writes to all writers. The write
// function of the returned Writer writes to all writers of the MultiWriter,
// returns the length of bytes written, and if any writer fails to write the
// full length an error is returned and writing to other writers stops, and if
// any writer returns an error the error is returned immediately and writing to
// other writers stops.
pub fn new_multi_writer(writers ...Writer) Writer {
	return &MultiWriter{
		writers: writers
	}
}

// MultiWriter writes to all its writers.
pub struct MultiWriter {
pub mut:
	writers []Writer
}

// write writes to all writers of the MultiWriter. Returns the length of bytes
// written. If any writer fails to write the full length an error is returned
// and writing to other writers stops. If any writer returns an error the error
// is returned immediately and writing to other writers stops.
pub fn (mut m MultiWriter) write(buf []u8) ?int {
	for mut w in m.writers {
		n := w.write(buf) ?
		if n != buf.len {
			return error('io: incomplete write to writer of MultiWriter')
		}
	}
	return buf.len
}
