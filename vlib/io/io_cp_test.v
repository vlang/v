import io
import os

fn test_cp() ? {
	mut f := os.open(@FILE) or { panic(err) }
	defer {
		f.close()
	}
	mut r := io.new_buffered_reader(reader: f)
	mut stdout := os.stdout()
	io.cp(mut r, mut stdout)?
	assert true
}
