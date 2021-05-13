import io
import os

fn test_cp() ? {
	my_source := @FILE
	mut f := os.open(my_source) or { panic(err) }
	defer {
		f.close()
	}
	mut r := io.new_buffered_reader(reader: f)
	mut stdout := os.stdout()
	io.cp(r, mut stdout) ?
	assert true
}
