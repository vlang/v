import os
import time

if os.args.len == 1 {
	eprintln('A small `tail -f file` like program, written in V.')
	eprintln('Usage: `v run examples/vtail.v your_long_file.log`')
	exit(0)
}

tfile := os.args[1] or { panic('pass 1 file path as argument') }
mut f := os.open_file(tfile, 'r') or { panic('file ${tfile} does not exist') }
f.seek(0, .end)!
mut read_pos := f.tell()!
mut buf := []u8{len: 10 * 1024}
for {
	bytes := f.read_bytes_with_newline(mut buf)!
	if bytes == 0 && f.eof() {
		// The end of the file has been reached, so wait a bit, and retry from the same position:
		f.close()
		time.sleep(500 * time.millisecond)
		f = os.open_file(tfile, 'r')!
		f.seek(read_pos, .start)!
		continue
	}
	read_pos += bytes
	print(unsafe { (&u8(buf.data)).vstring_with_len(bytes) })
}
