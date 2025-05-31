// vtest build: present_node?
import os

fn test_read_from_file() {
	mut buf := []u8{len: 10}
	f := os.open(@FILE)!
	n := f.read(mut &buf)!
	println(buf[..n])
}
