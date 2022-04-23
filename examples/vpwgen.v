import os
import os.cmdline
import crypto.rand
import strings

fn main() {
	blocksize := 256
	size := cmdline.option(os.args, '-size', '80').int()
	repeats := cmdline.option(os.args, '-repeats', '4').int()
	for _ in 0 .. repeats {
		mut sb := strings.new_builder(blocksize)
		for {
			x := rand.read(blocksize) ?
			for c in x {
				if c >= `0` && c <= `~` {
					sb.write_u8(c)
				}
			}
			if sb.len > size {
				println(sb.str()[0..size])
				break
			}
		}
	}
}
