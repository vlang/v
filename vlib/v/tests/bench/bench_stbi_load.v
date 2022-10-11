import os
import stbi

fn load_image(path string) {
	img := stbi.load(path) or { return }
	assert img.ok
	// unsafe { img.free() }
}

fn main() {
	pid := os.getpid()
	image_path := os.args[1] or { '${@VEXEROOT}/examples/assets/logo.png' }
	for i in 1 .. 1001 {
		if i % 100 == 0 {
			println('pid: $pid | Loaded $image_path $i times.')
		}
		load_image(image_path)
	}
}
