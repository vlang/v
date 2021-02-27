import szip
import os

fn test_szip() {
	mut z := szip.open('test_compile.zip', .best_speed, .write) or {
		assert false
		return
	}
	defer {
		z.close()
		os.rm('test_compile.zip') or { }
	}
}
