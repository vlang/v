import szip
import os

fn test_compile() {
	mut z := szip.open('test_compile.zip', szip.best_speed, szip.m_write) or {
		assert false
		return
	}
	defer {
		z.close()
		os.rm('test_compile.zip') or { }
	}
}
