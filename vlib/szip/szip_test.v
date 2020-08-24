import szip
import os

fn test_compile() {
	szip.open('test_compile.zip', szip.best_speed, szip.m_write) or {
		assert false
	}
	os.rm('test_compile.zip') or { }
}
