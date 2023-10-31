import dl

type RealOpen = fn (charptr, int, int) int

fn test_rtld_next() {
	$if windows {
		println('skipping test_rtld_next on windows')
		return
	}
	real_open := RealOpen((dl.sym(dl.rtld_next, 'open')))
	println(ptr_str(real_open))
	assert real_open != unsafe { nil }
	assert dl.dlerror() == ''
}
