module sim

// log is a helper function to print debug info
[inline]
pub fn log(info string) {
	$if verbose ? {
		println(info)
	}
}
