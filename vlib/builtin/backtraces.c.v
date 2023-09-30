module builtin

// print_backtrace shows a backtrace of the current call stack on stdout
pub fn print_backtrace() {
	// At the time of backtrace_symbols_fd call, the C stack would look something like this:
	// * print_backtrace_skipping_top_frames
	// * print_backtrace itself
	// * the rest of the backtrace frames
	// => top 2 frames should be skipped, since they will not be informative to the developer
	$if !no_backtrace ? {
		$if freestanding {
			println(bare_backtrace())
		} $else {
			$if tinyc {
				C.tcc_backtrace(c'Backtrace')
			} $else {
				// NOTE: TCC doesn't have the unwind library
				$if use_libbacktrace ? {
					print_libbacktrace(1)
				} $else {
					print_backtrace_skipping_top_frames(2)
				}
			}
		}
	}
}
