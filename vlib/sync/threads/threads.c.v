module threads

// This module adds the necessary compiler flags for using threads.
// It is automatically imported by code that does `go func()` .
// See the spawn-expression handling in vlib/v/parser/.
// The goal is that programs, that do not use threads at all will not need
// to link to -lpthread etc.
// Note: on some platforms like Android, linking -lpthread is not needed too.
// See https://stackoverflow.com/a/31277163/1904615

$if !windows && !android {
	$if freestanding && linux && !native && !i386 {
		// Standalone Linux uses the bare clone/TLS runtime. Embedded code
		// retains the host's pthread implementation.
		$if v_bare_no_startup ? {
			#flag -lpthread
		}
	} $else {
		#flag -lpthread
	}
}
