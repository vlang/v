module fastc

import os

// FastcPreparedLink holds a linker that can be initialized concurrently with
// the TinyCC processes compiling a program's translation units.
pub struct FastcPreparedLink {
mut:
	state       voidptr
	diagnostics voidptr
	program     string
	base_args   []string
}

// fastc_prepare_link initializes the in-process TinyCC linker on macOS. A compiler
// built by TinyCC uses the executable-based linker too, since Darwin TinyCC cannot
// link the libtcc archive that implements the in-process path.
pub fn fastc_prepare_link(program string, tcc_lib string, base_args []string) FastcPreparedLink {
	$if macos && !tinyc && !fastc_selfhost ? {
		return fastc_prepare_libtcc_link(program, tcc_lib, base_args)
	} $else {
		return FastcPreparedLink{
			program: program
			base_args: base_args.clone()
		}
	}
}

// fastc_prepared_link_skips_codesign reports whether the prepared linker
// suppresses TinyCC's codesign subprocess and needs in-process signing.
pub fn fastc_prepared_link_skips_codesign(link &FastcPreparedLink) bool {
	$if macos && !tinyc && !fastc_selfhost ? {
		return !isnil(link.state)
	} $else {
		return false
	}
}

// fastc_finish_link adds the compiled inputs to a prepared linker and writes
// the executable. `final_args` contains libraries and other link-only flags.
pub fn fastc_finish_link(mut link FastcPreparedLink, input_paths []string, final_args []string, output string) os.Result {
	$if macos && !tinyc && !fastc_selfhost ? {
		return fastc_finish_libtcc_link(mut link, input_paths, final_args, output)
	} $else {
		mut args := link.base_args.clone()
		args << ['-o', output]
		args << input_paths
		args << final_args
		return fastc_run_command(link.program, args)
	}
}

// fastc_discard_link releases a prepared linker after unit compilation fails.
pub fn fastc_discard_link(mut link FastcPreparedLink) {
	$if macos && !tinyc && !fastc_selfhost ? {
		fastc_discard_libtcc_link(mut link)
	}
}
