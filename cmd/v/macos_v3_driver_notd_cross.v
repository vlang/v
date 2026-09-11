module main

$if v1_fallback ? {
} $else {
	import v3.driver
}

// The V3 driver (vlib/v3) is linked directly into every native `cmd/v` build,
// where `v` can run the V3 compiler in the SAME process.
//
// The separately built `v1_fallback` command shell also takes the stub path, so
// it contains only the stable compiler. Portable `-os cross` VC generation gets
// the same stub below or the one in macos_v3_driver_d_cross.v,
// so V3's thread/parallel code is never cross-compiled into them.
$if v1_fallback ? {
	@[markused]
	fn macos_v3_driver_is_available() bool {
		return false
	}

	@[markused]
	fn macos_v3_driver_run(_ []string) {}
} $else {
	@[markused]
	fn macos_v3_driver_is_available() bool {
		return true
	}

	@[markused]
	fn macos_v3_driver_run(args []string) {
		driver.run(args)
	}
}
