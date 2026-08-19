module main

// The V3 driver (vlib/v3) is linked directly into `cmd/v` on the target
// platforms where V3 compiles and runs — macOS (its default compiler) and Linux
// (exercised by the V3 CI self-host). There `v` can run the V3 compiler in the
// SAME process: on macOS by default, and on any of those platforms when
// `-new-compiler` is passed.
//
// Other targets (Windows, the BSDs, and the portable `-os cross` VC generation)
// get the stub below or the one in macos_v3_driver_d_cross.v, so V3's
// thread/parallel code is never cross-compiled into them; `-new-compiler` then
// reports that this build does not embed V3.
$if macos || linux {
	import v3.driver

	@[markused]
	fn macos_v3_driver_is_available() bool {
		return true
	}

	@[markused]
	fn macos_v3_driver_run(args []string) {
		driver.run(args)
	}
} $else {
	@[markused]
	fn macos_v3_driver_is_available() bool {
		return false
	}

	@[markused]
	fn macos_v3_driver_run(_ []string) {}
}
