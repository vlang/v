module main

// The V3 driver (vlib/v3) is linked directly into `cmd/v` for every normal
// (non cross-VC) build, so `v` can run the V3 compiler in the SAME process — on
// macOS by default, and on any platform when `-new-compiler` is passed. The
// cross-VC bootstrap keeps the portable stub in macos_v3_driver_d_cross.v.
import v3.driver

@[markused]
fn macos_v3_driver_is_available() bool {
	return true
}

@[markused]
fn macos_v3_driver_run(args []string) {
	driver.run(args)
}
