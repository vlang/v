module main

$if v1_fallback ? {
} $else $if musl ? {
} $else $if bsd || linux || windows {
	import v3.driver
}

// The V3 driver (vlib/v3) is linked directly into `cmd/v` on the target
// platforms where V3 compiles and runs — macOS, the BSD family, glibc Linux, and
// Windows. There `v` can run the V3 compiler in the SAME process. musl builds
// deliberately use the stub path because the embedded V3 runtime currently depends
// on glibc-only C interfaces; their ordinary C compilations are delegated to
// v1_fallback.
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
} $else $if musl ? {
	@[markused]
	fn macos_v3_driver_is_available() bool {
		return false
	}

	@[markused]
	fn macos_v3_driver_run(_ []string) {}
} $else $if bsd || linux || windows {
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
