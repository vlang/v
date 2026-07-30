module main

$if macos {
	import v3.driver

	@[markused]
	fn macos_v3_driver_is_available() bool {
		return true
	}

	@[markused]
	fn macos_v3_driver_run(args []string) {
		driver.run(args)
	}
}
