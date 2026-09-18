	fn wl_data_device_motion(data voidptr, device &C.wl_data_device, time u32, x i32, y i32) {
	}

	// pipe() writes two C ints, not two platform-width V ints.
	fn wl_create_drop_pipe() ![2]i32 {
		mut fds := [2]i32{init: -1}
		if C.pipe(&fds[0]) == -1 {
			return error('sokol_app: failed to create Wayland drop pipe')
		}
		return fds
	}

	fn wl_data_device_drop(data voidptr, device &C.wl_data_device) {
		if g_sapp_state.wl.data_offer == unsafe { nil } {
			return
		}
		fds := wl_create_drop_pipe() or {
			C.wl_data_offer_destroy(g_sapp_state.wl.data_offer)
			g_sapp_state.wl.data_offer = unsafe { nil }
			return
		}
		C.wl_data_offer_receive(g_sapp_state.wl.data_offer, c'text/uri-list', fds[1])
		C.close(fds[1])
		C.wl_display_flush(g_sapp_state.wl.display)

		mut buffer := [8192]u8{}
		mut total_read := isize(0)
		for {
			n := C.read(fds[0], unsafe { &buffer[0] + total_read }, usize(buffer.len -
				int(total_read) - 1))
			if n <= 0 {
				break
			}
			total_read += n
			if total_read >= isize(buffer.len - 1) {
				break
			}
		}
		C.close(fds[0])

		if total_read > 0 && g_sapp_state.drop.enabled {
