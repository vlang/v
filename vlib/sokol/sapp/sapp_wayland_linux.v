module sapp

// Wayland backend implementation for sokol_app.
// Ports the C _sapp_wl_* functions from sokol_app.h to V.
// This file is only compiled when -d sokol_wayland is used.

$if sokol_wayland ? {
	const fallback_default_window_width = 640
	const fallback_default_window_height = 480

	// XKB keysym constants
	const xkb_key_space = u32(0x0020)
	const xkb_key_apostrophe = u32(0x0027)
	const xkb_key_comma = u32(0x002c)
	const xkb_key_minus = u32(0x002d)
	const xkb_key_period = u32(0x002e)
	const xkb_key_slash = u32(0x002f)
	const xkb_key_0 = u32(0x0030)
	const xkb_key_1 = u32(0x0031)
	const xkb_key_2 = u32(0x0032)
	const xkb_key_3 = u32(0x0033)
	const xkb_key_4 = u32(0x0034)
	const xkb_key_5 = u32(0x0035)
	const xkb_key_6 = u32(0x0036)
	const xkb_key_7 = u32(0x0037)
	const xkb_key_8 = u32(0x0038)
	const xkb_key_9 = u32(0x0039)
	const xkb_key_semicolon = u32(0x003b)
	const xkb_key_equal = u32(0x003d)
	const xkb_key_bracketleft = u32(0x005b)
	const xkb_key_backslash = u32(0x005c)
	const xkb_key_bracketright = u32(0x005d)
	const xkb_key_grave = u32(0x0060)
	const xkb_key_a = u32(0x0061)
	const xkb_key_b = u32(0x0062)
	const xkb_key_c = u32(0x0063)
	const xkb_key_d = u32(0x0064)
	const xkb_key_e = u32(0x0065)
	const xkb_key_f = u32(0x0066)
	const xkb_key_g = u32(0x0067)
	const xkb_key_h = u32(0x0068)
	const xkb_key_i = u32(0x0069)
	const xkb_key_j = u32(0x006a)
	const xkb_key_k = u32(0x006b)
	const xkb_key_l = u32(0x006c)
	const xkb_key_m = u32(0x006d)
	const xkb_key_n = u32(0x006e)
	const xkb_key_o = u32(0x006f)
	const xkb_key_p = u32(0x0070)
	const xkb_key_q = u32(0x0071)
	const xkb_key_r = u32(0x0072)
	const xkb_key_s = u32(0x0073)
	const xkb_key_t = u32(0x0074)
	const xkb_key_u = u32(0x0075)
	const xkb_key_v = u32(0x0076)
	const xkb_key_w = u32(0x0077)
	const xkb_key_x = u32(0x0078)
	const xkb_key_y = u32(0x0079)
	const xkb_key_z = u32(0x007a)
	const xkb_key_backspace = u32(0xff08)
	const xkb_key_tab = u32(0xff09)
	const xkb_key_return = u32(0xff0d)
	const xkb_key_pause = u32(0xff13)
	const xkb_key_scroll_lock = u32(0xff14)
	const xkb_key_escape = u32(0xff1b)
	const xkb_key_home = u32(0xff50)
	const xkb_key_left = u32(0xff51)
	const xkb_key_up = u32(0xff52)
	const xkb_key_right = u32(0xff53)
	const xkb_key_down = u32(0xff54)
	const xkb_key_page_up = u32(0xff55)
	const xkb_key_page_down = u32(0xff56)
	const xkb_key_end = u32(0xff57)
	const xkb_key_print = u32(0xff61)
	const xkb_key_insert = u32(0xff63)
	const xkb_key_menu = u32(0xff67)
	const xkb_key_num_lock = u32(0xff7f)
	const xkb_key_kp_enter = u32(0xff8d)
	const xkb_key_kp_multiply = u32(0xffaa)
	const xkb_key_kp_add = u32(0xffab)
	const xkb_key_kp_subtract = u32(0xffad)
	const xkb_key_kp_decimal = u32(0xffae)
	const xkb_key_kp_divide = u32(0xffaf)
	const xkb_key_kp_0 = u32(0xffb0)
	const xkb_key_kp_1 = u32(0xffb1)
	const xkb_key_kp_2 = u32(0xffb2)
	const xkb_key_kp_3 = u32(0xffb3)
	const xkb_key_kp_4 = u32(0xffb4)
	const xkb_key_kp_5 = u32(0xffb5)
	const xkb_key_kp_6 = u32(0xffb6)
	const xkb_key_kp_7 = u32(0xffb7)
	const xkb_key_kp_8 = u32(0xffb8)
	const xkb_key_kp_9 = u32(0xffb9)
	const xkb_key_kp_equal = u32(0xffbd)
	const xkb_key_f1 = u32(0xffbe)
	const xkb_key_f2 = u32(0xffbf)
	const xkb_key_f3 = u32(0xffc0)
	const xkb_key_f4 = u32(0xffc1)
	const xkb_key_f5 = u32(0xffc2)
	const xkb_key_f6 = u32(0xffc3)
	const xkb_key_f7 = u32(0xffc4)
	const xkb_key_f8 = u32(0xffc5)
	const xkb_key_f9 = u32(0xffc6)
	const xkb_key_f10 = u32(0xffc7)
	const xkb_key_f11 = u32(0xffc8)
	const xkb_key_f12 = u32(0xffc9)
	const xkb_key_f13 = u32(0xffca)
	const xkb_key_f14 = u32(0xffcb)
	const xkb_key_f15 = u32(0xffcc)
	const xkb_key_f16 = u32(0xffcd)
	const xkb_key_f17 = u32(0xffce)
	const xkb_key_f18 = u32(0xffcf)
	const xkb_key_f19 = u32(0xffd0)
	const xkb_key_f20 = u32(0xffd1)
	const xkb_key_f21 = u32(0xffd2)
	const xkb_key_f22 = u32(0xffd3)
	const xkb_key_f23 = u32(0xffd4)
	const xkb_key_f24 = u32(0xffd5)
	const xkb_key_f25 = u32(0xffd6)
	const xkb_key_shift_l = u32(0xffe1)
	const xkb_key_shift_r = u32(0xffe2)
	const xkb_key_control_l = u32(0xffe3)
	const xkb_key_control_r = u32(0xffe4)
	const xkb_key_caps_lock = u32(0xffe5)
	const xkb_key_alt_l = u32(0xffe9)
	const xkb_key_alt_r = u32(0xffea)
	const xkb_key_super_l = u32(0xffeb)
	const xkb_key_delete = u32(0xffff)

	// === Key translation ===

	fn wl_translate_key(keysym Xkb_keysym_t) KeyCode {
		key := linux_translate_navigation_or_keypad_keysym(u32(keysym))
		if key != .invalid {
			return key
		}
		return match keysym {
			xkb_key_space { .space }
			xkb_key_apostrophe { .apostrophe }
			xkb_key_comma { .comma }
			xkb_key_minus { .minus }
			xkb_key_period { .period }
			xkb_key_slash { .slash }
			xkb_key_0 { ._0 }
			xkb_key_1 { ._1 }
			xkb_key_2 { ._2 }
			xkb_key_3 { ._3 }
			xkb_key_4 { ._4 }
			xkb_key_5 { ._5 }
			xkb_key_6 { ._6 }
			xkb_key_7 { ._7 }
			xkb_key_8 { ._8 }
			xkb_key_9 { ._9 }
			xkb_key_semicolon { .semicolon }
			xkb_key_equal { .equal }
			xkb_key_a { .a }
			xkb_key_b { .b }
			xkb_key_c { .c }
			xkb_key_d { .d }
			xkb_key_e { .e }
			xkb_key_f { .f }
			xkb_key_g { .g }
			xkb_key_h { .h }
			xkb_key_i { .i }
			xkb_key_j { .j }
			xkb_key_k { .k }
			xkb_key_l { .l }
			xkb_key_m { .m }
			xkb_key_n { .n }
			xkb_key_o { .o }
			xkb_key_p { .p }
			xkb_key_q { .q }
			xkb_key_r { .r }
			xkb_key_s { .s }
			xkb_key_t { .t }
			xkb_key_u { .u }
			xkb_key_v { .v }
			xkb_key_w { .w }
			xkb_key_x { .x }
			xkb_key_y { .y }
			xkb_key_z { .z }
			xkb_key_bracketleft { .left_bracket }
			xkb_key_backslash { .backslash }
			xkb_key_bracketright { .right_bracket }
			xkb_key_grave { .grave_accent }
			xkb_key_escape { .escape }
			xkb_key_return { .enter }
			xkb_key_tab { .tab }
			xkb_key_backspace { .backspace }
			xkb_key_caps_lock { .caps_lock }
			xkb_key_scroll_lock { .scroll_lock }
			xkb_key_num_lock { .num_lock }
			xkb_key_print { .print_screen }
			xkb_key_pause { .pause }
			xkb_key_f1 { .f1 }
			xkb_key_f2 { .f2 }
			xkb_key_f3 { .f3 }
			xkb_key_f4 { .f4 }
			xkb_key_f5 { .f5 }
			xkb_key_f6 { .f6 }
			xkb_key_f7 { .f7 }
			xkb_key_f8 { .f8 }
			xkb_key_f9 { .f9 }
			xkb_key_f10 { .f10 }
			xkb_key_f11 { .f11 }
			xkb_key_f12 { .f12 }
			xkb_key_f13 { .f13 }
			xkb_key_f14 { .f14 }
			xkb_key_f15 { .f15 }
			xkb_key_f16 { .f16 }
			xkb_key_f17 { .f17 }
			xkb_key_f18 { .f18 }
			xkb_key_f19 { .f19 }
			xkb_key_f20 { .f20 }
			xkb_key_f21 { .f21 }
			xkb_key_f22 { .f22 }
			xkb_key_f23 { .f23 }
			xkb_key_f24 { .f24 }
			xkb_key_f25 { .f25 }
			xkb_key_shift_l { .left_shift }
			xkb_key_control_l { .left_control }
			xkb_key_alt_l { .left_alt }
			xkb_key_super_l { .left_super }
			xkb_key_shift_r { .right_shift }
			xkb_key_control_r { .right_control }
			xkb_key_alt_r { .right_alt }
			xkb_key_menu { .menu }
			else { .invalid }
		}
	}

	// === Modifier helpers ===

	fn wl_get_modifiers() u32 {
		mut mods := u32(0)
		if g_sapp_state.wl.xkb_state != unsafe { nil } {
			if C.xkb_state_mod_name_is_active(g_sapp_state.wl.xkb_state, xkb_mod_name_shift,
				xkb_state_mods_effective) > 0 {
				mods |= u32(Modifier.shift)
			}
			if C.xkb_state_mod_name_is_active(g_sapp_state.wl.xkb_state, xkb_mod_name_ctrl,
				xkb_state_mods_effective) > 0 {
				mods |= u32(Modifier.ctrl)
			}
			if C.xkb_state_mod_name_is_active(g_sapp_state.wl.xkb_state, xkb_mod_name_alt,
				xkb_state_mods_effective) > 0 {
				mods |= u32(Modifier.alt)
			}
			if C.xkb_state_mod_name_is_active(g_sapp_state.wl.xkb_state, xkb_mod_name_logo,
				xkb_state_mods_effective) > 0 {
				mods |= u32(Modifier.super)
			}
		}
		return mods
	}

	fn wl_app_event(etype EventType) {
		if g_sapp_state.desc.event_cb != unsafe { nil }
			|| g_sapp_state.desc.event_userdata_cb != unsafe { nil } {
			init_sapp_event(etype)
			call_sapp_event(&g_sapp_state.event)
		}
	}

	// === UTF-8 decoding ===

	fn utf8_decode(buf &u8, count int) u32 {
		if count <= 0 {
			return 0
		}
		b0 := unsafe { buf[0] }
		if (b0 & 0x80) == 0 {
			return u32(b0)
		} else if (b0 & 0xE0) == 0xC0 && count >= 2 {
			return (u32(b0 & 0x1F) << 6) | u32(unsafe { buf[1] } & 0x3F)
		} else if (b0 & 0xF0) == 0xE0 && count >= 3 {
			return (u32(b0 & 0x0F) << 12) | (u32(unsafe { buf[1] } & 0x3F) << 6) | u32(unsafe { buf[2] } & 0x3F)
		} else if (b0 & 0xF8) == 0xF0 && count >= 4 {
			return (u32(b0 & 0x07) << 18) | (u32(unsafe { buf[1] } & 0x3F) << 12) | (u32(unsafe { buf[2] } & 0x3F) << 6) | u32(unsafe { buf[3] } & 0x3F)
		}
		return 0
	}

	// === Key repeat ===

	fn wl_setup_key_repeat_timer() {
		if g_sapp_state.wl.key_repeat_timer_fd >= 0 {
			C.close(g_sapp_state.wl.key_repeat_timer_fd)
		}
		g_sapp_state.wl.key_repeat_timer_fd = C.timerfd_create(clock_monotonic,
			tfd_cloexec | tfd_nonblock)
	}

	fn wl_start_key_repeat(scancode u32) {
		if g_sapp_state.wl.key_repeat_timer_fd < 0 || g_sapp_state.wl.key_repeat_rate <= 0 {
			return
		}
		g_sapp_state.wl.key_repeat_keycode = scancode

		delay_ms := g_sapp_state.wl.key_repeat_delay
		rate := g_sapp_state.wl.key_repeat_rate
		repeat_interval_ns := i64(1000000000) / i64(rate)

		timer := C.itimerspec{
			it_value:    C.timespec{
				tv_sec:  i64(delay_ms / 1000)
				tv_nsec: i64((delay_ms % 1000)) * 1000000
			}
			it_interval: C.timespec{
				tv_sec:  repeat_interval_ns / i64(1000000000)
				tv_nsec: repeat_interval_ns % i64(1000000000)
			}
		}
		C.timerfd_settime(g_sapp_state.wl.key_repeat_timer_fd, 0, &timer, unsafe { nil })
	}

	fn wl_stop_key_repeat() {
		if g_sapp_state.wl.key_repeat_timer_fd < 0 {
			return
		}
		timer := C.itimerspec{}
		C.timerfd_settime(g_sapp_state.wl.key_repeat_timer_fd, 0, &timer, unsafe { nil })
		g_sapp_state.wl.key_repeat_keycode = 0
	}

	fn wl_handle_key_repeat() {
		if g_sapp_state.wl.key_repeat_timer_fd < 0 || g_sapp_state.wl.key_repeat_keycode == 0 {
			return
		}
		mut expirations := u64(0)
		if C.read(g_sapp_state.wl.key_repeat_timer_fd, &expirations, sizeof(expirations)) > 0
			&& expirations > 0 {
			if g_sapp_state.wl.xkb_state == unsafe { nil } {
				return
			}
			keysym := C.xkb_state_key_get_one_sym(g_sapp_state.wl.xkb_state,
				g_sapp_state.wl.key_repeat_keycode)
			sapp_key := wl_translate_key(keysym)

			if sapp_key != .invalid {
				init_sapp_event(.key_down)
				g_sapp_state.event.key_code = sapp_key
				g_sapp_state.event.key_repeat = true
				g_sapp_state.event.modifiers = wl_get_modifiers()
				call_sapp_event(&g_sapp_state.event)
			}

			mut buf := [8]u8{}
			count := C.xkb_state_key_get_utf8(g_sapp_state.wl.xkb_state,
				g_sapp_state.wl.key_repeat_keycode, &char(&buf[0]), usize(buf.len))
			if count > 0 && count < buf.len {
				codepoint := utf8_decode(&buf[0], count)
				if codepoint > 0 && codepoint < 0x110000 {
					init_sapp_event(.char)
					g_sapp_state.event.char_code = codepoint
					g_sapp_state.event.key_repeat = true
					g_sapp_state.event.modifiers = wl_get_modifiers()
					call_sapp_event(&g_sapp_state.event)
				}
			}
		}
	}

	// === Keyboard listener callbacks ===

	fn wl_keyboard_handle_keymap(data voidptr, keyboard &C.wl_keyboard, format u32, fd int, size u32) {
		if format != wl_keyboard_keymap_format_xkb_v1 {
			C.close(fd)
			return
		}
		map_str := C.mmap(unsafe { nil }, usize(size), prot_read, map_private, fd, 0)
		if map_str == map_failed {
			C.close(fd)
			return
		}
		if g_sapp_state.wl.xkb_keymap != unsafe { nil } {
			C.xkb_keymap_unref(g_sapp_state.wl.xkb_keymap)
		}
		g_sapp_state.wl.xkb_keymap = C.xkb_keymap_new_from_string(g_sapp_state.wl.xkb_context,
			map_str, xkb_keymap_format_text_v1, xkb_keymap_compile_no_flags)
		C.munmap(map_str, usize(size))
		C.close(fd)

		if g_sapp_state.wl.xkb_keymap == unsafe { nil } {
			return
		}
		if g_sapp_state.wl.xkb_state != unsafe { nil } {
			C.xkb_state_unref(g_sapp_state.wl.xkb_state)
		}
		g_sapp_state.wl.xkb_state = C.xkb_state_new(g_sapp_state.wl.xkb_keymap)
	}

	fn wl_keyboard_handle_enter(data voidptr, keyboard &C.wl_keyboard, serial u32, surface &C.wl_surface, keys &C.wl_array) {
		g_sapp_state.wl.focused = true
		wl_app_event(.focused)
	}

	fn wl_keyboard_handle_leave(data voidptr, keyboard &C.wl_keyboard, serial u32, surface &C.wl_surface) {
		g_sapp_state.wl.focused = false
		wl_app_event(.unfocused)
	}

	fn wl_keyboard_handle_key(data voidptr, keyboard &C.wl_keyboard, serial u32, time u32, key u32, state u32) {
		if g_sapp_state.wl.xkb_state == unsafe { nil } {
			return
		}
		// Wayland key codes are Linux input codes + 8
		keycode := key + 8
		keysym := C.xkb_state_key_get_one_sym(g_sapp_state.wl.xkb_state, keycode)
		sapp_key := wl_translate_key(keysym)

		if sapp_key != .invalid {
			etype := if state == wl_keyboard_key_state_pressed {
				EventType.key_down
			} else {
				EventType.key_up
			}
			init_sapp_event(etype)
			g_sapp_state.event.key_code = sapp_key
			g_sapp_state.event.key_repeat = false
			g_sapp_state.event.modifiers = wl_get_modifiers()
			call_sapp_event(&g_sapp_state.event)

			if state == wl_keyboard_key_state_pressed {
				wl_start_key_repeat(keycode)
			} else {
				if g_sapp_state.wl.key_repeat_keycode == keycode {
					wl_stop_key_repeat()
				}
			}
		}

		// Generate CHAR event for key press
		if state == wl_keyboard_key_state_pressed {
			mut buf := [8]u8{}
			count := C.xkb_state_key_get_utf8(g_sapp_state.wl.xkb_state, keycode, &char(&buf[0]),
				usize(buf.len))
			if count > 0 && count < buf.len {
				codepoint := utf8_decode(&buf[0], count)
				if codepoint > 0 && codepoint < 0x110000 {
					init_sapp_event(.char)
					g_sapp_state.event.char_code = codepoint
					g_sapp_state.event.key_repeat = false
					g_sapp_state.event.modifiers = wl_get_modifiers()
					call_sapp_event(&g_sapp_state.event)
				}
			}
		}
	}

	fn wl_keyboard_handle_modifiers(data voidptr, keyboard &C.wl_keyboard, serial u32, mods_depressed u32, mods_latched u32, mods_locked u32, group u32) {
		if g_sapp_state.wl.xkb_state != unsafe { nil } {
			C.xkb_state_update_mask(g_sapp_state.wl.xkb_state, mods_depressed, mods_latched,
				mods_locked, 0, 0, group)
		}
	}

	fn wl_keyboard_handle_repeat_info(data voidptr, keyboard &C.wl_keyboard, rate int, delay int) {
		g_sapp_state.wl.key_repeat_rate = rate
		g_sapp_state.wl.key_repeat_delay = delay
	}

	// === Pointer listener callbacks ===

	fn wl_pointer_handle_enter(data voidptr, pointer &C.wl_pointer, serial u32, surface &C.wl_surface, sx i32, sy i32) {
		mut x := f32(C.wl_fixed_to_double(sx))
		mut y := f32(C.wl_fixed_to_double(sy))
		if g_sapp_state.wl.viewport != unsafe { nil } && g_sapp_state.wl.scale > 1.0 {
			x *= g_sapp_state.wl.scale
			y *= g_sapp_state.wl.scale
		}
		g_sapp_state.mouse.x = x
		g_sapp_state.mouse.y = y
		g_sapp_state.wl.pointer_enter_serial = serial

		init_sapp_event(.mouse_enter)
		g_sapp_state.event.mouse_x = g_sapp_state.mouse.x
		g_sapp_state.event.mouse_y = g_sapp_state.mouse.y
		call_sapp_event(&g_sapp_state.event)
	}

	fn wl_pointer_handle_leave(data voidptr, pointer &C.wl_pointer, serial u32, surface &C.wl_surface) {
		init_sapp_event(.mouse_leave)
		call_sapp_event(&g_sapp_state.event)
	}

	fn wl_pointer_handle_motion(data voidptr, pointer &C.wl_pointer, time u32, sx i32, sy i32) {
		mut new_x := f32(C.wl_fixed_to_double(sx))
		mut new_y := f32(C.wl_fixed_to_double(sy))
		if g_sapp_state.wl.viewport != unsafe { nil } && g_sapp_state.wl.scale > 1.0 {
			new_x *= g_sapp_state.wl.scale
			new_y *= g_sapp_state.wl.scale
		}
		g_sapp_state.mouse.dx = new_x - g_sapp_state.mouse.x
		g_sapp_state.mouse.dy = new_y - g_sapp_state.mouse.y
		g_sapp_state.mouse.x = new_x
		g_sapp_state.mouse.y = new_y

		init_sapp_event(.mouse_move)
		g_sapp_state.event.mouse_x = g_sapp_state.mouse.x
		g_sapp_state.event.mouse_y = g_sapp_state.mouse.y
		g_sapp_state.event.mouse_dx = g_sapp_state.mouse.dx
		g_sapp_state.event.mouse_dy = g_sapp_state.mouse.dy
		call_sapp_event(&g_sapp_state.event)
	}

	fn wl_pointer_handle_button(data voidptr, pointer &C.wl_pointer, serial u32, time u32, button u32, state u32) {
		sapp_btn := match button {
			u32(btn_left) { MouseButton.left }
			u32(btn_right) { MouseButton.right }
			u32(btn_middle) { MouseButton.middle }
			else { MouseButton.invalid }
		}

		if sapp_btn != .invalid {
			etype := if state == wl_pointer_button_state_pressed {
				EventType.mouse_down
			} else {
				EventType.mouse_up
			}
			init_sapp_event(etype)
			g_sapp_state.event.mouse_button = sapp_btn
			g_sapp_state.event.modifiers = wl_get_modifiers()
			call_sapp_event(&g_sapp_state.event)
		}
	}

	fn wl_pointer_handle_axis(data voidptr, pointer &C.wl_pointer, time u32, axis u32, value i32) {
		scroll_value := f32(C.wl_fixed_to_double(value))
		init_sapp_event(.mouse_scroll)
		if axis == wl_pointer_axis_vertical_scroll {
			g_sapp_state.event.scroll_y = -scroll_value / 10.0
		} else if axis == wl_pointer_axis_horizontal_scroll {
			g_sapp_state.event.scroll_x = scroll_value / 10.0
		}
		g_sapp_state.event.modifiers = wl_get_modifiers()
		call_sapp_event(&g_sapp_state.event)
	}

	fn wl_pointer_handle_frame(data voidptr, pointer &C.wl_pointer) {}

	fn wl_pointer_handle_axis_source(data voidptr, pointer &C.wl_pointer, axis_source u32) {}

	fn wl_pointer_handle_axis_stop(data voidptr, pointer &C.wl_pointer, time u32, axis u32) {}

	fn wl_pointer_handle_axis_discrete(data voidptr, pointer &C.wl_pointer, axis u32, discrete i32) {}

	fn wl_pointer_handle_axis_value120(data voidptr, pointer &C.wl_pointer, axis u32, value120 i32) {}

	fn wl_pointer_handle_axis_relative_direction(data voidptr, pointer &C.wl_pointer, axis u32, direction u32) {}

	// === Seat listener callbacks ===

	fn wl_seat_handle_capabilities(data voidptr, seat &C.wl_seat, caps u32) {
		// Keyboard
		if (caps & wl_seat_capability_keyboard) != 0 && g_sapp_state.wl.keyboard == unsafe { nil } {
			g_sapp_state.wl.keyboard = C.wl_seat_get_keyboard(seat)
			C.wl_keyboard_add_listener(g_sapp_state.wl.keyboard, &wl_keyboard_listener,
				unsafe { nil })
		} else if (caps & wl_seat_capability_keyboard) == 0
			&& g_sapp_state.wl.keyboard != unsafe { nil } {
			C.wl_keyboard_destroy(g_sapp_state.wl.keyboard)
			g_sapp_state.wl.keyboard = unsafe { nil }
		}

		// Pointer
		if (caps & wl_seat_capability_pointer) != 0 && g_sapp_state.wl.pointer == unsafe { nil } {
			g_sapp_state.wl.pointer = C.wl_seat_get_pointer(seat)
			C.wl_pointer_add_listener(g_sapp_state.wl.pointer, &wl_pointer_listener, unsafe { nil })

			if g_sapp_state.wl.cursor_shape_manager != unsafe { nil }
				&& g_sapp_state.wl.cursor_shape_device == unsafe { nil } {
				g_sapp_state.wl.cursor_shape_device = C.wp_cursor_shape_manager_v1_get_pointer(g_sapp_state.wl.cursor_shape_manager,
					g_sapp_state.wl.pointer)
			}
			if g_sapp_state.wl.relative_pointer_mgr != unsafe { nil }
				&& g_sapp_state.wl.relative_pointer == unsafe { nil } {
				g_sapp_state.wl.relative_pointer = C.zwp_relative_pointer_manager_v1_get_relative_pointer(g_sapp_state.wl.relative_pointer_mgr,
					g_sapp_state.wl.pointer)
			}
			if g_sapp_state.wl.data_device_manager != unsafe { nil }
				&& g_sapp_state.wl.data_device == unsafe { nil } {
				g_sapp_state.wl.data_device =
					C.wl_data_device_manager_get_data_device(g_sapp_state.wl.data_device_manager, seat)
				C.wl_data_device_add_listener(g_sapp_state.wl.data_device,
					&wl_data_device_listener, unsafe { nil })
			}
		} else if (caps & wl_seat_capability_pointer) == 0
			&& g_sapp_state.wl.pointer != unsafe { nil } {
			if g_sapp_state.wl.cursor_shape_device != unsafe { nil } {
				C.wp_cursor_shape_device_v1_destroy(g_sapp_state.wl.cursor_shape_device)
				g_sapp_state.wl.cursor_shape_device = unsafe { nil }
			}
			if g_sapp_state.wl.relative_pointer != unsafe { nil } {
				C.zwp_relative_pointer_v1_destroy(g_sapp_state.wl.relative_pointer)
				g_sapp_state.wl.relative_pointer = unsafe { nil }
			}
			if g_sapp_state.wl.data_device != unsafe { nil } {
				C.wl_data_device_destroy(g_sapp_state.wl.data_device)
				g_sapp_state.wl.data_device = unsafe { nil }
			}
			C.wl_pointer_destroy(g_sapp_state.wl.pointer)
			g_sapp_state.wl.pointer = unsafe { nil }
		}

		// Touch
		if (caps & wl_seat_capability_touch) != 0 && g_sapp_state.wl.touch == unsafe { nil } {
			g_sapp_state.wl.touch = C.wl_seat_get_touch(seat)
		} else if (caps & wl_seat_capability_touch) == 0 && g_sapp_state.wl.touch != unsafe { nil } {
			C.wl_touch_destroy(g_sapp_state.wl.touch)
			g_sapp_state.wl.touch = unsafe { nil }
		}
	}

	fn wl_seat_handle_name(data voidptr, seat &C.wl_seat, name &char) {
	}

	// === Registry listener callbacks ===

	fn wl_registry_handle_global(data voidptr, registry &C.wl_registry, name u32, iface &char, version u32) {
		unsafe {
			if C.strcmp(iface, c'wl_compositor') == 0 {
				g_sapp_state.wl.compositor = &C.wl_compositor(C.wl_registry_bind(registry, name,
					&C.wl_compositor_interface, 4))
			} else if C.strcmp(iface, c'xdg_wm_base') == 0 {
				g_sapp_state.wl.xdg_wm_base = &C.xdg_wm_base(C.wl_registry_bind(registry, name,
					&C.xdg_wm_base_interface, 1))
			} else if C.strcmp(iface, c'wl_seat') == 0 {
				g_sapp_state.wl.seat = &C.wl_seat(C.wl_registry_bind(registry, name,
					&C.wl_seat_interface, 5))
				C.wl_seat_add_listener(g_sapp_state.wl.seat, &wl_seat_listener, nil)
			} else if C.strcmp(iface, c'wl_shm') == 0 {
				g_sapp_state.wl.shm = &C.wl_shm(C.wl_registry_bind(registry, name,
					&C.wl_shm_interface, 1))
			} else if C.strcmp(iface, c'wl_data_device_manager') == 0 {
				g_sapp_state.wl.data_device_manager = &C.wl_data_device_manager(C.wl_registry_bind(registry,
					name, &C.wl_data_device_manager_interface, 3))
			} else if C.strcmp(iface, c'wp_fractional_scale_manager_v1') == 0 {
				g_sapp_state.wl.fractional_scale_mgr = &C.wp_fractional_scale_manager_v1(C.wl_registry_bind(registry,
					name, &C.wp_fractional_scale_manager_v1_interface, 1))
			} else if C.strcmp(iface, c'wp_viewporter') == 0 {
				g_sapp_state.wl.viewporter = &C.wp_viewporter(C.wl_registry_bind(registry, name,
					&C.wp_viewporter_interface, 1))
			} else if C.strcmp(iface, c'wp_cursor_shape_manager_v1') == 0 {
				g_sapp_state.wl.cursor_shape_manager = &C.wp_cursor_shape_manager_v1(C.wl_registry_bind(registry,
					name, &C.wp_cursor_shape_manager_v1_interface, 1))
			} else if C.strcmp(iface, c'zwp_pointer_constraints_v1') == 0 {
				g_sapp_state.wl.pointer_constraints = &C.zwp_pointer_constraints_v1(C.wl_registry_bind(registry,
					name, &C.zwp_pointer_constraints_v1_interface, 1))
			} else if C.strcmp(iface, c'zwp_relative_pointer_manager_v1') == 0 {
				g_sapp_state.wl.relative_pointer_mgr = &C.zwp_relative_pointer_manager_v1(C.wl_registry_bind(registry,
					name, &C.zwp_relative_pointer_manager_v1_interface, 1))
			} else if C.strcmp(iface, c'zxdg_decoration_manager_v1') == 0 {
				g_sapp_state.wl.decoration_manager = &C.zxdg_decoration_manager_v1(C.wl_registry_bind(registry,
					name, &C.zxdg_decoration_manager_v1_interface, 1))
			}
		}
	}

	fn wl_registry_handle_global_remove(data voidptr, registry &C.wl_registry, name u32) {
	}

	// === XDG surface / toplevel callbacks ===

	fn wl_xdg_surface_configure(data voidptr, xdg_surface &C.xdg_surface, serial u32) {
		C.xdg_surface_ack_configure(xdg_surface, serial)
	}

	fn wl_xdg_toplevel_configure(data voidptr, xdg_toplevel &C.xdg_toplevel, width int, height int, states &C.wl_array) {
		// Iterate wl_array of uint32_t states
		mut is_fullscreen := false
		mut is_maximized := false
		if states.size > 0 {
			state_ptr := &u32(states.data)
			num_states := int(states.size / sizeof(u32))
			for i in 0 .. num_states {
				s := unsafe { state_ptr[i] }
				if s == xdg_toplevel_state_fullscreen {
					is_fullscreen = true
				}
				if s == xdg_toplevel_state_maximized {
					is_maximized = true
				}
			}
		}
		g_sapp_state.fullscreen = is_fullscreen
		_ = is_maximized

		if width > 0 && height > 0 {
			size_changed := g_sapp_state.wl.width != width || g_sapp_state.wl.height != height

			g_sapp_state.wl.width = width
			g_sapp_state.wl.height = height

			// Apply fractional scale if available and high_dpi is enabled
			if g_sapp_state.wl.scale_numerator > 0 && g_sapp_state.desc.high_dpi {
				g_sapp_state.wl.scale = f32(g_sapp_state.wl.scale_numerator) / 120.0
				g_sapp_state.dpi_scale = if g_sapp_state.wl.viewport != unsafe { nil } {
					f32(1.0)
				} else {
					g_sapp_state.wl.scale
				}
			}

			g_sapp_state.wl.fb_width = int(f32(g_sapp_state.wl.width) * g_sapp_state.wl.scale)
			g_sapp_state.wl.fb_height = int(f32(g_sapp_state.wl.height) * g_sapp_state.wl.scale)

			g_sapp_state.window_width = width
			g_sapp_state.window_height = height
			g_sapp_state.framebuffer_width = g_sapp_state.wl.fb_width
			g_sapp_state.framebuffer_height = g_sapp_state.wl.fb_height

			if g_sapp_state.wl.egl_window != unsafe { nil } {
				C.wl_egl_window_resize(g_sapp_state.wl.egl_window, g_sapp_state.wl.fb_width,
					g_sapp_state.wl.fb_height, 0, 0)
			}
			if g_sapp_state.wl.viewport != unsafe { nil } {
				C.wp_viewport_set_destination(g_sapp_state.wl.viewport, width, height)
			}
			if size_changed && !g_sapp_state.first_frame {
				wl_app_event(.resized)
			}
		}
	}

	fn wl_xdg_toplevel_close(data voidptr, xdg_toplevel &C.xdg_toplevel) {
		g_sapp_state.quit_requested = true
	}

	// === Fractional scale callback ===

	fn wl_fractional_scale_preferred_scale(data voidptr, fractional_scale &C.wp_fractional_scale_v1, scale u32) {
		g_sapp_state.wl.scale_numerator = scale

		if g_sapp_state.desc.high_dpi {
			g_sapp_state.wl.scale = f32(scale) / 120.0
			g_sapp_state.dpi_scale = if g_sapp_state.wl.viewport != unsafe { nil } {
				f32(1.0)
			} else {
				g_sapp_state.wl.scale
			}
		} else {
			g_sapp_state.wl.scale = 1.0
			g_sapp_state.dpi_scale = 1.0
		}

		g_sapp_state.wl.fb_width = int(f32(g_sapp_state.wl.width) * g_sapp_state.wl.scale)
		g_sapp_state.wl.fb_height = int(f32(g_sapp_state.wl.height) * g_sapp_state.wl.scale)
		g_sapp_state.framebuffer_width = g_sapp_state.wl.fb_width
		g_sapp_state.framebuffer_height = g_sapp_state.wl.fb_height

		if g_sapp_state.wl.egl_window != unsafe { nil } {
			C.wl_egl_window_resize(g_sapp_state.wl.egl_window, g_sapp_state.wl.fb_width,
				g_sapp_state.wl.fb_height, 0, 0)
		}
		if g_sapp_state.wl.viewport != unsafe { nil } {
			C.wp_viewport_set_destination(g_sapp_state.wl.viewport, g_sapp_state.wl.width,
				g_sapp_state.wl.height)
		}
	}

	// === Data device callbacks (drag and drop) ===

	fn wl_data_offer_offer(data voidptr, offer &C.wl_data_offer, mime_type &char) {
		if C.strcmp(mime_type, c'text/uri-list') == 0 {
			g_sapp_state.wl.data_offer = unsafe { offer }
		}
	}

	fn wl_data_offer_source_actions(data voidptr, offer &C.wl_data_offer, source_actions u32) {
	}

	fn wl_data_offer_action(data voidptr, offer &C.wl_data_offer, dnd_action u32) {
	}

	fn wl_data_device_data_offer(data voidptr, device &C.wl_data_device, offer &C.wl_data_offer) {
		C.wl_data_offer_add_listener(offer, &wl_data_offer_listener, unsafe { nil })
	}

	fn wl_data_device_enter(data voidptr, device &C.wl_data_device, serial u32, surface &C.wl_surface, x i32, y i32, offer &C.wl_data_offer) {
		if offer != unsafe { nil } && g_sapp_state.wl.data_offer == offer {
			C.wl_data_offer_accept(offer, serial, c'text/uri-list')
			C.wl_data_offer_set_actions(offer, wl_data_device_manager_dnd_action_copy,
				wl_data_device_manager_dnd_action_copy)
		}
	}

	fn wl_data_device_leave(data voidptr, device &C.wl_data_device) {
		if g_sapp_state.wl.data_offer != unsafe { nil } {
			C.wl_data_offer_destroy(g_sapp_state.wl.data_offer)
			g_sapp_state.wl.data_offer = unsafe { nil }
		}
	}

	fn wl_data_device_motion(data voidptr, device &C.wl_data_device, time u32, x i32, y i32) {
	}

	fn wl_data_device_drop(data voidptr, device &C.wl_data_device) {
		if g_sapp_state.wl.data_offer == unsafe { nil } {
			return
		}
		mut fds := [2]int{}
		if C.pipe(&fds[0]) == -1 {
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
			unsafe {
				buffer[total_read] = 0
			}
			// Clear drop buffer
			if g_sapp_state.drop.buffer != unsafe { nil } {
				unsafe { C.memset(g_sapp_state.drop.buffer, 0, usize(g_sapp_state.drop.buf_size)) }
			}
			g_sapp_state.drop.num_files = 0

			mut line := C.strtok(&char(&buffer[0]), c'\r\n')
			for line != unsafe { nil } && g_sapp_state.drop.num_files < g_sapp_state.drop.max_files {
				if C.strlen(line) == 0 {
					line = C.strtok(unsafe { nil }, c'\r\n')
					continue
				}
				mut file_path := line
				if C.strncmp(line, c'file://', 7) == 0 {
					file_path = unsafe { line + 7 }
				}
				// URL decode into drop buffer
				mut dst := sapp_dropped_file_path_ptr(g_sapp_state.drop.num_files)
				dst_end := unsafe { dst + (g_sapp_state.drop.max_path_length - 1) }
				mut i := usize(0)
				for unsafe { file_path[i] } != 0 && dst < dst_end {
					ch := unsafe { u8(file_path[i]) }
					if ch == `%` && unsafe { file_path[i + 1] } != 0
						&& unsafe { file_path[i + 2] } != 0 {
						mut hex := [3]u8{}
						hex[0] = unsafe { u8(file_path[i + 1]) }
						hex[1] = unsafe { u8(file_path[i + 2]) }
						hex[2] = 0
						unsafe {
							*dst = u8(C.strtol(&char(&hex[0]), nil, 16))
						}
						dst = unsafe { dst + 1 }
						i += 3
					} else {
						unsafe {
							*dst = ch
						}
						dst = unsafe { dst + 1 }
						i++
					}
				}
				unsafe {
					*dst = 0
				}
				g_sapp_state.drop.num_files++
				line = C.strtok(unsafe { nil }, c'\r\n')
			}

			if g_sapp_state.drop.num_files > 0 {
				C.wl_data_offer_finish(g_sapp_state.wl.data_offer)
				init_sapp_event(.files_dropped)
				g_sapp_state.event.modifiers = wl_get_modifiers()
				call_sapp_event(&g_sapp_state.event)
			}
		}

		C.wl_data_offer_destroy(g_sapp_state.wl.data_offer)
		g_sapp_state.wl.data_offer = unsafe { nil }
	}

	fn wl_data_device_selection(data voidptr, device &C.wl_data_device, offer &C.wl_data_offer) {
	}

	// === Listener struct definitions ===
	// These are arrays of function pointers matching the C listener struct layouts.

	const wl_keyboard_listener = [
		voidptr(wl_keyboard_handle_keymap),
		voidptr(wl_keyboard_handle_enter),
		voidptr(wl_keyboard_handle_leave),
		voidptr(wl_keyboard_handle_key),
		voidptr(wl_keyboard_handle_modifiers),
		voidptr(wl_keyboard_handle_repeat_info),
	]!

	const wl_pointer_listener = [
		voidptr(wl_pointer_handle_enter),
		voidptr(wl_pointer_handle_leave),
		voidptr(wl_pointer_handle_motion),
		voidptr(wl_pointer_handle_button),
		voidptr(wl_pointer_handle_axis),
		voidptr(wl_pointer_handle_frame),
		voidptr(wl_pointer_handle_axis_source),
		voidptr(wl_pointer_handle_axis_stop),
		voidptr(wl_pointer_handle_axis_discrete),
		voidptr(wl_pointer_handle_axis_value120),
		voidptr(wl_pointer_handle_axis_relative_direction),
	]!

	const wl_seat_listener = [
		voidptr(wl_seat_handle_capabilities),
		voidptr(wl_seat_handle_name),
	]!

	const wl_registry_listener = [
		voidptr(wl_registry_handle_global),
		voidptr(wl_registry_handle_global_remove),
	]!

	const wl_xdg_surface_listener = [
		voidptr(wl_xdg_surface_configure),
	]!

	const wl_xdg_toplevel_listener = [
		voidptr(wl_xdg_toplevel_configure),
		voidptr(wl_xdg_toplevel_close),
	]!

	const wl_fractional_scale_listener = [
		voidptr(wl_fractional_scale_preferred_scale),
	]!

	const wl_data_offer_listener = [
		voidptr(wl_data_offer_offer),
		voidptr(wl_data_offer_source_actions),
		voidptr(wl_data_offer_action),
	]!

	const wl_data_device_listener = [
		voidptr(wl_data_device_data_offer),
		voidptr(wl_data_device_enter),
		voidptr(wl_data_device_leave),
		voidptr(wl_data_device_motion),
		voidptr(wl_data_device_drop),
		voidptr(wl_data_device_selection),
	]!
	// === Main Wayland run function ===

	fn wl_set_resizable(resizable bool) {
		if g_sapp_state.wl.xdg_toplevel == unsafe { nil }
			|| g_sapp_state.wl.surface == unsafe { nil } {
			return
		}
		if resizable {
			C.xdg_toplevel_set_min_size(g_sapp_state.wl.xdg_toplevel, 0, 0)
			C.xdg_toplevel_set_max_size(g_sapp_state.wl.xdg_toplevel, 0, 0)
		} else {
			width := if g_sapp_state.window_width > 0 {
				g_sapp_state.window_width
			} else if g_sapp_state.wl.width > 0 {
				g_sapp_state.wl.width
			} else {
				fallback_default_window_width
			}
			height := if g_sapp_state.window_height > 0 {
				g_sapp_state.window_height
			} else if g_sapp_state.wl.height > 0 {
				g_sapp_state.wl.height
			} else {
				fallback_default_window_height
			}
			C.xdg_toplevel_set_min_size(g_sapp_state.wl.xdg_toplevel, i32(width), i32(height))
			C.xdg_toplevel_set_max_size(g_sapp_state.wl.xdg_toplevel, i32(width), i32(height))
		}
		C.wl_surface_commit(g_sapp_state.wl.surface)
		if g_sapp_state.wl.display != unsafe { nil } {
			C.wl_display_flush(g_sapp_state.wl.display)
		}
	}

	pub fn wl_run(desc &Desc) {
		sapp_init_state(desc)

		// Initialize key repeat
		g_sapp_state.wl.key_repeat_timer_fd = -1
		g_sapp_state.wl.key_repeat_rate = 25
		g_sapp_state.wl.key_repeat_delay = 600

		// Connect to Wayland display
		g_sapp_state.wl.display = C.wl_display_connect(unsafe { nil })
		if g_sapp_state.wl.display == unsafe { nil } {
			eprintln('sokol_app: failed to connect to Wayland display')
			return
		}

		// Get registry and bind global objects
		g_sapp_state.wl.registry = C.wl_display_get_registry(g_sapp_state.wl.display)
		C.wl_registry_add_listener(g_sapp_state.wl.registry, &wl_registry_listener, unsafe { nil })
		C.wl_display_roundtrip(g_sapp_state.wl.display)

		if g_sapp_state.wl.compositor == unsafe { nil }
			|| g_sapp_state.wl.xdg_wm_base == unsafe { nil } {
			eprintln('sokol_app: Wayland compositor or xdg_wm_base not available')
			C.wl_display_disconnect(g_sapp_state.wl.display)
			return
		}

		// Create surface
		g_sapp_state.wl.surface = C.wl_compositor_create_surface(g_sapp_state.wl.compositor)

		// Initialize XKB
		g_sapp_state.wl.xkb_context = C.xkb_context_new(xkb_context_no_flags)
		if g_sapp_state.wl.xkb_context == unsafe { nil } {
			eprintln('sokol_app: failed to create XKB context')
			return
		}

		// Setup key repeat timer
		wl_setup_key_repeat_timer()

		g_sapp_state.wl.width = g_sapp_state.window_width
		g_sapp_state.wl.height = g_sapp_state.window_height
		if g_sapp_state.wl.width == 0 {
			g_sapp_state.wl.width = fallback_default_window_width
		}
		if g_sapp_state.wl.height == 0 {
			g_sapp_state.wl.height = fallback_default_window_height
		}

		g_sapp_state.wl.scale = 1.0
		g_sapp_state.dpi_scale = 1.0
		g_sapp_state.wl.fb_width = int(f32(g_sapp_state.wl.width) * g_sapp_state.wl.scale)
		g_sapp_state.wl.fb_height = int(f32(g_sapp_state.wl.height) * g_sapp_state.wl.scale)

		// Create EGL window
		g_sapp_state.wl.egl_window = C.wl_egl_window_create(g_sapp_state.wl.surface,
			g_sapp_state.wl.fb_width, g_sapp_state.wl.fb_height)
		if g_sapp_state.wl.egl_window == unsafe { nil } {
			eprintln('sokol_app: failed to create Wayland EGL window')
			return
		}

		// Create XDG surface and toplevel
		g_sapp_state.wl.xdg_surface = C.xdg_wm_base_get_xdg_surface(g_sapp_state.wl.xdg_wm_base,
			g_sapp_state.wl.surface)
		C.xdg_surface_add_listener(g_sapp_state.wl.xdg_surface, &wl_xdg_surface_listener,
			unsafe { nil })

		g_sapp_state.wl.xdg_toplevel = C.xdg_surface_get_toplevel(g_sapp_state.wl.xdg_surface)
		C.xdg_toplevel_add_listener(g_sapp_state.wl.xdg_toplevel, &wl_xdg_toplevel_listener,
			unsafe { nil })
		C.xdg_toplevel_set_title(g_sapp_state.wl.xdg_toplevel, &char(&g_sapp_state.window_title[0]))

		// Request server-side decorations if available
		if g_sapp_state.wl.decoration_manager != unsafe { nil } {
			g_sapp_state.wl.toplevel_decoration = C.zxdg_decoration_manager_v1_get_toplevel_decoration(g_sapp_state.wl.decoration_manager,
				g_sapp_state.wl.xdg_toplevel)
			if g_sapp_state.wl.toplevel_decoration != unsafe { nil } {
				C.zxdg_toplevel_decoration_v1_set_mode(g_sapp_state.wl.toplevel_decoration,
					zxdg_toplevel_decoration_v1_mode_server_side)
			}
		}

		// Enable fractional scaling if available and high_dpi is enabled
		if g_sapp_state.desc.high_dpi && g_sapp_state.wl.fractional_scale_mgr != unsafe { nil } {
			g_sapp_state.wl.fractional_scale = C.wp_fractional_scale_manager_v1_get_fractional_scale(g_sapp_state.wl.fractional_scale_mgr,
				g_sapp_state.wl.surface)
			if g_sapp_state.wl.fractional_scale != unsafe { nil } {
				C.wp_fractional_scale_v1_add_listener(g_sapp_state.wl.fractional_scale,
					&wl_fractional_scale_listener, unsafe { nil })
			}
		}

		// Create viewport for setting logical size with fractional scaling
		if g_sapp_state.desc.high_dpi && g_sapp_state.wl.viewporter != unsafe { nil } {
			g_sapp_state.wl.viewport = C.wp_viewporter_get_viewport(g_sapp_state.wl.viewporter,
				g_sapp_state.wl.surface)
			if g_sapp_state.wl.viewport != unsafe { nil } {
				C.wp_viewport_set_destination(g_sapp_state.wl.viewport, g_sapp_state.wl.width,
					g_sapp_state.wl.height)
			}
		}

		// Set initial fullscreen state if requested
		if g_sapp_state.desc.fullscreen {
			C.xdg_toplevel_set_fullscreen(g_sapp_state.wl.xdg_toplevel, unsafe { nil })
		}

		// Commit the surface to trigger initial configure
		C.wl_surface_commit(g_sapp_state.wl.surface)
		C.wl_display_roundtrip(g_sapp_state.wl.display)

		// Initialize EGL
		sapp_egl_init_wayland()
		sapp_egl_create_surface(voidptr(g_sapp_state.wl.egl_window))
		sapp_egl_make_current()

		// Update public API values
		g_sapp_state.window_width = g_sapp_state.wl.width
		g_sapp_state.window_height = g_sapp_state.wl.height
		g_sapp_state.framebuffer_width = g_sapp_state.wl.fb_width
		g_sapp_state.framebuffer_height = g_sapp_state.wl.fb_height

		g_sapp_state.valid = true

		// Main event loop
		for !g_sapp_state.quit_ordered {
			C.wl_display_dispatch_pending(g_sapp_state.wl.display)

			wl_handle_key_repeat()

			// Handle quit requested
			if g_sapp_state.quit_requested && !g_sapp_state.quit_ordered {
				wl_app_event(.quit_requested)
				if g_sapp_state.quit_requested {
					g_sapp_state.quit_ordered = true
				}
			}

			sapp_do_frame()

			C.eglSwapBuffers(g_sapp_state.egl.display, g_sapp_state.egl.surface)
			C.wl_display_flush(g_sapp_state.wl.display)
		}

		sapp_call_cleanup()

		// Cleanup
		sapp_egl_destroy()

		if g_sapp_state.wl.egl_window != unsafe { nil } {
			C.wl_egl_window_destroy(g_sapp_state.wl.egl_window)
		}
		if g_sapp_state.wl.toplevel_decoration != unsafe { nil } {
			C.zxdg_toplevel_decoration_v1_destroy(g_sapp_state.wl.toplevel_decoration)
		}
		if g_sapp_state.wl.fractional_scale != unsafe { nil } {
			C.wp_fractional_scale_v1_destroy(g_sapp_state.wl.fractional_scale)
		}
		if g_sapp_state.wl.viewport != unsafe { nil } {
			C.wp_viewport_destroy(g_sapp_state.wl.viewport)
		}
		if g_sapp_state.wl.viewporter != unsafe { nil } {
			C.wp_viewporter_destroy(g_sapp_state.wl.viewporter)
		}
		if g_sapp_state.wl.xdg_toplevel != unsafe { nil } {
			C.xdg_toplevel_destroy(g_sapp_state.wl.xdg_toplevel)
		}
		if g_sapp_state.wl.xdg_surface != unsafe { nil } {
			C.xdg_surface_destroy(g_sapp_state.wl.xdg_surface)
		}
		if g_sapp_state.wl.surface != unsafe { nil } {
			C.wl_surface_destroy(g_sapp_state.wl.surface)
		}
		if g_sapp_state.wl.xkb_state != unsafe { nil } {
			C.xkb_state_unref(g_sapp_state.wl.xkb_state)
		}
		if g_sapp_state.wl.xkb_keymap != unsafe { nil } {
			C.xkb_keymap_unref(g_sapp_state.wl.xkb_keymap)
		}
		if g_sapp_state.wl.xkb_context != unsafe { nil } {
			C.xkb_context_unref(g_sapp_state.wl.xkb_context)
		}
		if g_sapp_state.wl.key_repeat_timer_fd >= 0 {
			C.close(g_sapp_state.wl.key_repeat_timer_fd)
			g_sapp_state.wl.key_repeat_timer_fd = -1
		}
		if g_sapp_state.wl.locked_pointer != unsafe { nil } {
			C.zwp_locked_pointer_v1_destroy(g_sapp_state.wl.locked_pointer)
		}
		if g_sapp_state.wl.relative_pointer != unsafe { nil } {
			C.zwp_relative_pointer_v1_destroy(g_sapp_state.wl.relative_pointer)
		}
		if g_sapp_state.wl.cursor_shape_device != unsafe { nil } {
			C.wp_cursor_shape_device_v1_destroy(g_sapp_state.wl.cursor_shape_device)
		}
		if g_sapp_state.wl.data_device != unsafe { nil } {
			C.wl_data_device_destroy(g_sapp_state.wl.data_device)
		}
		if g_sapp_state.wl.data_offer != unsafe { nil } {
			C.wl_data_offer_destroy(g_sapp_state.wl.data_offer)
		}
		if g_sapp_state.wl.data_source != unsafe { nil } {
			C.wl_data_source_destroy(g_sapp_state.wl.data_source)
		}
		if g_sapp_state.wl.data_device_manager != unsafe { nil } {
			C.wl_data_device_manager_destroy(g_sapp_state.wl.data_device_manager)
		}
		if g_sapp_state.wl.pointer_constraints != unsafe { nil } {
			C.zwp_pointer_constraints_v1_destroy(g_sapp_state.wl.pointer_constraints)
		}
		if g_sapp_state.wl.relative_pointer_mgr != unsafe { nil } {
			C.zwp_relative_pointer_manager_v1_destroy(g_sapp_state.wl.relative_pointer_mgr)
		}
		if g_sapp_state.wl.cursor_shape_manager != unsafe { nil } {
			C.wp_cursor_shape_manager_v1_destroy(g_sapp_state.wl.cursor_shape_manager)
		}
		if g_sapp_state.wl.fractional_scale_mgr != unsafe { nil } {
			C.wp_fractional_scale_manager_v1_destroy(g_sapp_state.wl.fractional_scale_mgr)
		}
		if g_sapp_state.wl.decoration_manager != unsafe { nil } {
			C.zxdg_decoration_manager_v1_destroy(g_sapp_state.wl.decoration_manager)
		}
		if g_sapp_state.wl.compositor != unsafe { nil } {
			C.wl_compositor_destroy(g_sapp_state.wl.compositor)
		}
		if g_sapp_state.wl.registry != unsafe { nil } {
			C.wl_registry_destroy(g_sapp_state.wl.registry)
		}
		if g_sapp_state.wl.display != unsafe { nil } {
			C.wl_display_disconnect(g_sapp_state.wl.display)
		}

		sapp_discard_state()
	}
}
