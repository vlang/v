module sapp

// EGL initialization and teardown.
// Shared between Wayland and X11 backends.

fn sapp_egl_choose_config() EGLConfig {
	sample_count := if g_sapp_state.desc.sample_count > 1 {
		EGLint(g_sapp_state.desc.sample_count)
	} else {
		EGLint(0)
	}
	alpha_size := if g_sapp_state.desc.alpha { EGLint(8) } else { EGLint(0) }
	sample_buffers := if g_sapp_state.desc.sample_count > 1 { EGLint(1) } else { EGLint(0) }

	config_attrs := [
		egl_surface_type,
		egl_window_bit,
		egl_renderable_type,
		egl_opengl_bit,
		egl_red_size,
		EGLint(8),
		egl_green_size,
		EGLint(8),
		egl_blue_size,
		EGLint(8),
		egl_alpha_size,
		alpha_size,
		egl_depth_size,
		EGLint(24),
		egl_stencil_size,
		EGLint(8),
		egl_sample_buffers,
		sample_buffers,
		egl_samples,
		sample_count,
		egl_none,
	]!

	mut egl_configs := unsafe { [32]EGLConfig{} }
	mut config_count := EGLint(0)
	if C.eglChooseConfig(g_sapp_state.egl.display, &config_attrs[0], &egl_configs[0], 32, &config_count) == 0
		|| config_count == 0 {
		eprintln('sokol_app: EGL: no configs found')
		return egl_configs[0]
	}

	mut config := egl_configs[0]
	for i in 0 .. int(config_count) {
		c := egl_configs[i]
		mut r := EGLint(0)
		mut g := EGLint(0)
		mut b := EGLint(0)
		mut a := EGLint(0)
		mut d := EGLint(0)
		mut s := EGLint(0)
		mut n := EGLint(0)
		if C.eglGetConfigAttrib(g_sapp_state.egl.display, c, egl_red_size, &r) != 0
			&& C.eglGetConfigAttrib(g_sapp_state.egl.display, c, egl_green_size, &g) != 0
			&& C.eglGetConfigAttrib(g_sapp_state.egl.display, c, egl_blue_size, &b) != 0
			&& C.eglGetConfigAttrib(g_sapp_state.egl.display, c, egl_alpha_size, &a) != 0
			&& C.eglGetConfigAttrib(g_sapp_state.egl.display, c, egl_depth_size, &d) != 0
			&& C.eglGetConfigAttrib(g_sapp_state.egl.display, c, egl_stencil_size, &s) != 0
			&& C.eglGetConfigAttrib(g_sapp_state.egl.display, c, egl_samples, &n) != 0 && r == 8
			&& g == 8 && b == 8 && a == alpha_size && d == 24 && s == 8 && n == sample_count {
			config = c
			break
		}
	}
	return config
}

fn sapp_egl_create_context(config EGLConfig) {
	gl_major := if g_sapp_state.desc.gl.major_version != 0 {
		g_sapp_state.desc.gl.major_version
	} else {
		4
	}
	gl_minor := if g_sapp_state.desc.gl.minor_version != 0 {
		g_sapp_state.desc.gl.minor_version
	} else {
		1
	}

	ctx_attrs := [
		egl_context_major_version,
		EGLint(gl_major),
		egl_context_minor_version,
		EGLint(gl_minor),
		egl_context_opengl_profile_mask,
		egl_context_opengl_core_profile_bit,
		egl_none,
	]!

	g_sapp_state.egl.context = C.eglCreateContext(g_sapp_state.egl.display, config, unsafe { nil },
		&ctx_attrs[0])
	if g_sapp_state.egl.context == unsafe { nil } {
		eprintln('sokol_app: EGL: failed to create context')
	}
}

fn sapp_egl_create_surface(native_window voidptr) {
	config := sapp_egl_choose_config()
	g_sapp_state.egl.config = config
	g_sapp_state.egl.surface = C.eglCreateWindowSurface(g_sapp_state.egl.display, config,
		native_window, unsafe { nil })
	if g_sapp_state.egl.surface == unsafe { nil } {
		eprintln('sokol_app: EGL: failed to create window surface')
	}
	if g_sapp_state.egl.context == unsafe { nil } {
		sapp_egl_create_context(config)
	}
}

fn sapp_egl_make_current() {
	if C.eglMakeCurrent(g_sapp_state.egl.display, g_sapp_state.egl.surface, g_sapp_state.egl.surface,
		g_sapp_state.egl.context) == 0 {
		eprintln('sokol_app: EGL: eglMakeCurrent failed')
	}
	mut fb := i32(0)
	C.glGetIntegerv(gl_framebuffer_binding, &fb)
	g_sapp_state.gl.framebuffer = u32(fb)
	C.eglSwapInterval(g_sapp_state.egl.display, EGLint(g_sapp_state.swap_interval))
}

$if sokol_wayland ? {
	fn sapp_egl_init_wayland() {
		if C.eglBindAPI(egl_opengl_api) == 0 {
			eprintln('sokol_app: EGL: failed to bind OpenGL API')
		}

		g_sapp_state.egl.display = C.eglGetDisplay(voidptr(g_sapp_state.wl.display))
		if g_sapp_state.egl.display == unsafe { nil } {
			eprintln('sokol_app: EGL: failed to get display')
		}

		mut major := EGLint(0)
		mut minor := EGLint(0)
		if C.eglInitialize(g_sapp_state.egl.display, &major, &minor) == 0 {
			eprintln('sokol_app: EGL: failed to initialize')
		}
		g_sapp_state.gl.major = int(major)
		g_sapp_state.gl.minor = int(minor)
	}
}

fn sapp_egl_init_x11() {
	if C.eglBindAPI(egl_opengl_api) == 0 {
		eprintln('sokol_app: EGL: failed to bind OpenGL API')
	}

	g_sapp_state.egl.display = C.eglGetDisplay(voidptr(g_sapp_state.x11.display))
	if g_sapp_state.egl.display == unsafe { nil } {
		eprintln('sokol_app: EGL: failed to get display')
	}

	mut major := EGLint(0)
	mut minor := EGLint(0)
	if C.eglInitialize(g_sapp_state.egl.display, &major, &minor) == 0 {
		eprintln('sokol_app: EGL: failed to initialize')
	}
	g_sapp_state.gl.major = int(major)
	g_sapp_state.gl.minor = int(minor)

	config := sapp_egl_choose_config()

	mut visual_id := EGLint(0)
	if C.eglGetConfigAttrib(g_sapp_state.egl.display, config, EGLint(0x302E), &visual_id) == 0 {
		eprintln('sokol_app: EGL: failed to get native visual ID')
	}

	mut visual_info_template := C.XVisualInfo{}
	visual_info_template.visualid = VisualID(visual_id)

	mut num_visuals := 0
	visual_info := C.XGetVisualInfo(g_sapp_state.x11.display, visual_id_mask, &visual_info_template,
		&num_visuals)
	if visual_info == unsafe { nil } {
		eprintln('sokol_app: EGL: failed to get visual info')
	}

	x11_create_window(visual_info.visual, visual_info.depth)
	C.XFree(visual_info)

	g_sapp_state.egl.surface = C.eglCreateWindowSurface(g_sapp_state.egl.display, config,
		EGLNativeWindowType(g_sapp_state.x11.window), unsafe { nil })
	if g_sapp_state.egl.surface == unsafe { nil } {
		eprintln('sokol_app: EGL: failed to create window surface')
	}

	sapp_egl_create_context(config)
	sapp_egl_make_current()
}

fn sapp_egl_destroy() {
	if g_sapp_state.egl.display != unsafe { nil } {
		C.eglMakeCurrent(g_sapp_state.egl.display, unsafe { nil }, unsafe { nil }, unsafe { nil })

		if g_sapp_state.egl.context != unsafe { nil } {
			C.eglDestroyContext(g_sapp_state.egl.display, g_sapp_state.egl.context)
			g_sapp_state.egl.context = unsafe { nil }
		}

		if g_sapp_state.egl.surface != unsafe { nil } {
			C.eglDestroySurface(g_sapp_state.egl.display, g_sapp_state.egl.surface)
			g_sapp_state.egl.surface = unsafe { nil }
		}

		C.eglTerminate(g_sapp_state.egl.display)
		g_sapp_state.egl.display = unsafe { nil }
	}
}
