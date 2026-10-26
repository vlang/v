// Copyright(C) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// file that is distributed with this source code.
module sapp

// Provides the Android NativeActivity entry point required by sokol_app.h.
// See sokol_main_android.c for details. sokol_app.h itself is included by
// `sokol.c` when the program does not define `no_sokol_app`, which is also
// the condition for this glue: without the header there is no Android glue
// and no `sapp_desc` type to return.
$if android && !no_sokol_app ? {
	#flag -I@VEXEROOT/vlib/sokol/sapp
	@[use_once]
	#include "sokol_main_android.c"
}
