module main

import fontstash

fn test_c_struct_fields_are_pub() {
	params := &C.FONSparams{
		width:        512
		height:       512
		flags:        0
		userPtr:      unsafe { nil }
		renderCreate: fn (uptr voidptr, width int, height int) int {
			return 1
		}
		renderResize: fn (uptr voidptr, width int, height int) int {
			return 1
		}
		renderUpdate: fn (uptr voidptr, rect &int, data &u8) {}
		renderDraw:   fn (uptr voidptr, verts &f32, tcoords &f32, colors &u32, nverts int) {}
		renderDelete: fn (uptr voidptr) {}
	}

	context := fontstash.create_internal(params)
	// After porting sfons/fontstash to V, C.FONScontext now has public fields
	// so the string representation shows the fields instead of being empty
	assert context.str().starts_with('fontstash.Context(C.FONScontext{')
}
