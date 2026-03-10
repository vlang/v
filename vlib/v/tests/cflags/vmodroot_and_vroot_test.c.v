// Tests that VMODROOT finds the nearest v.mod file, which in
// this case is in the current folder, so in effect, @VMODROOT
// is the same as the absolute path of `.` .
// ==> @VMODROOT/includes === ./includes
#flag -I@VMODROOT/includes
#include "myinclude.h"

fn C.add(i32, i32) i32

// Tests that VROOT works no matter the current folder
#flag -I @VEXEROOT/thirdparty/stb_image
#include "stb_image.h"

// Tests that deprecated @VROOT still resolves to the current module root for object-file flags
#flag @VROOT/vroot_obj.o
#include "@VMODROOT/vroot_obj.h"

fn C.meaning_of_life() i32

fn test_vroot_and_vmodroot() {
	x := C.add(123, 456)
	dump(x)
	assert x == 579
}

fn test_vroot_object_flags_resolve_to_module_root() {
	x := C.meaning_of_life()
	dump(x)
	assert x == 42
}
