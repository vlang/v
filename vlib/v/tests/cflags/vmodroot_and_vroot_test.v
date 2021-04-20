// Tests that VMODROOT finds the nearest v.mod file, which in
// this case is in the current folder, so in effect, @VMODROOT
// is the same as the absolute path of `.` .
// ==> @VMODROOT/includes === ./includes
#flag -I@VMODROOT/includes
#include "myinclude.h"

fn C.add(int, int) int

// Tests that VROOT works no matter the current folder
#flag -I @VEXEROOT/thirdparty/stb_image
#include "stb_image.h"

fn test_vroot_and_vmodroot() {
	x := C.add(123, 456)
	dump(x)
	assert x == 579
}
