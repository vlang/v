// Tests that the deprecated @VROOT alias still resolves to the nearest v.mod file,
// which in this case is in the current folder.
// ==> @VROOT/includes === ./includes
#flag -I@VROOT/includes
#include "myinclude.h"

fn C.add(i32, i32) i32

// Tests that VEXEROOT works no matter the current folder.
#flag -I @VEXEROOT/thirdparty/stb_image
#include "stb_image.h"

fn test_vroot_and_vmodroot() {
	x := C.add(123, 456)
	dump(x)
	assert x == 579
}
