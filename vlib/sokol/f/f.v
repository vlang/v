module f

import fontstash as _
import sokol.c as _

#flag linux -I.

//#include "ft2build.h"

$if macos {
	#flag darwin @VEXEROOT/thirdparty/sokol/sokol_fontstash_native.o
} $else {
	@[use_once]
	#define SOKOL_FONTSTASH_IMPL
	#include "util/sokol_fontstash.h"
}
