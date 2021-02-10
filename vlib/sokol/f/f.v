module f

import fontstash
import sokol.c

pub const (
  used_import = fontstash.used_import + c.used_import
)

#flag linux -I.

//#include "ft2build.h"

#define SOKOL_FONTSTASH_IMPL
#include "util/sokol_fontstash.h"
