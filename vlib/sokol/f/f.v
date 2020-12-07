module f

import fontstash
import sokol.c

pub const (
  used_import = fontstash.used_import + c.used_import
)

/*
#flag windows -I @VROOT/thirdparty/freetype/include
#flag windows -L @VROOT/thirdparty/freetype/win64

#flag linux -I/usr/include/freetype2
#flag darwin -I/usr/local/include/freetype2
// MacPorts
#flag darwin -I/opt/local/include/freetype2
#flag darwin -L/opt/local/lib
#flag freebsd -I/usr/local/include/freetype2
#flag freebsd -Wl -L/usr/local/lib

#flag windows -lfreetype
#flag linux -lfreetype
#flag darwin -lfreetype

#flag darwin -lpng -lbz2 -lz
*/






#flag linux -I.

//#include "ft2build.h"

#define SOKOL_FONTSTASH_IMPL
#include "util/sokol_fontstash.h"
