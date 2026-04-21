module fontstash

#define FONS_USE_FREETYPE 1
// Windows fontstash forces stb_truetype in thirdparty/fontstash/fontstash.h,
// so adding freetype link flags here only creates a stale linker dependency.
#flag linux -I/usr/include/freetype2
#flag darwin -I/usr/local/include/freetype2
// brew on m1
#flag darwin -I/opt/homebrew/include/freetype2
#flag darwin -L/opt/homebrew/lib
// MacPorts
#flag darwin -I/opt/local/include/freetype2
#flag darwin -L/opt/local/lib
#flag freebsd -I/usr/local/include/freetype2
#flag freebsd -Wl -L/usr/local/lib
#flag linux -lfreetype
#flag darwin -lfreetype
#flag darwin -lpng -lbz2 -lz
