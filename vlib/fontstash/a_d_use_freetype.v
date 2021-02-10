module fontstash

#define FONS_USE_FREETYPE
#flag windows -I @VROOT/thirdparty/freetype/include
#flag windows -L @VROOT/thirdparty/freetype/win64
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
#flag windows -lfreetype
#flag linux -lfreetype
#flag darwin -lfreetype
#flag darwin -lpng -lbz2 -lz
