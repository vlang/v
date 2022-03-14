// malloc/free implementation for freestanding webassembly target. We just use walloc at the moment
module builtin

#flag -I @VEXEROOT/thirdparty/walloc/
#include "walloc.c"
