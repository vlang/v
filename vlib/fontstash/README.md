## Description

`fontstash` is a thin wrapper over <https://github.com/memononen/fontstash>,
which in turn is a light-weight online font texture atlas builder written in C.
It uses stb_truetype to render fonts on demand to a texture atlas.

## Using another stb_truetype implementation

`fontstash` supplies an implementation of `stb_truetype.h`. Importing `gg` also imports
`fontstash`. If another module defines `STB_TRUETYPE_IMPLEMENTATION` in a `.c.v` file,
both implementations can end up in the same generated C translation unit. `STBTT_STATIC`
gives functions internal linkage, but does not allow duplicate definitions in that unit.

Compile the other module's implementation and its stb calls in a separate C source file.
Expose wrapper functions through a header, and link the source with `#flag`. For example,
these files can live beside the other module's `v.mod`:

`stb_bridge.c`:

```c
#include <stdlib.h>
#define STBTT_malloc(size, user) malloc(size)
#define STBTT_free(ptr, user) free(ptr)
#define STB_TRUETYPE_IMPLEMENTATION
#define STBTT_STATIC
#include "stb_truetype.h"
#include "stb_bridge.h"

int bridge_font_count(const unsigned char *data) {
    return stbtt_GetNumberOfFonts(data);
}
```

`stb_bridge.h`:

```c
#ifndef STB_BRIDGE_H
#define STB_BRIDGE_H
int bridge_font_count(const unsigned char *data);
#endif
```

The module's `.c.v` file:

```v oksyntax
module fontbridge

#flag -I @VEXEROOT/thirdparty/fontstash
#flag @VMODROOT/stb_bridge.c
#include "@VMODROOT/stb_bridge.h"

fn C.bridge_font_count(data &u8) int
```

Keep `STB_TRUETYPE_IMPLEMENTATION`, custom allocator macros, and calls into that copy of
stb_truetype inside `stb_bridge.c`. Do not `#include` that C source from V; `#flag` compiles
it separately. Keep `STBTT_STATIC` there so its stb symbols do not collide at link time.
Use a module-specific prefix for the exported wrapper names.

The bundled header defaults to fontstash's allocator macros. The example supplies standard
C allocation hooks instead; a module can substitute its own matching allocator and deallocator.

This also lets each implementation retain its own allocation hooks. Removing an include
or sharing one module's stb functions can silently select the wrong allocator.
