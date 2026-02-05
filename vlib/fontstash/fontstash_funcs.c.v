module fontstash

// Constructor and destructor.
fn C.fonsCreateInternal(params &C.FONSparams) &C.FONScontext
fn C.fonsDeleteInternal(s &C.FONScontext)

fn C.fonsSetErrorCallback(s &C.FONScontext, callback fn (voidptr, int, int), uptr voidptr)

// Returns current atlas size.
fn C.fonsGetAtlasSize(s &C.FONScontext, width &int, height &int)

// Expands the atlas size.
fn C.fonsExpandAtlas(s &C.FONScontext, width i32, height i32) i32

// Resets the whole stash.
fn C.fonsResetAtlas(s &C.FONScontext, width i32, height i32) i32

// Add fonts
fn C.fonsGetFontByName(s &C.FONScontext, name &char) i32
fn C.fonsAddFallbackFont(s &C.FONScontext, base i32, fallback i32) i32
fn C.fonsAddFontMem(s &C.FONScontext, name &char, data &u8, dataSize i32, freeData i32) i32

// State handling
fn C.fonsPushState(s &C.FONScontext)
fn C.fonsPopState(s &C.FONScontext)
fn C.fonsClearState(s &C.FONScontext)

// State setting
fn C.fonsSetSize(s &C.FONScontext, size f32)
fn C.fonsSetColor(s &C.FONScontext, color u32)
fn C.fonsSetSpacing(s &C.FONScontext, spacing f32)
fn C.fonsSetBlur(s &C.FONScontext, blur f32)
fn C.fonsSetAlign(s &C.FONScontext, align i32)
fn C.fonsSetFont(s &C.FONScontext, font i32)

// Draw text
fn C.fonsDrawText(s &C.FONScontext, x f32, y f32, str &char, end &char) f32

// Measure text
fn C.fonsTextBounds(s &C.FONScontext, x f32, y f32, str &char, end &char, bounds &f32) f32
fn C.fonsLineBounds(s &C.FONScontext, y f32, miny &f32, maxy &f32)
fn C.fonsVertMetrics(s &C.FONScontext, ascender &f32, descender &f32, lineh &f32)

// Text iterator
fn C.fonsTextIterInit(s &C.FONScontext, iter &C.FONStextIter, x f32, y f32, str &char, end &char) i32
fn C.fonsTextIterNext(s &C.FONScontext, iter &C.FONStextIter, quad &C.FONSquad) i32

// Pull texture changes
fn C.fonsGetTextureData(s &C.FONScontext, width &int, height &int) &char
fn C.fonsValidateTexture(s &C.FONScontext, dirty &int) i32

// Draws the stash texture for debugging
fn C.fonsDrawDebug(s &C.FONScontext, x f32, y f32)
