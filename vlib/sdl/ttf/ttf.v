module ttf

import sdl

#include <SDL_ttf.h>

[typedef]
struct C.TTF_Font {}

fn C.TTF_Init() int
fn C.TTF_Quit()

fn C.TTF_OpenFont(file byteptr, ptsize int) &TTF_Font
fn C.TTF_OpenFontIndex(file byteptr, ptsize int, index i64) &TTF_Font
fn C.TTF_OpenFontRW(src &SDL_RWops, freesrc int, ptsize int) &TTF_Font
fn C.TTF_OpenFontIndexRW(src &SDL_RWops, freesrc int, ptsize int, index i64) &TTF_Font

/* Set and retrieve the font style */
const (
	TTF_STYLE_NORMAL = C.TTF_STYLE_NORMAL
	TTF_STYLE_BOLD = C.TTF_STYLE_BOLD
	TTF_STYLE_ITALIC = C.TTF_STYLE_ITALIC
	TTF_STYLE_UNDERLINE = C.TTF_STYLE_UNDERLINE
	TTF_STYLE_STRIKETHROUGH = C.TTF_STYLE_STRIKETHROUGH
)
fn C.TTF_GetFontStyle(font &TTF_Font) int
fn C.TTF_SetFontStyle(font &TTF_Font, style int)
fn C.TTF_GetFontOutline(font &TTF_Font) int
fn C.TTF_SetFontOutline(font &TTF_Font, outline int)

/* Set and retrieve FreeType hinter settings */
const (
	TTF_HINTING_NORMAL = C.TTF_HINTING_NORMAL
	TTF_HINTING_LIGHT = C.TTF_HINTING_LIGHT
	TTF_HINTING_MONO = C.TTF_HINTING_MONO
	TTF_HINTING_NONE = C.TTF_HINTING_NONE
)
fn C.TTF_GetFontHinting(font &TTF_Font) int
fn C.TTF_SetFontHinting(font &TTF_Font, hinting int)

/* Get the total height of the font - usually equal to point size */
fn C.TTF_FontHeight(font &TTF_Font) int

/* Get the offset from the baseline to the top of the font This is a positive value, relative to the baseline.
 */
fn C.TTF_FontAscent(font &TTF_Font) int

/* Get the offset from the baseline to the bottom of the font This is a negative value, relative to the baseline. */
fn C.TTF_FontDescent(font &TTF_Font) int

/* Get the recommended spacing between lines of text for this font */
fn C.TTF_FontLineSkip(font &TTF_Font) int

/* Get/Set whether or not kerning is allowed for this font */
fn C.TTF_GetFontKerning(font &TTF_Font) int
fn C.TTF_SetFontKerning(font &TTF_Font, allowed int)

/* Get the kerning size of two glyphs */
fn C.TTF_GetFontKerningSizeGlyphs(font &TTF_Font, previous_ch u16, ch u16) int

/* Get the number of faces of the font */
fn C.TTF_FontFaces(font &TTF_Font) i64

/* Get the font face attributes, if any */
fn C.TTF_FontFaceIsFixedWidth(font &TTF_Font) int
fn C.TTF_FontFaceFamilyName(font &TTF_Font) byteptr
fn C.TTF_FontFaceStyleName(font &TTF_Font) byteptr

/* Check wether a glyph is provided by the font or not */
fn C.TTF_GlyphIsProvided(font &TTF_Font, ch u16) int

/* Get the metrics (dimensions) of a glyph To understand what these metrics mean, here is a useful link:
    http://freetype.sourceforge.net/freetype2/docs/tutorial/step2.html
 */
fn C.TTF_GlyphMetrics(font &TTF_Font, ch u16, minx &int, maxx &int, miny &int, maxy &int, advance &int) int

/* Get the dimensions of a rendered string of text */
fn C.TTF_SizeText(font &TTF_Font, text byteptr, w &int, h &int) int
fn C.TTF_SizeUTF8(font &TTF_Font, text byteptr, w &int, h &int) int
fn C.TTF_SizeUNICODE(font &TTF_Font, text &u16, w &int, h &int) int

/* Create an 8-bit palettized surface and render the given text at fast quality with the given font and color.  The 0 pixel is the
   colorkey, giving a transparent background, and the 1 pixel is set to the text color.
   This function returns the new surface, or NULL if there was an error.
*/
fn C.TTF_RenderText_Solid(font &TTF_Font, text byteptr, fg SDL_Color) &SDL_Surface
fn C.TTF_RenderUTF8_Solid(font &TTF_Font, text byteptr, fg SDL_Color) &SDL_Surface
fn C.TTF_RenderUNICODE_Solid(font &TTF_Font, text &u16, fg SDL_Color) &SDL_Surface

/* Create an 8-bit palettized surface and render the given glyph at fast quality with the given font and color.  The 0 pixel is the
   colorkey, giving a transparent background, and the 1 pixel is set to the text color.  The glyph is rendered without any padding or
   centering in the X direction, and aligned normally in the Y direction. This function returns the new surface, or NULL if there was an error.
*/
fn C.TTF_RenderGlyph_Solid(font &TTF_Font, ch u16, fg C.SDL_Color) &SDL_Surface

/* Create an 8-bit palettized surface and render the given text at high quality with the given font and colors.  The 0 pixel is background,
   while other pixels have varying degrees of the foreground color. This function returns the new surface, or NULL if there was an error.
*/
fn C.TTF_RenderText_Shaded(font &TTF_Font, text byteptr, fg SDL_Color, bg SDL_Color) &SDL_Surface
fn C.TTF_RenderUTF8_Shaded(font &TTF_Font, text byteptr, fg SDL_Color, bg SDL_Color) &SDL_Surface
fn C.TTF_RenderUNICODE_Shaded(font &TTF_Font, text &u16, fg SDL_Color, bg SDL_Color) &SDL_Surface

/* Create an 8-bit palettized surface and render the given glyph at high quality with the given font and colors.  The 0 pixel is background,
   while other pixels have varying degrees of the foreground color. The glyph is rendered without any padding or centering in the X
   direction, and aligned normally in the Y direction. This function returns the new surface, or NULL if there was an error.
*/
fn C.TTF_RenderGlyph_Shaded(font &TTF_Font, ch u16, fg C.SDL_Color) &SDL_Surface

/* Create a 32-bit ARGB surface and render the given text at high quality, using alpha blending to dither the font with the given color.
   This function returns the new surface, or NULL if there was an error.
*/
fn C.TTF_RenderText_Blended(font &TTF_Font, text byteptr, fg SDL_Color, bg SDL_Color) &SDL_Surface
fn C.TTF_RenderUTF8_Blended(font &TTF_Font, text byteptr, fg SDL_Color, bg SDL_Color) &SDL_Surface
fn C.TTF_RenderUNICODE_Blended(font &TTF_Font, text &u16, fg SDL_Color, bg SDL_Color) &SDL_Surface


/* Create a 32-bit ARGB surface and render the given text at high quality, using alpha blending to dither the font with the given color.
   Text is wrapped to multiple lines on line endings and on word boundaries if it extends beyond wrapLength in pixels.
   This function returns the new surface, or NULL if there was an error.
*/
fn C.TTF_RenderText_Blended_Wrapped(font &TTF_Font, text byteptr, fg SDL_Color, wrap_length u32) &SDL_Surface
fn C.TTF_RenderUTF8_Blended_Wrapped(font &TTF_Font, text byteptr, fg SDL_Color, wrap_length u32) &SDL_Surface
fn C.TTF_RenderUNICODE_Blended_Wrapped(font &TTF_Font, text &u16, fg SDL_Color, wrap_length u32) &SDL_Surface

/* Create a 32-bit ARGB surface and render the given glyph at high quality, using alpha blending to dither the font with the given color.
   The glyph is rendered without any padding or centering in the X direction, and aligned normally in the Y direction.
   This function returns the new surface, or NULL if there was an error.
*/
fn C.TTF_RenderGlyph_Blended(font &TTF_Font, ch u16, fg C.SDL_Color) &SDL_Surface

fn C.TTF_WasInit() int

fn C.TTF_CloseFont(font &TTF_Font)

pub const (
  version = '0.0.1'
  sdl_version = sdl.version // TODO: remove this hack to mark sdl as used; avoids warning
)
