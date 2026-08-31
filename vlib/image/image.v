// Copyright (c) 2026 The V Authors. All rights reserved.
// Portions derived from Go's image package.
// Copyright 2009 The Go Authors. All rights reserved.
// Use of the original Go source is governed by Go's BSD-style license.
module image

import image.color

// Config holds an image's color model and dimensions.
pub struct Config {
pub mut:
	color_model color.Model
	width       int
	height      int
}

// Image is a finite rectangular grid of color values.
pub interface Image {
	color_model() color.Model
	bounds() Rectangle
	at(x int, y int) color.Color
}

// RGBA64Image is an image that can return pixels as RGBA64 directly.
pub interface RGBA64Image {
	color_model() color.Model
	bounds() Rectangle
	at(x int, y int) color.Color
	rgba64_at(x int, y int) color.RGBA64
}

// PalettedImage is an image whose pixels are palette indices.
pub interface PalettedImage {
	color_model() color.Model
	bounds() Rectangle
	at(x int, y int) color.Color
	color_index_at(x int, y int) u8
}

// Uniform is an infinite image of a single color.
pub struct Uniform {
pub mut:
	c color.Color
}

pub const black = Uniform{
	c: color.black
}
pub const white = Uniform{
	c: color.white
}
pub const transparent = Uniform{
	c: color.transparent
}
pub const opaque = Uniform{
	c: color.opaque
}

// rgba returns the uniform color as alpha-premultiplied 16-bit channels.
pub fn (u Uniform) rgba() (u32, u32, u32, u32) {
	return u.c.rgba()
}

// color_model returns a model that converts every color to u.c.
pub fn (u Uniform) color_model() color.Model {
	return color.constant_model(u.c)
}

// convert converts any color to the uniform color.
pub fn (u Uniform) convert(c color.Color) color.Color {
	return u.c
}

// bounds returns a large rectangle for the infinite uniform image.
pub fn (u Uniform) bounds() Rectangle {
	return Rectangle{
		min: pt(-1_000_000_000, -1_000_000_000)
		max: pt(1_000_000_000, 1_000_000_000)
	}
}

// at returns the uniform image color.
pub fn (u Uniform) at(x int, y int) color.Color {
	return u.c
}

// rgba64_at returns the uniform image color as RGBA64.
pub fn (u Uniform) rgba64_at(x int, y int) color.RGBA64 {
	r, g, b, a := u.c.rgba()
	return color.RGBA64{u16(r), u16(g), u16(b), u16(a)}
}

// opaque reports whether the uniform image is fully opaque.
pub fn (u Uniform) opaque() bool {
	_, _, _, a := u.c.rgba()
	return a == 0xffff
}

// new_uniform returns a new Uniform image of c.
pub fn new_uniform(c color.Color) Uniform {
	return Uniform{
		c: c
	}
}

// RGBA is an in-memory image with pixels in R, G, B, A byte order.
pub struct RGBA {
pub mut:
	pix    []u8
	stride int
	rect   Rectangle
}

// color_model returns the RGBA color model.
pub fn (p RGBA) color_model() color.Model {
	return color.rgba_model
}

// bounds returns the image bounds.
pub fn (p RGBA) bounds() Rectangle {
	return p.rect
}

// at returns the pixel at x, y as a color.
pub fn (p RGBA) at(x int, y int) color.Color {
	return color.Color(p.rgba_at(x, y))
}

// rgba64_at returns the pixel at x, y as RGBA64.
pub fn (p RGBA) rgba64_at(x int, y int) color.RGBA64 {
	if !pt(x, y).in_rect(p.rect) {
		return color.RGBA64{}
	}
	i := p.pix_offset(x, y)
	r := u16(p.pix[i])
	g := u16(p.pix[i + 1])
	b := u16(p.pix[i + 2])
	a := u16(p.pix[i + 3])
	return color.RGBA64{
		r: (r << 8) | r
		g: (g << 8) | g
		b: (b << 8) | b
		a: (a << 8) | a
	}
}

// rgba_at returns the pixel at x, y as RGBA.
pub fn (p RGBA) rgba_at(x int, y int) color.RGBA {
	if !pt(x, y).in_rect(p.rect) {
		return color.RGBA{}
	}
	i := p.pix_offset(x, y)
	return color.RGBA{p.pix[i], p.pix[i + 1], p.pix[i + 2], p.pix[i + 3]}
}

// pix_offset returns the first pix index for x, y.
pub fn (p RGBA) pix_offset(x int, y int) int {
	return (y - p.rect.min.y) * p.stride + (x - p.rect.min.x) * 4
}

// set stores c at x, y.
pub fn (mut p RGBA) set(x int, y int, c color.Color) {
	if !pt(x, y).in_rect(p.rect) {
		return
	}
	p.set_rgba(x, y, color.to_rgba(c))
}

// set_rgba64 stores c at x, y.
pub fn (mut p RGBA) set_rgba64(x int, y int, c color.RGBA64) {
	if !pt(x, y).in_rect(p.rect) {
		return
	}
	i := p.pix_offset(x, y)
	p.pix[i] = u8(c.r >> 8)
	p.pix[i + 1] = u8(c.g >> 8)
	p.pix[i + 2] = u8(c.b >> 8)
	p.pix[i + 3] = u8(c.a >> 8)
}

// set_rgba stores c at x, y.
pub fn (mut p RGBA) set_rgba(x int, y int, c color.RGBA) {
	if !pt(x, y).in_rect(p.rect) {
		return
	}
	i := p.pix_offset(x, y)
	p.pix[i] = c.r
	p.pix[i + 1] = c.g
	p.pix[i + 2] = c.b
	p.pix[i + 3] = c.a
}

// sub_image returns the portion of p visible through r.
pub fn (p RGBA) sub_image(r Rectangle) RGBA {
	rr := r.intersect(p.rect)
	if rr.empty() {
		return RGBA{}
	}
	i := p.pix_offset(rr.min.x, rr.min.y)
	return RGBA{
		pix:    p.pix[i..]
		stride: p.stride
		rect:   rr
	}
}

// opaque reports whether every pixel is fully opaque.
pub fn (p RGBA) opaque() bool {
	if p.rect.empty() {
		return true
	}
	mut i0 := 3
	mut i1 := p.rect.dx() * 4
	for _ in p.rect.min.y .. p.rect.max.y {
		for i := i0; i < i1; i += 4 {
			if p.pix[i] != 0xff {
				return false
			}
		}
		i0 += p.stride
		i1 += p.stride
	}
	return true
}

// new_rgba returns a new RGBA image with bounds r.
pub fn new_rgba(r Rectangle) RGBA {
	return RGBA{
		pix:    []u8{len: pixel_buffer_length(4, r, 'RGBA')}
		stride: 4 * r.dx()
		rect:   r
	}
}

// RGBA64 is an in-memory image with big-endian 16-bit RGBA channels.
pub struct RGBA64 {
pub mut:
	pix    []u8
	stride int
	rect   Rectangle
}

// color_model returns the RGBA64 color model.
pub fn (p RGBA64) color_model() color.Model {
	return color.rgba64_model
}

// bounds returns the image bounds.
pub fn (p RGBA64) bounds() Rectangle {
	return p.rect
}

// at returns the pixel at x, y as a color.
pub fn (p RGBA64) at(x int, y int) color.Color {
	return color.Color(p.rgba64_at(x, y))
}

// rgba64_at returns the pixel at x, y as RGBA64.
pub fn (p RGBA64) rgba64_at(x int, y int) color.RGBA64 {
	if !pt(x, y).in_rect(p.rect) {
		return color.RGBA64{}
	}
	i := p.pix_offset(x, y)
	return color.RGBA64{
		r: read_u16(p.pix, i)
		g: read_u16(p.pix, i + 2)
		b: read_u16(p.pix, i + 4)
		a: read_u16(p.pix, i + 6)
	}
}

// pix_offset returns the first pix index for x, y.
pub fn (p RGBA64) pix_offset(x int, y int) int {
	return (y - p.rect.min.y) * p.stride + (x - p.rect.min.x) * 8
}

// set stores c at x, y.
pub fn (mut p RGBA64) set(x int, y int, c color.Color) {
	if !pt(x, y).in_rect(p.rect) {
		return
	}
	p.set_rgba64(x, y, color.to_rgba64(c))
}

// set_rgba64 stores c at x, y.
pub fn (mut p RGBA64) set_rgba64(x int, y int, c color.RGBA64) {
	if !pt(x, y).in_rect(p.rect) {
		return
	}
	i := p.pix_offset(x, y)
	write_u16(mut p.pix, i, c.r)
	write_u16(mut p.pix, i + 2, c.g)
	write_u16(mut p.pix, i + 4, c.b)
	write_u16(mut p.pix, i + 6, c.a)
}

// sub_image returns the portion of p visible through r.
pub fn (p RGBA64) sub_image(r Rectangle) RGBA64 {
	rr := r.intersect(p.rect)
	if rr.empty() {
		return RGBA64{}
	}
	i := p.pix_offset(rr.min.x, rr.min.y)
	return RGBA64{
		pix:    p.pix[i..]
		stride: p.stride
		rect:   rr
	}
}

// opaque reports whether every pixel is fully opaque.
pub fn (p RGBA64) opaque() bool {
	if p.rect.empty() {
		return true
	}
	mut i0 := 6
	mut i1 := p.rect.dx() * 8
	for _ in p.rect.min.y .. p.rect.max.y {
		for i := i0; i < i1; i += 8 {
			if p.pix[i] != 0xff || p.pix[i + 1] != 0xff {
				return false
			}
		}
		i0 += p.stride
		i1 += p.stride
	}
	return true
}

// new_rgba64 returns a new RGBA64 image with bounds r.
pub fn new_rgba64(r Rectangle) RGBA64 {
	return RGBA64{
		pix:    []u8{len: pixel_buffer_length(8, r, 'RGBA64')}
		stride: 8 * r.dx()
		rect:   r
	}
}

// NRGBA is an in-memory image with non-alpha-premultiplied R, G, B, A bytes.
pub struct NRGBA {
pub mut:
	pix    []u8
	stride int
	rect   Rectangle
}

// color_model returns the NRGBA color model.
pub fn (p NRGBA) color_model() color.Model {
	return color.nrgba_model
}

// bounds returns the image bounds.
pub fn (p NRGBA) bounds() Rectangle {
	return p.rect
}

// at returns the pixel at x, y as a color.
pub fn (p NRGBA) at(x int, y int) color.Color {
	return color.Color(p.nrgba_at(x, y))
}

// rgba64_at returns the pixel at x, y as RGBA64.
pub fn (p NRGBA) rgba64_at(x int, y int) color.RGBA64 {
	return color.to_rgba64(color.Color(p.nrgba_at(x, y)))
}

// nrgba_at returns the pixel at x, y as NRGBA.
pub fn (p NRGBA) nrgba_at(x int, y int) color.NRGBA {
	if !pt(x, y).in_rect(p.rect) {
		return color.NRGBA{}
	}
	i := p.pix_offset(x, y)
	return color.NRGBA{p.pix[i], p.pix[i + 1], p.pix[i + 2], p.pix[i + 3]}
}

// pix_offset returns the first pix index for x, y.
pub fn (p NRGBA) pix_offset(x int, y int) int {
	return (y - p.rect.min.y) * p.stride + (x - p.rect.min.x) * 4
}

// set stores c at x, y.
pub fn (mut p NRGBA) set(x int, y int, c color.Color) {
	if !pt(x, y).in_rect(p.rect) {
		return
	}
	p.set_nrgba(x, y, color.to_nrgba(c))
}

// set_rgba64 stores c at x, y after converting to non-premultiplied RGBA.
pub fn (mut p NRGBA) set_rgba64(x int, y int, c color.RGBA64) {
	if !pt(x, y).in_rect(p.rect) {
		return
	}
	mut r := u32(c.r)
	mut g := u32(c.g)
	mut b := u32(c.b)
	a := u32(c.a)
	if a != 0 && a != 0xffff {
		r = (r * 0xffff) / a
		g = (g * 0xffff) / a
		b = (b * 0xffff) / a
	}
	p.set_nrgba(x, y, color.NRGBA{u8(r >> 8), u8(g >> 8), u8(b >> 8), u8(a >> 8)})
}

// set_nrgba stores c at x, y.
pub fn (mut p NRGBA) set_nrgba(x int, y int, c color.NRGBA) {
	if !pt(x, y).in_rect(p.rect) {
		return
	}
	i := p.pix_offset(x, y)
	p.pix[i] = c.r
	p.pix[i + 1] = c.g
	p.pix[i + 2] = c.b
	p.pix[i + 3] = c.a
}

// sub_image returns the portion of p visible through r.
pub fn (p NRGBA) sub_image(r Rectangle) NRGBA {
	rr := r.intersect(p.rect)
	if rr.empty() {
		return NRGBA{}
	}
	i := p.pix_offset(rr.min.x, rr.min.y)
	return NRGBA{
		pix:    p.pix[i..]
		stride: p.stride
		rect:   rr
	}
}

// opaque reports whether every pixel is fully opaque.
pub fn (p NRGBA) opaque() bool {
	if p.rect.empty() {
		return true
	}
	mut i0 := 3
	mut i1 := p.rect.dx() * 4
	for _ in p.rect.min.y .. p.rect.max.y {
		for i := i0; i < i1; i += 4 {
			if p.pix[i] != 0xff {
				return false
			}
		}
		i0 += p.stride
		i1 += p.stride
	}
	return true
}

// new_nrgba returns a new NRGBA image with bounds r.
pub fn new_nrgba(r Rectangle) NRGBA {
	return NRGBA{
		pix:    []u8{len: pixel_buffer_length(4, r, 'NRGBA')}
		stride: 4 * r.dx()
		rect:   r
	}
}

// NRGBA64 is an in-memory image with non-premultiplied big-endian 16-bit channels.
pub struct NRGBA64 {
pub mut:
	pix    []u8
	stride int
	rect   Rectangle
}

// color_model returns the NRGBA64 color model.
pub fn (p NRGBA64) color_model() color.Model {
	return color.nrgba64_model
}

// bounds returns the image bounds.
pub fn (p NRGBA64) bounds() Rectangle {
	return p.rect
}

// at returns the pixel at x, y as a color.
pub fn (p NRGBA64) at(x int, y int) color.Color {
	return color.Color(p.nrgba64_at(x, y))
}

// rgba64_at returns the pixel at x, y as RGBA64.
pub fn (p NRGBA64) rgba64_at(x int, y int) color.RGBA64 {
	return color.to_rgba64(color.Color(p.nrgba64_at(x, y)))
}

// nrgba64_at returns the pixel at x, y as NRGBA64.
pub fn (p NRGBA64) nrgba64_at(x int, y int) color.NRGBA64 {
	if !pt(x, y).in_rect(p.rect) {
		return color.NRGBA64{}
	}
	i := p.pix_offset(x, y)
	return color.NRGBA64{
		r: read_u16(p.pix, i)
		g: read_u16(p.pix, i + 2)
		b: read_u16(p.pix, i + 4)
		a: read_u16(p.pix, i + 6)
	}
}

// pix_offset returns the first pix index for x, y.
pub fn (p NRGBA64) pix_offset(x int, y int) int {
	return (y - p.rect.min.y) * p.stride + (x - p.rect.min.x) * 8
}

// set stores c at x, y.
pub fn (mut p NRGBA64) set(x int, y int, c color.Color) {
	if !pt(x, y).in_rect(p.rect) {
		return
	}
	p.set_nrgba64(x, y, color.to_nrgba64(c))
}

// set_rgba64 stores c at x, y after converting to non-premultiplied RGBA.
pub fn (mut p NRGBA64) set_rgba64(x int, y int, c color.RGBA64) {
	if !pt(x, y).in_rect(p.rect) {
		return
	}
	mut r := u32(c.r)
	mut g := u32(c.g)
	mut b := u32(c.b)
	a := u32(c.a)
	if a != 0 && a != 0xffff {
		r = (r * 0xffff) / a
		g = (g * 0xffff) / a
		b = (b * 0xffff) / a
	}
	p.set_nrgba64(x, y, color.NRGBA64{u16(r), u16(g), u16(b), u16(a)})
}

// set_nrgba64 stores c at x, y.
pub fn (mut p NRGBA64) set_nrgba64(x int, y int, c color.NRGBA64) {
	if !pt(x, y).in_rect(p.rect) {
		return
	}
	i := p.pix_offset(x, y)
	write_u16(mut p.pix, i, c.r)
	write_u16(mut p.pix, i + 2, c.g)
	write_u16(mut p.pix, i + 4, c.b)
	write_u16(mut p.pix, i + 6, c.a)
}

// sub_image returns the portion of p visible through r.
pub fn (p NRGBA64) sub_image(r Rectangle) NRGBA64 {
	rr := r.intersect(p.rect)
	if rr.empty() {
		return NRGBA64{}
	}
	i := p.pix_offset(rr.min.x, rr.min.y)
	return NRGBA64{
		pix:    p.pix[i..]
		stride: p.stride
		rect:   rr
	}
}

// opaque reports whether every pixel is fully opaque.
pub fn (p NRGBA64) opaque() bool {
	if p.rect.empty() {
		return true
	}
	mut i0 := 6
	mut i1 := p.rect.dx() * 8
	for _ in p.rect.min.y .. p.rect.max.y {
		for i := i0; i < i1; i += 8 {
			if p.pix[i] != 0xff || p.pix[i + 1] != 0xff {
				return false
			}
		}
		i0 += p.stride
		i1 += p.stride
	}
	return true
}

// new_nrgba64 returns a new NRGBA64 image with bounds r.
pub fn new_nrgba64(r Rectangle) NRGBA64 {
	return NRGBA64{
		pix:    []u8{len: pixel_buffer_length(8, r, 'NRGBA64')}
		stride: 8 * r.dx()
		rect:   r
	}
}

// Alpha is an in-memory image of alpha samples.
pub struct Alpha {
pub mut:
	pix    []u8
	stride int
	rect   Rectangle
}

// color_model returns the Alpha color model.
pub fn (p Alpha) color_model() color.Model {
	return color.alpha_model
}

// bounds returns the image bounds.
pub fn (p Alpha) bounds() Rectangle {
	return p.rect
}

// at returns the pixel at x, y as a color.
pub fn (p Alpha) at(x int, y int) color.Color {
	return color.Color(p.alpha_at(x, y))
}

// rgba64_at returns the pixel at x, y as RGBA64.
pub fn (p Alpha) rgba64_at(x int, y int) color.RGBA64 {
	a0 := u16(p.alpha_at(x, y).a)
	a := (a0 << 8) | a0
	return color.RGBA64{a, a, a, a}
}

// alpha_at returns the pixel at x, y as Alpha.
pub fn (p Alpha) alpha_at(x int, y int) color.Alpha {
	if !pt(x, y).in_rect(p.rect) {
		return color.Alpha{}
	}
	return color.Alpha{
		a: p.pix[p.pix_offset(x, y)]
	}
}

// pix_offset returns the first pix index for x, y.
pub fn (p Alpha) pix_offset(x int, y int) int {
	return (y - p.rect.min.y) * p.stride + (x - p.rect.min.x)
}

// set stores c at x, y.
pub fn (mut p Alpha) set(x int, y int, c color.Color) {
	if !pt(x, y).in_rect(p.rect) {
		return
	}
	p.pix[p.pix_offset(x, y)] = color.to_alpha(c).a
}

// set_rgba64 stores c at x, y.
pub fn (mut p Alpha) set_rgba64(x int, y int, c color.RGBA64) {
	if !pt(x, y).in_rect(p.rect) {
		return
	}
	p.pix[p.pix_offset(x, y)] = u8(c.a >> 8)
}

// set_alpha stores c at x, y.
pub fn (mut p Alpha) set_alpha(x int, y int, c color.Alpha) {
	if !pt(x, y).in_rect(p.rect) {
		return
	}
	p.pix[p.pix_offset(x, y)] = c.a
}

// sub_image returns the portion of p visible through r.
pub fn (p Alpha) sub_image(r Rectangle) Alpha {
	rr := r.intersect(p.rect)
	if rr.empty() {
		return Alpha{}
	}
	i := p.pix_offset(rr.min.x, rr.min.y)
	return Alpha{
		pix:    p.pix[i..]
		stride: p.stride
		rect:   rr
	}
}

// opaque reports whether every pixel is fully opaque.
pub fn (p Alpha) opaque() bool {
	if p.rect.empty() {
		return true
	}
	mut i0 := 0
	mut i1 := p.rect.dx()
	for _ in p.rect.min.y .. p.rect.max.y {
		for i := i0; i < i1; i++ {
			if p.pix[i] != 0xff {
				return false
			}
		}
		i0 += p.stride
		i1 += p.stride
	}
	return true
}

// new_alpha returns a new Alpha image with bounds r.
pub fn new_alpha(r Rectangle) Alpha {
	return Alpha{
		pix:    []u8{len: pixel_buffer_length(1, r, 'Alpha')}
		stride: r.dx()
		rect:   r
	}
}

// Alpha16 is an in-memory image of big-endian 16-bit alpha samples.
pub struct Alpha16 {
pub mut:
	pix    []u8
	stride int
	rect   Rectangle
}

// color_model returns the Alpha16 color model.
pub fn (p Alpha16) color_model() color.Model {
	return color.alpha16_model
}

// bounds returns the image bounds.
pub fn (p Alpha16) bounds() Rectangle {
	return p.rect
}

// at returns the pixel at x, y as a color.
pub fn (p Alpha16) at(x int, y int) color.Color {
	return color.Color(p.alpha16_at(x, y))
}

// rgba64_at returns the pixel at x, y as RGBA64.
pub fn (p Alpha16) rgba64_at(x int, y int) color.RGBA64 {
	a := p.alpha16_at(x, y).a
	return color.RGBA64{a, a, a, a}
}

// alpha16_at returns the pixel at x, y as Alpha16.
pub fn (p Alpha16) alpha16_at(x int, y int) color.Alpha16 {
	if !pt(x, y).in_rect(p.rect) {
		return color.Alpha16{}
	}
	return color.Alpha16{
		a: read_u16(p.pix, p.pix_offset(x, y))
	}
}

// pix_offset returns the first pix index for x, y.
pub fn (p Alpha16) pix_offset(x int, y int) int {
	return (y - p.rect.min.y) * p.stride + (x - p.rect.min.x) * 2
}

// set stores c at x, y.
pub fn (mut p Alpha16) set(x int, y int, c color.Color) {
	if !pt(x, y).in_rect(p.rect) {
		return
	}
	p.set_alpha16(x, y, color.to_alpha16(c))
}

// set_rgba64 stores c at x, y.
pub fn (mut p Alpha16) set_rgba64(x int, y int, c color.RGBA64) {
	if !pt(x, y).in_rect(p.rect) {
		return
	}
	write_u16(mut p.pix, p.pix_offset(x, y), c.a)
}

// set_alpha16 stores c at x, y.
pub fn (mut p Alpha16) set_alpha16(x int, y int, c color.Alpha16) {
	if !pt(x, y).in_rect(p.rect) {
		return
	}
	write_u16(mut p.pix, p.pix_offset(x, y), c.a)
}

// sub_image returns the portion of p visible through r.
pub fn (p Alpha16) sub_image(r Rectangle) Alpha16 {
	rr := r.intersect(p.rect)
	if rr.empty() {
		return Alpha16{}
	}
	i := p.pix_offset(rr.min.x, rr.min.y)
	return Alpha16{
		pix:    p.pix[i..]
		stride: p.stride
		rect:   rr
	}
}

// opaque reports whether every pixel is fully opaque.
pub fn (p Alpha16) opaque() bool {
	if p.rect.empty() {
		return true
	}
	mut i0 := 0
	mut i1 := p.rect.dx() * 2
	for _ in p.rect.min.y .. p.rect.max.y {
		for i := i0; i < i1; i += 2 {
			if p.pix[i] != 0xff || p.pix[i + 1] != 0xff {
				return false
			}
		}
		i0 += p.stride
		i1 += p.stride
	}
	return true
}

// new_alpha16 returns a new Alpha16 image with bounds r.
pub fn new_alpha16(r Rectangle) Alpha16 {
	return Alpha16{
		pix:    []u8{len: pixel_buffer_length(2, r, 'Alpha16')}
		stride: 2 * r.dx()
		rect:   r
	}
}

// Gray is an in-memory image of 8-bit grayscale samples.
pub struct Gray {
pub mut:
	pix    []u8
	stride int
	rect   Rectangle
}

// color_model returns the Gray color model.
pub fn (p Gray) color_model() color.Model {
	return color.gray_model
}

// bounds returns the image bounds.
pub fn (p Gray) bounds() Rectangle {
	return p.rect
}

// at returns the pixel at x, y as a color.
pub fn (p Gray) at(x int, y int) color.Color {
	return color.Color(p.gray_at(x, y))
}

// rgba64_at returns the pixel at x, y as RGBA64.
pub fn (p Gray) rgba64_at(x int, y int) color.RGBA64 {
	y0 := u16(p.gray_at(x, y).y)
	yy := (y0 << 8) | y0
	return color.RGBA64{yy, yy, yy, 0xffff}
}

// gray_at returns the pixel at x, y as Gray.
pub fn (p Gray) gray_at(x int, y int) color.Gray {
	if !pt(x, y).in_rect(p.rect) {
		return color.Gray{}
	}
	return color.Gray{
		y: p.pix[p.pix_offset(x, y)]
	}
}

// pix_offset returns the first pix index for x, y.
pub fn (p Gray) pix_offset(x int, y int) int {
	return (y - p.rect.min.y) * p.stride + (x - p.rect.min.x)
}

// set stores c at x, y.
pub fn (mut p Gray) set(x int, y int, c color.Color) {
	if !pt(x, y).in_rect(p.rect) {
		return
	}
	p.pix[p.pix_offset(x, y)] = color.to_gray(c).y
}

// set_rgba64 stores c at x, y.
pub fn (mut p Gray) set_rgba64(x int, y int, c color.RGBA64) {
	if !pt(x, y).in_rect(p.rect) {
		return
	}
	gray := (19595 * u32(c.r) + 38470 * u32(c.g) + 7471 * u32(c.b) + (1 << 15)) >> 24
	p.pix[p.pix_offset(x, y)] = u8(gray)
}

// set_gray stores c at x, y.
pub fn (mut p Gray) set_gray(x int, y int, c color.Gray) {
	if !pt(x, y).in_rect(p.rect) {
		return
	}
	p.pix[p.pix_offset(x, y)] = c.y
}

// sub_image returns the portion of p visible through r.
pub fn (p Gray) sub_image(r Rectangle) Gray {
	rr := r.intersect(p.rect)
	if rr.empty() {
		return Gray{}
	}
	i := p.pix_offset(rr.min.x, rr.min.y)
	return Gray{
		pix:    p.pix[i..]
		stride: p.stride
		rect:   rr
	}
}

// opaque reports whether every pixel is fully opaque.
pub fn (p Gray) opaque() bool {
	return true
}

// new_gray returns a new Gray image with bounds r.
pub fn new_gray(r Rectangle) Gray {
	return Gray{
		pix:    []u8{len: pixel_buffer_length(1, r, 'Gray')}
		stride: r.dx()
		rect:   r
	}
}

// Gray16 is an in-memory image of big-endian 16-bit grayscale samples.
pub struct Gray16 {
pub mut:
	pix    []u8
	stride int
	rect   Rectangle
}

// color_model returns the Gray16 color model.
pub fn (p Gray16) color_model() color.Model {
	return color.gray16_model
}

// bounds returns the image bounds.
pub fn (p Gray16) bounds() Rectangle {
	return p.rect
}

// at returns the pixel at x, y as a color.
pub fn (p Gray16) at(x int, y int) color.Color {
	return color.Color(p.gray16_at(x, y))
}

// rgba64_at returns the pixel at x, y as RGBA64.
pub fn (p Gray16) rgba64_at(x int, y int) color.RGBA64 {
	yy := p.gray16_at(x, y).y
	return color.RGBA64{yy, yy, yy, 0xffff}
}

// gray16_at returns the pixel at x, y as Gray16.
pub fn (p Gray16) gray16_at(x int, y int) color.Gray16 {
	if !pt(x, y).in_rect(p.rect) {
		return color.Gray16{}
	}
	return color.Gray16{
		y: read_u16(p.pix, p.pix_offset(x, y))
	}
}

// pix_offset returns the first pix index for x, y.
pub fn (p Gray16) pix_offset(x int, y int) int {
	return (y - p.rect.min.y) * p.stride + (x - p.rect.min.x) * 2
}

// set stores c at x, y.
pub fn (mut p Gray16) set(x int, y int, c color.Color) {
	if !pt(x, y).in_rect(p.rect) {
		return
	}
	p.set_gray16(x, y, color.to_gray16(c))
}

// set_rgba64 stores c at x, y.
pub fn (mut p Gray16) set_rgba64(x int, y int, c color.RGBA64) {
	if !pt(x, y).in_rect(p.rect) {
		return
	}
	gray := (19595 * u32(c.r) + 38470 * u32(c.g) + 7471 * u32(c.b) + (1 << 15)) >> 16
	write_u16(mut p.pix, p.pix_offset(x, y), u16(gray))
}

// set_gray16 stores c at x, y.
pub fn (mut p Gray16) set_gray16(x int, y int, c color.Gray16) {
	if !pt(x, y).in_rect(p.rect) {
		return
	}
	write_u16(mut p.pix, p.pix_offset(x, y), c.y)
}

// sub_image returns the portion of p visible through r.
pub fn (p Gray16) sub_image(r Rectangle) Gray16 {
	rr := r.intersect(p.rect)
	if rr.empty() {
		return Gray16{}
	}
	i := p.pix_offset(rr.min.x, rr.min.y)
	return Gray16{
		pix:    p.pix[i..]
		stride: p.stride
		rect:   rr
	}
}

// opaque reports whether every pixel is fully opaque.
pub fn (p Gray16) opaque() bool {
	return true
}

// new_gray16 returns a new Gray16 image with bounds r.
pub fn new_gray16(r Rectangle) Gray16 {
	return Gray16{
		pix:    []u8{len: pixel_buffer_length(2, r, 'Gray16')}
		stride: 2 * r.dx()
		rect:   r
	}
}

// CMYK is an in-memory image with C, M, Y, K byte order.
pub struct CMYK {
pub mut:
	pix    []u8
	stride int
	rect   Rectangle
}

// color_model returns the CMYK color model.
pub fn (p CMYK) color_model() color.Model {
	return color.cmyk_model
}

// bounds returns the image bounds.
pub fn (p CMYK) bounds() Rectangle {
	return p.rect
}

// at returns the pixel at x, y as a color.
pub fn (p CMYK) at(x int, y int) color.Color {
	return color.Color(p.cmyk_at(x, y))
}

// rgba64_at returns the pixel at x, y as RGBA64.
pub fn (p CMYK) rgba64_at(x int, y int) color.RGBA64 {
	return color.to_rgba64(color.Color(p.cmyk_at(x, y)))
}

// cmyk_at returns the pixel at x, y as CMYK.
pub fn (p CMYK) cmyk_at(x int, y int) color.CMYK {
	if !pt(x, y).in_rect(p.rect) {
		return color.CMYK{}
	}
	i := p.pix_offset(x, y)
	return color.CMYK{p.pix[i], p.pix[i + 1], p.pix[i + 2], p.pix[i + 3]}
}

// pix_offset returns the first pix index for x, y.
pub fn (p CMYK) pix_offset(x int, y int) int {
	return (y - p.rect.min.y) * p.stride + (x - p.rect.min.x) * 4
}

// set stores c at x, y.
pub fn (mut p CMYK) set(x int, y int, c color.Color) {
	if !pt(x, y).in_rect(p.rect) {
		return
	}
	p.set_cmyk(x, y, color.to_cmyk(c))
}

// set_rgba64 stores c at x, y.
pub fn (mut p CMYK) set_rgba64(x int, y int, c color.RGBA64) {
	if !pt(x, y).in_rect(p.rect) {
		return
	}
	cc, mm, yy, kk := color.rgb_to_cmyk(u8(c.r >> 8), u8(c.g >> 8), u8(c.b >> 8))
	p.set_cmyk(x, y, color.CMYK{cc, mm, yy, kk})
}

// set_cmyk stores c at x, y.
pub fn (mut p CMYK) set_cmyk(x int, y int, c color.CMYK) {
	if !pt(x, y).in_rect(p.rect) {
		return
	}
	i := p.pix_offset(x, y)
	p.pix[i] = c.c
	p.pix[i + 1] = c.m
	p.pix[i + 2] = c.y
	p.pix[i + 3] = c.k
}

// sub_image returns the portion of p visible through r.
pub fn (p CMYK) sub_image(r Rectangle) CMYK {
	rr := r.intersect(p.rect)
	if rr.empty() {
		return CMYK{}
	}
	i := p.pix_offset(rr.min.x, rr.min.y)
	return CMYK{
		pix:    p.pix[i..]
		stride: p.stride
		rect:   rr
	}
}

// opaque reports whether every pixel is fully opaque.
pub fn (p CMYK) opaque() bool {
	return true
}

// new_cmyk returns a new CMYK image with bounds r.
pub fn new_cmyk(r Rectangle) CMYK {
	return CMYK{
		pix:    []u8{len: pixel_buffer_length(4, r, 'CMYK')}
		stride: 4 * r.dx()
		rect:   r
	}
}

// Paletted is an in-memory image of palette indices.
pub struct Paletted {
pub mut:
	pix     []u8
	stride  int
	rect    Rectangle
	palette color.Palette
}

// color_model returns the image palette.
pub fn (p Paletted) color_model() color.Model {
	return color.Model(p.palette)
}

// bounds returns the image bounds.
pub fn (p Paletted) bounds() Rectangle {
	return p.rect
}

// at returns the color at x, y.
pub fn (p Paletted) at(x int, y int) color.Color {
	if p.palette.colors.len == 0 {
		return color.transparent
	}
	if !pt(x, y).in_rect(p.rect) {
		return p.palette.colors[0]
	}
	i := p.pix_offset(x, y)
	return p.palette.colors[p.pix[i]]
}

// rgba64_at returns the color at x, y as RGBA64.
pub fn (p Paletted) rgba64_at(x int, y int) color.RGBA64 {
	return color.to_rgba64(p.at(x, y))
}

// pix_offset returns the first pix index for x, y.
pub fn (p Paletted) pix_offset(x int, y int) int {
	return (y - p.rect.min.y) * p.stride + (x - p.rect.min.x)
}

// set stores c's nearest palette index at x, y.
pub fn (mut p Paletted) set(x int, y int, c color.Color) {
	if !pt(x, y).in_rect(p.rect) {
		return
	}
	p.pix[p.pix_offset(x, y)] = u8(p.palette.index(c))
}

// set_rgba64 stores c's nearest palette index at x, y.
pub fn (mut p Paletted) set_rgba64(x int, y int, c color.RGBA64) {
	p.set(x, y, color.Color(c))
}

// color_index_at returns the palette index at x, y.
pub fn (p Paletted) color_index_at(x int, y int) u8 {
	if !pt(x, y).in_rect(p.rect) {
		return 0
	}
	return p.pix[p.pix_offset(x, y)]
}

// set_color_index stores index at x, y.
pub fn (mut p Paletted) set_color_index(x int, y int, index u8) {
	if !pt(x, y).in_rect(p.rect) {
		return
	}
	p.pix[p.pix_offset(x, y)] = index
}

// sub_image returns the portion of p visible through r.
pub fn (p Paletted) sub_image(r Rectangle) Paletted {
	rr := r.intersect(p.rect)
	if rr.empty() {
		return Paletted{
			palette: p.palette
		}
	}
	i := p.pix_offset(rr.min.x, rr.min.y)
	return Paletted{
		pix:     p.pix[i..]
		stride:  p.stride
		rect:    rr
		palette: p.palette
	}
}

// opaque reports whether every referenced palette color is fully opaque.
pub fn (p Paletted) opaque() bool {
	mut present := []bool{len: 256}
	mut i0 := 0
	mut i1 := p.rect.dx()
	for _ in p.rect.min.y .. p.rect.max.y {
		for i := i0; i < i1; i++ {
			present[p.pix[i]] = true
		}
		i0 += p.stride
		i1 += p.stride
	}
	for i, c in p.palette.colors {
		if !present[i] {
			continue
		}
		_, _, _, a := c.rgba()
		if a != 0xffff {
			return false
		}
	}
	return true
}

// new_paletted returns a new Paletted image with bounds r and palette p.
pub fn new_paletted(r Rectangle, p color.Palette) Paletted {
	return Paletted{
		pix:     []u8{len: pixel_buffer_length(1, r, 'Paletted')}
		stride:  r.dx()
		rect:    r
		palette: p
	}
}

fn read_u16(pix []u8, i int) u16 {
	return (u16(pix[i]) << 8) | u16(pix[i + 1])
}

fn write_u16(mut pix []u8, i int, v u16) {
	pix[i] = u8(v >> 8)
	pix[i + 1] = u8(v)
}
