// Copyright (c) 2026 The V Authors. All rights reserved.
// Portions derived from Go's image package.
// Copyright 2011 The Go Authors. All rights reserved.
// Use of the original Go source is governed by Go's BSD-style license.
module image

import image.color

// YCbCrSubsampleRatio is the chroma subsample ratio used by a YCbCr image.
pub enum YCbCrSubsampleRatio {
	ratio_444
	ratio_422
	ratio_420
	ratio_440
	ratio_411
	ratio_410
}

// str returns the Go-compatible name of the subsample ratio.
pub fn (s YCbCrSubsampleRatio) str() string {
	return match s {
		.ratio_444 { 'YCbCrSubsampleRatio444' }
		.ratio_422 { 'YCbCrSubsampleRatio422' }
		.ratio_420 { 'YCbCrSubsampleRatio420' }
		.ratio_440 { 'YCbCrSubsampleRatio440' }
		.ratio_411 { 'YCbCrSubsampleRatio411' }
		.ratio_410 { 'YCbCrSubsampleRatio410' }
	}
}

// YCbCr is an in-memory image of Y'CbCr colors.
pub struct YCbCr {
pub mut:
	y               []u8
	cb              []u8
	cr              []u8
	y_stride        int
	c_stride        int
	subsample_ratio YCbCrSubsampleRatio
	rect            Rectangle
}

// color_model returns the YCbCr color model.
pub fn (p YCbCr) color_model() color.Model {
	return color.ycbcr_model
}

// bounds returns the image bounds.
pub fn (p YCbCr) bounds() Rectangle {
	return p.rect
}

// at returns the pixel at x, y as a color.
pub fn (p YCbCr) at(x int, y int) color.Color {
	return color.Color(p.ycbcr_at(x, y))
}

// rgba64_at returns the pixel at x, y as RGBA64.
pub fn (p YCbCr) rgba64_at(x int, y int) color.RGBA64 {
	return color.to_rgba64(color.Color(p.ycbcr_at(x, y)))
}

// ycbcr_at returns the pixel at x, y as YCbCr.
pub fn (p YCbCr) ycbcr_at(x int, y int) color.YCbCr {
	if !pt(x, y).in_rect(p.rect) {
		return color.YCbCr{}
	}
	yi := p.y_offset(x, y)
	ci := p.c_offset(x, y)
	return color.YCbCr{
		y:  p.y[yi]
		cb: p.cb[ci]
		cr: p.cr[ci]
	}
}

// y_offset returns the first Y index for x, y.
pub fn (p YCbCr) y_offset(x int, y int) int {
	return (y - p.rect.min.y) * p.y_stride + (x - p.rect.min.x)
}

// c_offset returns the first Cb or Cr index for x, y.
pub fn (p YCbCr) c_offset(x int, y int) int {
	match p.subsample_ratio {
		.ratio_422 {
			return (y - p.rect.min.y) * p.c_stride + (x / 2 - p.rect.min.x / 2)
		}
		.ratio_420 {
			return (y / 2 - p.rect.min.y / 2) * p.c_stride + (x / 2 - p.rect.min.x / 2)
		}
		.ratio_440 {
			return (y / 2 - p.rect.min.y / 2) * p.c_stride + (x - p.rect.min.x)
		}
		.ratio_411 {
			return (y - p.rect.min.y) * p.c_stride + (x / 4 - p.rect.min.x / 4)
		}
		.ratio_410 {
			return (y / 2 - p.rect.min.y / 2) * p.c_stride + (x / 4 - p.rect.min.x / 4)
		}
		.ratio_444 {
			return (y - p.rect.min.y) * p.c_stride + (x - p.rect.min.x)
		}
	}
}

// sub_image returns the portion of p visible through r.
pub fn (p YCbCr) sub_image(r Rectangle) YCbCr {
	rr := r.intersect(p.rect)
	if rr.empty() {
		return YCbCr{
			subsample_ratio: p.subsample_ratio
		}
	}
	yi := p.y_offset(rr.min.x, rr.min.y)
	ci := p.c_offset(rr.min.x, rr.min.y)
	return YCbCr{
		y:               p.y[yi..]
		cb:              p.cb[ci..]
		cr:              p.cr[ci..]
		y_stride:        p.y_stride
		c_stride:        p.c_stride
		subsample_ratio: p.subsample_ratio
		rect:            rr
	}
}

// opaque reports whether every pixel is fully opaque.
pub fn (p YCbCr) opaque() bool {
	return true
}

// new_ycbcr returns a new YCbCr image with bounds r and subsample_ratio.
pub fn new_ycbcr(r Rectangle, subsample_ratio YCbCrSubsampleRatio) YCbCr {
	w, h, cw, ch := ycbcr_size(r, subsample_ratio)
	total_length := add2_non_neg(mul3_non_neg(1, w, h), mul3_non_neg(2, cw, ch))
	if total_length < 0 {
		panic('image: new_ycbcr rectangle has huge or negative dimensions')
	}
	i0 := w * h
	c_len := cw * ch
	return YCbCr{
		y:               []u8{len: i0}
		cb:              []u8{len: c_len}
		cr:              []u8{len: c_len}
		y_stride:        w
		c_stride:        cw
		subsample_ratio: subsample_ratio
		rect:            r
	}
}

// NYCbCrA is an in-memory image of non-alpha-premultiplied Y'CbCr with alpha.
pub struct NYCbCrA {
pub mut:
	ycbcr    YCbCr
	a        []u8
	a_stride int
}

// color_model returns the NYCbCrA color model.
pub fn (p NYCbCrA) color_model() color.Model {
	return color.nycbcra_model
}

// bounds returns the image bounds.
pub fn (p NYCbCrA) bounds() Rectangle {
	return p.ycbcr.rect
}

// at returns the pixel at x, y as a color.
pub fn (p NYCbCrA) at(x int, y int) color.Color {
	return color.Color(p.nycbcra_at(x, y))
}

// rgba64_at returns the pixel at x, y as RGBA64.
pub fn (p NYCbCrA) rgba64_at(x int, y int) color.RGBA64 {
	return color.to_rgba64(color.Color(p.nycbcra_at(x, y)))
}

// nycbcra_at returns the pixel at x, y as NYCbCrA.
pub fn (p NYCbCrA) nycbcra_at(x int, y int) color.NYCbCrA {
	if !pt(x, y).in_rect(p.ycbcr.rect) {
		return color.NYCbCrA{}
	}
	yi := p.ycbcr.y_offset(x, y)
	ci := p.ycbcr.c_offset(x, y)
	ai := p.a_offset(x, y)
	return color.NYCbCrA{
		ycbcr: color.YCbCr{
			y:  p.ycbcr.y[yi]
			cb: p.ycbcr.cb[ci]
			cr: p.ycbcr.cr[ci]
		}
		a:     p.a[ai]
	}
}

// a_offset returns the first alpha index for x, y.
pub fn (p NYCbCrA) a_offset(x int, y int) int {
	return (y - p.ycbcr.rect.min.y) * p.a_stride + (x - p.ycbcr.rect.min.x)
}

// sub_image returns the portion of p visible through r.
pub fn (p NYCbCrA) sub_image(r Rectangle) NYCbCrA {
	rr := r.intersect(p.ycbcr.rect)
	if rr.empty() {
		return NYCbCrA{
			ycbcr: YCbCr{
				subsample_ratio: p.ycbcr.subsample_ratio
			}
		}
	}
	yi := p.ycbcr.y_offset(rr.min.x, rr.min.y)
	ci := p.ycbcr.c_offset(rr.min.x, rr.min.y)
	ai := p.a_offset(rr.min.x, rr.min.y)
	return NYCbCrA{
		ycbcr:    YCbCr{
			y:               p.ycbcr.y[yi..]
			cb:              p.ycbcr.cb[ci..]
			cr:              p.ycbcr.cr[ci..]
			y_stride:        p.ycbcr.y_stride
			c_stride:        p.ycbcr.c_stride
			subsample_ratio: p.ycbcr.subsample_ratio
			rect:            rr
		}
		a:        p.a[ai..]
		a_stride: p.a_stride
	}
}

// opaque reports whether every pixel is fully opaque.
pub fn (p NYCbCrA) opaque() bool {
	if p.ycbcr.rect.empty() {
		return true
	}
	mut i0 := 0
	mut i1 := p.ycbcr.rect.dx()
	for _ in p.ycbcr.rect.min.y .. p.ycbcr.rect.max.y {
		for i := i0; i < i1; i++ {
			if p.a[i] != 0xff {
				return false
			}
		}
		i0 += p.a_stride
		i1 += p.a_stride
	}
	return true
}

// new_nycbcra returns a new NYCbCrA image with bounds r and subsample_ratio.
pub fn new_nycbcra(r Rectangle, subsample_ratio YCbCrSubsampleRatio) NYCbCrA {
	w, h, cw, ch := ycbcr_size(r, subsample_ratio)
	total_length := add2_non_neg(mul3_non_neg(2, w, h), mul3_non_neg(2, cw, ch))
	if total_length < 0 {
		panic('image: new_nycbcra rectangle has huge or negative dimensions')
	}
	i0 := w * h
	c_len := cw * ch
	return NYCbCrA{
		ycbcr:    YCbCr{
			y:               []u8{len: i0}
			cb:              []u8{len: c_len}
			cr:              []u8{len: c_len}
			y_stride:        w
			c_stride:        cw
			subsample_ratio: subsample_ratio
			rect:            r
		}
		a:        []u8{len: i0}
		a_stride: w
	}
}

fn ycbcr_size(r Rectangle, subsample_ratio YCbCrSubsampleRatio) (int, int, int, int) {
	w := r.dx()
	h := r.dy()
	mut cw := 0
	mut ch := 0
	match subsample_ratio {
		.ratio_422 {
			cw = (r.max.x + 1) / 2 - r.min.x / 2
			ch = h
		}
		.ratio_420 {
			cw = (r.max.x + 1) / 2 - r.min.x / 2
			ch = (r.max.y + 1) / 2 - r.min.y / 2
		}
		.ratio_440 {
			cw = w
			ch = (r.max.y + 1) / 2 - r.min.y / 2
		}
		.ratio_411 {
			cw = (r.max.x + 3) / 4 - r.min.x / 4
			ch = h
		}
		.ratio_410 {
			cw = (r.max.x + 3) / 4 - r.min.x / 4
			ch = (r.max.y + 1) / 2 - r.min.y / 2
		}
		.ratio_444 {
			cw = w
			ch = h
		}
	}

	return w, h, cw, ch
}
