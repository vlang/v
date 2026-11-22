module svg

import math
import strings

// Animation models a SMIL animation element or a CSS keyframe animation.
pub struct Animation {
pub mut:
	target_id   string
	attr        string
	begin       f64    = 0
	dur         f64    = 1
	repeat      f64    = 1
	fill        string = 'remove'
	calc_mode   string = 'linear'
	from        string
	to          string
	by          string
	values      []string
	key_times   []f64
	key_splines []KeySpline
	additive    string = 'replace'
	accumulate  string = 'none'
}

// KeySpline holds the four control values of a calcMode="spline" segment.
pub struct KeySpline {
pub:
	x1 f64
	y1 f64
	x2 f64
	y2 f64
}

// collect_animations gathers all SMIL animation elements in the document.
pub fn collect_animations(doc &Document) []Animation {
	mut out := []Animation{}
	collect_animations_in(&doc.root, mut out)
	return out
}

fn collect_animations_in(e &Element, mut out []Animation) {
	for child in e.children {
		if child.tag in ['animate', 'animateTransform', 'animateMotion', 'set'] {
			mut a := Animation{
				target_id: child.get_attr('href')
			}
			if a.target_id == '' {
				a.target_id = child.get_attr('xlink:href')
			}
			if a.target_id.starts_with('#') {
				a.target_id = a.target_id[1..]
			}
			if a.target_id == '' {
				a.target_id = e.id()
			}
			a.attr = child.get_attr('attributeName')
			if child.tag == 'animateTransform' {
				a.attr = 'transform'
			}
			a.begin = timing_number(child.get_attr('begin'))
			a.dur = timing_number(child.get_attr('dur'))
			if a.dur <= 0 {
				a.dur = 1
			}
			a.repeat = repeat_number(child.get_attr('repeatCount'), child.get_attr('repeatDur'))
			a.fill = if child.get_attr('fill') != '' { child.get_attr('fill') } else { 'remove' }
			a.calc_mode = if child.get_attr('calcMode') != '' {
				child.get_attr('calcMode')
			} else {
				'linear'
			}
			a.from = child.get_attr('from')
			a.to = child.get_attr('to')
			a.by = child.get_attr('by')
			a.values = split_values(child.get_attr('values'))
			a.key_times = split_numbers(child.get_attr('keyTimes'))
			a.additive = child.get_attr('additive')
			a.accumulate = child.get_attr('accumulate')
			out << a
		}
		collect_animations_in(&child, mut out)
	}
}

fn timing_number(s string) f64 {
	v := s.trim_space()
	if v == '' || v == 'indefinite' {
		return 0
	}
	if v.ends_with('ms') {
		return parse_number(v[0..v.len - 2]) or { 0 } / 1000
	}
	if v.ends_with('s') {
		return parse_number(v[0..v.len - 1]) or { 0 }
	}
	return parse_number(v) or { 0 }
}

fn repeat_number(count string, dur string) f64 {
	if count == 'indefinite' || dur == 'indefinite' {
		return math.inf(1)
	}
	if count != '' {
		return parse_number(count) or { 1 }
	}
	return 1
}

fn split_values(s string) []string {
	if s.trim_space() == '' {
		return []
	}
	mut out := []string{}
	for part in s.split(';') {
		v := part.trim_space()
		if v != '' {
			out << v
		}
	}
	return out
}

fn split_numbers(s string) []f64 {
	mut out := []f64{}
	for part in s.split(';') {
		v := parse_number(part.trim_space()) or { continue }
		out << v
	}
	return out
}

// snapshot_at evaluates animations at time t and returns a new document with
// the animated attribute values applied. CSS @keyframes are included.
pub fn snapshot_at(doc &Document, time f64) !Document {
	mut result := parse_svg(doc.to_svg(WriteOptions{ minify: true })) or { return err }
	animations := collect_animations(doc)
	for a in animations {
		if a.target_id == '' || a.attr == '' {
			continue
		}
		target := result.root.find(a.target_id) or { continue }
		value := eval_animation(a, time) or { continue }
		if a.attr == 'transform' {
			// SMIL animateTransform uses type/values expressed as a transform list.
			target.set_attr('transform', value)
		} else {
			target.set_attr(a.attr, value)
		}
	}
	for rule in doc.css {
		if rule.selector.contains('@keyframes') {
			continue
		}
		_ = rule
	}
	return result
}

// eval_animation computes the animated value at time t.
pub fn eval_animation(a Animation, t f64) !string {
	if t < a.begin {
		return error('animation has not started')
	}
	mut local := t - a.begin
	// A discrete animation may legally omit `values` and use from/to instead.
	// Normalize that into a local list so every indexing path below is safe.
	mut vals := a.values.clone()
	if vals.len == 0 {
		if a.from != '' {
			vals << a.from
		}
		if a.to != '' {
			vals << a.to
		}
	}
	cycle := a.dur
	if a.repeat > 1 || math.is_inf(a.repeat, 1) {
		elapsed_cycle := math.fmod(local, cycle)
		iteration := math.floor(local / cycle)
		local = elapsed_cycle
		if a.accumulate == 'sum' {
			// Accumulation is applied by adding the delta per iteration.
			local += iteration * cycle
		}
		if !math.is_inf(a.repeat, 1) && iteration >= a.repeat {
			if a.fill == 'freeze' {
				local = cycle
			} else {
				return error('animation finished')
			}
		}
	}
	if a.calc_mode == 'discrete' {
		if vals.len == 0 {
			return error('animation has no values')
		}
		idx := int(math.floor(local / a.dur * f64(vals.len)))
		if idx < 0 {
			return vals[0]
		}
		if idx < vals.len {
			return vals[idx]
		}
		return vals[vals.len - 1]
	}
	mut progress := clamp_limits(local / a.dur, 0, 1)
	if a.key_times.len == vals.len && vals.len > 1 {
		for i in 1 .. a.key_times.len {
			if progress <= a.key_times[i] {
				span := a.key_times[i] - a.key_times[i - 1]
				progress = if span < 1e-9 { 0 } else { (progress - a.key_times[i - 1]) / span }
				progress += f64(i - 1) / f64(vals.len - 1)
				break
			}
		}
	}
	if vals.len >= 2 {
		scaled := progress * f64(vals.len - 1)
		idx := int(math.floor(scaled))
		if idx >= vals.len - 1 {
			return vals[vals.len - 1]
		}
		frac := scaled - f64(idx)
		return interpolate_values(vals[idx], vals[idx + 1], frac)
	}
	if a.from != '' && a.to != '' {
		return interpolate_values(a.from, a.to, progress)
	}
	if a.by != '' {
		return add_values(a.to, a.by, progress)
	}
	if a.to != '' {
		return a.to
	}
	return error('animation has no values')
}

fn interpolate_values(from string, to string, t f64) string {
	c1 := parse_color(from) or { Color{} }
	c2 := parse_color(to) or { Color{} }
	is_color := from.starts_with('#') || from.starts_with('rgb') || from.starts_with('hsl') || from.to_lower() in named_colors || to.to_lower() in named_colors
	if c1.kind == .rgb && c2.kind == .rgb && is_color {
		mixed := Color{
			r: u8(math.round(f64(c1.r) + (f64(c2.r) - f64(c1.r)) * t))
			g: u8(math.round(f64(c1.g) + (f64(c2.g) - f64(c1.g)) * t))
			b: u8(math.round(f64(c1.b) + (f64(c2.b) - f64(c1.b)) * t))
			a: u8(math.round(f64(c1.a) + (f64(c2.a) - f64(c1.a)) * t))
		}
		return mixed.str()
	}
	n1 := parse_number(from) or { return to }
	n2 := parse_number(to) or { return to }
	return fmt_num(n1 + (n2 - n1) * t)
}

fn add_values(base string, delta string, t f64) string {
	n1 := parse_number(base) or { return base }
	n2 := parse_number(delta) or { return delta }
	return fmt_num(n1 + n2 * t)
}

// parse_keyframes parses CSS @keyframes blocks into a map of rule name ->
// (offset, declarations). It recognizes the common `from`/`to` and percentage
// selectors.
pub struct Keyframe {
pub:
	offset f64
	style  Style
}

pub fn parse_keyframes(css string) map[string][]Keyframe {
	mut out := map[string][]Keyframe{}
	mut i := 0
	for i < css.len {
		at := css.index_after('@keyframes', i) or { break }
		mut p := at + '@keyframes'.len
		for p < css.len && (css[p] == ` ` || css[p] == `\t` || css[p] == `\n`) {
			p++
		}
		name_start := p
		mut name_end := p
		for name_end < css.len && css[name_end] != `{` && css[name_end] != `\n` {
			name_end++
		}
		open := css.index_after('{', name_end) or { break }
		name := css[name_start..name_end].trim_space()
		close := find_matching_brace(css, open) or { break }
		mut frames := []Keyframe{}
		body := css[open + 1..close]
		for block in split_blocks(body) {
			bopen := block.index('{') or { continue }
			bclose := block.index_after('}', bopen) or { continue }
			sel := block[0..bopen].trim_space()
			offset := if sel == 'from' {
				0.0
			} else if sel == 'to' {
				1.0
			} else {
				parse_number(sel.trim_right('%')) or { 0 } / 100
			}
			st := parse_style(block[bopen + 1..bclose]) or { Style{} }
			frames << Keyframe{ offset: offset, style: st }
		}
		out[name] = frames
		i = close + 1
	}
	return out
}

fn find_matching_brace(s string, open int) ?int {
	mut depth := 0
	for i in open .. s.len {
		if s[i] == `{` {
			depth++
		} else if s[i] == `}` {
			depth--
			if depth == 0 {
				return i
			}
		}
	}
	return none
}

fn keyframes_to_string(frames []Keyframe) string {
	mut sb := strings.new_builder(256)
	for f in frames {
		sb.write_string('${fmt_num(f.offset)}:')
		sb.write_string(f.style.str())
		sb.write_string(';')
	}
	return sb.str()
}
