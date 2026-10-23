module ifoptstyle

import ifoptdom

pub struct Length {
pub:
	px int
}

pub struct Keyword {
pub:
	name string
}

pub type StyleValue = Keyword | Length

pub enum RectangularColorSpace {
	srgb
	oklab
}

pub struct PolarMethod {
pub:
	hue int
}

pub type ColorInterpolationMethod = PolarMethod | RectangularColorSpace

pub struct MethodStyleValue {
pub:
	hue int
}

pub struct Context {
pub:
	factor int
}

pub struct ColorStop {
pub:
	hint     ?StyleValue
	position ?StyleValue
}

pub struct Shadow {
pub:
	radius ?StyleValue
	color  StyleValue
}

struct ForInitDeclaration {
	n int
}

struct ForInitExpression {
	s string
}

type ForInit = ForInitDeclaration | ForInitExpression

struct LocalDeclaration {
	n int
}

struct LocalExpression {
	s string
}

type LocalForInit = LocalDeclaration | LocalExpression

// describe returns a readable form of an optional style value.
pub fn describe(value ?StyleValue) string {
	v := value or { return 'none' }
	return match v {
		Keyword { 'keyword ${v.name}' }
		Length { 'length ${v.px}' }
	}
}

fn (v StyleValue) absolutized(context Context) StyleValue {
	return match v {
		Keyword { StyleValue(Keyword{v.name.to_upper()}) }
		Length { StyleValue(Length{v.px * context.factor}) }
	}
}

// absolutized absolutizes the present values of a color stop.
pub fn (stop ColorStop) absolutized(context Context) ColorStop {
	hint := if value := stop.hint {
		?StyleValue(value.absolutized(context))
	} else {
		?StyleValue(none)
	}
	position := if value := stop.position {
		?StyleValue(value.absolutized(context))
	} else {
		?StyleValue(none)
	}
	return ColorStop{
		hint:     hint
		position: position
	}
}

// lookup returns the value stored for `key` as an optional pointer to the
// `ifoptdom.StyleValue` struct (not the `StyleValue` sum of this module).
pub fn lookup(values map[int]&ifoptdom.StyleValue, key int) int {
	value := if found := values[key] {
		?&ifoptdom.StyleValue(found)
	} else {
		?&ifoptdom.StyleValue(none)
	}
	v := value or { return -1 }
	return v.n
}

fn (m MethodStyleValue) color_interpolation_method() ColorInterpolationMethod {
	return PolarMethod{m.hue}
}

fn describe_method(method ?ColorInterpolationMethod) string {
	m := method or { return 'none' }
	return match m {
		PolarMethod { 'polar ${m.hue}' }
		RectangularColorSpace { 'rectangular ${m}' }
	}
}

// method_name resolves an optional color interpolation method.
pub fn method_name(method ?MethodStyleValue) string {
	color_interpolation_method := if m := method {
		?ColorInterpolationMethod(m.color_interpolation_method())
	} else {
		?ColorInterpolationMethod(RectangularColorSpace.oklab)
	}
	return describe_method(color_interpolation_method)
}

fn interpolate_color(from StyleValue, method ?ColorInterpolationMethod) ?StyleValue {
	if method == none {
		return from
	}
	return none
}

// shadow keeps its radius only when `keeps` is set.
pub fn shadow(keeps bool, px int) Shadow {
	radius := StyleValue(Length{px})
	color := interpolate_color(StyleValue(Keyword{'red'}), none) or { StyleValue(Keyword{'black'}) }
	return Shadow{
		radius: if keeps {
			radius
		} else {
			none
		}
		color:  color
	}
}

// shadow_from_match picks the radius with a `match` that has a `none` arm.
pub fn shadow_from_match(k int) Shadow {
	return Shadow{
		radius: match k {
			1 { StyleValue(Keyword{'thin'}) }
			2 { StyleValue(Length{k}) }
			else { none }
		}
		color:  Keyword{'blue'}
	}
}

fn describe_for_init(init ?ForInit) string {
	i := init or { return 'none' }
	return match i {
		ForInitDeclaration { 'declaration ${i.n}' }
		ForInitExpression { 'expression ${i.s}' }
	}
}

fn convert_for_init(init LocalForInit) string {
	for_init := match init {
		LocalDeclaration {
			?ForInit(ForInitDeclaration{
				n: init.n
			})
		}
		LocalExpression {
			?ForInit(ForInitExpression{
				s: init.s
			})
		}
	}
	return describe_for_init(for_init)
}

// for_init_names converts both for-init kinds.
pub fn for_init_names() []string {
	return [convert_for_init(LocalDeclaration{3}), convert_for_init(LocalExpression{'i'})]
}
