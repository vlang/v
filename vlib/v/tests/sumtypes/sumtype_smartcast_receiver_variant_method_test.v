// In a method of a sum type, a smartcast receiver calls the variant's method of the
// same name, including its return type, not the sum type's own method.
struct ReifyKeyword {
	name string
}

struct ReifyUnresolved {
	text string
}

type ReifyValue = ReifyKeyword | ReifyUnresolved

struct Reified {
	label string
}

struct ReifyUnparsed {
	raw string
}

fn (k ReifyKeyword) reify(prop string) Reified {
	return Reified{'kw:${k.name}:${prop}'}
}

fn (u ReifyUnresolved) reify(prop string) &ReifyUnparsed {
	return &ReifyUnparsed{'${u.text}:${prop}'}
}

fn Reified.from_unparsed(value &ReifyUnparsed) Reified {
	return Reified{'unparsed:${value.raw}'}
}

fn (style_value ReifyValue) reify(prop string) Reified {
	if style_value is ReifyKeyword {
		return style_value.reify(prop)
	}
	if style_value is ReifyUnresolved {
		return Reified.from_unparsed(style_value.reify(prop))
	}
	return Reified{}
}

fn test_smartcast_receiver_calls_variant_method() {
	assert ReifyValue(ReifyKeyword{'a'}).reify('x').label == 'kw:a:x'
	assert ReifyValue(ReifyUnresolved{'b'}).reify('y').label == 'unparsed:b:y'
}
