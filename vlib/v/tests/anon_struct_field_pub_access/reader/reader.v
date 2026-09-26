module reader

import owner

// read returns the public fields of the anonymous struct fields of `o`.
pub fn read(o owner.Outer) (int, int, string) {
	return o.inner.value, o.inner.deep.x, o.info.name
}

// bump updates the public fields of the anonymous struct fields of `o`.
pub fn bump(mut o owner.Outer) {
	o.inner.value++
	o.inner.deep.x += 10
}

// make initializes the anonymous struct field of an `owner.Outer`.
pub fn make(value int) owner.Outer {
	return owner.Outer{
		inner: struct {
			value: value
		}
	}
}

// bump_id updates the public field that the anonymous struct field of `o`
// promotes from its embedded `owner.Meta`.
pub fn bump_id(mut o owner.Outer) int {
	o.inner.id += 5
	return o.inner.id
}
