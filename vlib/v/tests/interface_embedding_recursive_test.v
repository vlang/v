// This test orders the interface definitions intentionally
// in such a way that interface `Re` is first, and `Fe` is
// last. The goal is testing that the embedding expansion
// works independently from the source order, and that both
// can be checked/compiled/used at the same time.
interface Re {
	I1
	I2
	m_ie() int
}

interface I1 {
	I0
	m1() int
}

interface I2 {
	I0
	m2() int
}

interface I0 {
	m0() int
}

interface Fe {
	I1
	I2
	m_ie() int
}

struct StructIE {
	x int = 456
}

fn (s StructIE) m0() int {
	println(@METHOD)
	return 0
}

fn (s StructIE) m1() int {
	println(@METHOD)
	return 1
}

fn (s StructIE) m2() int {
	println(@METHOD)
	return 2
}

fn (s StructIE) m_ie() int {
	println(@METHOD)
	return 3
}

fn test_ie_recursive_forward() {
	i := Fe(StructIE{})
	eprintln(i)
	assert 0 == i.m0()
	assert 1 == i.m1()
	assert 2 == i.m2()
	assert 3 == i.m_ie()
	if i is StructIE {
		assert i.x == 456
	}
}

fn test_ie_recursive_backward() {
	i := Re(StructIE{})
	eprintln(i)
	assert 0 == i.m0()
	assert 1 == i.m1()
	assert 2 == i.m2()
	assert 3 == i.m_ie()
	if i is StructIE {
		assert i.x == 456
	}
}
