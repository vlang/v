struct S1 {
	p voidptr
}

struct S2 {
	i int
}

struct S3 {
	x &S2
	y int
}

struct S4 {
	x S2
mut:
	y f64
}

struct S5 {
	S3
	a f32
}

fn test_isreftype() {
	assert isreftype(S1) == true
	assert isreftype(S2) == false
	assert isreftype(S3) == true
	assert isreftype(S4) == false
	assert isreftype(S5) == true
	assert isreftype(f64) == false
	assert isreftype([]f64) == true
	assert isreftype([3]int) == false
}

fn check_ref<T>() string {
	if isreftype(T) {
		return 'ref'
	} else {
		return 'no ref'
	}
}

fn test_generic_ref() {
	assert check_ref<f64>() == 'no ref'
	assert check_ref<S3>() == 'ref'
}

fn test_expression_ref() {
	mut a := S3{
		x: &S2{}
	}
	b := S4{}
	c := 32.5 * 6
	assert isreftype(a) == true
	assert isreftype(b) == false
	assert isreftype(c) == false
}
