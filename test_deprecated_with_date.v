@[deprecated: '2025-10-10 !! use new function instead']
fn d0() {}

@[deprecated: '1234567890']
fn d1() {}

@[deprecated: '']
fn d2() {}

@[deprecated: 'a']
fn d3() {}

@[deprecated: 'lorem ipsum dolor sit']
fn d4() {}

@[deprecated: '2025-10-10!!close']
fn d5() {}

@[deprecated: ' 2025-10-10']
fn d6() {}

fn main() {
	d0()
	d1()
	d2()
	d3()
	d4()
	d5()
	d6()
}
