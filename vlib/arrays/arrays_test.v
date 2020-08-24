module arrays


fn test_min() {
	a := [8, 2, 6, 4]
	assert min<int>(a)==2
	assert min<int>(a[2..])==4
	
	b := [f32(5.1), 3.1, 1.1, 9.1]
	assert min<f32>(b) == f32(1.1)
	assert min<f32>(b[..2]) == f32(3.1)
	
	c := [byte(4), 9, 3, 1]
	assert min<byte>(c) == byte(1)
	assert min<byte>(c[..3]) == byte(3)
}


fn test_max() {
	a := [8, 2, 6, 4]
	assert max<int>(a)==8
	assert max<int>(a[1..])==6
	
	b := [f32(5.1), 3.1, 1.1, 9.1]
	assert max<f32>(b) == f32(9.1)
	assert max<f32>(b[..3]) == f32(5.1)
	
	c := [byte(4), 9, 3, 1]
	assert max<byte>(c) == byte(9)
	assert max<byte>(c[2..]) == byte(3)
}


fn test_argmin() {
	a := [8, 2, 6, 4]
	assert argmin<int>(a)==1
	
	b := [f32(5.1), 3.1, 1.1, 9.1]
	assert argmin<f32>(b) == 2
	
	c := [byte(4), 9, 3, 1]
	assert argmin<byte>(c) == 3
}


fn test_argmax() {
	a := [8, 2, 6, 4]
	assert argmax<int>(a)==0
	
	b := [f32(5.1), 3.1, 1.1, 9.1]
	assert argmax<f32>(b) == 3
	
	c := [byte(4), 9, 3, 1]
	assert argmax<byte>(c) == 1
}


fn test_shuffle() {
	a := [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
	mut b := a.clone()
	mut c := a.clone()
	shuffle<int>(mut b)
	shuffle<int>(mut c)
	assert a != b // false negative chance: 1/20!
	assert a != c // false negative chance: 1/20!
	assert b != c // false negative chance: 1/20!
	mut d := a.clone()
	shuffle<int>(mut d[..15])
	assert d[..15] != a[..15] // false negative chance: 1/15!
	assert d[15..] == a[15..]
}


fn test_all() {
	a := [1,1,1,1]
	assert all<int>(a, 1) == true
	assert all<int>(a, 2) == false
	b := [1,1,2,2]
	assert all<int>(b, 1) == false
	assert all<int>(b, 2) == false
	assert all<int>(b[2..], 1) == false
	assert all<int>(b[2..], 2) == true
	c := [f32(1.1), 1.1, 1.1]
	assert all<f32>(c, 1.1) == true
	assert all<f32>(c, 1.2) == false
	d := [f32(1.1), 1.1, 2.2]
	assert all<f32>(d, 1.1) == false
	assert all<f32>(d, 2.2) == false
}


/*
fn test_replace() {
	mut a := [1,1,2,2,3,3]
	replace<int>(mut a, 2, 1)
	assert a == [1,1,1,1,3,3]
	replace<int>(mut a, 0, 1)
	assert a == [1,1,1,1,3,3]
}
*/
