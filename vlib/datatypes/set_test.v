module datatypes

fn test_is_empty() {
	mut set := Set<int>{}
	assert set.is_empty() == true
	set.add(1)
	assert set.is_empty() == false
}

fn test_len() {
	mut set := Set<int>{}
	assert set.len() == 0
	set.add(...[1,1, 4,7,1])
	assert set.len() == 3
}
fn test_delete() {
	mut set := Set<int>{}
	set.add(...[]int{len:10,init:it})
	assert set.len() == 10
	set.delete(5)
	set.delete(5)// meant to be duplicate
	set.delete(6)
	assert set.len() == 8
}
fn test_difference() {
	mut set_a := Set<int>{}
	set_a.add(...[]int{len:10,init:it})
	mut set_b := Set<int>{}
	set_b.add(...[]int{len:10,init:it+5})
	set_c := set_a.difference(set_b) or {panic('ERROR with difference')}
	assert set_c.len() == 5
}
/*fn test_equal() {
	mut set_a := Set<int>{}
	set_a.add(...[]int{len:10,init:it})
	mut set_b := Set<int>{}
	set_b.add(...[]int{len:10,init:it+5})
	mut set_c := Set<string>{}
	//println('${typeof(set_a).name} ${typeof(set_b).name}')
	set_c.add(...['one','two','three'])
	set_d := set_a.clone()
	assert set_a.equal(set_b) == false
	assert set_a.equal(set_c) == false
	assert (set_a == set_d) == true

	set_a.delete(0)
	set_a.delete(1)
	set_a.delete(2)
	set_a.delete(3)
	set_a.delete(4)
	set_b.delete(10)
	set_b.delete(11)
	set_b.delete(12)
	set_b.delete(13)
	set_b.delete(14)
	assert set_a.equal(set_b) == true
}*/

fn test_union_with() {
	mut set_a := Set<int>{}
	set_a.add(...[]int{len:10,init:it})
	mut set_b := Set<int>{}
	set_b.add(...[]int{len:10,init:it+5})
	set_c := set_a.union_with(set_b) or { Set<int>{} }
	assert set_c.len() == 15
	set_a.delete(0)
	set_a.delete(1)
	assert (set_a+set_b).len() == 13
}
/*fn test_intersection(){
	mut colors := Set<string>{}
	colors.add(...['orange','green','red','blue'])
	mut fruit := Set<string>{}
	fruit.add(...['orange','apple','banana'])
	set_c := colors.intersection(fruit) or {panic('Problem with intersection')}
	assert set_c.len() == 1
	assert set_c.contains('orange') == true
}*/

/*fn test_is_subset() {
	mut parent :=  Set<f64>{}
	parent.add(...[6.5,9.4,3.3,55,1,1.5])
	mut child := parent.clone()

	assert child.is_subset(parent) == true
	assert child.is_proper_subset(parent) == false

	child.delete(6.5)
	assert child.is_proper_subset(parent) == true
}
fn test_is_superset() {
	mut parent :=  Set<bool>{}
	parent.add(...[true, false])
	mut child := parent.clone()

	assert parent.is_superset(child) == true
	assert parent.is_proper_superset(child) == false

	child.delete(true)
	assert parent.is_proper_superset(child) == true
}*/
