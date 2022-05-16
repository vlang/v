import datatypes as dt

fn test_is_empty() {
	mut set := dt.Set<int>{}
	assert set.is_empty() == true
	set.add(1)
	assert set.is_empty() == false
}

fn test_len() {
	mut set := dt.Set<int>{}
	assert set.len() == 0
	set.add(...[1,1, 4,7,1])
	assert set.len() == 3
}
fn test_delete() {
	mut set := dt.Set<int>{}
	set.add(...[]int{len:10,init:it})
	assert set.len() == 10
	set.delete(5)
	set.delete(5)// meant to be duplicate
	set.delete(6)
	assert set.len() == 8
}
/*fn test_difference() {
	mut set_a := dt.Set<int>{}
	set_a.add(...[]int{len:10,init:it})
	mut set_b := dt.Set<int>{}
	set_b.add(...[]int{len:10,init:it+5})
	//println('$set_a $set_b ${set_a.difference(set_b)}')
	assert set_a.difference(set_b).str() == '[5,6,7,8,9]'
}*/
/*fn test_equal() {
	mut set_a := dt.Set<int>{}
	set_a.add(...[]int{len:10,init:it})
	mut set_b := dt.Set<int>{}
	set_b.add(...[]int{len:10,init:it+5})
	mut set_c := dt.Set<string>{}
	//println('${typeof(set_a).name} ${typeof(set_b).name}')
	set_c.add(...['one','two','three'])
	set_d := set_a.clone()
	assert set_a.equal(set_b) == false
	assert set_a.equal(set_c) == false
	assert (set_a == set_d) == true
}*/

fn test_union_with() {
	mut set_a := dt.Set<int>{}
	set_a.add(...[]int{len:10,init:it})
	mut set_b := dt.Set<int>{}
	set_b.add(...[]int{len:10,init:it+5})
	set_c := set_a.union_with(set_b) or { dt.Set<int>{} }
	assert set_c.len() == 15
	set_a.delete(0)
	set_a.delete(1)
	assert (set_a+set_b).len() == 13
}
fn test_intersection(){
	mut colors := dt.Set<string>{}
	colors.add(...['orange','green','red','blue'])
	mut fruit := dt.Set<string>{}
	fruit.add(...['orange','apple','banana'])
	set_c := colors.intersection(fruit) or {panic('Problem with intersection')}
	assert set_c.len() == 1
	assert set_c.contains('orange') == true
}

