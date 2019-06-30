struct User {
	name string 
} 

struct A {
	m map[string]int 
	users map[string]User 
}

fn (a mut A) set(key string, val int) {
	a.m[key] = val
}

fn test_map() {
	mut m := map[string]int{}
	m['hi'] = 80
	assert m['hi'] == 80
	//// 
	mut users := map[string]User{} 
	users['1'] = User{'Peter'} 
	peter := users['1']
	assert  peter.name == 'Peter' 

	mut a := A{
		m: map[string]int{} 
		users: map[string]User{} 
	}
	a.users['Bob'] = User{'Bob'} 
	q := a.users['Bob'] 
	assert q.name == 'Bob' 
	a.m['one'] = 1
	a.set('two', 2)
	assert a.m['one'] == 1
	assert a.m['two'] == 2
}
