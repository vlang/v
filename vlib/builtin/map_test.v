import time 
 
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
	assert m.size == 0 
	m['hi'] = 80
	m['hello'] = 101 
	assert m['hi'] == 80
	assert m['hello'] == 101 
	assert m.size == 2 
	mut sum := 0
	mut key_sum := '' 
	// Test `for in` 
	for key, val in m {
		sum += val 
		key_sum += key 
	} 
	assert sum == 80 + 101 
	assert key_sum == 'hihello' 
	// Test `.keys()` 
	keys := m.keys() 
	assert keys.len == 2 
	assert keys[0] == 'hi'
	assert keys[1] == 'hello' 
	//// 
	mut users := map[string]User{} 
	users['1'] = User{'Peter'} 
	peter := users['1']
	assert  peter.name == 'Peter' 
	println(peter.name) 

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

fn test_string_map() {
	//m := map[string]Fn
} 

fn test_large_map() { 
	//ticks := time.ticks() 
	mut nums := map[string]int{} 
	N := 30 * 1000
	for i := 0; i < N; i++ {
	        key := i.str()
	        nums[key] = i
	}
	assert nums['1'] == 1 
	assert nums['999'] == 999 
	assert nums['1000000'] == 0 
	//println(time.ticks() - ticks) 
} 
 
