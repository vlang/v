import rand
import strings

fn test_random_strings() {
	mut m := new_hashmap(1000)
	for i in 0..1000 {
		mut buf := []byte
		for j in 0..10 {
			buf << byte(rand.next(int(`z`) - int(`a`)) + `a`)
		}
		s := string(buf)
		//println(s)
		m.set(s, i)
		assert m.get(s) == i
	}
	m.set('foo', 12)
	val := m.get('foo')
	assert val == 12
}	

fn test_large_hashmap() {
	N := 300 * 1000
	mut nums := new_hashmap(N)
	for i := 0; i < N; i++ {
	        key := i.str()
	        nums.set(key, i)
	}
	println('nr collisions: $nums.nr_collisions')
	for i := 0; i < N; i++ {
		key := i.str()
		assert nums.get(key) == i
	}
}	
