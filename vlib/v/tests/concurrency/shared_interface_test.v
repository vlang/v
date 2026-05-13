import time

interface MyInterface {
	foo() string
}

struct MyStruct {
pub mut:
	fooer shared MyInterface
}

struct MyImplementor {
mut:
	num int
}

fn (m MyImplementor) foo() string {
	// Can read member properties:
	num := m.num
	return 'Hello World ${num}!'
}

fn test_shared_interface_lock_1() {
	shared imp1 := MyImplementor{
		num: 1
	}
	s1 := MyStruct{
		fooer: imp1
	}
	lock s1.fooer {
		assert s1.fooer.foo() == 'Hello World 1!'
	}
}

fn test_shared_interface_lock_2() {
	shared imp := MyOtherImplementor{
		x: 1
		y: 2
		s: 'testing'
	}
	s := MyStruct{
		fooer: imp
	}

	lock s.fooer {
		assert s.fooer.foo() == 'Hello World (1, 2, testing)!'
	}

	// Lock is released and can be locked again:
	lock s.fooer {
		assert s.fooer.foo() == 'Hello World (1, 2, testing)!'
	}
}

fn test_shared_interface_lock_blocks_the_original_shared_value() {
	shared imp := MyImplementor{
		num: 6
	}
	s := MyStruct{
		fooer: imp
	}
	ready := chan bool{}
	proceed := chan bool{}
	done := chan bool{}
	task := spawn modify_shared_interface_impl(shared imp, ready, proceed, done)
	_ := <-ready
	lock s.fooer {
		proceed <- true
		select {
			_ := <-done {
				assert false, 'shared interface value was modified before the field lock was released'
			}
			20 * time.millisecond {}
		}
		assert s.fooer.foo() == 'Hello World 6!'
	}
	_ := <-done
	task.wait()
	lock s.fooer {
		assert s.fooer.foo() == 'Hello World 7!'
	}
}

fn modify_shared_interface_impl(shared x MyImplementor, ready chan bool, proceed chan bool, done chan bool) {
	ready <- true
	_ := <-proceed
	lock x {
		x.num++
	}
	done <- true
}

// TODO: Fix modifying shared interface value
// fn test_shared_interface_can_be_modified() {
// 	shared imp1 := MyImplementor{num: 6}
// 	shared imp2 := MyOtherImplementor{
// 		x: 7
// 		y: 3
// 		s: 'here be dragons...'
// 	}
// 	s := MyStruct{
// 		fooer: imp1
// 	}
// 	lock s.fooer {
// 		assert(s.fooer.foo() == 'Hello World 6!')
// 		s.fooer = imp2
// 	}
// 	lock s.fooer {
// 		assert(s.fooer.foo() == 'Hello World (7, 3, here be dragons...)!')
// 	}
// }

struct MyOtherImplementor {
	x int
	y int
	s string
}

fn (m MyOtherImplementor) foo() string {
	// Different implementation:
	return 'Hello World (${m.x}, ${m.y}, ${m.s})!'
}
