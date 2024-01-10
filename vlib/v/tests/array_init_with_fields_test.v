import datatypes

struct Fluid[T] {
mut:
	elements  datatypes.Queue[T]
	switch    bool // 0 by default
	selements datatypes.Stack[T]
}

fn (mut fluid Fluid[T]) push(item T) {
	if fluid.switch {
		fluid.selements.push(item)
	} else {
		fluid.elements.push(item)
	}
}

fn (mut fluid Fluid[T]) pop() !T {
	if fluid.switch {
		return fluid.selements.pop()!
	} else {
		return fluid.elements.pop()!
	}
}

fn (fluid Fluid[T]) str() string {
	if fluid.switch {
		return fluid.selements.str()
	} else {
		return fluid.elements.str()
	}
}

fn (fluid Fluid[T]) peek() !T {
	if fluid.switch {
		return fluid.selements.peek()!
	} else {
		return fluid.elements.peek()!
	}
}

// this function converts queue to stack
fn (mut fluid Fluid[T]) to_stack() !bool {
	if !fluid.switch {
		// convert queue to stack
		for i in 1 .. fluid.elements.len() + 1 {
			fluid.selements.push(fluid.elements.index(fluid.elements.len() - i)!)
		}
		fluid.switch = true
	}
	return true
}

fn test_array_init_with_fields() {
	mut arr := []Fluid[string]{len: 10}
	for i in 0 .. arr.len {
		// add str to queue
		arr[i].push('${10 + i}')
	}
	println(arr)
	ret := arr[1].to_stack()!
	assert ret
}
