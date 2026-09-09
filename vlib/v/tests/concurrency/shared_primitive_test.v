fn read_int(n int) int {
	return n
}

fn read_string(s string) string {
	return s
}

fn test_shared_primitive_types() {
	shared counter := int(41)
	shared greeting := 'hi'

	rlock counter {
		assert read_int(counter) == 41
	}
	lock counter {
		counter++
		counter = 43
	}
	rlock counter {
		assert read_int(counter) == 43
	}

	rlock greeting {
		assert read_string(greeting) == 'hi'
	}
	lock greeting {
		greeting = 'hi!'
	}
	rlock greeting {
		assert read_string(greeting) == 'hi!'
	}
}
