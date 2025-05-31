module main

struct In[T] {
	source chan T
}

fn emit[T](s In[T], ar []T) {
	for _, i in ar {
		s.source <- i
	}
}

fn from[T](ar []T) In[T] {
	s := In[T]{}

	spawn emit(s, ar)

	return s
}

fn (i In[T]) get() T {
	return <-i.source
}

fn test_main() {
	v := from[int]([1]).get()

	assert v == 1
}
