struct Test {
}

fn (tt Test) encode[U](val U) {
	tt.g_array(val)
	// tt.g_array2(typeof(val).name, val)
	// tt.g_array3(typeof(val).name, val, val)
}

fn (tt Test) encode2[U](idx int, val U) {
	tt.g_array(val)
	// tt.g_array2(typeof(val).name, val)
	// tt.g_array3(typeof(val).name, val, val)
}

fn (tt Test) g_array[T](t []T) {
	assert true
}

fn (tt Test) g_array2[T](name string, t []T) {
	assert name == typeof(t).name
}

fn (tt Test) g_array3[T, U](name string, t []T, u []U) {
	assert name == typeof(t).name
	assert name == typeof(u).name
}

fn test_func() {
	encode([true])
	encode([1])
	encode(['1'])
	encode([1.2])

	encode2(1, [true])
	encode2(2, [1])
	encode2(3, ['1'])
	encode2(4, [1.2])
}

fn test_method() {
	t := Test{}
	t.encode([true])
	t.encode([1])
	t.encode(['1'])
	t.encode([1.2])

	t.encode2(1, [true])
	t.encode2(2, [1])
	t.encode2(3, ['1'])
	t.encode2(4, [1.2])
}

fn encode[U](val U) {
	g_array(val)
	g_array2(typeof(val).name, val)
	g_array3(typeof(val).name, val, val)
}

fn encode2[U](idx int, val U) {
	g_array(val)
	g_array2(typeof(val).name, val)
	g_array3(typeof(val).name, val, val)
}

fn g_array[T](t []T) {
	assert true
}

fn g_array2[T](name string, t []T) {
	assert name == typeof(t).name
}

fn g_array3[T, U](name string, t []T, u []U) {
	assert name == typeof(t).name
	assert name == typeof(u).name
}
