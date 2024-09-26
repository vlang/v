struct St1 {
	val     int = 5
	another chan f64
}

fn fn1(c chan St1) string {
	println('1')
	println(c)
	x := <-c
	println(x)
	return x.str()
}

fn test_printing_of_channels() {
	ch := chan St1{cap: 10}
	fch := chan f64{cap: 100}
	ch <- St1{
		val:     1000
		another: fch
	}
	res := (spawn fn1(ch)).wait()
	println(res)
	println(ch)
	assert res.str().contains('another: chan f64{cap: 100, closed: 0}')
	assert ch.str() == 'chan St1{cap: 10, closed: 0}'
	assert fch.str() == 'chan f64{cap: 100, closed: 0}'
	fch.close()
	assert fch.str() == 'chan f64{cap: 100, closed: 1}'
}

struct Aa {}

struct Ab {}

type As = Aa | Ab

fn func(ch chan As) {
	ch <- Aa{}
}

fn test_chan_of_sumtype() {
	a := chan As{}
	spawn func(a)
	ret := <-a
	println(ret)
	assert '${ret}' == 'As(Aa{})'
}

struct Iter[T] {
	item chan T
}

fn new_iter[T](ch chan T) Iter[T] {
	return Iter[T]{
		item: ch
	}
}

fn (self Iter[T]) next() ?T {
	self.item.close()
	ch := <-self.item or { return none }
	return ch
}

fn test_channel_with_or_block() {
	ch := chan int{}
	iter := new_iter[int](ch)
	ret := iter.next() or {
		assert true
		return
	}
	println(ret)
	assert false
}
