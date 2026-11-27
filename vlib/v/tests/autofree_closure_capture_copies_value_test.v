// vtest vflags: -autofree -gc none
// A closure that captures a local by value gets its own copy: releasing the
// closure must not free a value the local (or the caller) still owns.
import builtin.closure

struct Item {
	name string
	tags []string
}

fn text(s string) string {
	return s.repeat(2)
}

fn test_lifetime_reclaim_keeps_captured_locals() {
	mut lifetime := closure.new_lifetime()
	names := [text('a'), text('b')]
	s := text('x')
	item := Item{text('i'), [text('t')]}
	m := {
		'k': text('v')
	}
	mut count := [text('c')]
	lifetime.frame(fn [names, s, item, m, mut count] () {
		count << text('d')
		assert names.len == 2
		assert s == 'xx'
		assert item.tags == ['tt']
		assert m['k'] == 'vv'
		assert count == ['cc', 'dd']
	})!
	lifetime.reclaim_all()!
	assert names == ['aa', 'bb']
	assert s == 'xx'
	assert item.name == 'ii'
	assert item.tags == ['tt']
	assert m['k'] == 'vv'
	assert count == ['cc']
}

fn join_borrowed(names []string, sep string) string {
	f := fn [names, sep] () string {
		return names.join(sep)
	}
	return f()
}

fn test_capture_of_borrowed_params() {
	names := [text('a'), text('b')]
	assert join_borrowed(names, ',') == 'aa,bb'
	assert join_borrowed(names, ';') == 'aa;bb'
	assert names == ['aa', 'bb']
}

fn make_prefixer(prefix string) fn (string) string {
	return fn [prefix] (x string) string {
		return prefix + x
	}
}

fn test_returned_closure_and_lambda() {
	p := text('p')
	g := make_prefixer(p)
	assert g('1') == 'pp1'
	assert g('2') == 'pp2'
	assert p == 'pp'
	suffix := text('s')
	out := [1, 2].map(|n| '${n}${suffix}')
	assert out == ['1ss', '2ss']
	assert suffix == 'ss'
}
