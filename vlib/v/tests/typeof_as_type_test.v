// `typeof(expr)` written in a type position names the type of `expr`.

struct TypeofRow {
	id    int
	title string
}

struct TypeofApp {
	rows  []TypeofRow
	count int
}

struct TypeofMemo[T] {
	f fn () !T = unsafe { nil }
}

fn (m TypeofMemo[T]) get() !T {
	return m.f()!
}

fn typeof_memo[T](f fn () !T) TypeofMemo[T] {
	return TypeofMemo[T]{f}
}

fn typeof_each[E](items []E, render fn (item E, index int) string) []string {
	mut out := []string{}
	for i, item in items {
		out << render(item, i)
	}
	return out
}

fn typeof_build[A](mut app A) (int, []string, int) {
	count := typeof_memo(fn [mut app] [A]() !typeof(app.count) {
		return app.count
	})
	doubled := typeof_memo(fn [count] () !typeof(count.get()! * 2) {
		return count.get()! * 2
	})
	mut picked := []typeof(app.rows[0]){}
	picked << app.rows[1]
	labels := typeof_each(app.rows, fn (item typeof(app.rows[0]), index int) string {
		return '${index}:${item.title}'
	})
	return doubled.get() or { 0 }, labels, picked[0].id
}

fn test_typeof_types_resolve_in_their_lexical_scope() {
	mut app := TypeofApp{
		count: 21
		rows:  [TypeofRow{7, 'ñ'}, TypeofRow{8, 'b'}]
	}
	doubled, labels, id := typeof_build(mut app)
	assert doubled == 42
	assert labels == ['0:ñ', '1:b']
	assert id == 8
}

fn test_typeof_return_type_of_an_anonymous_function() {
	base := 2.5
	half := fn [base] () typeof(base) {
		return base / 2
	}
	assert half() == 1.25
}
