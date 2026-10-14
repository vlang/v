// The methods of `App` are called only from generic functions, on a value of a type
// parameter (`app.if_arg(mut ctx)` with `app A`), and `Direct.handle` is first called
// through its interface inside a generic function. The body of such a method was
// lowered twice. The second lowering turned the receiver that had been evaluated
// before a hoisted argument (`if`, `match`) into a copy of the `mut` parameter, so
// the mutating method changed the copy, or the generated C did not compile.
struct Base {
mut:
	headers []string
	url     string
}

fn (mut b Base) set_header(key string, value string) {
	b.headers << '${key}=${value}'
}

fn (mut b Base) set_three(first string, second string, third string) {
	b.headers << '${first}=${second}=${third}'
}

// The receiver is taken by value: it is read before the arguments are evaluated.
fn (b Base) count_plus(n int) int {
	return b.headers.len + n
}

struct Context {
	Base
}

struct Holder {
mut:
	ctx Context
}

struct Counter {
mut:
	calls int
}

fn (mut c Counter) next() string {
	c.calls++
	return 'call${c.calls}'
}

fn lookup(key string) !string {
	if key == 'known' {
		return 'found'
	}
	return error('unknown key')
}

fn add_two(mut b Base) int {
	b.headers << 'x=1'
	b.headers << 'y=2'
	return 10
}

struct App {}

fn (app &App) if_arg(mut ctx Base) {
	ctx.set_header('cache', if ctx.url.len > 3 { 'max-age=1' } else { 'no-cache' })
}

fn (app &App) match_arg(mut ctx Base) {
	ctx.set_header('kind', match ctx.url {
		'/' { 'root' }
		'/abcdef' { 'long' }
		else { 'other' }
	})
}

fn (app &App) or_arg(mut ctx Base) {
	ctx.set_header('known', lookup('known') or { 'fallback' })
	ctx.set_header('unknown', lookup('nope') or { 'fallback' })
}

fn (app &App) later_arg(mut ctx Base, mut counter Counter) {
	ctx.set_three(counter.next(), counter.next(), if ctx.url.len > 3 { 'long' } else { 'short' })
}

fn (app &App) embedded(mut ctx Context) {
	ctx.set_header('cache', if ctx.url.len > 3 { 'max-age=1' } else { 'no-cache' })
}

fn (app &App) field_receiver(mut holder Holder) {
	holder.ctx.set_header('cache', if holder.ctx.url.len > 3 { 'max-age=1' } else { 'no-cache' })
}

fn (app &App) value_receiver(url string) (int, []string) {
	mut local := Base{
		url: url
	}
	n := local.count_plus(if local.url.len > 3 { add_two(mut local) } else { 0 })
	return n, local.headers
}

fn call_if_arg[A](app A, mut ctx Base) {
	app.if_arg(mut ctx)
}

fn call_match_arg[A](app A, mut ctx Base) {
	app.match_arg(mut ctx)
}

fn call_or_arg[A](app A, mut ctx Base) {
	app.or_arg(mut ctx)
}

fn call_later_arg[A](app A, mut ctx Base, mut counter Counter) {
	app.later_arg(mut ctx, mut counter)
}

fn call_embedded[A](app A, mut ctx Context) {
	app.embedded(mut ctx)
}

fn call_field_receiver[A](app A, mut holder Holder) {
	app.field_receiver(mut holder)
}

fn call_value_receiver[A](app A, url string) (int, []string) {
	return app.value_receiver(url)
}

interface Handler {
	handle(mut ctx Base)
}

struct Direct {}

fn (d Direct) handle(mut ctx Base) {
	ctx.set_header('cache', if ctx.url.len > 3 { 'max-age=1' } else { 'no-cache' })
}

fn dispatch[T](handler T, mut ctx Base) {
	handler.handle(mut ctx)
}

fn test_if_expr_argument_of_a_method_on_a_mut_parameter() {
	mut ctx := Base{
		url: '/abcdef'
	}
	call_if_arg(&App{}, mut ctx)
	assert ctx.headers == ['cache=max-age=1']
	ctx.url = '/'
	call_if_arg(&App{}, mut ctx)
	assert ctx.headers == ['cache=max-age=1', 'cache=no-cache']
}

fn test_match_expr_argument_of_a_method_on_a_mut_parameter() {
	mut ctx := Base{
		url: '/abcdef'
	}
	call_match_arg(&App{}, mut ctx)
	assert ctx.headers == ['kind=long']
}

fn test_or_block_argument_of_a_method_on_a_mut_parameter() {
	mut ctx := Base{}
	call_or_arg(&App{}, mut ctx)
	assert ctx.headers == ['known=found', 'unknown=fallback']
}

fn test_hoisted_argument_after_other_arguments() {
	mut ctx := Base{
		url: '/abcdef'
	}
	mut counter := Counter{}
	call_later_arg(&App{}, mut ctx, mut counter)
	assert ctx.headers == ['call1=call2=long']
	assert counter.calls == 2
}

fn test_method_of_an_embedded_struct_on_a_mut_parameter() {
	mut ctx := Context{
		url: '/abcdef'
	}
	call_embedded(&App{}, mut ctx)
	assert ctx.headers == ['cache=max-age=1']
}

fn test_struct_field_receiver_of_a_mut_parameter() {
	mut holder := Holder{
		ctx: Context{
			url: '/abcdef'
		}
	}
	call_field_receiver(&App{}, mut holder)
	assert holder.ctx.headers == ['cache=max-age=1']
}

fn test_value_receiver_is_read_before_the_hoisted_argument() {
	// `count_plus` takes its receiver by value: it sees the headers from before
	// `add_two` ran, while the variable itself keeps what `add_two` appended.
	n, headers := call_value_receiver(&App{}, '/abcdef')
	assert n == 10
	assert headers == ['x=1', 'y=2']
}

fn test_interface_method_first_called_through_the_interface_in_a_generic_fn() {
	mut ctx := Base{
		url: '/abcdef'
	}
	direct := Direct{}
	direct.handle(mut ctx)
	assert ctx.headers == ['cache=max-age=1']
	dispatch[Handler](Handler(direct), mut ctx)
	assert ctx.headers == ['cache=max-age=1', 'cache=max-age=1']
}
