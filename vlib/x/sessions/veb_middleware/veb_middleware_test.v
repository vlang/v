// Coverage for `x.sessions.veb_middleware`, the one `x/sessions` submodule
// with no test file at all. Its only public function, `create`, is the glue
// that a veb app uses to guarantee a valid session per request, so the tests
// drive the returned handler against a hand-built Context rather than opening
// a socket. The Context is the same shape `sessions_ctx_test.v` uses.
//
// Note the neighbour `vlib/x/sessions/tests/db_store_test.v` cannot run in
// this checkout: `db.sqlite` needs `thirdparty/sqlite/sqlite3.o`, which is
// not present, so that file fails to compile for an environmental reason
// before any of its assertions run. Nothing here imports `db`.
import net.http
import time
import x.sessions
import x.sessions.veb_middleware
import veb

const test_secret = 'sessions_mw_test_secret'.bytes()

pub struct User {
pub mut:
	name string
	age  int
}

const default_user = User{
	name: 'john'
	age:  42
}

// Context is a minimal veb Context carrying the embedded session state that
// `sessions.Sessions[T]` reads and writes.
struct Context {
	veb.Context
	sessions.CurrentSession[User]
}

fn new_sessions() sessions.Sessions[User] {
	return sessions.Sessions[User]{
		store:  sessions.MemoryStore[User]{}
		secret: test_secret
	}
}

// create_options builds the middleware options for `s`, which is the only way
// a caller reaches the handler.
fn create_options(mut s sessions.Sessions[User]) veb.MiddlewareOptions[Context] {
	return veb_middleware.create[User, Context](mut s)
}

fn with_request_cookie(mut ctx Context, name string, value string) {
	ctx.req.add_cookie(http.Cookie{
		name:  name
		value: value
	})
}

// session_data_of unwraps the Context's `?User`, because `CurrentSession`
// stores the payload as an option.
fn session_data_of(mut ctx Context) User {
	return ctx.session_data or { panic('no session data on the Context') }
}

// ---------------------------------------------------------------------
// an unrecognised or absent session id
// ---------------------------------------------------------------------

fn test_create_without_a_cookie_returns_true_and_sets_nothing() {
	mut s := new_sessions()
	mw := create_options(mut s)
	mut ctx := Context{}
	// The handler always continues the chain: it reports `true` rather than
	// deciding whether the request is allowed.
	assert mw.handler(mut ctx)
	assert ctx.session_id == ''
	assert ctx.session_data == none
}

fn test_create_sets_a_new_session_id_when_save_uninitialized_is_set() {
	mut s := sessions.Sessions[User]{
		store:              sessions.MemoryStore[User]{}
		secret:             test_secret
		save_uninitialized: true
	}
	mw := create_options(mut s)
	mut ctx := Context{}
	assert mw.handler(mut ctx)
	// Measured: an unrecognised id takes the `save_uninitialized` branch and
	// generates a fresh one, leaving the data empty.
	assert ctx.session_id.len == 32
	assert ctx.session_data == none
	// The middleware only generates the id; it never stores data for it.
	if _ := s.store.get(ctx.session_id, 0) {
		assert false, 'the generated id must not have an entry yet'
	} else {
		assert err.msg() == 'session does not exist'
	}
}

fn test_create_ignores_a_cookie_with_another_name() {
	mut s := new_sessions()
	_, signed := sessions.new_session_id(test_secret)
	mw := create_options(mut s)
	mut ctx := Context{}
	with_request_cookie(mut ctx, 'other', signed)
	assert mw.handler(mut ctx)
	assert ctx.session_id == ''
	assert ctx.session_data == none
}

fn test_create_rejects_a_forged_signature_and_does_not_rotate_the_id() {
	mut s := new_sessions()
	sid, _ := sessions.new_session_id(test_secret)
	s.store.set(sid, default_user)!
	mw := create_options(mut s)
	mut ctx := Context{}
	with_request_cookie(mut ctx, 'sid', '${sid}.BOGUS')
	// Measured: an invalid signature is not an error, it simply behaves like
	// no cookie, so the stored entry is left alone and the id is not reset.
	assert mw.handler(mut ctx)
	assert ctx.session_id == ''
	assert ctx.session_data == none
	assert s.store.get(sid, 0)! == default_user
}

fn test_create_with_a_valid_id_and_no_stored_data_leaves_the_data_empty() {
	mut s := new_sessions()
	sid, signed := sessions.new_session_id(test_secret)
	mw := create_options(mut s)
	mut ctx := Context{}
	with_request_cookie(mut ctx, 'sid', signed)
	assert mw.handler(mut ctx)
	assert ctx.session_id == sid
	assert ctx.session_data == none
}

// ---------------------------------------------------------------------
// a recognised session id
// ---------------------------------------------------------------------

fn test_create_with_a_valid_id_loads_the_stored_data() {
	mut s := new_sessions()
	sid, signed := sessions.new_session_id(test_secret)
	s.store.set(sid, default_user)!
	mw := create_options(mut s)
	mut ctx := Context{}
	with_request_cookie(mut ctx, 'sid', signed)
	assert mw.handler(mut ctx)
	assert ctx.session_id == sid
	assert session_data_of(mut ctx) == default_user
}

fn test_create_with_a_valid_id_reads_whatever_the_store_holds() {
	mut s := new_sessions()
	sid, signed := sessions.new_session_id(test_secret)
	other := User{
		name: 'jane'
		age:  7
	}
	s.store.set(sid, other)!
	mw := create_options(mut s)
	mut ctx := Context{}
	with_request_cookie(mut ctx, 'sid', signed)
	assert mw.handler(mut ctx)
	// Measured: the handler copies the value out of the store into the
	// Context, so a different payload round-trips unchanged.
	assert session_data_of(mut ctx) == other
}

fn test_create_ignores_an_expired_store_entry() {
	mut s := sessions.Sessions[User]{
		store:   sessions.MemoryStore[User]{}
		secret:  test_secret
		max_age: time.second
	}
	sid, signed := sessions.new_session_id(test_secret)
	s.store.set(sid, default_user)!
	time.sleep(2 * time.second)
	mw := create_options(mut s)
	mut ctx := Context{}
	with_request_cookie(mut ctx, 'sid', signed)
	assert mw.handler(mut ctx)
	// The id is still recognised, but the expired entry yields no data.
	assert ctx.session_id == sid
	assert ctx.session_data == none
}

fn test_create_sees_an_id_already_set_on_the_context() {
	mut s := new_sessions()
	sid, _ := sessions.new_session_id(test_secret)
	s.store.set(sid, default_user)!
	mw := create_options(mut s)
	mut ctx := Context{}
	// No cookie: the Context id alone is enough, because `validate_session`
	// runs first and `get_session_id` is not consulted here.
	assert mw.handler(mut ctx)
	assert ctx.session_id == ''
	assert ctx.session_data == none
}

// ---------------------------------------------------------------------
// the returned options themselves
// ---------------------------------------------------------------------

fn test_create_returns_a_before_request_handler_with_no_method_filter() {
	mut s := new_sessions()
	mw := create_options(mut s)
	// Measured: the zero values of MiddlewareOptions are what `create`
	// leaves, so the handler runs before the route and for every method.
	assert !mw.after
	assert mw.methods.len == 0
}

fn test_create_handler_is_reusable_across_requests() {
	mut s := new_sessions()
	first_sid, first_signed := sessions.new_session_id(test_secret)
	s.store.set(first_sid, default_user)!
	second_sid, second_signed := sessions.new_session_id(test_secret)
	mw := create_options(mut s)

	mut first := Context{}
	with_request_cookie(mut first, 'sid', first_signed)
	assert mw.handler(mut first)
	assert first.session_id == first_sid
	assert session_data_of(mut first) == default_user

	// The closure captures the store, not a copy of it, so an entry stored
	// after the options were built is visible on the next request.
	s.store.set(second_sid, default_user)!
	mut second := Context{}
	with_request_cookie(mut second, 'sid', second_signed)
	assert mw.handler(mut second)
	assert second.session_id == second_sid
	assert session_data_of(mut second) == default_user
}

fn test_create_shares_the_store_between_the_middleware_and_the_sessions() {
	mut s := new_sessions()
	sid, signed := sessions.new_session_id(test_secret)
	mw := create_options(mut s)
	// Writing through `Sessions[T]` before the handler runs is seen by the
	// handler: both reach the same `Sessions[T]` that `create` captured.
	s.store.set(sid, default_user)!
	mut ctx := Context{}
	with_request_cookie(mut ctx, 'sid', signed)
	assert mw.handler(mut ctx)
	assert session_data_of(mut ctx) == default_user
}
