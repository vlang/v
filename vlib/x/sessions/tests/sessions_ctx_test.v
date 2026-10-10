// Coverage for the x.sessions entry points that the sibling files in this
// directory do not reach: `Sessions[T]` is normally driven through a veb
// server by session_app_test.v, so these tests drive the same methods with a
// hand-built Context and never open a socket.
import net.http
import time
import x.sessions
import veb

const test_secret = 'sessions_ctx_test_secret'.bytes()

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

fn with_request_cookie(mut ctx Context, name string, value string) {
	ctx.req.add_cookie(http.Cookie{
		name:  name
		value: value
	})
}

// store_has probes the store through the `Store[T]` interface, which is how
// `Sessions[T]` itself reaches it. `max_age` of 0 disables expiry.
fn store_has(mut s sessions.Sessions[User], sid string) bool {
	if _ := s.store.get(sid, 0) {
		return true
	}
	return false
}

fn session_data_of(mut ctx Context) User {
	return ctx.session_data or { panic('no session data on the Context') }
}

fn test_cookie_options_has_the_documented_defaults() {
	options := sessions.CookieOptions{}
	assert options.cookie_name == 'sid'
	assert options.http_only == true
	assert options.path == '/'
	assert options.same_site == http.SameSite.same_site_strict_mode
	assert options.secure == false
	assert options.domain == ''
}

fn test_max_age_defaults_to_thirty_days() {
	s := new_sessions()
	assert s.max_age == time.hour * 24 * 30
	assert s.save_uninitialized == false
}

fn test_memory_store_all_returns_every_session() {
	mut store := sessions.MemoryStore[User]{}
	store.set('a', default_user)!
	store.set('b', User{ name: 'jane', age: 7 })!
	all := store.all()!
	assert all.len == 2
	assert default_user in all
	assert User{ name: 'jane', age: 7 } in all
}

fn test_memory_store_clear_removes_every_session() {
	mut store := sessions.MemoryStore[User]{}
	store.set('a', default_user)!
	store.set('b', default_user)!
	store.clear()!
	assert store.all()!.len == 0
}

// NOTE: the `Store[T]` interface's own `all`/`clear` are the empty defaults from
// store.v:16 and store.v:22, so a store reached only through the interface type
// cannot expose its richer implementation. `Sessions[T].store` is declared as
// `Store[T]`, so an application cannot call `all`/`clear` through it either.
fn test_store_interface_optional_methods_are_the_empty_defaults() {
	mut store := sessions.MemoryStore[User]{}
	store.set('a', default_user)!
	mut iface := sessions.Store[User](store)
	assert iface.all()!.len == 0
	iface.clear()!
	assert store.data.len == 1
}

fn test_store_interface_defaults_are_used_by_a_minimal_store() {
	mut minimal := MinimalStore{}
	minimal.set('a', default_user)!
	mut iface := sessions.Store[User](minimal)
	assert iface.all()!.len == 0
	iface.clear()!
}

fn test_get_session_id_reads_the_request_cookie() {
	s := new_sessions()
	sid, signed := sessions.new_session_id(test_secret)
	mut ctx := Context{}
	with_request_cookie(mut ctx, 'sid', signed)
	got := s.get_session_id(ctx) or { panic('no session id') }
	assert got == sid
}

fn test_get_session_id_rejects_a_forged_request_cookie() {
	s := new_sessions()
	sid, _ := sessions.new_session_id(test_secret)
	mut ctx := Context{}
	with_request_cookie(mut ctx, 'sid', '${sid}.BOGUS')
	assert s.get_session_id(ctx) == none
}

fn test_get_session_id_ignores_a_cookie_with_another_name() {
	s := new_sessions()
	_, signed := sessions.new_session_id(test_secret)
	mut ctx := Context{}
	with_request_cookie(mut ctx, 'other', signed)
	assert s.get_session_id(ctx) == none
}

fn test_get_session_id_prefers_the_context_over_the_cookie() {
	s := new_sessions()
	sid, _ := sessions.new_session_id(test_secret)
	mut ctx := Context{}
	with_request_cookie(mut ctx, 'sid', '${sid}.BOGUS')
	ctx.session_id = 'already-set'
	got := s.get_session_id(ctx) or { panic('no session id') }
	assert got == 'already-set'
}

// NOTE: this branch returns the whole cookie value, which is the signed
// `sid.hmac` pair, not the session id the other two branches return. The value
// is then used as the store key, so data saved through it is unreachable from a
// request cookie for the same session.
fn test_get_session_id_reads_a_response_cookie_verbatim() {
	s := new_sessions()
	sid, signed := sessions.new_session_id(test_secret)
	mut ctx := Context{}
	ctx.res.header.add(.set_cookie, 'sid=${signed}')
	got := s.get_session_id(ctx) or { panic('no session id') }
	assert got == signed
	assert got != sid
}

fn test_get_session_id_is_none_without_any_cookie() {
	s := new_sessions()
	mut ctx := Context{}
	assert s.get_session_id(ctx) == none
}

fn test_get_session_id_ignores_a_response_cookie_with_another_name() {
	s := new_sessions()
	_, signed := sessions.new_session_id(test_secret)
	mut ctx := Context{}
	ctx.res.header.add(.set_cookie, 'other=${signed}')
	assert s.get_session_id(ctx) == none
}

fn test_validate_session_accepts_a_signed_request_cookie() {
	mut s := new_sessions()
	sid, signed := sessions.new_session_id(test_secret)
	mut ctx := Context{}
	with_request_cookie(mut ctx, 'sid', signed)
	got_sid, valid := s.validate_session(ctx)
	assert valid
	assert got_sid == sid
}

fn test_validate_session_rejects_a_forged_cookie() {
	mut s := new_sessions()
	sid, _ := sessions.new_session_id(test_secret)
	mut ctx := Context{}
	with_request_cookie(mut ctx, 'sid', '${sid}.BOGUS')
	_, valid := s.validate_session(ctx)
	assert !valid
}

fn test_validate_session_is_false_without_a_cookie() {
	mut s := new_sessions()
	mut ctx := Context{}
	sid, valid := s.validate_session(ctx)
	assert !valid
	assert sid == ''
}

fn test_set_session_id_sets_the_context_and_a_cookie() {
	mut s := new_sessions()
	mut ctx := Context{}
	sid := s.set_session_id(mut ctx)
	assert sid.len == 32
	assert ctx.session_id == sid

	// the cookie carries the signed value, the Context the bare id
	cookie := ctx.res.header.get(.set_cookie) or { panic('no Set-Cookie header') }
	assert cookie.starts_with('sid=${sid}.')
	assert cookie.contains('path=/')
	assert cookie.contains('Max-Age=2592000')
	assert cookie.contains('HttpOnly')
	assert cookie.contains('SameSite=Strict')
	assert !cookie.contains('Secure')

	// rfc7234#section-5.2.1.4
	assert ctx.res.header.get(.cache_control) or { '' } == 'no-cache="Set-Cookie"'
}

fn test_set_session_id_honours_the_configured_cookie_name() {
	mut s := sessions.Sessions[User]{
		store:          sessions.MemoryStore[User]{}
		secret:         test_secret
		cookie_options: sessions.CookieOptions{
			cookie_name: 'SESSION_ID'
			secure:      true
			domain:      'example.com'
		}
	}
	mut ctx := Context{}
	s.set_session_id(mut ctx)
	cookie := ctx.res.header.get(.set_cookie) or { panic('no Set-Cookie header') }
	assert cookie.starts_with('SESSION_ID=')
	assert cookie.contains('Secure')
	// NOTE: `path` and `domain` are lower cased while the other attributes are
	// not; that is what net.http's Cookie.str() emits.
	assert cookie.contains('domain=example.com')
}

fn test_set_session_id_uses_the_configured_max_age() {
	mut s := sessions.Sessions[User]{
		store:   sessions.MemoryStore[User]{}
		secret:  test_secret
		max_age: time.minute * 5
	}
	mut ctx := Context{}
	s.set_session_id(mut ctx)
	cookie := ctx.res.header.get(.set_cookie) or { panic('no Set-Cookie header') }
	assert cookie.contains('Max-Age=300')
}

fn test_get_returns_the_stored_data() {
	mut s := new_sessions()
	sid, _ := sessions.new_session_id(test_secret)
	s.store.set(sid, default_user)!
	mut ctx := Context{}
	ctx.session_id = sid
	assert s.get(ctx)! == default_user
}

fn test_get_without_a_session_id_errors() {
	mut s := new_sessions()
	mut ctx := Context{}
	if _ := s.get(ctx) {
		assert false, 'get should fail without a session id'
	} else {
		assert err.msg() == 'cannot find session id'
	}
}

fn test_get_reports_a_missing_store_entry() {
	mut s := new_sessions()
	mut ctx := Context{}
	ctx.session_id = 'never-stored'
	if _ := s.get(ctx) {
		assert false, 'get should fail for an unknown session id'
	} else {
		assert err.msg() == 'session does not exist'
	}
}

fn test_get_of_an_expired_session_errors_and_destroys_it() {
	mut s := sessions.Sessions[User]{
		store:   sessions.MemoryStore[User]{}
		secret:  test_secret
		max_age: time.second
	}
	sid, _ := sessions.new_session_id(test_secret)
	s.store.set(sid, default_user)!
	mut ctx := Context{}
	ctx.session_id = sid
	time.sleep(2 * time.second)
	if _ := s.get(ctx) {
		assert false, 'the session should have expired'
	} else {
		assert err.msg() == 'session is expired'
	}
	assert !store_has(mut s, sid)
}

fn test_destroy_clears_the_context_data() {
	mut s := new_sessions()
	sid, _ := sessions.new_session_id(test_secret)
	s.store.set(sid, default_user)!
	mut ctx := Context{}
	ctx.session_id = sid
	ctx.CurrentSession.session_data = default_user
	s.destroy(mut ctx)!
	assert ctx.session_data == none
	assert !store_has(mut s, sid)
}

fn test_destroy_without_a_session_id_is_a_no_op() {
	mut s := new_sessions()
	sid, _ := sessions.new_session_id(test_secret)
	s.store.set(sid, default_user)!
	mut ctx := Context{}
	s.destroy(mut ctx)!
	assert store_has(mut s, sid)
}

fn test_save_stores_data_for_an_existing_session() {
	mut s := new_sessions()
	sid, _ := sessions.new_session_id(test_secret)
	mut ctx := Context{}
	ctx.session_id = sid
	s.save(mut ctx, default_user)!
	assert session_data_of(mut ctx) == default_user
	assert s.get(ctx)! == default_user
}

// NOTE: the branch at sessions.v:149 tests `save_uninitialized == false`, which
// is the opposite of the documented meaning of the flag ("set to true if you
// want to create a session if there isn't any data stored yet"). This pins the
// behaviour as implemented, not as documented.
fn test_save_creates_a_session_when_save_uninitialized_is_false() {
	mut s := new_sessions()
	mut ctx := Context{}
	s.save(mut ctx, default_user)!
	assert ctx.session_id.len == 32
	assert session_data_of(mut ctx) == default_user
	assert s.get(ctx)! == default_user
}

fn test_save_creates_nothing_when_save_uninitialized_is_true() {
	mut s := sessions.Sessions[User]{
		store:              sessions.MemoryStore[User]{}
		secret:             test_secret
		save_uninitialized: true
	}
	mut ctx := Context{}
	s.save(mut ctx, default_user)!
	assert ctx.session_id == ''
	assert ctx.session_data == none
}

fn test_save_stores_under_a_session_id_taken_from_a_cookie() {
	mut s := new_sessions()
	sid, signed := sessions.new_session_id(test_secret)
	mut ctx := Context{}
	with_request_cookie(mut ctx, 'sid', signed)
	s.save(mut ctx, default_user)!
	assert store_has(mut s, sid)
	assert s.store.get(sid, 0)! == default_user
	// NOTE: the Context id is not updated on this path, only the store is.
	assert ctx.session_id == ''
}

fn test_save_overwrites_existing_data() {
	mut s := new_sessions()
	sid, _ := sessions.new_session_id(test_secret)
	s.store.set(sid, default_user)!
	mut ctx := Context{}
	ctx.session_id = sid
	s.save(mut ctx, User{ name: 'updated', age: 0 })!
	assert s.get(ctx)! == User{ name: 'updated', age: 0 }
}

// NOTE: `resave` calls `get_session_id`, which returns the Context id unchanged
// when it is already set, so the old id survives and the store entry is simply
// overwritten rather than the id being rotated.
fn test_resave_destroys_the_old_data_and_saves_the_new() {
	mut s := new_sessions()
	sid, _ := sessions.new_session_id(test_secret)
	s.store.set(sid, default_user)!
	mut ctx := Context{}
	ctx.session_id = sid
	s.resave(mut ctx, User{ name: 'rotated', age: 7 })!
	assert s.get(ctx)! == User{ name: 'rotated', age: 7 }
}

fn test_resave_creates_a_session_when_there_was_none() {
	mut s := new_sessions()
	mut ctx := Context{}
	s.resave(mut ctx, default_user)!
	assert ctx.session_id.len == 32
	assert s.get(ctx)! == default_user
	assert store_has(mut s, ctx.session_id)
}

fn test_logout_expires_the_cookie_and_clears_the_data() {
	mut s := new_sessions()
	sid, _ := sessions.new_session_id(test_secret)
	s.store.set(sid, default_user)!
	mut ctx := Context{}
	ctx.session_id = sid
	ctx.CurrentSession.session_data = default_user
	s.logout(mut ctx)!
	assert ctx.session_data == none
	assert !store_has(mut s, sid)
	cookie := ctx.res.header.get(.set_cookie) or { panic('no Set-Cookie header') }
	assert cookie.starts_with('sid=;')
	assert cookie.contains('expires=Thu, 01 Jan 1970 00:00:00 GMT')
}

fn test_logout_without_a_session_only_expires_the_cookie() {
	mut s := new_sessions()
	mut ctx := Context{}
	s.logout(mut ctx)!
	assert ctx.res.header.get(.set_cookie) or { '' } != ''
}

// MinimalStore implements only the three methods the Store[T] interface
// requires, so the optional ones fall through to their defaults.
struct MinimalStore {
mut:
	data map[string]User
}

fn (mut s MinimalStore) get(sid string, max_age time.Duration) !User {
	return s.data[sid]
}

fn (mut s MinimalStore) destroy(sid string) ! {
	s.data.delete(sid)
}

fn (mut s MinimalStore) set(sid string, val User) ! {
	s.data[sid] = val
}
