import net.http
import veb
import time
import x.sessions
import x.sessions.veb_middleware

struct PreSessionContext {
	veb.Context
	sessions.CurrentSession[string]
}

fn test_explicit_save_creates_session_in_both_modes() {
	for save_uninitialized in [false, true] {
		mut s := sessions.Sessions[string]{
			secret:             'pre-session-secret'.bytes()
			store:              sessions.MemoryStore[string]{}
			save_uninitialized: save_uninitialized
		}
		mut ctx := PreSessionContext{}
		s.save(mut ctx, 'saved')!
		assert ctx.session_id != ''
		assert ctx.session_data == ?string('saved')
		assert s.get(ctx)! == 'saved'
		cookies := ctx.res.cookies()
		assert cookies.len == 1
		sid, valid := sessions.verify_session_id(cookies[0].value, s.secret)
		assert valid
		assert sid == ctx.session_id
	}
}

fn test_middleware_only_creates_pre_session_when_enabled() {
	for save_uninitialized in [false, true] {
		mut s := sessions.Sessions[string]{
			secret:             'pre-session-secret'.bytes()
			store:              sessions.MemoryStore[string]{}
			save_uninitialized: save_uninitialized
		}
		middleware := veb_middleware.create[string, PreSessionContext](mut s)
		mut ctx := PreSessionContext{}
		assert middleware.handler(mut ctx)
		assert ctx.session_data == none
		if save_uninitialized {
			assert ctx.session_id != ''
			assert ctx.res.cookies().len == 1
		} else {
			assert ctx.session_id == ''
			assert ctx.res.cookies().len == 0
		}
		pre_session_id := ctx.session_id
		s.save(mut ctx, 'saved')!
		assert ctx.session_data == ?string('saved')
		assert s.get(ctx)! == 'saved'
		assert ctx.res.cookies().len == 1
		if save_uninitialized {
			assert ctx.session_id == pre_session_id
		}
	}
}

fn test_save_replaces_invalid_cookie_in_both_modes() {
	for save_uninitialized in [false, true] {
		mut s := sessions.Sessions[string]{
			secret:             'pre-session-secret'.bytes()
			store:              sessions.MemoryStore[string]{}
			save_uninitialized: save_uninitialized
		}
		mut ctx := PreSessionContext{}
		ctx.req.add_cookie(http.Cookie{ name: 'sid', value: 'invalid.signature' })
		s.save(mut ctx, 'saved')!
		assert ctx.session_id != ''
		assert ctx.session_id != 'invalid'
		assert s.get(ctx)! == 'saved'
	}
}

struct FailingSaveStore {}

fn (mut store FailingSaveStore) get(sid string, max_age time.Duration) !string {
	_ = sid
	_ = max_age
	return error('missing')
}

fn (mut store FailingSaveStore) destroy(sid string) ! {
	_ = sid
}

fn (mut store FailingSaveStore) set(sid string, val string) ! {
	_ = sid
	_ = val
	return error('cannot save')
}

fn test_save_propagates_store_errors_without_updating_context_data() {
	mut s := sessions.Sessions[string]{
		secret: 'pre-session-secret'.bytes()
		store:  FailingSaveStore{}
	}
	mut ctx := PreSessionContext{}
	s.save(mut ctx, 'saved') or {
		assert err.msg() == 'cannot save'
		assert ctx.session_data == none
		return
	}
	assert false, 'save must propagate store errors'
}
