import net.http
import time
import veb
import x.sessions

struct ResaveContext {
	veb.Context
	sessions.CurrentSession[string]
}

fn test_resave_rotates_session_id_and_destroys_old_data() {
	for save_uninitialized in [false, true] {
		mut s := sessions.Sessions[string]{
			secret:             'resave-secret'.bytes()
			store:              sessions.MemoryStore[string]{}
			save_uninitialized: save_uninitialized
		}
		mut ctx := ResaveContext{}
		old_sid := s.set_session_id(mut ctx)
		s.save(mut ctx, 'anonymous')!
		old_cookie := ctx.res.cookies()[0].value
		ctx.res = http.Response{}

		s.resave(mut ctx, 'authenticated')!

		assert ctx.session_id != old_sid
		assert ctx.session_data == ?string('authenticated')
		assert s.get(ctx)! == 'authenticated'
		cookies := ctx.res.cookies()
		assert cookies.len == 1
		new_sid, valid := sessions.verify_session_id(cookies[0].value, s.secret)
		assert valid
		assert new_sid == ctx.session_id
		mut old_ctx := ResaveContext{}
		old_ctx.req.add_cookie(http.Cookie{ name: 'sid', value: old_cookie })
		if _ := s.get(old_ctx) {
			assert false, 'old session data must be destroyed'
		}
	}
}

fn test_resave_creates_session_without_existing_id() {
	for save_uninitialized in [false, true] {
		mut s := sessions.Sessions[string]{
			secret:             'resave-secret'.bytes()
			store:              sessions.MemoryStore[string]{}
			save_uninitialized: save_uninitialized
		}
		mut ctx := ResaveContext{}
		s.resave(mut ctx, 'authenticated')!
		assert ctx.session_id != ''
		assert ctx.session_data == ?string('authenticated')
		assert s.get(ctx)! == 'authenticated'
		assert ctx.res.cookies().len == 1
	}
}

struct FailingResaveStore {
	fail_destroy bool
}

fn (mut store FailingResaveStore) get(sid string, max_age time.Duration) !string {
	_ = sid
	_ = max_age
	return error('missing')
}

fn (mut store FailingResaveStore) destroy(sid string) ! {
	_ = sid
	if store.fail_destroy {
		return error('cannot destroy')
	}
}

fn (mut store FailingResaveStore) set(sid string, val string) ! {
	_ = sid
	_ = val
	return error('cannot save')
}

fn test_resave_propagates_store_errors() {
	mut s := sessions.Sessions[string]{
		secret: 'resave-secret'.bytes()
		store:  FailingResaveStore{}
	}
	mut ctx := ResaveContext{}
	s.resave(mut ctx, 'authenticated') or {
		assert err.msg() == 'cannot save'
		return
	}
	assert false, 'resave must propagate store errors'
}

fn test_resave_rotates_session_id_from_request_cookie() {
	mut s := sessions.Sessions[string]{
		secret: 'resave-secret'.bytes()
		store:  sessions.MemoryStore[string]{}
	}
	mut original := ResaveContext{}
	old_sid := s.set_session_id(mut original)
	s.save(mut original, 'anonymous')!
	mut ctx := ResaveContext{}
	ctx.req.add_cookie(http.Cookie{ name: 'sid', value: original.res.cookies()[0].value })
	s.resave(mut ctx, 'authenticated')!
	assert ctx.session_id != old_sid
	assert ctx.res.cookies().len == 1
	assert s.get(ctx)! == 'authenticated'
	if _ := s.get(original) {
		assert false, 'old session data must be destroyed'
	}
}

fn test_resave_aborts_when_old_session_cannot_be_destroyed() {
	mut s := sessions.Sessions[string]{
		secret: 'resave-secret'.bytes()
		store:  FailingResaveStore{ fail_destroy: true }
	}
	mut ctx := ResaveContext{}
	old_sid := s.set_session_id(mut ctx)
	ctx.res = http.Response{}
	s.resave(mut ctx, 'authenticated') or {
		assert err.msg() == 'cannot destroy'
		assert ctx.session_id == old_sid
		assert ctx.res.cookies().len == 0
		return
	}
	assert false, 'resave must propagate destroy errors'
}
