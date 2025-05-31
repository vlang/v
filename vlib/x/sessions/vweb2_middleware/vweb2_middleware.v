module vweb2_middleware

import x.sessions
import veb

// middleware can be used to add session middleware to your vweb app to ensure
// a valid session always exists. If a valid session exists the session data will
// be loaded into `session_data`, else a new session id will be generated.
// You have to pass the Context type as the generic type
// Example: app.use(app.sessions.middleware[Context]())
pub fn create[T, X](mut s sessions.Sessions[T]) veb.MiddlewareOptions[X] {
	return veb.MiddlewareOptions[X]{
		handler: fn [mut s] [T, X](mut ctx X) bool {
			// a session id is retrieved from the client, so it must be considered
			// untrusted and has to be verified on every request
			sid, valid := s.validate_session(ctx)

			if !valid {
				if s.save_uninitialized {
					// invalid session id, so create a new one
					s.set_session_id(mut ctx)
				}
				return true
			}

			ctx.CurrentSession.session_id = sid
			if data := s.store.get(sid, s.max_age) {
				ctx.CurrentSession.session_data = data
			}

			return true
		}
	}
}
