module wcontext

import veb

pub struct WsCtx {
	veb.Context
pub mut:
	lang string = 'en'
}
