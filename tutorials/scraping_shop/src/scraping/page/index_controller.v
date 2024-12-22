module page

import veb
import shareds.wcontext

pub struct PageIndex {
	veb.Middleware[wcontext.WsCtx]
	current_lang shared Lang
}

pub struct Lang {
pub mut:
	lang        string
	abbrev_lang string
}
