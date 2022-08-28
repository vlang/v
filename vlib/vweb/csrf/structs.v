module csrf

import vweb
import net.http

pub struct App {
	vweb.Context
}

struct CsrfCookie {
	http.Cookie
}

interface CheckedApp {}