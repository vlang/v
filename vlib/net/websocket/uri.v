module websocket

// Uri represents an Uri for websocket connections
struct Uri {
mut:
	url         string // url to the websocket endpoint
	hostname    string // hostname of the websocket endpoint
	port        string // port of the websocket endpoint
	resource    string // resource of the websocket endpoint
	querystring string // query string of the websocket endpoint
}

// str returns the string representation of the Uri
pub fn (u Uri) str() string {
	return u.url
}
