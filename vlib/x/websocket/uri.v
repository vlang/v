module websocket

// Represents an Uri for websocket connections
struct Uri {
mut:
	url         string // The url to the websocket endpoint
	hostname    string // The hostname to the websocket endpoint
	port        string // The port to the websocket endpoint
	resource    string // The resource used on the websocket endpoint
	querystring string // The query string on the websocket endpoint
}

// str returns the string representation of the Uri
pub fn (u Uri) str() string {
	return u.url
}
