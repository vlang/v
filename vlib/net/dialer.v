module net

// Dialer is an abstract dialer interface for producing connections to adresses.
pub interface Dialer {
	dial(address string) !Connection
}
