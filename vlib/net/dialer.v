module net

// Dialer is an abstract dialer interface for producing connections to addresses.
pub interface Dialer {
	dial(address string) !Connection
}
