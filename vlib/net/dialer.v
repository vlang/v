module net

// IDialer is an abstract dialer interface for producing connections to adresses.
pub interface IDialer {
	dial(address string) !IConn
}
