module net

pub interface IDialer {
	dial(address string) !IConn
}
