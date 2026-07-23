module http

fn vschannel_ssl_do(_req &Request, _port int, _method Method, _host_name string, _path string, _data string, _header Header) !Response {
	return error('vschannel is unavailable in cross C generation')
}

fn h2_dial_probe_vschannel(_req &Request, _host string, _port int) !H2ProbeResult {
	return error('vschannel is unavailable in cross C generation')
}

fn (mut t Transport) vschannel_fresh_round_trip(_req &Request, _key string, _raw string, _method Method, _host string, _port int, _path string, _data string, _header Header) !Response {
	return error('vschannel is unavailable in cross C generation')
}
