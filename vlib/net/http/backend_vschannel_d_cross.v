module http

fn vschannel_ssl_do(_req &Request, _port int, _method Method, _host_name string, _path string, _data string, _header Header) !Response {
	return error('vschannel is unavailable in cross C generation')
}
