module jsonrpc

import log

// LoggingInterceptor is simple logging full-fledged Interceptor
// messages will be written in `log`.get_level() Level
@[heap]
pub struct LoggingInterceptor {
pub mut:
	log log.Log
}

// on_event logs event `name` and `data` into provided `log`
pub fn (mut l LoggingInterceptor) on_event(name string, data string) {
	msg := '[EVENT] name=${name} data=${data}'
	l.log.send_output(msg, l.log.get_level())
}

// on_encoded_request logs json encoded `jsonrpc.Request` as string
pub fn (mut l LoggingInterceptor) on_encoded_request(req []u8) ! {
	msg := '[RAW REQ] ${req.bytestr()}'
	l.log.send_output(msg, l.log.get_level())
}

// on_request logs `jsonrpc.Request` method, params and id
pub fn (mut l LoggingInterceptor) on_request(req &Request) ! {
	msg := '[REQ] method=${req.method} params=${req.params} id=${req.id}'
	l.log.send_output(msg, l.log.get_level())
}

// on_response logs `jsonrpc.Response` result, error and id
pub fn (mut l LoggingInterceptor) on_response(resp &Response) {
	mut msg := '[RESP] result=${resp.result} '
	if resp.error.code != 0 {
		msg = msg + 'error=${resp.error}'
	} else {
		msg = msg + 'error=none'
	}
	msg = msg + ' id=${resp.id}'
	
	l.log.send_output(msg, l.log.get_level())
}

// on_encoded_response logs json encoded `jsonrpc.Response` as string
pub fn (mut l LoggingInterceptor) on_encoded_response(resp []u8) {
	msg := '[RAW RESP] ${resp.bytestr()}'
	l.log.send_output(msg, l.log.get_level())
}
