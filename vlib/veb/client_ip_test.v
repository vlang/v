// client_ip_test.v - `Context.ip()` trusts only the peers it is told to trust.
//
// Every one of `CF-Connecting-IP`, `X-Forwarded-For` and `X-Real-Ip` is set by
// the client, so believing any of them by default hands any client the pen for
// `ip()`. Before this change `ip()` preferred all three over the socket peer.
// An app opts in by holding a `trusted_proxies` field, which the framework
// copies into every request context; the default stays the socket peer.
module veb

import net.http

struct IpApp {
	trusted_proxies []string
}

struct IpContext {
	Context
}

@['/who']
fn (app &IpApp) who(mut ctx IpContext) Result {
	return ctx.text(ctx.ip())
}

// ip_request renders one spoofed `X-Forwarded-For` through the dispatcher.
fn ip_request(spoofed string) string {
	mut app := IpApp{}
	mut header := http.new_header_from_map({
		http.CommonHeader.host: 'localhost'
	})
	if spoofed != '' {
		header.set(.x_forwarded_for, spoofed)
	}
	ctx := render_request[IpApp, IpContext](mut app, http.Request{
		method: .get
		url:    '/who'
		header: header
	}) or { panic(err) }
	return ctx.res.body
}

fn test_a_forwarding_header_is_ignored_without_a_trusted_peer() {
	// The app declares an (empty) `trusted_proxies` list and `render_request` has
	// no socket, so the peer is empty and untrusted, and the spoofed header must
	// not reach the handler. With the old `ip()` this returned the spoof verbatim.
	assert ip_request('1.2.3.4') != '1.2.3.4'
}

fn test_the_app_list_reaches_the_request_context() {
	// The wiring half of the change: the framework copies the app's list, so a
	// proxy the app knows is trusted the next request after this one.
	mut app := IpApp{
		trusted_proxies: ['10.0.0.1']
	}
	ctx := render_request[IpApp, IpContext](mut app, http.Request{
		method: .get
		url:    '/who'
	}) or { panic(err) }
	assert ctx.trusted_proxies == ['10.0.0.1']
}

fn test_an_untrusted_peer_keeps_its_own_address() {
	// 203.0.113.7 is not trusted, so its X-Forwarded-For is a client claim and
	// the peer wins.
	assert resolved_client_ip('203.0.113.7', '198.51.100.9', ['10.0.0.1']) == '203.0.113.7'
}

fn test_a_trusted_peer_reveals_the_client_behind_it() {
	// 10.0.0.1 is our proxy and it says the client is 198.51.100.9.
	assert resolved_client_ip('10.0.0.1', '198.51.100.9', ['10.0.0.1']) == '198.51.100.9'
}

fn test_a_chain_skips_proxies_we_trust() {
	// The header is left-most client first, and each proxy appends: the client
	// is the first entry that is not one of our own proxies.
	assert resolved_client_ip('10.0.0.2', '198.51.100.9, 10.0.0.1', ['10.0.0.1', '10.0.0.2']) == '198.51.100.9'
}

fn test_a_chain_of_only_trusted_proxies_answers_the_left_most() {
	// The client is behind proxies only; there is no untrusted address, so the
	// left-most entry is the best answer there is.
	assert resolved_client_ip('10.0.0.2', '10.0.0.1, 10.0.0.2', ['10.0.0.1', '10.0.0.2']) == '10.0.0.1'
}

fn test_an_empty_peer_is_never_trusted() {
	// No socket address is available in this path (for example a test render),
	// and an empty peer must not unlock the forwarding headers.
	assert resolved_client_ip('', '198.51.100.9', ['']) == ''
}
