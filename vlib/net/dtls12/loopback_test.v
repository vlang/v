// loopback handshake + echo tests (UDP, with and without loss, with and
// without cookie exchange).
module dtls12

import time

fn dtls12_run_echo_server(cfg Config, addr_ch chan string, done chan bool) {
	mut srv := bind('127.0.0.1:0', cfg) or {
		eprintln('server bind err: ${err}')
		addr_ch <- ''
		done <- false
		return
	}
	addr := srv.local_addr() or {
		eprintln('server addr err: ${err}')
		addr_ch <- ''
		done <- false
		return
	}
	addr_ch <- addr.str()
	srv.handshake() or {
		eprintln('server hs err: ${err}')
		done <- false
		return
	}
	mut buf := []u8{len: 256}
	n := srv.read(mut buf) or {
		eprintln('server read err: ${err}')
		done <- false
		return
	}
	srv.write(buf[..n]) or {
		eprintln('server write err: ${err}')
		done <- false
		return
	}
	srv.close() or {}
	done <- true
}

fn test_dtls12_loopback_no_loss() {
	cfg := default_config()
	addr_ch := chan string{cap: 1}
	done := chan bool{cap: 1}
	spawn dtls12_run_echo_server(cfg, addr_ch, done)
	srv_addr := <-addr_ch
	assert srv_addr != ''
	mut cli := dial(srv_addr, default_config()) or { panic('dial: ${err}') }
	cli.handshake() or { panic('client hs: ${err}') }
	assert cli.is_handshake_done()
	msg := 'hello dtls12'.bytes()
	cli.write(msg) or { panic('client write: ${err}') }
	mut buf := []u8{len: 256}
	n := cli.read(mut buf) or { panic('client read: ${err}') }
	assert buf[..n] == msg
	cli.close() or {}
	assert <-done == true
	time.sleep(50 * time.millisecond)
}

fn test_dtls12_loopback_with_loss() {
	mut scfg := default_config()
	scfg.drop_tx_first_n = 1
	scfg.drop_rx_first_n = 1
	mut ccfg := default_config()
	ccfg.drop_tx_first_n = 2
	addr_ch := chan string{cap: 1}
	done := chan bool{cap: 1}
	spawn dtls12_run_echo_server(scfg, addr_ch, done)
	srv_addr := <-addr_ch
	assert srv_addr != ''
	mut cli := dial(srv_addr, ccfg) or { panic('dial: ${err}') }
	cli.handshake() or { panic('client hs with loss: ${err}') }
	assert cli.is_handshake_done()
	msg := 'hello lossy dtls12'.bytes()
	cli.write(msg) or { panic('client write: ${err}') }
	mut buf := []u8{len: 256}
	n := cli.read(mut buf) or { panic('client read: ${err}') }
	assert buf[..n] == msg
	cli.close() or {}
	assert <-done == true
	time.sleep(50 * time.millisecond)
}

fn test_dtls12_loopback_cbc() {
	mut scfg := default_config()
	scfg.cipher_suites = [u16(suite_ecdhe_rsa_aes128_cbc_sha256)]
	mut ccfg := default_config()
	ccfg.cipher_suites = [u16(suite_ecdhe_rsa_aes128_cbc_sha256)]
	addr_ch := chan string{cap: 1}
	done := chan bool{cap: 1}
	spawn dtls12_run_echo_server(scfg, addr_ch, done)
	srv_addr := <-addr_ch
	assert srv_addr != ''
	mut cli := dial(srv_addr, ccfg) or { panic('dial: ${err}') }
	cli.handshake() or { panic('client hs cbc: ${err}') }
	assert cli.cipher_suite() == suite_ecdhe_rsa_aes128_cbc_sha256
	msg := 'hello dtls12 cbc'.bytes()
	cli.write(msg) or { panic('client write: ${err}') }
	mut buf := []u8{len: 256}
	n := cli.read(mut buf) or { panic('client read: ${err}') }
	assert buf[..n] == msg
	cli.close() or {}
	assert <-done == true
	time.sleep(50 * time.millisecond)
}

fn test_dtls12_loopback_cookie() {
	mut scfg := default_config()
	scfg.enable_cookie = true
	mut ccfg := default_config()
	// Client does not set enable_cookie; the server challenges and the
	// client answers transparently via HelloVerifyRequest.
	addr_ch := chan string{cap: 1}
	done := chan bool{cap: 1}
	spawn dtls12_run_echo_server(scfg, addr_ch, done)
	srv_addr := <-addr_ch
	assert srv_addr != ''
	mut cli := dial(srv_addr, ccfg) or { panic('dial: ${err}') }
	cli.handshake() or { panic('client hs cookie: ${err}') }
	assert cli.is_handshake_done()
	msg := 'hello cookie dtls12'.bytes()
	cli.write(msg) or { panic('client write: ${err}') }
	mut buf := []u8{len: 256}
	n := cli.read(mut buf) or { panic('client read: ${err}') }
	assert buf[..n] == msg
	cli.close() or {}
	assert <-done == true
	time.sleep(50 * time.millisecond)
}
