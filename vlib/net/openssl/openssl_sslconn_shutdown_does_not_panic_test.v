// vtest build: present_openssl? && !(windows && tinyc)
module openssl

import os
import time

fn shutdown_test_config() SSLConnectConfig {
	return SSLConnectConfig{
		cert:     os.join_path(@VMODROOT, 'examples', 'ssl_server', 'cert', 'server.crt')
		cert_key: os.join_path(@VMODROOT, 'examples', 'ssl_server', 'cert', 'server.key')
	}
}

fn run_shutdown_test_server(port_ch chan int, done_ch chan bool) {
	mut listener := new_ssl_listener('127.0.0.1:0', shutdown_test_config()) or {
		port_ch <- -1
		done_ch <- false
		return
	}
	defer {
		listener.shutdown() or {}
	}
	port_ch <- listener.tcp_listener.addr() or {
		done_ch <- false
		return
	}.port() or {
		done_ch <- false
		return
	}
	mut conn := listener.accept() or {
		done_ch <- false
		return
	}
	conn.duration = 200 * time.millisecond
	conn.shutdown() or {
		done_ch <- false
		return
	}
	done_ch <- true
}

@[if network ?]
fn test_shutdown_does_not_panic() {
	port_ch := chan int{}
	done_ch := chan bool{}
	spawn run_shutdown_test_server(port_ch, done_ch)
	port := <-port_ch
	assert port > 0
	mut client := new_ssl_conn(validate: false) or {
		assert false, err.msg()
		return
	}
	client.duration = 200 * time.millisecond
	client.dial('127.0.0.1', port) or {
		assert false, err.msg()
		return
	}
	client.shutdown() or {
		assert false, err.msg()
		return
	}
	assert <-done_ch
}
