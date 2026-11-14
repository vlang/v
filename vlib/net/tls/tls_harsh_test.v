module tls

import crypto.ecdsa
import crypto.pem
import crypto.x509
import time

// ---------- minimal DER helpers (test-only) ----------

fn tlv(tag u8, content []u8) []u8 {
	mut out := []u8{}
	out << tag
	mut v := content.len
	mut len_bytes := []u8{}
	if v < 0x80 {
		len_bytes << u8(v)
	} else {
		mut tmp := []u8{}
		for v > 0 {
			tmp << u8(v & 0xff)
			v >>= 8
		}
		len_bytes << u8(0x80 | tmp.len)
		for i := tmp.len - 1; i >= 0; i-- {
			len_bytes << tmp[i]
		}
	}
	for b in len_bytes {
		out << b
	}
	for b in content {
		out << b
	}
	return out
}

fn tlv_seq(content []u8) []u8 {
	return tlv(0x30, content)
}

fn tlv_int(content []u8) []u8 {
	return tlv(0x02, content)
}

fn tlv_oct(content []u8) []u8 {
	return tlv(0x04, content)
}

fn tlv_bitstr(content []u8) []u8 {
	mut c := []u8{}
	c << u8(0)
	for b in content {
		c << b
	}
	return tlv(0x03, c)
}

fn tlv_oid(first int, rest ...int) []u8 {
	mut c := []u8{}
	c << u8(first)
	for v in rest {
		if v < 0x80 {
			c << u8(v)
		} else {
			mut tmp := []u8{}
			mut x := v
			tmp << u8(x & 0x7f)
			x >>= 7
			for x > 0 {
				tmp << u8((x & 0x7f) | 0x80)
				x >>= 7
			}
			for i := tmp.len - 1; i >= 0; i-- {
				c << tmp[i]
			}
		}
	}
	return tlv(0x06, c)
}

// sec1_pem builds a SEC1 'EC PRIVATE KEY' PEM for a P-256 private key.
fn sec1_pem(priv ecdsa.PrivateKey) string {
	raw := priv.bytes() or { panic('seed') }
	mut s := []u8{}
	for s.len + raw.len < 32 {
		s << u8(0)
	}
	for b in raw {
		s << b
	}
	pbk := priv.public_key() or { panic('pubkey') }
	point := pbk.bytes() or { panic('point') }
	oid := tlv_oid(42, 10045, 3, 1, 7) // prime256v1
	mut ver := tlv_int([u8(1)])
	mut body := []u8{}
	for b in ver {
		body << b
	}
	for b in tlv_oct(s) {
		body << b
	}
	for b in tlv(0xa0, oid) {
		body << b
	}
	for b in tlv(0xa1, tlv_bitstr(point)) {
		body << b
	}
	der := tlv_seq(body)
	pem_str := pem.Block{
		block_type: 'EC PRIVATE KEY'
		data:       der
	}.encode(pem.EncodeConfig{}) or { panic('pem encode') }
	return pem_str
}

fn cert_pem_str(der []u8) string {
	return pem.Block{
		block_type: 'CERTIFICATE'
		data:       der
	}.encode(pem.EncodeConfig{}) or { panic('pem encode') }
}

fn read_exact(mut c Conn, n int) []u8 {
	mut out := []u8{}
	for out.len < n {
		mut buf := []u8{len: n - out.len}
		r := c.read(mut buf) or { panic('read: ${err}') }
		if r <= 0 {
			panic('short read')
		}
		out << buf[..r]
	}
	return out
}

fn serve_echo(mut l Listener, n int) {
	mut conn := l.accept() or { return }
	defer {
		conn.close() or {}
	}
	got := read_exact(mut conn, n)
	conn.write(got) or {}
}

fn echo_once(pq PostQuantumMode, payload []u8) {
	mut l := listen(Config{ verify: false, pq_mode: pq }) or { panic(err) }
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	_ = spawn serve_echo(mut l, payload.len)
	mut c := dial('127.0.0.1', port, Config{
		verify:      false
		server_name: 'localhost'
		pq_mode:     pq
	}) or { panic(err) }
	c.handshake() or { panic('client hs: ${err}') }
	c.write(payload) or { panic('write: ${err}') }
	back := read_exact(mut c, payload.len)
	assert back == payload
	c.close() or {}
	l.close() or {}
}

// Matrix over key-exchange modes and payload sizes with ephemeral ports.
fn test_tls_harsh_matrix() {
	sizes := [1, 256, 4096, 16384]
	for pq in [PostQuantumMode.off, .hybrid_prefer] {
		for sz in sizes {
			mut payload := []u8{len: sz}
			for i in 0 .. sz {
				payload[i] = u8((i * 31 + 7) & 0xff)
			}
			echo_once(pq, payload)
		}
	}
}

// Verified handshake against a generated cert trusted via ca_store.
fn test_tls_harsh_verified_chain() {
	_, priv := ecdsa.generate_key(ecdsa.CurveOptions{}) or { panic('keygen') }
	now := time.now()
	opts := x509.CertGenOptions{
		common_name: 'localhost'
		dns_names:   ['localhost']
		not_before:  now.add_days(-1)
		not_after:   now.add_days(30)
	}
	cert, der := x509.generate_self_signed(opts, priv) or { panic('gen') }
	server_cfg := Config{
		verify:    false
		cert_pem:  cert_pem_str(der).bytes()
		key_pem:   sec1_pem(priv).bytes()
		in_memory: true
	}
	mut l := listen(server_cfg) or { panic(err) }
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	msg := 'verified hello'.bytes()
	_ = spawn serve_echo(mut l, msg.len)
	mut c := dial('127.0.0.1', port, Config{
		verify:       true
		verify_chain: true
		server_name:  'localhost'
		ca_store:     CertStore{
			roots: [cert]
		}
	}) or { panic(err) }
	c.handshake() or { panic('verified hs: ${err}') }
	c.write(msg) or { panic('write: ${err}') }
	back := read_exact(mut c, msg.len)
	assert back == msg
	c.close() or {}
	l.close() or {}
}

// Sequential handshakes must all succeed; reports throughput.
fn test_tls_harsh_sequential_handshakes() {
	start := time.now()
	n := 5
	for _ in 0 .. n {
		echo_once(.off, 'ping'.bytes())
	}
	elapsed := time.since(start)
	println('tls ${n}x loopback handshakes+echo: ${elapsed.milliseconds()} ms')
	assert elapsed.milliseconds() < 120_000
}

// Tampered records and wrong sequence numbers must fail to open.
fn test_tls_harsh_record_tamper() {
	key := []u8{len: 16}
	iv := []u8{len: 12}
	plain := 'tamper me'.bytes()
	rec_bytes := seal_record(key, iv, 0, .application_data, plain) or { panic(err) }
	rec, _ := decode_cipher_record(rec_bytes) or { panic(err) }
	_, out := open_record(key, iv, 0, rec) or { panic(err) }
	assert out == plain
	// flip a tag byte
	mut bad_bytes := rec_bytes.clone()
	bad_bytes[bad_bytes.len - 1] ^= 0x01
	bad_rec, _ := decode_cipher_record(bad_bytes) or { panic(err) }
	mut tampered_opened := true
	open_record(key, iv, 0, bad_rec) or { tampered_opened = false }
	assert !tampered_opened
	// wrong sequence number
	mut wrong_seq_opened := true
	open_record(key, iv, 1, rec) or { wrong_seq_opened = false }
	assert !wrong_seq_opened
}
