module pop

import net
import io
import strings
import strconv

// Config stores the settings used to connect a POP3 client over plain TCP.
// TLS is not supported.
pub struct Config {
pub:
	server   string
	port     int = 110
	username string
	password string
}

// Client is a POP3 connection. Use dial, then login; quit (or close) ends it.
pub struct Client {
	Config
mut:
	conn   &net.TcpConn       = unsafe { nil }
	reader &io.BufferedReader = unsafe { nil }
	opened bool
}

// dial connects to the server and reads its greeting.
// Credentials must not contain CR, LF, or NUL bytes.
pub fn dial(config Config) !&Client {
	if config.server == '' || config.server.contains('\x00') {
		return error('pop: server must be a nonempty host name without NUL bytes')
	}
	if config.server.contains_any('[]') {
		if !config.server.starts_with('[') || !config.server.ends_with(']')
			|| config.server.count('[') != 1 || config.server.count(']') != 1
			|| !config.server.contains(':') {
			return error('pop: invalid bracketed host name')
		}
	}
	if config.port < 1 || config.port > 65535 {
		return error('pop: port must be between 1 and 65535')
	}
	if config.username.contains_any('\r\n\x00') || config.password.contains_any('\r\n\x00') {
		return error('pop: credentials must not contain control characters')
	}
	mut c := &Client{
		Config: config
	}
	host := if config.server.contains(':') && !config.server.starts_with('[') {
		'[${config.server}]'
	} else {
		config.server
	}
	c.conn = net.dial_tcp('${host}:${config.port}')!
	c.reader = io.new_buffered_reader(reader: c.conn)
	c.opened = true
	greeting := c.reader.read_line() or {
		c.close() or {}
		return err
	}
	c.expect_ok(greeting) or {
		c.close() or {}
		return err
	}
	return c
}

// login authenticates with USER/PASS.
pub fn (mut c Client) login() ! {
	c.send_cmd('USER ${c.username}')!
	c.send_cmd('PASS ${c.password}')!
}

// stat returns the message count and the mailbox size in octets.
// Malformed numbers and values outside the int range return an error.
pub fn (mut c Client) stat() !(int, int) {
	line := c.send_cmd('STAT')!
	parts := line.fields()
	if parts.len < 3 {
		return error('pop: malformed STAT response `${line}`')
	}
	for field in parts[1..3] {
		if field == '' || field.bytes().any(it < `0` || it > `9`) {
			return error('pop: malformed STAT response `${line}`')
		}
	}
	count := strconv.atoi(parts[1]) or { return error('pop: invalid message count') }
	octets := strconv.atoi(parts[2]) or { return error('pop: invalid mailbox size') }
	return count, octets
}

// list_all returns the server's LIST response (one `number octets` line per
// message).
pub fn (mut c Client) list_all() !string {
	c.send_cmd('LIST')!
	return c.read_multiline()!
}

// retr returns the full RFC 5322 text of message n.
// n must be positive. The returned text uses LF line endings.
pub fn (mut c Client) retr(n int) !string {
	if n <= 0 { return error('pop: message number must be positive') }
	c.send_cmd('RETR ${n}')!
	return c.read_multiline()!
}

// dele marks message n for deletion.
// n must be positive.
pub fn (mut c Client) dele(n int) ! {
	if n <= 0 { return error('pop: message number must be positive') }
	c.send_cmd('DELE ${n}')!
}

// quit ends the session cleanly (pending deletions take effect) and closes.
// It also closes the connection if the QUIT command fails.
pub fn (mut c Client) quit() ! {
	c.send_cmd('QUIT') or {
		c.close() or {}
		return err
	}
	c.close()!
}

// close releases the connection without QUIT (pending deletions are lost).
pub fn (mut c Client) close() ! {
	if c.opened {
		c.opened = false
		c.conn.close()!
	}
}

// send_cmd writes one command line and returns the single-line response,
// erroring on -ERR and on anything but +OK.
fn (mut c Client) send_cmd(cmd string) !string {
	if !c.opened { return error('pop: connection is closed') }
	if cmd.contains_any('\r\n\x00') { return error('pop: invalid command') }
	wire := cmd + '\r\n'
	written := c.conn.write_string(wire) or {
		c.close() or {}
		return err
	}
	if written != wire.len {
		c.close() or {}
		return error('pop: incomplete command write')
	}
	line := c.reader.read_line() or {
		c.close() or {}
		return err
	}
	return c.expect_ok(line)!
}

// expect_ok validates a single-line response.
fn (c &Client) expect_ok(line string) !string {
	if line == '-ERR' || line.starts_with('-ERR ') {
		return error('pop: ${line}')
	}
	if line != '+OK' && !line.starts_with('+OK ') {
		return error('pop: unexpected response `${line}`')
	}
	return line
}

// read_multiline reads a dot-terminated block, un-stuffing `..` to `.`.
// The returned text uses `\n` line endings.
fn (mut c Client) read_multiline() !string {
	mut out := strings.new_builder(1024)
	for {
		line := c.reader.read_line() or {
			c.close() or {}
			return error('pop: incomplete multiline response: ${err}')
		}
		if line == '.' {
			break
		}
		if line.starts_with('..') {
			out.writeln(line[1..])
		} else {
			out.writeln(line)
		}
	}
	return out.str()
}
