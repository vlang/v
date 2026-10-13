module pop

import net
import io
import strings

// Config stores the settings used to connect a POP3 client. Only cleartext
// login over plain TCP is supported for now; TLS (implicit TLS on 995 and
// STARTTLS) is a stated follow-up, like `net.smtp`'s starttls support.
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
pub fn dial(config Config) !&Client {
	mut c := &Client{
		Config: config
	}
	c.conn = net.dial_tcp('${config.server}:${config.port}')!
	c.reader = io.new_buffered_reader(reader: c.conn)
	c.opened = true
	c.expect_ok(c.reader.read_line()!)!
	return c
}

// login authenticates with USER/PASS.
pub fn (mut c Client) login() ! {
	c.send_cmd('USER ${c.username}')!
	c.send_cmd('PASS ${c.password}')!
}

// stat returns the message count and the mailbox size in octets.
pub fn (mut c Client) stat() !(int, int) {
	line := c.send_cmd('STAT')!
	parts := line.fields()
	if parts.len < 3 {
		return error('pop: malformed STAT response `${line}`')
	}
	return parts[1].int(), parts[2].int()
}

// list_all returns the server's LIST response (one `number octets` line per
// message).
pub fn (mut c Client) list_all() !string {
	c.send_cmd('LIST')!
	return c.read_multiline()!
}

// retr returns the full RFC 5322 text of message n.
pub fn (mut c Client) retr(n int) !string {
	c.send_cmd('RETR ${n}')!
	return c.read_multiline()!
}

// dele marks message n for deletion.
pub fn (mut c Client) dele(n int) ! {
	c.send_cmd('DELE ${n}')!
}

// quit ends the session cleanly (pending deletions take effect) and closes.
pub fn (mut c Client) quit() ! {
	c.send_cmd('QUIT')!
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
	c.conn.write_string(cmd + '\r\n')!
	return c.expect_ok(c.reader.read_line()!)!
}

// expect_ok validates a single-line response.
fn (c &Client) expect_ok(line string) !string {
	if line.starts_with('-ERR') {
		return error('pop: ${line}')
	}
	if !line.starts_with('+OK') {
		return error('pop: unexpected response `${line}`')
	}
	return line
}

// read_multiline reads a dot-terminated block, un-stuffing `..` to `.`.
// The returned text uses `\n` line endings.
fn (mut c Client) read_multiline() !string {
	mut out := strings.new_builder(1024)
	for {
		line := c.reader.read_line()!
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
