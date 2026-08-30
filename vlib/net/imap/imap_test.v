module imap

import io
import net
import time

// These drive the client over a real socket against a scripted server, so that
// tagging, literal sending, continuations and the mailbox name encoding are
// checked as they behave on the wire.
const mock_greeting = '* OK [CAPABILITY IMAP4rev1 STARTTLS] mock ready'

// run_server answers one session and reports every command it received on
// `seen`, so that a test can check what the client actually sent.
fn run_server(mut l net.TcpListener, hello string, seen chan string) {
	defer {
		seen.close()
	}
	mut c := l.accept() or { return }
	defer {
		c.close() or {}
	}
	// A client that fails mid-session may never say goodbye, so the read must
	// give up rather than leave the thread pinned for the rest of the suite.
	c.set_read_timeout(10 * time.second)
	c.write_string('${hello}\r\n') or { return }
	mut r := io.new_buffered_reader(reader: c)
	for {
		mut line := r.read_line() or { return }
		// A command may carry literals. Each one is announced by the client,
		// acknowledged here, and only then sent.
		for {
			size := literal_size(line) or { break }
			c.write_string('+ go ahead\r\n') or { return }
			payload := read_exactly(mut r, size) or { return }
			rest := r.read_line() or { return }
			line = '${line}<${payload}>${rest}'
		}
		seen <- line
		fields := line.split(' ')
		if fields.len < 2 {
			continue
		}
		c.write_string(mock_reply(fields[0], fields[1].to_upper(), line)) or { return }
		if fields[1].to_upper() == 'LOGOUT' {
			return
		}
	}
}

// literal_size reads the octet count a command line ends with, if any.
fn literal_size(line string) ?int {
	if !line.ends_with('}') {
		return none
	}
	start := line.last_index_u8(`{`)
	if start < 0 {
		return none
	}
	digits := line[start + 1..line.len - 1]
	if digits == '' {
		return none
	}
	for ch in digits {
		if ch < `0` || ch > `9` {
			return none
		}
	}
	return digits.int()
}

fn read_exactly(mut r io.BufferedReader, n int) !string {
	mut buf := []u8{len: n}
	mut got := 0
	for got < n {
		read := r.read(mut buf[got..])!
		if read <= 0 {
			return error('the client stopped short of the literal it announced')
		}
		got += read
	}
	return buf.bytestr()
}

// mock_reply renders the response to one command. The bodies are the examples
// printed in RFC 3501, with the numbers left as the RFC has them.
fn mock_reply(tag string, cmd string, line string) string {
	if cmd == 'CAPABILITY' {
		return '* CAPABILITY IMAP4rev1 STARTTLS AUTH=PLAIN UIDPLUS MOVE\r\n' + '${tag} OK CAPABILITY completed\r\n'
	}
	if cmd == 'LOGIN' {
		if line.contains('wrong') {
			return '${tag} NO [AUTHENTICATIONFAILED] Invalid credentials\r\n'
		}
		return '${tag} OK LOGIN completed\r\n'
	}
	if cmd == 'NOOP' {
		// A mailbox can change under a client at any moment, and this is how
		// the server says so: attached to whatever command is in flight.
		return '* 9 EXISTS\r\n* 2 RECENT\r\n${tag} OK NOOP completed\r\n'
	}
	if cmd == 'LIST' || cmd == 'LSUB' {
		return '* ${cmd} (\\Noselect) "/" ""\r\n' + '* ${cmd} (\\HasNoChildren) "/" INBOX\r\n' + '* ${cmd} (\\HasChildren) "/" "Travail"\r\n' + '* ${cmd} () "/" "Travail/&AMk-t&AOk- 2026"\r\n' + '${tag} OK ${cmd} completed\r\n'
	}
	if cmd == 'SELECT' || cmd == 'EXAMINE' {
		if line.contains('Nope') {
			return '${tag} NO Mailbox does not exist\r\n'
		}
		mode := if cmd == 'SELECT' { 'READ-WRITE' } else { 'READ-ONLY' }
		return '* 172 EXISTS\r\n* 1 RECENT\r\n' + '* OK [UNSEEN 12] Message 12 is first unseen\r\n' + '* OK [UIDVALIDITY 3857529045] UIDs valid\r\n' + '* OK [UIDNEXT 4392] Predicted next UID\r\n' + '* FLAGS (\\Answered \\Flagged \\Deleted \\Seen \\Draft)\r\n' + '* OK [PERMANENTFLAGS (\\Deleted \\Seen \\*)] Limited\r\n' + '${tag} OK [${mode}] ${cmd} completed\r\n'
	}
	if cmd == 'STATUS' {
		return '* STATUS "Travail/&AMk-t&AOk- 2026" (MESSAGES 231 UIDNEXT 44292 UNSEEN 3)\r\n' + '${tag} OK STATUS completed\r\n'
	}
	if cmd == 'SEARCH' {
		return '* SEARCH 1 2 3 4 5 9 84 882\r\n${tag} OK SEARCH completed\r\n'
	}
	if cmd == 'FETCH' {
		return fetch_reply(tag)
	}
	if cmd == 'UID' {
		if line.contains(' SEARCH ') {
			return '* SEARCH 4827313 4827943\r\n${tag} OK UID SEARCH completed\r\n'
		}
		if line.contains(' FETCH ') {
			return fetch_reply(tag)
		}
		return '${tag} OK UID completed\r\n'
	}
	if cmd == 'STORE' {
		return '* 2 FETCH (FLAGS (\\Seen \\Deleted))\r\n${tag} OK STORE completed\r\n'
	}
	if cmd == 'EXPUNGE' {
		return '* 3 EXPUNGE\r\n* 3 EXPUNGE\r\n* 5 EXPUNGE\r\n* 8 EXISTS\r\n' + '${tag} OK EXPUNGE completed\r\n'
	}
	if cmd == 'APPEND' {
		return '${tag} OK [APPENDUID 3857529045 45] APPEND completed\r\n'
	}
	if cmd == 'LOGOUT' {
		return '* BYE mock signing off\r\n${tag} OK LOGOUT completed\r\n'
	}
	if cmd in ['CREATE', 'DELETE', 'RENAME', 'CLOSE', 'CHECK', 'SUBSCRIBE', 'UNSUBSCRIBE', 'UNSELECT',
		'COPY', 'MOVE'] {
		return '${tag} OK ${cmd} completed\r\n'
	}
	return '${tag} BAD unknown command\r\n'
}

// fetch_reply returns two messages, the first carrying a literal whose payload
// deliberately reads like protocol.
fn fetch_reply(tag string) string {
	body := 'Subject: UID 999 FLAGS (\\Deleted)\r\n\r\nnot protocol\r\n'
	return '* 2 FETCH (UID 4827313 RFC822.SIZE 44827 FLAGS (\\Seen) BODY[] {${body.len}}\r\n' + '${body})\r\n' + '* 84 FETCH (UID 4827943 FLAGS (\\Answered))\r\n' + '${tag} OK FETCH completed\r\n'
}

// drain collects everything the server reported, once it has closed the
// channel at the end of the session.
fn drain(seen chan string) []string {
	mut out := []string{}
	for {
		line := <-seen or { break }
		out << line
	}
	return out
}

// start starts a server and returns the port it is listening on.
fn start(mut l net.TcpListener, hello string, seen chan string) !(int, thread) {
	port := l.addr()!.port()!
	return port, spawn run_server(mut l, hello, seen)
}

fn test_a_full_session() {
	mut l := net.listen_tcp(.ip, '127.0.0.1:0')!
	seen := chan string{ cap: 64 }
	port, th := start(mut l, mock_greeting, seen)!

	mut c := new_client(
		server: '127.0.0.1'
		port: port
		username: 'bob'
		password: 'hunter2'
	)!
	assert c.is_open
	assert !c.encrypted

	assert c.capability()! == ['IMAP4rev1', 'STARTTLS', 'AUTH=PLAIN', 'UIDPLUS', 'MOVE']
	assert c.supports('move')!
	assert !c.supports('IDLE')!

	boxes := c.list_mailboxes('', '*')!
	assert boxes.len == 4
	assert boxes[0].name == ''
	assert boxes[1].name == 'INBOX'
	assert boxes[1].delimiter == '/'
	// The name comes back as UTF-8, not as the modified UTF-7 it travelled in.
	assert boxes[3].name == 'Travail/Été 2026'

	inbox := c.select_mailbox('INBOX')!
	assert inbox.exists == 172
	assert inbox.recent == 1
	assert inbox.unseen == 12
	assert inbox.uid_validity == 3857529045
	assert inbox.uid_next == 4392
	assert inbox.permanent_flags == ['\\Deleted', '\\Seen', '\\*']
	assert !inbox.read_only
	assert c.selected == 'INBOX'
	assert c.exists == 172

	found := c.search('UNSEEN')!
	// The consecutive numbers come back as a range, which is what will be sent
	// to the server next.
	assert found.str() == '1:5,9,84,882'

	msgs := c.fetch(found, 'BODY.PEEK[]')!
	assert msgs.len == 2
	assert msgs[0].uid == 4827313
	assert msgs[0].size == 44827
	assert msgs[0].flags == ['\\Seen']
	assert msgs[0].body().ends_with('not protocol\r\n')
	assert msgs[1].uid == 4827943
	assert msgs[1].body() == ''

	assert c.uid_search('UNSEEN')!.str() == '4827313,4827943'

	c.mark_seen(seq_set([u32(2)]))!
	assert c.expunge()! == [u32(3), 3, 5]
	assert c.exists == 8

	c.close_mailbox()!
	assert c.selected == ''
	c.close()!
	assert !c.is_open
	th.wait()
	l.close() or {}

	sent := drain(seen)
	assert sent == [
		'a0001 LOGIN "bob" "hunter2"',
		'a0002 CAPABILITY',
		'a0003 CAPABILITY',
		'a0004 CAPABILITY',
		'a0005 LIST "" "*"',
		'a0006 SELECT "INBOX"',
		'a0007 SEARCH UNSEEN',
		'a0008 FETCH 1:5,9,84,882 BODY.PEEK[]',
		'a0009 UID SEARCH UNSEEN',
		'a0010 STORE 2 +FLAGS.SILENT (\\Seen)',
		'a0011 EXPUNGE',
		'a0012 CLOSE',
		'a0013 LOGOUT',
	]
}

fn test_mailbox_names_travel_encoded() {
	mut l := net.listen_tcp(.ip, '127.0.0.1:0')!
	seen := chan string{ cap: 64 }
	port, th := start(mut l, mock_greeting, seen)!

	mut c := new_client(server: '127.0.0.1', port: port)!
	c.create_mailbox('Travail/Été 2026')!
	c.rename_mailbox('Travail/Été 2026', 'Archive/受信')!
	c.subscribe('Archive/受信')!
	c.delete_mailbox('Archive/受信')!
	status := c.status('Travail/Été 2026', ['MESSAGES', 'UIDNEXT', 'UNSEEN'])!
	assert status.name == 'Travail/Été 2026'
	assert status.messages == 231
	assert status.uid_next == 44292
	c.close()!
	th.wait()
	l.close() or {}

	sent := drain(seen)
	assert sent == [
		'a0001 CREATE "Travail/&AMk-t&AOk- 2026"',
		'a0002 RENAME "Travail/&AMk-t&AOk- 2026" "Archive/&U9dP4Q-"',
		'a0003 SUBSCRIBE "Archive/&U9dP4Q-"',
		'a0004 DELETE "Archive/&U9dP4Q-"',
		'a0005 STATUS "Travail/&AMk-t&AOk- 2026" (MESSAGES UIDNEXT UNSEEN)',
		'a0006 LOGOUT',
	]
}

fn test_append_sends_the_message_as_a_literal() {
	mut l := net.listen_tcp(.ip, '127.0.0.1:0')!
	seen := chan string{ cap: 64 }
	port, th := start(mut l, mock_greeting, seen)!

	mut c := new_client(server: '127.0.0.1', port: port)!
	body := 'Subject: hi\r\n\r\nThe body, with a } and a { in it.\r\n'.bytes()
	c.append('Sent', ['\\Seen'], time.unix(1739577600), body)!
	// A message with no flags and no date is the common case.
	c.append('Sent', [], time.Time{}, 'x'.bytes())!
	c.close()!
	th.wait()
	l.close() or {}

	sent := drain(seen)
	// The literal is reported by the mock inside angle brackets, so the whole
	// command including the payload is visible here.
	assert sent[0] == 'a0001 APPEND "Sent" (\\Seen) "15-Feb-2025 00:00:00 +0000" {${body.len}}<${body.bytestr()}>'
	assert sent[1] == 'a0002 APPEND "Sent" {1}<x>'
	assert sent[2] == 'a0003 LOGOUT'
}

fn test_a_password_a_quoted_string_cannot_hold_becomes_a_literal() {
	mut l := net.listen_tcp(.ip, '127.0.0.1:0')!
	seen := chan string{ cap: 64 }
	port, th := start(mut l, mock_greeting, seen)!

	// A password with an accent is eight bit, which a quoted string is not
	// defined over.
	mut c := new_client(server: '127.0.0.1', port: port, username: 'bob', password: 'mot-de-passé')!
	c.close()!
	th.wait()
	l.close() or {}

	sent := drain(seen)
	assert sent[0] == 'a0001 LOGIN "bob" {13}<mot-de-passé>'
}

fn test_examine_reports_the_mailbox_as_read_only() {
	mut l := net.listen_tcp(.ip, '127.0.0.1:0')!
	seen := chan string{ cap: 64 }
	port, th := start(mut l, mock_greeting, seen)!

	mut c := new_client(server: '127.0.0.1', port: port)!
	box := c.examine_mailbox('INBOX')!
	assert box.read_only
	assert box.exists == 172
	c.close()!
	th.wait()
	l.close() or {}

	// No username was configured, so no LOGIN should have gone out.
	assert drain(seen)[0] == 'a0001 EXAMINE "INBOX"'
}

fn test_unsolicited_updates_reach_the_client() {
	mut l := net.listen_tcp(.ip, '127.0.0.1:0')!
	seen := chan string{ cap: 64 }
	port, th := start(mut l, mock_greeting, seen)!

	mut c := new_client(server: '127.0.0.1', port: port)!
	c.select_mailbox('INBOX')!
	assert c.exists == 172
	// The NOOP reply carries an EXISTS that has nothing to do with the NOOP:
	// mail arrived while the session was doing something else.
	c.noop()!
	assert c.exists == 9
	assert c.recent == 2
	c.close()!
	th.wait()
	l.close() or {}
	drain(seen)
}

fn test_a_refused_login_is_an_error() {
	mut l := net.listen_tcp(.ip, '127.0.0.1:0')!
	seen := chan string{ cap: 64 }
	port, th := start(mut l, mock_greeting, seen)!

	// Driven by hand rather than through new_client, so that the connection is
	// still in reach once the login is refused.
	mut c := Client{
		Config: Config{
			server: '127.0.0.1'
			port: port
			username: 'bob'
			password: 'wrong'
		}
	}
	c.connect()!
	c.login() or {
		assert err.msg().contains('Invalid credentials')
		c.close()!
		th.wait()
		l.close() or {}
		assert drain(seen) == ['a0001 LOGIN "bob" "wrong"', 'a0002 LOGOUT']
		return
	}
	assert false, 'a NO completion must not be reported as success'
}

fn test_a_refused_command_leaves_the_session_usable() {
	mut l := net.listen_tcp(.ip, '127.0.0.1:0')!
	seen := chan string{ cap: 64 }
	port, th := start(mut l, mock_greeting, seen)!

	mut c := new_client(server: '127.0.0.1', port: port)!
	c.select_mailbox('Nope') or {
		assert err.msg().contains('Mailbox does not exist')
		// The failure was the server's answer, not a broken connection, so the
		// next command must still work.
		c.noop()!
		c.close()!
		th.wait()
		l.close() or {}
		drain(seen)
		return
	}
	assert false, 'selecting a missing mailbox must fail'
}

fn test_an_unknown_command_reports_bad() {
	mut l := net.listen_tcp(.ip, '127.0.0.1:0')!
	seen := chan string{ cap: 64 }
	port, th := start(mut l, mock_greeting, seen)!

	mut c := new_client(server: '127.0.0.1', port: port)!
	c.command('XYZZY') or {
		assert err.msg().contains('unknown command')
		c.close()!
		th.wait()
		l.close() or {}
		drain(seen)
		return
	}
	assert false, 'a BAD completion must not be reported as success'
}

fn test_a_bye_greeting_is_refused() {
	mut l := net.listen_tcp(.ip, '127.0.0.1:0')!
	seen := chan string{ cap: 64 }
	port, th := start(mut l, '* BYE too many connections', seen)!

	mut c := Client{
		Config: Config{
			server: '127.0.0.1'
			port: port
		}
	}
	c.connect() or {
		assert err.msg().contains('refused the connection')
		assert !c.is_open
		l.close() or {}
		th.wait()
		return
	}
	assert false, 'a BYE greeting must not open a session'
}

fn test_a_preauth_greeting_opens_the_session() {
	mut l := net.listen_tcp(.ip, '127.0.0.1:0')!
	seen := chan string{ cap: 64 }
	port, th := start(mut l, '* PREAUTH IMAP4rev1 already authenticated', seen)!

	mut c := new_client(server: '127.0.0.1', port: port)!
	assert c.is_open
	c.noop()!
	c.close()!
	th.wait()
	l.close() or {}
	drain(seen)
}

fn test_ssl_and_starttls_are_mutually_exclusive() {
	new_client(server: 'imap.example.com', ssl: true, starttls: true) or {
		assert err.msg().contains('cannot use both')
		return
	}
	assert false, 'asking for both TLS modes must be rejected'
}

fn test_empty_sets_do_not_reach_the_server() {
	mut l := net.listen_tcp(.ip, '127.0.0.1:0')!
	seen := chan string{ cap: 64 }
	port, th := start(mut l, mock_greeting, seen)!

	mut c := new_client(server: '127.0.0.1', port: port)!
	empty := SeqSet{}
	assert c.fetch(empty, 'BODY.PEEK[]')!.len == 0
	assert c.uid_fetch(empty, 'BODY.PEEK[]')!.len == 0
	assert c.store(empty, '+FLAGS', ['\\Seen'])!.len == 0
	c.copy(empty, 'Archive')!
	c.move(empty, 'Archive')!
	c.close()!
	th.wait()
	l.close() or {}

	// An empty sequence set is not valid syntax, so nothing may be sent.
	assert drain(seen) == ['a0001 LOGOUT']
}

fn test_close_is_idempotent() {
	mut l := net.listen_tcp(.ip, '127.0.0.1:0')!
	seen := chan string{ cap: 64 }
	port, th := start(mut l, mock_greeting, seen)!

	mut c := new_client(server: '127.0.0.1', port: port)!
	c.close()!
	c.close()!
	th.wait()
	l.close() or {}
	assert drain(seen) == ['a0001 LOGOUT']
}

fn test_default_ports() {
	plain := Client{
		Config: Config{}
	}
	assert plain.effective_port() == default_port
	implicit := Client{
		Config: Config{
			ssl: true
		}
	}
	assert implicit.effective_port() == default_ssl_port
	explicit := Client{
		Config: Config{
			ssl: true
			port: 1143
		}
	}
	assert explicit.effective_port() == 1143
}

fn test_build_args_splits_out_what_must_be_a_literal() {
	text, literals := build_args('LOGIN', ['bob', 'plain'])
	assert text == ['LOGIN "bob" "plain"']
	assert literals == [][]u8{}

	// A newline cannot travel in a quoted string, which may not span lines.
	multi, payloads := build_args('LOGIN', ['bob', 'two\r\nlines'])
	assert multi == ['LOGIN "bob" ', '']
	assert payloads.len == 1
	assert payloads[0].bytestr() == 'two\r\nlines'

	// Quotes and backslashes are escaped rather than promoted to a literal.
	escaped, none_needed := build_args('CREATE', ['od"d\\name'])
	assert escaped == ['CREATE "od\\"d\\\\name"']
	assert none_needed.len == 0
}
