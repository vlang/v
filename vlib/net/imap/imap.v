// Package imap implements a client for the Internet Message Access Protocol,
// version 4rev1, as described in RFC 3501.
//
// IMAP is the reading half of the mail pair whose writing half is `net.smtp`:
// where SMTP hands a message to a server, IMAP browses what a server already
// holds. Messages stay on the server, so a client selects a mailbox, searches
// it, and fetches only the parts it wants.
//
// Mailbox names are ordinary UTF-8 strings here. They are encoded and decoded
// on the way in and out, so the modified UTF-7 the protocol carries them in
// never reaches a caller.
//
// A minimal session looks like this:
//
// ```v ignore
// mut c := imap.new_client(
//     server:   'imap.example.com'
//     username: 'someone@example.com'
//     password: 'hunter2'
//     ssl:      true
// )!
// defer { c.close() or {} }
//
// inbox := c.select_mailbox('INBOX')!
// println('${inbox.exists} messages, ${inbox.recent} recent')
//
// unread := c.search('UNSEEN')!
// for msg in c.fetch(unread, '(UID ENVELOPE)')! {
//     if envelope := msg.envelope {
//         println('${envelope.subject}')
//     }
// }
// ```
module imap

import io
import net
import net.ssl
import encoding.base64
import time

// The port a server listens on depends on whether TLS is negotiated up front.
pub const default_port = 143
pub const default_ssl_port = 993

// Every command carries a tag that its completion repeats back, which is what
// lets a client tell a reply to its command from the mailbox updates a server
// may interleave with it.
const tag_prefix = 'a'

// Config holds the settings used to open a session.
//
// Leave `port` unset to take 993 when `ssl` is true and 143 otherwise. Set
// `ssl` for a connection encrypted from the first byte, or `starttls` to
// upgrade a plain connection once it is open. The two are mutually exclusive.
pub struct Config {
pub:
	server   string
	port     int
	username string
	password string
	ssl      bool
	starttls bool
	timeout  time.Duration
}

// Client is a connection to an IMAP server.
pub struct Client {
	Config
mut:
	conn     net.TcpConn
	ssl_conn &ssl.SSLConn = unsafe { nil }
	dec      ?&Decoder
	tag_seq  int
pub mut:
	is_open   bool
	encrypted bool
	// selected names the mailbox the session is working in, and is empty when
	// none has been selected.
	selected string
	// exists and recent track the counts the server reports, which it may do
	// during any command as messages arrive or are removed by another client.
	exists u32
	recent u32
}

// new_client opens a session, greets the server, upgrades the connection if
// asked to, and logs in.
pub fn new_client(config Config) !&Client {
	if config.ssl && config.starttls {
		return error('imap: cannot use both implicit SSL and STARTTLS')
	}
	mut c := &Client{
		Config: config
	}
	c.connect()!
	c.login()!
	return c
}

// connect opens the transport and reads the server greeting, without logging
// in. `new_client` calls it; call it directly only to drive a session by hand.
pub fn (mut c Client) connect() ! {
	c.conn = net.dial_tcp('${c.server}:${c.effective_port()}')!
	if c.timeout != 0 {
		c.conn.set_read_timeout(c.timeout)
		c.conn.set_write_timeout(c.timeout)
	}
	c.dec = &Decoder{
		reader: io.new_buffered_reader(reader: c.conn)
	}
	c.is_open = true

	if c.ssl {
		c.upgrade_to_tls()!
	}
	// A greeting that turns the connection away leaves nothing to say goodbye
	// to, so the socket goes rather than being left for the server to time
	// out.
	c.read_greeting() or {
		c.shutdown()
		return err
	}
	if c.starttls {
		c.run('STARTTLS')!
		c.upgrade_to_tls()!
	}
}

// login authenticates with the LOGIN command, and does nothing when no
// username was configured.
pub fn (mut c Client) login() ! {
	if c.username == '' {
		return
	}
	text, literals := build_args('LOGIN', [c.username, c.password])
	c.send(text, literals)!
}

// login_plain authenticates with the SASL PLAIN mechanism instead, which some
// servers require and others prefer.
pub fn (mut c Client) login_plain() ! {
	tag := c.next_tag()
	c.write_line('${tag} AUTHENTICATE PLAIN')!
	c.await_continuation(tag)!
	// RFC 4616: an authorisation identity, an authentication identity and a
	// password, joined by NUL bytes.
	c.write_line(base64.encode_str('\0${c.username}\0${c.password}'))!
	c.read_response(tag)!
}

// capability returns the extensions the server advertises.
pub fn (mut c Client) capability() ![]string {
	return c.run('CAPABILITY')!.capabilities
}

// supports reports whether the server advertises `name`, which is how an
// optional command should be guarded before it is sent.
pub fn (mut c Client) supports(name string) !bool {
	for cap in c.capability()! {
		if cap.to_upper() == name.to_upper() {
			return true
		}
	}
	return false
}

// noop asks the server for nothing. It keeps the connection alive, and gives
// the server the chance to report anything that changed in the mailbox.
pub fn (mut c Client) noop() ! {
	c.run('NOOP')!
}

// list_mailboxes returns the mailboxes matching `pattern` below `reference`.
//
// The usual call is `list_mailboxes('', '*')`, which lists everything the
// account can see. `%` matches within one level of the hierarchy where `*`
// crosses levels.
pub fn (mut c Client) list_mailboxes(reference string, pattern string) ![]MailboxInfo {
	return c.run_list('LIST', reference, pattern)
}

// list_subscribed returns the subscribed mailboxes matching `pattern`, which
// is the set a mail client would show by default.
pub fn (mut c Client) list_subscribed(reference string, pattern string) ![]MailboxInfo {
	return c.run_list('LSUB', reference, pattern)
}

// select_mailbox opens a mailbox for reading and writing, and returns its
// state. Later fetches, searches and stores act on it.
pub fn (mut c Client) select_mailbox(name string) !Mailbox {
	return c.open_mailbox('SELECT', name)
}

// examine_mailbox opens a mailbox read-only. It behaves like
// `select_mailbox`, except that nothing the session does marks messages as
// seen or otherwise alters the mailbox.
pub fn (mut c Client) examine_mailbox(name string) !Mailbox {
	return c.open_mailbox('EXAMINE', name)
}

// status reports on a mailbox without selecting it, which is how a client
// polls for new mail while working in another mailbox.
//
// `items` names what to ask for: MESSAGES, RECENT, UIDNEXT, UIDVALIDITY and
// UNSEEN.
pub fn (mut c Client) status(name string, items []string) !MailboxStatus {
	if items.len == 0 {
		return error('imap: a STATUS command must ask for at least one item')
	}
	text, literals := build_args('STATUS', [utf7_encode(name)])
	last := text.len - 1
	mut tail := text.clone()
	tail[last] = '${text[last]} (${items.join(' ')})'
	res := c.send(tail, literals)!
	if res.statuses.len == 0 {
		return error('imap: the server reported no status for `${name}`')
	}
	return res.statuses[0]
}

// create_mailbox creates a mailbox.
pub fn (mut c Client) create_mailbox(name string) ! {
	c.run_with_mailbox('CREATE', name)!
}

// delete_mailbox deletes a mailbox and everything in it.
pub fn (mut c Client) delete_mailbox(name string) ! {
	c.run_with_mailbox('DELETE', name)!
}

// subscribe adds a mailbox to the set a client shows by default.
pub fn (mut c Client) subscribe(name string) ! {
	c.run_with_mailbox('SUBSCRIBE', name)!
}

// unsubscribe removes it from that set, without touching the mailbox itself.
pub fn (mut c Client) unsubscribe(name string) ! {
	c.run_with_mailbox('UNSUBSCRIBE', name)!
}

// rename_mailbox renames a mailbox, along with everything below it in the
// hierarchy.
pub fn (mut c Client) rename_mailbox(from string, to string) ! {
	text, literals := build_args('RENAME', [utf7_encode(from), utf7_encode(to)])
	c.send(text, literals)!
}

// append adds a message to a mailbox, which is how a sent message is filed
// into a Sent folder or a draft is saved.
//
// `flags` are set on the new message, commonly `\Seen` for a message the user
// has already read. `stamp` becomes its internal date; pass the zero time to
// let the server use the moment of delivery.
pub fn (mut c Client) append(mailbox string, flags []string, stamp time.Time, body []u8) ! {
	mut head := 'APPEND ${quote_arg(utf7_encode(mailbox))}'
	if flags.len > 0 {
		head += ' (${flags.join(' ')})'
	}
	// A zero year is the unset time, and means the server should stamp the
	// message with the moment it took delivery.
	if stamp.year != 0 {
		if stamp.month < 1 || stamp.month > 12 {
			return error('imap: ${stamp.month} is not a month')
		}
		head += ' ${quote_arg(format_internal_date(stamp))}'
	}
	// The message travels as a literal: it holds line endings, and very
	// probably octets no quoted string may carry.
	c.send(['${head} ', ''], [body])!
}

// search returns the sequence numbers of the messages in the selected mailbox
// matching `criteria`, which is an IMAP search key such as `UNSEEN`,
// `FROM "someone@example.com"` or `SINCE 1-Jan-2026`.
pub fn (mut c Client) search(criteria string) !SeqSet {
	return result_set(c.run('SEARCH ${criteria}')!)
}

// uid_search behaves like `search` but returns UIDs, which stay valid across
// sessions where sequence numbers do not.
pub fn (mut c Client) uid_search(criteria string) !SeqSet {
	return result_set(c.run('UID SEARCH ${criteria}')!)
}

// fetch retrieves `items` for each message in `set`.
//
// `items` is an IMAP fetch specification. `BODY.PEEK[]` takes the whole
// message without marking it read, `BODY.PEEK[HEADER]` takes just the
// headers, and `(UID FLAGS ENVELOPE)` takes metadata alone.
pub fn (mut c Client) fetch(set SeqSet, items string) ![]Message {
	return c.run_fetch('FETCH', set, items)
}

// uid_fetch behaves like `fetch` but addresses messages by UID.
pub fn (mut c Client) uid_fetch(set SeqSet, items string) ![]Message {
	return c.run_fetch('UID FETCH', set, items)
}

// store changes the flags of the messages in `set`, and returns what the
// server reports the new flags to be.
//
// `action` is `+FLAGS` to add, `-FLAGS` to remove, or `FLAGS` to replace.
// Appending `.SILENT` tells the server not to echo the result back, in which
// case the returned list is empty.
pub fn (mut c Client) store(set SeqSet, action string, flags []string) ![]Message {
	return c.run_store('STORE', set, action, flags)
}

// uid_store behaves like `store` but addresses messages by UID.
pub fn (mut c Client) uid_store(set SeqSet, action string, flags []string) ![]Message {
	return c.run_store('UID STORE', set, action, flags)
}

// mark_seen flags messages as read.
pub fn (mut c Client) mark_seen(set SeqSet) ! {
	c.store(set, '+FLAGS.SILENT', ['\\Seen'])!
}

// mark_deleted flags messages for deletion. They go on being readable until
// `expunge` runs.
pub fn (mut c Client) mark_deleted(set SeqSet) ! {
	c.store(set, '+FLAGS.SILENT', ['\\Deleted'])!
}

// copy copies messages into another mailbox, leaving the originals in place.
pub fn (mut c Client) copy(set SeqSet, dest string) ! {
	c.run_transfer('COPY', set, dest)!
}

// uid_copy behaves like `copy` but addresses messages by UID.
pub fn (mut c Client) uid_copy(set SeqSet, dest string) ! {
	c.run_transfer('UID COPY', set, dest)!
}

// move moves messages into another mailbox in one step (RFC 6851), which a
// copy followed by a delete cannot do without a window where the message
// exists twice or not at all.
//
// It needs the MOVE capability; check `supports('MOVE')` first when the server
// is not known.
pub fn (mut c Client) move(set SeqSet, dest string) ! {
	c.run_transfer('MOVE', set, dest)!
}

// uid_move behaves like `move` but addresses messages by UID.
pub fn (mut c Client) uid_move(set SeqSet, dest string) ! {
	c.run_transfer('UID MOVE', set, dest)!
}

// expunge permanently removes every message flagged `\Deleted` from the
// selected mailbox, and returns the sequence numbers the server reported as
// removed.
//
// The numbers are reported one at a time, each already renumbered by the
// removals before it, which is why the same number can appear twice.
pub fn (mut c Client) expunge() ![]u32 {
	return c.run('EXPUNGE')!.expunged
}

// check asks the server to bring its own housekeeping up to date. It is not a
// NOOP: it may take real time, and it is the right thing to call before a long
// idle period.
pub fn (mut c Client) check() ! {
	c.run('CHECK')!
}

// close_mailbox leaves the selected mailbox, silently expunging any message
// flagged `\Deleted` on the way out.
pub fn (mut c Client) close_mailbox() ! {
	c.run('CLOSE')!
	c.selected = ''
}

// unselect leaves the selected mailbox without expunging anything (RFC 3691),
// which is what a client wants when the user simply navigated away.
//
// It needs the UNSELECT capability.
pub fn (mut c Client) unselect() ! {
	c.run('UNSELECT')!
	c.selected = ''
}

// logout ends the session politely, giving the server the chance to close the
// connection itself.
pub fn (mut c Client) logout() ! {
	c.run('LOGOUT')!
}

// close logs out and tears down the connection. It is safe to call on a
// session that is already closed.
pub fn (mut c Client) close() ! {
	if !c.is_open {
		return
	}
	c.logout() or {}
	c.shutdown()
}

// shutdown tears the transport down without saying goodbye, which is what a
// session that never opened properly is left with.
fn (mut c Client) shutdown() {
	if c.encrypted {
		c.ssl_conn.shutdown() or {}
		c.encrypted = false
	}
	c.conn.close() or {}
	c.is_open = false
	c.selected = ''
}

// command sends one command with a fresh tag and returns the completion text,
// so that a caller can reach an extension this module does not wrap. The tag
// is added here; pass the command without one.
pub fn (mut c Client) command(cmd string) !string {
	return c.run(cmd)!.text
}

// run sends a command that needs no literal and reads its response.
fn (mut c Client) run(cmd string) !Response {
	return c.send([cmd], [])
}

// send writes a command and reads its response. `text` and `literals` are
// interleaved: the first text, then the first literal, then the second text,
// and so on, which is how an argument no quoted string can hold is passed.
fn (mut c Client) send(text []string, literals [][]u8) !Response {
	if text.len != literals.len + 1 {
		return error('imap: a command needs one more text part than it has literals')
	}
	tag := c.next_tag()
	mut line := '${tag} ${text[0]}'
	for i, payload in literals {
		// The server has to agree to take the octets before they are sent.
		c.write_line('${line}{${payload.len}}')!
		c.await_continuation(tag)!
		c.write_raw(payload)!
		line = text[i + 1]
	}
	c.write_line(line)!
	mut res := c.read_response(tag)!
	c.absorb(res)
	return res
}

// await_continuation reads until the server asks for the rest of the command.
//
// It may send mailbox updates first, or reject the command outright, and both
// have to be handled: a client that assumed the next line was the `+` would
// send a message body into the middle of a response.
fn (mut c Client) await_continuation(tag string) ! {
	for {
		mut d := c.decoder()!
		if d.accept(`+`)! {
			d.text()!
			d.crlf()!
			return
		}
		if d.accept(`*`)! {
			d.sp()!
			mut res := Response{}
			c.read_untagged(mut d, mut res)!
			d.crlf()!
			c.absorb(res)
			continue
		}
		// A tagged completion here means the command was refused before the
		// argument was ever sent.
		got := d.astring()!
		d.sp()!
		mut res := Response{}
		read_completion(mut d, mut res)!
		d.crlf()!
		if got != tag {
			return error('imap: the server answered tag `${got}` while `${tag}` was outstanding')
		}
		return error('imap: ${res.status} ${res.text}')
	}
}

// read_greeting reads the untagged response a server opens with.
fn (mut c Client) read_greeting() ! {
	mut d := c.decoder()!
	d.expect(`*`)!
	d.sp()!
	mut res := Response{}
	read_completion(mut d, mut res)!
	d.crlf()!
	status := res.status
	// `OK` is a usable connection and `PREAUTH` one the transport already
	// authenticated. `BYE` is the server turning the connection away.
	if status == .bye {
		c.is_open = false
		return error('imap: the server refused the connection: ${res.text}')
	}
	if status != .ok && status != .preauth {
		return error('imap: unexpected greeting: ${status} ${res.text}')
	}
}

// absorb takes the mailbox updates out of a response. A server reports new
// mail and removals during whatever command happens to be running, so these
// arrive attached to a reply that has nothing to do with them.
fn (mut c Client) absorb(res Response) {
	if res.has_exists {
		c.exists = res.exists
	}
	if res.has_recent {
		c.recent = res.recent
	}
}

// result_set takes the search result out of a response, whichever of the two
// forms the server answered in.
fn result_set(res Response) SeqSet {
	if res.has_set {
		return res.set
	}
	return seq_set(res.numbers)
}

fn (mut c Client) run_list(verb string, reference string, pattern string) ![]MailboxInfo {
	// The pattern holds the wildcards, so it is encoded but never given the
	// literal treatment that would hide them from the server.
	text, literals := build_args(verb, [utf7_encode(reference), utf7_encode(pattern)])
	return c.send(text, literals)!.mailboxes
}

fn (mut c Client) run_with_mailbox(verb string, name string) !Response {
	text, literals := build_args(verb, [utf7_encode(name)])
	return c.send(text, literals)
}

fn (mut c Client) run_fetch(verb string, set SeqSet, items string) ![]Message {
	if set.is_empty() {
		return []Message{}
	}
	return c.run('${verb} ${set} ${items}')!.messages
}

fn (mut c Client) run_store(verb string, set SeqSet, action string, flags []string) ![]Message {
	if set.is_empty() {
		return []Message{}
	}
	return c.run('${verb} ${set} ${action} (${flags.join(' ')})')!.messages
}

fn (mut c Client) run_transfer(verb string, set SeqSet, dest string) !Response {
	if set.is_empty() {
		return Response{}
	}
	text, literals := build_args('${verb} ${set}', [utf7_encode(dest)])
	return c.send(text, literals)
}

// open_mailbox runs SELECT or EXAMINE and reads back the state the server
// reports alongside it.
fn (mut c Client) open_mailbox(verb string, name string) !Mailbox {
	res := c.run_with_mailbox(verb, name)!
	c.selected = name
	c.exists = res.exists
	c.recent = res.recent
	return Mailbox{
		name: name
		exists: res.exists
		recent: res.recent
		unseen: res.unseen
		flags: res.flags
		permanent_flags: res.permanent_flags
		uid_validity: res.uid_validity
		uid_next: res.uid_next
		// EXAMINE is read-only by definition, and a server may hand back a
		// read-only mailbox to a SELECT as well.
		read_only: res.read_only || verb == 'EXAMINE'
	}
}

fn (c &Client) effective_port() int {
	if c.port != 0 {
		return c.port
	}
	if c.ssl {
		return default_ssl_port
	}
	return default_port
}

// upgrade_to_tls wraps the current connection in TLS and points the decoder at
// it.
fn (mut c Client) upgrade_to_tls() ! {
	c.ssl_conn = ssl.new_ssl_conn()!
	c.ssl_conn.connect(mut c.conn, c.server) or {
		return error('imap: TLS handshake with ${c.server} failed: ${err}')
	}
	c.dec = &Decoder{
		reader: io.new_buffered_reader(reader: c.ssl_conn)
	}
	c.encrypted = true
}

fn (mut c Client) decoder() !&Decoder {
	return c.dec or { return error('imap: the session is not connected') }
}

// next_tag returns the tag for the next command. Tags only have to be unique
// within a session, so a counter is enough.
fn (mut c Client) next_tag() string {
	c.tag_seq++
	return '${tag_prefix}${c.tag_seq:04}'
}

fn (mut c Client) write_line(line string) ! {
	$if imap_debug? {
		eprintln('[imap send] ${line}')
	}
	c.write_raw('${line}\r\n'.bytes())!
}

fn (mut c Client) write_raw(data []u8) ! {
	if c.encrypted {
		c.ssl_conn.write(data)!
		return
	}
	c.conn.write(data)!
}

// build_args renders a command and its arguments, splitting out the ones a
// quoted string cannot hold so that they travel as literals.
fn build_args(verb string, args []string) ([]string, [][]u8) {
	mut text := []string{}
	mut literals := [][]u8{}
	mut current := verb
	for a in args {
		current += ' '
		if !needs_literal(a) {
			current += quote_arg(a)
			continue
		}
		// The text ends here; the literal introducer and the octets follow.
		text << current
		literals << a.bytes()
		current = ''
	}
	text << current
	return text, literals
}

// needs_literal reports whether a value has to be sent as a literal. A quoted
// string may not span lines, and is defined over seven bit characters.
fn needs_literal(s string) bool {
	for ch in s {
		if ch == 0 || ch == `\r` || ch == `\n` || ch >= 0x80 {
			return true
		}
	}
	return false
}

// quote_arg wraps a value as a quoted string, escaping the two characters that
// would otherwise end it early.
fn quote_arg(s string) string {
	escaped := s.replace('\\', '\\\\').replace('"', '\\"')
	return '"${escaped}"'
}

// format_internal_date renders a time the way APPEND wants it, which is a
// fixed two digit day and an English month.
fn format_internal_date(t time.Time) string {
	months := ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
	return '${t.day:02}-${months[t.month - 1]}-${t.year:04} ${t.hour:02}:${t.minute:02}:${t.second:02} +0000'
}
