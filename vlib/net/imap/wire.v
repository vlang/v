module imap

import io

// A decoder over the IMAP grammar of RFC 3501 section 9.
//
// Responses are built from four things: atoms, quoted strings, literals and
// parenthesised lists. Reading them as such is what keeps message content from
// ever being mistaken for protocol, which no amount of searching a response
// line for `UID ` can guarantee: a message whose subject is `UID 999` would
// answer that search just as readily as the real field.

// A literal states its own length, so a server can ask a client to hold an
// arbitrary amount of memory. This is the most one literal may claim.
const max_literal_size = u32(64 * 1024 * 1024)

// Decoder reads the grammar off a connection, or off a fixed buffer when it is
// being exercised without one.
//
// Bytes are served from `buf`, refilled a chunk at a time. Reaching into the
// reader for one octet at a time would mean a call and an allocation per byte
// of every response.
struct Decoder {
mut:
	reader ?&io.BufferedReader
	buf    []u8
	pos    int
	filled int
}

// The size of one read from the connection.
const decoder_chunk = 8192

// decoder_over builds a decoder that reads a fixed buffer, which is how the
// grammar is tested without a server.
fn decoder_over(s string) &Decoder {
	bytes := s.bytes()
	return &Decoder{
		buf: bytes
		filled: bytes.len
	}
}

fn decoder_on(reader &io.BufferedReader) &Decoder {
	return &Decoder{
		reader: unsafe { reader }
		buf: []u8{len: decoder_chunk}
	}
}

fn (mut d Decoder) read_byte() !u8 {
	if d.pos >= d.filled {
		d.refill()!
	}
	ch := d.buf[d.pos]
	d.pos++
	return ch
}

// refill pulls the next chunk from the connection. A decoder over a fixed
// buffer has none, so running out is simply the end of the input.
fn (mut d Decoder) refill() ! {
	mut r := d.reader or { return error('imap: the response ended early') }
	n := r.read(mut d.buf)!
	if n <= 0 {
		return error('imap: the connection closed in the middle of a response')
	}
	d.filled = n
	d.pos = 0
}

// unread puts the last octet back, which is all the lookahead this grammar
// needs. It is only ever called on a byte just taken, so the position cannot
// go below the start of the chunk that byte came from.
fn (mut d Decoder) unread() {
	d.pos--
}

fn (mut d Decoder) peek_byte() !u8 {
	ch := d.read_byte()!
	d.unread()
	return ch
}

// read_n takes exactly `n` octets, which is how a literal is read: it is
// defined by its length and has no terminator to look for.
fn (mut d Decoder) read_n(n int) ![]u8 {
	mut out := []u8{len: n}
	// Whatever is already buffered comes first.
	mut got := d.filled - d.pos
	if got > n {
		got = n
	}
	if got > 0 {
		copy(mut out, d.buf[d.pos..d.pos + got])
		d.pos += got
	}
	if got == n {
		return out
	}
	// The rest goes from the connection straight into the caller's buffer,
	// since a literal can be many times the size of a chunk.
	mut r := d.reader or { return error('imap: the literal is shorter than it claimed') }
	for got < n {
		read := r.read(mut out[got..])!
		if read <= 0 {
			return error('imap: the connection closed inside a literal')
		}
		got += read
	}
	return out
}

// accept consumes the next octet when it is the one wanted, and reports
// whether it did.
fn (mut d Decoder) accept(want u8) !bool {
	ch := d.read_byte()!
	if ch == want {
		return true
	}
	d.unread()
	return false
}

fn (mut d Decoder) expect(want u8) ! {
	ch := d.read_byte()!
	if ch != want {
		return error('imap: expected `${rune(want)}`, got `${rune(ch)}`')
	}
}

// sp consumes the single space that separates two elements.
fn (mut d Decoder) sp() ! {
	d.expect(` `)!
}

// crlf consumes the line ending, tolerating a bare LF from a server that sends
// one.
fn (mut d Decoder) crlf() ! {
	ch := d.read_byte()!
	if ch == `\n` {
		return
	}
	if ch != `\r` {
		return error('imap: expected the end of the line, got `${rune(ch)}`')
	}
	d.expect(`\n`)!
}

// TokenKind names the four runs of bare characters the grammar has. They differ
// from each other by a single character, so one reader covers all of them.
enum TokenKind {
	// ATOM-CHAR only.
	atom
	// ATOM-CHAR plus `]`, which a mailbox name written bare may hold.
	astring
	// ATOM-CHAR minus `[`, which opens a section specification with no space
	// in front of it.
	item_name
	// The digits and punctuation a sequence set is made of, including the `*`
	// that an atom may not hold.
	seq_set
}

fn (k TokenKind) accepts(ch u8) bool {
	return match k {
		.atom { is_atom_char(ch) }
		.astring { is_atom_char(ch) || ch == `]` }
		.item_name { is_atom_char(ch) && ch != `[` }
		.seq_set { is_seq_set_char(ch) }
	}
}

// take_token reads one bare run, stopping at the first character the kind does
// not accept, so a token never swallows the syntax around it.
fn (mut d Decoder) take_token(kind TokenKind, name string) !string {
	mut out := []u8{cap: 32}
	for {
		ch := d.read_byte() or { break }
		if !kind.accepts(ch) {
			d.unread()
			break
		}
		out << ch
	}
	if out.len == 0 {
		return error('imap: expected ${name}')
	}
	return out.bytestr()
}

// atom reads an unquoted word.
fn (mut d Decoder) atom() !string {
	return d.take_token(.atom, 'an atom')
}

// item_name reads the name of a fetch item.
//
// It stops at `[` as well as at the atom specials. A section specification
// follows the name with no space between them, and `[` is a perfectly ordinary
// atom character, so an atom would otherwise swallow the opening bracket of
// `BODY[]` and leave the rest unreadable.
fn (mut d Decoder) item_name() !string {
	return d.take_token(.item_name, 'a fetch item name')
}

// number reads an unsigned 32 bit integer.
fn (mut d Decoder) number() !u32 {
	mut n := u64(0)
	mut digits := 0
	for {
		ch := d.read_byte() or { break }
		if ch < `0` || ch > `9` {
			d.unread()
			break
		}
		n = n * 10 + u64(ch - `0`)
		if n > 0xffffffff {
			return error('imap: a number in the response overflows 32 bits')
		}
		digits++
	}
	if digits == 0 {
		return error('imap: expected a number')
	}
	return u32(n)
}

// seq_set_token reads a sequence set as a response carries it.
//
// An atom cannot hold one: `*` is a list wildcard and therefore not an atom
// character, yet it is an ordinary part of a set.
fn (mut d Decoder) seq_set_token() !string {
	return d.take_token(.seq_set, 'a sequence set')
}

fn is_seq_set_char(ch u8) bool {
	if ch >= `0` && ch <= `9` {
		return true
	}
	return ch == `:` || ch == `,` || ch == `*`
}

// quoted reads a quoted string, where a backslash escapes the next octet.
fn (mut d Decoder) quoted() !string {
	d.expect(`"`)!
	mut out := []u8{cap: 64}
	for {
		mut ch := d.read_byte()!
		if ch == `"` {
			return out.bytestr()
		}
		if ch == `\r` || ch == `\n` {
			return error('imap: a quoted string cannot span lines')
		}
		if ch == `\\` {
			ch = d.read_byte()!
		}
		out << ch
	}
	return error('imap: unterminated quoted string')
}

// literal reads `{n}` followed by a line ending and exactly n octets. This is
// how a server sends anything a quoted string cannot hold, message bodies
// above all.
fn (mut d Decoder) literal() !string {
	d.expect(`{`)!
	size := d.number()!
	// A `+` marks a non-synchronising literal (RFC 7888). Nothing changes on
	// the reading side; the octets still follow.
	_ := d.accept(`+`)!
	d.expect(`}`)!
	d.crlf()!
	if size > max_literal_size {
		return error('imap: the server announced a ${size} octet literal, over the ${max_literal_size} limit')
	}
	if size == 0 {
		return ''
	}
	return d.read_n(int(size))!.bytestr()
}

// string_value reads either form a string may take.
fn (mut d Decoder) string_value() !string {
	ch := d.peek_byte()!
	if ch == `"` {
		return d.quoted()
	}
	if ch == `{` {
		return d.literal()
	}
	return error('imap: expected a quoted string or a literal')
}

// astring reads a string that may also be written bare. Mailbox names arrive
// this way, and a bare one may contain `]`, which an atom may not.
fn (mut d Decoder) astring() !string {
	ch := d.peek_byte()!
	if ch == `"` || ch == `{` {
		return d.string_value()
	}
	return d.take_token(.astring, 'a mailbox name or an atom')
}

// nstring_text reads a string or the atom NIL, which stands for a field the
// server has no value for.
//
// NIL comes back as the empty string. Nothing that reads these fields, a
// missing subject or an absent body section alike, has anything different to
// say about the two.
fn (mut d Decoder) nstring_text() !string {
	ch := d.peek_byte()!
	if ch == `"` || ch == `{` {
		return d.string_value()
	}
	word := d.atom()!
	if word.to_upper() != 'NIL' {
		return error('imap: expected a string or NIL, got `${word}`')
	}
	return ''
}

// text reads the free-form remainder of a line, which carries the human
// readable part of a status response.
fn (mut d Decoder) text() !string {
	mut out := []u8{cap: 64}
	for {
		ch := d.read_byte() or { break }
		if ch == `\r` || ch == `\n` {
			d.unread()
			break
		}
		out << ch
	}
	return out.bytestr()
}

// accept_nil consumes the atom NIL where a parenthesised value could stand,
// and reports whether it did. NIL is how a server says a header the message
// never carried, or a content type with no parameters.
fn (mut d Decoder) accept_nil() !bool {
	if d.peek_byte()! == `(` {
		return false
	}
	word := d.atom()!
	if word.to_upper() != 'NIL' {
		return error('imap: expected a parenthesised value or NIL, got `${word}`')
	}
	return true
}

// mailbox_name reads a name off the wire and hands it back as UTF-8. A name
// the encoding cannot explain is passed through rather than lost.
fn (mut d Decoder) mailbox_name() !string {
	raw := d.astring()!
	return utf7_decode(raw) or { raw }
}

// open_list consumes the opening parenthesis and reports whether the list has
// any element at all.
fn (mut d Decoder) open_list() !bool {
	d.expect(`(`)!
	return !d.accept(`)`)!
}

// more_in_list consumes what follows an element, and reports whether another
// one comes after it.
fn (mut d Decoder) more_in_list() !bool {
	if d.accept(`)`)! {
		return false
	}
	d.sp()!
	return true
}

// flag_list reads `(\Seen \Answered)`, the form flags take everywhere.
fn (mut d Decoder) flag_list() ![]string {
	mut flags := []string{}
	if !d.open_list()! {
		return flags
	}
	for {
		flags << d.flag()!
		if !d.more_in_list()! {
			return flags
		}
	}
	return flags
}

// flag reads one flag: an atom, prefixed by a backslash for the ones the
// protocol defines itself.
fn (mut d Decoder) flag() !string {
	if !d.accept(`\\`)! {
		return d.atom()
	}
	// `\*` in PERMANENTFLAGS means the mailbox accepts new keywords.
	if d.accept(`*`)! {
		return '\\*'
	}
	return '\\' + d.atom()!
}

// skip_value consumes one element of any shape, so that an item this module
// does not model cannot derail the rest of the response.
fn (mut d Decoder) skip_value() ! {
	ch := d.peek_byte()!
	if ch == `"` || ch == `{` {
		d.string_value()!
		return
	}
	if ch != `(` {
		d.atom()!
		return
	}
	if !d.open_list()! {
		return
	}
	for {
		d.skip_value()!
		if !d.more_in_list()! {
			return
		}
	}
}

// is_atom_char reports whether `ch` may appear in an atom. The exclusions are
// the `atom-specials` of the grammar: the list and literal punctuation, the
// space, the control characters, the two wildcards, the two quoting
// characters, and the bracket that closes a response code.
fn is_atom_char(ch u8) bool {
	if ch <= 0x1f || ch == 0x7f {
		return false
	}
	return match ch {
		`(`, `)`, `{`, ` `, `%`, `*`, `"`, `\\`, `]` { false }
		else { true }
	}
}
