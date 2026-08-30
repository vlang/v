module imap

import time

// The response types, and the grammar that fills them in.
//
// A server answers a command with any number of untagged responses followed by
// one tagged completion. The untagged ones are not all a reply to the command
// at hand: a mailbox can change under a client at any moment, and the server
// reports that by slipping EXISTS, EXPUNGE and FETCH responses into whatever
// exchange happens to be in flight. They are collected here rather than
// discarded.

// Status is the outcome a tagged completion reports.
pub enum Status {
	ok
	no
	bad
	bye
	preauth
}

// Mailbox is the state of a mailbox at the moment it was selected.
pub struct Mailbox {
pub:
	name string
	// exists is the number of messages in the mailbox.
	exists u32
	// recent is how many of them arrived since the last session looked.
	recent u32
	// unseen is the sequence number of the first unread message, zero when the
	// server did not report one.
	unseen u32
	// flags are the flags this mailbox can hold.
	flags []string
	// permanent_flags are the ones a STORE can make stick. A `\*` among them
	// means the mailbox accepts new keywords.
	permanent_flags []string
	// uid_validity changes when the server can no longer promise that UIDs
	// from an earlier session still name the same messages.
	uid_validity u32
	// uid_next is the UID the next arriving message is expected to take.
	uid_next u32
	// read_only is true for a mailbox opened with `examine`, and also for one
	// opened with `select` that the server would only give out read-only.
	read_only bool
}

// MailboxInfo is one entry of a mailbox listing.
pub struct MailboxInfo {
pub:
	name string
	// delimiter separates the levels of a hierarchical name, and is empty when
	// the server presents a flat namespace.
	delimiter string
	// attributes are the server's notes about the mailbox, such as `\Noselect`
	// for a name that only exists to hold children.
	attributes []string
}

// MailboxStatus is what STATUS reports about a mailbox without selecting it.
pub struct MailboxStatus {
pub:
	name         string
	messages     u32
	recent       u32
	uid_next     u32
	uid_validity u32
	unseen       u32
}

// Address is one sender or recipient, as an envelope carries it. `name` is
// the display name, empty when the message gave none.
pub struct Address {
pub:
	name string
	// mailbox is the part before the `@`, or the group name when `host` is
	// empty and this address opens a group.
	mailbox string
	// host is the part after the `@`.
	host string
}

// addr renders the address as `someone@example.com`, and is empty for the
// markers that open and close a group.
pub fn (a &Address) addr() string {
	if a.mailbox == '' || a.host == '' {
		return ''
	}
	return '${a.mailbox}@${a.host}'
}

// Envelope is the parsed header of a message, which a server can produce
// without the client fetching and parsing the header itself.
pub struct Envelope {
pub:
	date        string
	subject     string
	from        []Address
	sender      []Address
	reply_to    []Address
	to          []Address
	cc          []Address
	bcc         []Address
	in_reply_to string
	message_id  string
}

// BodyStructure describes one MIME part. A multipart has `parts` filled in and
// a media type of `multipart`; every other part is a leaf.
pub struct BodyStructure {
pub:
	media_type    string
	media_subtype string
	params        map[string]string
	id            string
	description   string
	encoding      string
	// size is the part's length in octets, and `lines` its line count for a
	// text part.
	size  u32
	lines u32
	parts []BodyStructure
}

// mime_type renders the pair as it appears in a Content-Type header.
pub fn (b &BodyStructure) mime_type() string {
	return '${b.media_type.to_lower()}/${b.media_subtype.to_lower()}'
}

// Message is one fetched message, holding whatever the fetch asked for.
//
// `seq` is its sequence number in the selected mailbox and is always set. The
// rest is filled in only when the fetch requested it.
pub struct Message {
pub:
	seq   u32
	uid   u32
	flags []string
	// size is what RFC822.SIZE reported.
	size u32
	// internal_date is when the server took delivery, which is not the Date
	// header and does not change when a message is copied.
	internal_date time.Time
	envelope      ?Envelope
	structure     ?BodyStructure
	// sections holds every body section the fetch asked for, keyed by the
	// specification the server echoed back: `BODY[]` for the whole message,
	// `BODY[HEADER]` for the header block, `BODY[1.2]` for one MIME part.
	sections map[string]string
}

// body returns the whole message when it was fetched, and the lone section
// when exactly one was, which is what a fetch of a single section wants.
//
// A fetch of several sections has no one answer, so it gives back nothing;
// read `sections` directly in that case.
pub fn (m &Message) body() string {
	if whole := m.sections['BODY[]'] {
		return whole
	}
	if m.sections.len != 1 {
		return ''
	}
	return m.sections[m.sections.keys()[0]]
}

// Response gathers everything a command produced: what it was asked for, and
// whatever the server volunteered along the way.
struct Response {
mut:
	status       Status
	code         string
	text         string
	capabilities []string
	mailboxes    []MailboxInfo
	messages     []Message
	numbers      []u32
	// set is what an ESEARCH response carries instead of a flat list, kept as
	// ranges rather than expanded: a result covering a whole large mailbox is
	// a handful of ranges and millions of numbers.
	set      SeqSet
	has_set  bool
	expunged []u32
	statuses []MailboxStatus
	// The mailbox state SELECT and EXAMINE report, and the running counts an
	// untagged EXISTS or RECENT updates at any other time.
	flags           []string
	permanent_flags []string
	exists          u32
	recent          u32
	unseen          u32
	uid_next        u32
	uid_validity    u32
	read_only       bool
	// has_exists and has_recent record whether the server said anything at
	// all, since zero is a perfectly ordinary count.
	has_exists bool
	has_recent bool
}

// read_response reads responses until the one tagged `tag`, and turns a NO or
// BAD completion into an error.
fn (mut c Client) read_response(tag string) !Response {
	mut out := Response{}
	for {
		mut d := c.decoder()!
		if d.accept(`+`)! {
			return error('imap: the server asked to continue a command that sends nothing more')
		}
		if d.accept(`*`)! {
			d.sp()!
			c.read_untagged(mut d, mut out)!
			d.crlf()!
			continue
		}
		got := d.astring()!
		d.sp()!
		read_completion(mut d, mut out)!
		d.crlf()!
		if got != tag {
			return error('imap: the server answered tag `${got}` while `${tag}` was outstanding')
		}
		if out.status == .ok {
			return out
		}
		return error('imap: ${out.status} ${out.text}')
	}
	return out
}

// read_untagged reads one `* ...` response, whichever of the several shapes it
// takes. Everything it learns is folded into `out`.
fn (mut c Client) read_untagged(mut d Decoder, mut out Response) ! {
	// A message data response opens with the message number, so a leading
	// digit settles the shape before any keyword is read.
	first := d.peek_byte()!
	if first >= `0` && first <= `9` {
		n := d.number()!
		d.sp()!
		return read_message_data(mut d, mut out, n)
	}
	name := d.atom()!.to_upper()
	match name {
		'OK', 'NO', 'BAD', 'BYE', 'PREAUTH' {
			out.status = status_from(name)!
			_ := d.accept(` `)!
			read_resp_text(mut d, mut out)!
			// An untagged BYE means the server is closing the connection, so
			// the session must not be treated as usable afterwards.
			if name == 'BYE' {
				c.is_open = false
			}
		}
		'CAPABILITY' {
			out.capabilities = read_space_separated(mut d)!
		}
		'FLAGS' {
			d.sp()!
			out.flags = d.flag_list()!
		}
		'LIST', 'LSUB' {
			d.sp()!
			out.mailboxes << read_mailbox_list(mut d)!
		}
		'SEARCH', 'SORT' {
			out.numbers = read_numbers(mut d)!
		}
		'STATUS' {
			d.sp()!
			out.statuses << read_status_data(mut d)!
		}
		'ESEARCH' {
			// RFC 4731 replaces the flat SEARCH list with a keyed one. Only
			// the message set is of interest here.
			read_esearch(mut d, mut out)!
		}
		else {
			// An extension this module does not model. Its line is skipped
			// whole rather than half read.
			d.text()!
		}
	}
}

// read_message_data reads `<n> EXISTS`, `<n> RECENT`, `<n> EXPUNGE` or
// `<n> FETCH (...)`.
fn read_message_data(mut d Decoder, mut out Response, n u32) ! {
	name := d.atom()!.to_upper()
	if name == 'EXISTS' {
		out.exists = n
		out.has_exists = true
		return
	}
	if name == 'RECENT' {
		out.recent = n
		out.has_recent = true
		return
	}
	if name == 'EXPUNGE' {
		out.expunged << n
		return
	}
	if name == 'FETCH' {
		d.sp()!
		out.messages << read_msg_att(mut d, n)!
		return
	}
	return error('imap: unknown message data response `${name}`')
}

// read_msg_att reads the parenthesised list of items a FETCH response carries.
// The items may come in any order, and an unknown one is stepped over whole.
fn read_msg_att(mut d Decoder, seq u32) !Message {
	mut uid := u32(0)
	mut flags := []string{}
	mut size := u32(0)
	mut internal_date := time.Time{}
	mut envelope := ?Envelope(none)
	mut structure := ?BodyStructure(none)
	mut sections := map[string]string{}

	if !d.open_list()! {
		return Message{
			seq: seq
		}
	}
	for {
		name := d.item_name()!.to_upper()
		match name {
			'UID' {
				d.sp()!
				uid = d.number()!
			}
			'FLAGS' {
				d.sp()!
				flags = d.flag_list()!
			}
			'RFC822.SIZE' {
				d.sp()!
				size = d.number()!
			}
			'INTERNALDATE' {
				d.sp()!
				internal_date = parse_internal_date(d.quoted()!)!
			}
			'ENVELOPE' {
				d.sp()!
				envelope = read_envelope(mut d)!
			}
			'BODYSTRUCTURE' {
				d.sp()!
				structure = read_body_structure(mut d)!
			}
			'RFC822', 'RFC822.HEADER', 'RFC822.TEXT' {
				// The older spellings of the three commonest sections.
				d.sp()!
				sections[rfc822_section(name)] = d.nstring_text()!
			}
			'BODY' {
				// `BODY` alone is the body structure; `BODY[...]` is content.
				is_section := d.accept(`[`)!
				if is_section {
					key := read_section_key(mut d)!
					d.sp()!
					sections[key] = d.nstring_text()!
				}
				if !is_section {
					d.sp()!
					structure = read_body_structure(mut d)!
				}
			}
			else {
				d.sp()!
				d.skip_value()!
			}
		}
		if !d.more_in_list()! {
			break
		}
	}
	return Message{
		seq: seq
		uid: uid
		flags: flags
		size: size
		internal_date: internal_date
		envelope: envelope
		structure: structure
		sections: sections
	}
}

// read_section_key reads what sits between the brackets of `BODY[...]`, along
// with the byte range that may follow it, and gives back the whole thing as
// the server wrote it. That is the key a caller looks the section up by, so it
// has to come back in one piece: `BODY[HEADER]`, or `BODY[]<0>` for a fetch
// that asked for only part of the message.
fn read_section_key(mut d Decoder) !string {
	mut out := []u8{}
	mut depth := 0
	mut in_quotes := false
	for {
		ch := d.read_byte()!
		if in_quotes {
			out << ch
			if ch == `\\` {
				out << d.read_byte()!
				continue
			}
			if ch == `"` {
				in_quotes = false
			}
			continue
		}
		if ch == `"` {
			in_quotes = true
			out << ch
			continue
		}
		if ch == `(` {
			depth++
		}
		if ch == `)` {
			depth--
		}
		if ch == `]` && depth == 0 {
			break
		}
		out << ch
	}
	// A partial fetch echoes the offset it started at, as `<1024>`.
	if !d.accept(`<`)! {
		return 'BODY[${out.bytestr()}]'
	}
	offset := d.number()!
	d.expect(`>`)!
	return 'BODY[${out.bytestr()}]<${offset}>'
}

// rfc822_section maps the RFC822 spellings onto the section they name, so that
// a caller finds the content under one key whichever form the server used.
fn rfc822_section(name string) string {
	if name == 'RFC822.HEADER' {
		return 'BODY[HEADER]'
	}
	if name == 'RFC822.TEXT' {
		return 'BODY[TEXT]'
	}
	return 'BODY[]'
}

// read_envelope reads the ten fields of an envelope, in the fixed order the
// grammar gives them.
fn read_envelope(mut d Decoder) !Envelope {
	d.expect(`(`)!
	date := d.nstring_text()!
	d.sp()!
	subject := d.nstring_text()!
	d.sp()!
	from := read_address_list(mut d)!
	d.sp()!
	sender := read_address_list(mut d)!
	d.sp()!
	reply_to := read_address_list(mut d)!
	d.sp()!
	to := read_address_list(mut d)!
	d.sp()!
	cc := read_address_list(mut d)!
	d.sp()!
	bcc := read_address_list(mut d)!
	d.sp()!
	in_reply_to := d.nstring_text()!
	d.sp()!
	message_id := d.nstring_text()!
	d.expect(`)`)!
	return Envelope{
		date: date
		subject: subject
		from: from
		sender: sender
		reply_to: reply_to
		to: to
		cc: cc
		bcc: bcc
		in_reply_to: in_reply_to
		message_id: message_id
	}
}

// read_address_list reads a run of addresses, or NIL for a header the message
// did not carry.
//
// The addresses are concatenated with nothing between them: the grammar is
// `"(" 1*address ")"`, not a list whose elements are separated by spaces.
fn read_address_list(mut d Decoder) ![]Address {
	mut out := []Address{}
	if d.peek_byte()! != `(` {
		word := d.atom()!
		if word.to_upper() != 'NIL' {
			return error('imap: expected an address list or NIL, got `${word}`')
		}
		return out
	}
	d.expect(`(`)!
	for d.peek_byte()! == `(` {
		out << read_address(mut d)!
	}
	d.expect(`)`)!
	return out
}

// read_address reads `(name adl mailbox host)`.
fn read_address(mut d Decoder) !Address {
	d.expect(`(`)!
	name := d.nstring_text()!
	d.sp()!
	// The source route of RFC 2822 is obsolete, and no sender writes one.
	d.nstring_text()!
	d.sp()!
	mailbox := d.nstring_text()!
	d.sp()!
	host := d.nstring_text()!
	d.expect(`)`)!
	return Address{
		name: name
		mailbox: mailbox
		host: host
	}
}

// read_body_structure reads one MIME part. A part whose first element is a
// list is a multipart, and the lists it opens with are its children.
fn read_body_structure(mut d Decoder) !BodyStructure {
	d.expect(`(`)!
	if d.peek_byte()! == `(` {
		return read_multipart(mut d)
	}
	media_type := d.string_value()!
	d.sp()!
	media_subtype := d.string_value()!
	d.sp()!
	params := read_param_list(mut d)!
	d.sp()!
	id := d.nstring_text()!
	d.sp()!
	description := d.nstring_text()!
	d.sp()!
	encoding := d.string_value()!
	d.sp()!
	size := d.number()!
	mut lines := u32(0)
	// A text part states its line count, and an embedded message states its
	// envelope, its own structure and its line count. Neither is required to
	// be followed by the extension fields, so each step is guarded.
	if media_type.to_upper() == 'TEXT' && d.accept(` `)! {
		lines = d.number()!
	}
	if media_type.to_upper() == 'MESSAGE' && media_subtype.to_upper() == 'RFC822' && d.accept(` `)! {
		read_envelope(mut d)!
		d.sp()!
		read_body_structure(mut d)!
		d.sp()!
		lines = d.number()!
	}
	skip_extensions(mut d)!
	return BodyStructure{
		media_type: media_type
		media_subtype: media_subtype
		params: params
		id: id
		description: description
		encoding: encoding
		size: size
		lines: lines
	}
}

fn read_multipart(mut d Decoder) !BodyStructure {
	mut parts := []BodyStructure{}
	for d.peek_byte()! == `(` {
		parts << read_body_structure(mut d)!
	}
	d.sp()!
	media_subtype := d.string_value()!
	skip_extensions(mut d)!
	return BodyStructure{
		media_type: 'multipart'
		media_subtype: media_subtype
		parts: parts
	}
}

// skip_extensions steps over the optional trailing fields of a body structure
// and consumes the closing parenthesis. They carry the disposition, the
// language and the location, none of which this module models, but a part that
// has them still has to be read past.
fn skip_extensions(mut d Decoder) ! {
	for {
		if d.accept(`)`)! {
			return
		}
		d.sp()!
		d.skip_value()!
	}
}

// read_param_list reads the `("charset" "utf-8")` pairs of a content type, or
// NIL when there are none.
fn read_param_list(mut d Decoder) !map[string]string {
	mut out := map[string]string{}
	if d.peek_byte()! != `(` {
		word := d.atom()!
		if word.to_upper() != 'NIL' {
			return error('imap: expected a parameter list or NIL, got `${word}`')
		}
		return out
	}
	if !d.open_list()! {
		return out
	}
	for {
		key := d.string_value()!
		d.sp()!
		out[key.to_lower()] = d.string_value()!
		if !d.more_in_list()! {
			return out
		}
	}
	return out
}

// read_mailbox_list reads the body of a LIST or LSUB response.
fn read_mailbox_list(mut d Decoder) !MailboxInfo {
	attributes := d.flag_list()!
	d.sp()!
	// The delimiter is a quoted character, or NIL for a flat namespace.
	mut delimiter := ''
	is_quoted := d.peek_byte()! == `"`
	if is_quoted {
		delimiter = d.quoted()!
	}
	if !is_quoted {
		word := d.atom()!
		if word.to_upper() != 'NIL' {
			return error('imap: expected a hierarchy delimiter or NIL, got `${word}`')
		}
	}
	d.sp()!
	raw := d.astring()!
	return MailboxInfo{
		name: utf7_decode(raw) or { raw }
		delimiter: delimiter
		attributes: attributes
	}
}

// read_status_data reads `STATUS mailbox (MESSAGES 231 UIDNEXT 44292)`.
fn read_status_data(mut d Decoder) !MailboxStatus {
	raw := d.astring()!
	d.sp()!
	mut messages := u32(0)
	mut recent := u32(0)
	mut uid_next := u32(0)
	mut uid_validity := u32(0)
	mut unseen := u32(0)
	if d.open_list()! {
		for {
			name := d.atom()!.to_upper()
			d.sp()!
			value := d.number()!
			match name {
				'MESSAGES' {
					messages = value
				}
				'RECENT' {
					recent = value
				}
				'UIDNEXT' {
					uid_next = value
				}
				'UIDVALIDITY' {
					uid_validity = value
				}
				'UNSEEN' {
					unseen = value
				}
				else {}
			}
			if !d.more_in_list()! {
				break
			}
		}
	}
	return MailboxStatus{
		name: utf7_decode(raw) or { raw }
		messages: messages
		recent: recent
		uid_next: uid_next
		uid_validity: uid_validity
		unseen: unseen
	}
}

// read_esearch reads the keyed search result of RFC 4731, keeping the message
// set and stepping over the counts.
fn read_esearch(mut d Decoder, mut out Response) ! {
	for {
		if !d.accept(` `)! {
			return
		}
		if d.peek_byte()! == `(` {
			// The correlator, naming the command this answers.
			d.skip_value()!
			continue
		}
		name := d.atom()!.to_upper()
		// `UID` is a bare marker saying the numbers are UIDs, with no value of
		// its own.
		if name == 'UID' {
			continue
		}
		d.sp()!
		if name == 'ALL' {
			out.set = parse_seq_set(d.seq_set_token()!)!
			out.has_set = true
			continue
		}
		// MIN, MAX, COUNT and the rest all carry a single number.
		d.atom()!
	}
}

// read_resp_text reads the human readable tail of a status response, along
// with the machine readable code that may precede it in brackets.
fn read_resp_text(mut d Decoder, mut out Response) ! {
	if d.accept(`[`)! {
		read_resp_text_code(mut d, mut out)!
		d.expect(`]`)!
		// A code with nothing after it is unusual but legal.
		_ := d.accept(` `)!
	}
	out.text = d.text()!
}

// read_resp_text_code reads one bracketed response code. Several of them carry
// the mailbox state that SELECT reports.
fn read_resp_text_code(mut d Decoder, mut out Response) ! {
	name := d.atom()!.to_upper()
	out.code = name
	match name {
		'UNSEEN' {
			d.sp()!
			out.unseen = d.number()!
		}
		'UIDNEXT' {
			d.sp()!
			out.uid_next = d.number()!
		}
		'UIDVALIDITY' {
			d.sp()!
			out.uid_validity = d.number()!
		}
		'PERMANENTFLAGS' {
			d.sp()!
			out.permanent_flags = d.flag_list()!
		}
		'READ-ONLY' {
			out.read_only = true
		}
		'READ-WRITE' {
			out.read_only = false
		}
		'CAPABILITY' {
			out.capabilities = read_space_separated(mut d)!
		}
		else {
			// Any other code, with or without an argument, is stepped over.
			for d.peek_byte()! != `]` {
				d.read_byte()!
			}
		}
	}
}

// read_space_separated reads a run of space separated atoms, which is how
// capabilities are listed.
fn read_space_separated(mut d Decoder) ![]string {
	mut out := []string{}
	for d.accept(` `)! {
		out << d.atom()!
	}
	return out
}

// read_numbers reads a run of space separated numbers, which is how SEARCH
// answers.
fn read_numbers(mut d Decoder) ![]u32 {
	mut out := []u32{}
	for d.accept(` `)! {
		out << d.number()!
	}
	return out
}

// read_completion reads a status word and the text that follows it.
//
// The grammar makes the space between them mandatory, and the text non-empty.
// A server that ends the line right after the status word is out of spec, but
// failing the whole response over a missing pleasantry helps nobody.
fn read_completion(mut d Decoder, mut out Response) ! {
	out.status = read_status(mut d)!
	if !d.accept(` `)! {
		return
	}
	read_resp_text(mut d, mut out)!
}

fn read_status(mut d Decoder) !Status {
	return status_from(d.atom()!)
}

fn status_from(word string) !Status {
	return match word.to_upper() {
		'OK' { Status.ok }
		'NO' { Status.no }
		'BAD' { Status.bad }
		'BYE' { Status.bye }
		'PREAUTH' { Status.preauth }
		else { error('imap: unknown completion `${word}`') }
	}
}

// parse_internal_date reads the fixed `1-Feb-2026 12:34:56 +0100` form the
// protocol uses, which is not any of the formats `time` already parses: the
// day may be space padded and the month is an English abbreviation.
fn parse_internal_date(s string) !time.Time {
	parts := s.trim_space().split(' ')
	if parts.len != 3 {
		return error('imap: `${s}` is not an internal date')
	}
	day_parts := parts[0].split('-')
	if day_parts.len != 3 {
		return error('imap: `${s}` has no day, month and year')
	}
	clock := parts[1].split(':')
	if clock.len != 3 {
		return error('imap: `${s}` has no hour, minute and second')
	}
	month := month_number(day_parts[1])!
	zone := parse_zone(parts[2])!
	stamp := time.new(
		year: day_parts[2].int()
		month: month
		day: day_parts[0].int()
		hour: clock[0].int()
		minute: clock[1].int()
		second: clock[2].int()
	)
	// The stamp is local to the sender's zone, so the offset is taken back off
	// to land on the same instant everywhere.
	return time.unix(stamp.unix() - zone)
}

fn month_number(name string) !int {
	months := ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
	for i, m in months {
		if m.to_lower() == name.to_lower() {
			return i + 1
		}
	}
	return error('imap: `${name}` is not a month')
}

// parse_zone reads `+0100` and returns the offset in seconds.
fn parse_zone(s string) !i64 {
	if s.len != 5 || (s[0] != `+` && s[0] != `-`) {
		return error('imap: `${s}` is not a time zone offset')
	}
	hours := s[1..3].int()
	minutes := s[3..5].int()
	seconds := i64(hours) * 3600 + i64(minutes) * 60
	if s[0] == `-` {
		return -seconds
	}
	return seconds
}
