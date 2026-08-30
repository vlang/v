module imap

// The transcripts below are the examples printed in RFC 3501, so that the
// grammar is checked against what the specification says a server sends rather
// than against what this implementation happens to produce.

// client_over drives the response reader off a fixed buffer, which is the
// whole session apart from the socket.
fn client_over(s string) &Client {
	return &Client{
		dec: decoder_over(s)
		is_open: true
	}
}

fn test_decoder_reads_the_four_shapes_of_the_grammar() {
	mut d := decoder_over('ATOM "a quoted \\"string\\"" {5}\r\nab\r\ncd (a (b c) NIL)')
	assert d.atom()! == 'ATOM'
	d.sp()!
	assert d.quoted()! == 'a quoted "string"'
	d.sp()!
	// A literal is defined by its length, so the line ending inside it is
	// content and the one after it is not.
	assert d.literal()! == 'ab\r\nc'
	assert d.atom()! == 'd'
	d.sp()!
	// A nested list is stepped over whole, whatever it holds.
	d.skip_value()!
	assert d.src_pos == d.src.len
}

fn test_a_quoted_string_never_ends_early() {
	mut d := decoder_over('"a\\\\b" "c\\"d" ""')
	assert d.quoted()! == 'a\\b'
	d.sp()!
	assert d.quoted()! == 'c"d'
	d.sp()!
	assert d.quoted()! == ''
}

fn test_atoms_stop_at_the_specials() {
	// Each of these characters ends an atom, which is what stops one from
	// swallowing the syntax around it.
	for special in ['(', ')', '{', ' ', '%', '*', '"', '\\', ']'] {
		mut d := decoder_over('WORD${special}rest')
		assert d.atom()! == 'WORD', '`${special}` did not end the atom'
	}
}

fn test_a_number_that_overflows_is_refused() {
	mut d := decoder_over('4294967296')
	d.number() or { return }
	assert false, 'a number past 32 bits must not be accepted'
}

fn test_select_reports_the_mailbox_state() {
	// RFC 3501 section 6.3.1.
	mut c := client_over('* 172 EXISTS\r\n' + '* 1 RECENT\r\n' + '* OK [UNSEEN 12] Message 12 is first unseen\r\n' + '* OK [UIDVALIDITY 3857529045] UIDs valid\r\n' + '* OK [UIDNEXT 4392] Predicted next UID\r\n' + '* FLAGS (\\Answered \\Flagged \\Deleted \\Seen \\Draft)\r\n' + '* OK [PERMANENTFLAGS (\\Deleted \\Seen \\*)] Limited\r\n' + 'a0001 OK [READ-WRITE] SELECT completed\r\n')
	res := c.read_response('a0001')!
	assert res.exists == 172
	assert res.recent == 1
	assert res.unseen == 12
	assert res.uid_validity == 3857529045
	assert res.uid_next == 4392
	assert res.flags == ['\\Answered', '\\Flagged', '\\Deleted', '\\Seen', '\\Draft']
	assert res.permanent_flags == ['\\Deleted', '\\Seen', '\\*']
	assert !res.read_only
	assert res.code == 'READ-WRITE'
}

fn test_uid_validity_holds_the_full_32_bit_range() {
	mut c := client_over('* OK [UIDVALIDITY 4294967295] UIDs valid\r\na1 OK done\r\n')
	assert c.read_response('a1')!.uid_validity == 4294967295
}

fn test_list_reads_every_form_of_a_mailbox_name() {
	mut c := client_over('* LIST (\\Noselect) "/" ""\r\n' + '* LIST () "." INBOX\r\n' + '* LIST (\\HasChildren \\Marked) "/" "Work/Reports 2026"\r\n' + '* LIST (\\Noinferiors) NIL "Flat"\r\n' + '* LIST () "/" "od\\"d"\r\n' + '* LIST () "/" {7}\r\nliteral\r\n' + '* LIST () "/" "&AMk-l&AOk-ments"\r\n' + 'a1 OK LIST completed\r\n')
	boxes := c.read_response('a1')!.mailboxes
	assert boxes.len == 7
	// The empty name is how a client discovers the delimiter, so it survives.
	assert boxes[0].name == ''
	assert boxes[0].delimiter == '/'
	assert boxes[0].attributes == ['\\Noselect']
	// A bare atom is a valid mailbox name.
	assert boxes[1].name == 'INBOX'
	assert boxes[1].delimiter == '.'
	assert boxes[2].name == 'Work/Reports 2026'
	assert boxes[2].attributes == ['\\HasChildren', '\\Marked']
	// NIL means a flat namespace.
	assert boxes[3].delimiter == ''
	// An escaped quote does not end the name.
	assert boxes[4].name == 'od"d'
	// A name may arrive as a literal.
	assert boxes[5].name == 'literal'
	// And it comes back as UTF-8, not as the modified UTF-7 it travelled in.
	assert boxes[6].name == 'Éléments'
}

fn test_fetch_reads_metadata() {
	mut c := client_over('* 23 FETCH (FLAGS (\\Seen) UID 4827313 RFC822.SIZE 44827)\r\n' + '* 24 FETCH (UID 4827943 FLAGS (\\Seen \\Answered))\r\n' + 'a1 OK FETCH completed\r\n')
	msgs := c.read_response('a1')!.messages
	assert msgs.len == 2
	assert msgs[0].seq == 23
	assert msgs[0].uid == 4827313
	assert msgs[0].size == 44827
	assert msgs[0].flags == ['\\Seen']
	// Items may come in any order.
	assert msgs[1].seq == 24
	assert msgs[1].uid == 4827943
	assert msgs[1].flags == ['\\Seen', '\\Answered']
}

fn test_message_content_is_never_read_as_protocol() {
	// The reason this module tokenises rather than searching the line: a
	// message whose subject says `UID 999` answers a search for `UID ` just as
	// readily as the real field does.
	body := 'Subject: UID 999 FLAGS (\\Deleted)\r\n\r\nRFC822.SIZE 1\r\n'
	mut c := client_over('* 5 FETCH (UID 7 RFC822.SIZE 42 BODY[] {${body.len}}\r\n' + '${body})\r\n' + 'a1 OK FETCH completed\r\n')
	msgs := c.read_response('a1')!.messages
	assert msgs.len == 1
	assert msgs[0].uid == 7
	assert msgs[0].size == 42
	assert msgs[0].flags == []
	assert msgs[0].body() == body
}

fn test_an_envelope_subject_cannot_forge_a_field() {
	// The same trap, in a quoted string rather than a literal.
	mut c := client_over('* 1 FETCH (ENVELOPE (NIL "UID 999 RFC822.SIZE 5" NIL NIL NIL NIL NIL NIL NIL NIL) UID 3)\r\n' + 'a1 OK done\r\n')
	msgs := c.read_response('a1')!.messages
	assert msgs[0].uid == 3
	assert msgs[0].size == 0
	envelope := msgs[0].envelope or {
		assert false, 'the envelope was not parsed'
		return
	}
	assert envelope.subject == 'UID 999 RFC822.SIZE 5'
}

fn test_envelope_from_the_rfc_example() {
	// RFC 3501 section 7.4.2.
	mut c := client_over('* 12 FETCH (ENVELOPE ' + '("Wed, 17 Jul 1996 02:23:25 -0700 (PDT)" ' + '"IMAP4rev1 WG mtg summary and minutes" ' + '(("Terry Gray" NIL "gray" "cac.washington.edu")) ' + '(("Terry Gray" NIL "gray" "cac.washington.edu")) ' + '(("Terry Gray" NIL "gray" "cac.washington.edu")) ' + '((NIL NIL "imap" "cac.washington.edu")) ' + '((NIL NIL "minutes" "CNRI.Reston.VA.US")' + '("John Klensin" NIL "KLENSIN" "MIT.EDU")) ' + 'NIL NIL "<B27397-0100000@cac.washington.edu>"))\r\n' + 'a1 OK done\r\n')
	msgs := c.read_response('a1')!.messages
	envelope := msgs[0].envelope or {
		assert false, 'the envelope was not parsed'
		return
	}
	assert envelope.date == 'Wed, 17 Jul 1996 02:23:25 -0700 (PDT)'
	assert envelope.subject == 'IMAP4rev1 WG mtg summary and minutes'
	assert envelope.from.len == 1
	assert envelope.from[0].name == 'Terry Gray'
	assert envelope.from[0].addr() == 'gray@cac.washington.edu'
	assert envelope.to.len == 1
	assert envelope.to[0].name == ''
	assert envelope.to[0].addr() == 'imap@cac.washington.edu'
	assert envelope.cc.len == 2
	assert envelope.cc[1].addr() == 'KLENSIN@MIT.EDU'
	// NIL for a header the message did not carry.
	assert envelope.bcc == []
	assert envelope.in_reply_to == ''
	assert envelope.message_id == '<B27397-0100000@cac.washington.edu>'
}

fn test_a_single_part_body_structure() {
	// RFC 3501 section 6.4.5.
	mut c := client_over('* 5 FETCH (BODY ("TEXT" "PLAIN" ("CHARSET" "US-ASCII") NIL NIL "7BIT" 3028 92))\r\n' + 'a1 OK done\r\n')
	structure := c.read_response('a1')!.messages[0].structure or {
		assert false, 'the body structure was not parsed'
		return
	}
	assert structure.mime_type() == 'text/plain'
	assert structure.params['charset'] == 'US-ASCII'
	assert structure.encoding == '7BIT'
	assert structure.size == 3028
	assert structure.lines == 92
	assert structure.parts == []
}

fn test_a_nested_multipart_body_structure() {
	mut c := client_over('* 6 FETCH (BODYSTRUCTURE (' + '(("TEXT" "PLAIN" ("CHARSET" "US-ASCII") NIL NIL "7BIT" 1152 23)' + '("TEXT" "HTML" ("CHARSET" "US-ASCII") NIL NIL "QUOTED-PRINTABLE" 1024 15)' + ' "ALTERNATIVE" ("BOUNDARY" "inner") NIL NIL)' + '("IMAGE" "GIF" ("NAME" "cat.gif") "<id@x>" "a cat" "BASE64" 4554)' + ' "MIXED" ("BOUNDARY" "outer") NIL NIL))\r\n' + 'a1 OK done\r\n')
	structure := c.read_response('a1')!.messages[0].structure or {
		assert false, 'the body structure was not parsed'
		return
	}
	assert structure.mime_type() == 'multipart/mixed'
	assert structure.parts.len == 2
	alternative := structure.parts[0]
	assert alternative.mime_type() == 'multipart/alternative'
	assert alternative.parts.len == 2
	assert alternative.parts[0].mime_type() == 'text/plain'
	assert alternative.parts[1].mime_type() == 'text/html'
	assert alternative.parts[1].encoding == 'QUOTED-PRINTABLE'
	image := structure.parts[1]
	assert image.mime_type() == 'image/gif'
	assert image.params['name'] == 'cat.gif'
	assert image.id == '<id@x>'
	assert image.description == 'a cat'
	assert image.size == 4554
}

fn test_several_sections_come_back_keyed() {
	header := 'Subject: hi\r\n'
	text := 'body text\r\n'
	mut c := client_over('* 1 FETCH (BODY[HEADER] {${header.len}}\r\n${header} ' + 'BODY[TEXT] {${text.len}}\r\n${text} ' + 'BODY[]<1024> "partial")\r\n' + 'a1 OK done\r\n')
	msg := c.read_response('a1')!.messages[0]
	assert msg.sections['BODY[HEADER]'] == header
	assert msg.sections['BODY[TEXT]'] == text
	// A partial fetch is a section of its own, keyed by where it started.
	assert msg.sections['BODY[]<1024>'] == 'partial'
	// With three of them there is no single body to return.
	assert msg.body() == ''
}

fn test_a_section_specification_with_a_list_inside_it() {
	mut c := client_over('* 1 FETCH (BODY[HEADER.FIELDS (SUBJECT FROM)] "x")\r\na1 OK done\r\n')
	msg := c.read_response('a1')!.messages[0]
	assert msg.sections['BODY[HEADER.FIELDS (SUBJECT FROM)]'] == 'x'
	assert msg.body() == 'x'
}

fn test_the_rfc822_spellings_map_onto_sections() {
	mut c := client_over('* 1 FETCH (RFC822.HEADER "h" RFC822.TEXT "t" RFC822 "whole")\r\na1 OK done\r\n')
	msg := c.read_response('a1')!.messages[0]
	assert msg.sections['BODY[HEADER]'] == 'h'
	assert msg.sections['BODY[TEXT]'] == 't'
	assert msg.sections['BODY[]'] == 'whole'
	// `body` prefers the whole message when it is among them.
	assert msg.body() == 'whole'
}

fn test_an_unknown_fetch_item_does_not_derail_the_rest() {
	// An extension this module does not model sits between two it does.
	mut c := client_over('* 1 FETCH (UID 5 MODSEQ (12345) X-GM-LABELS ("a" "b") FLAGS (\\Seen))\r\n' + 'a1 OK done\r\n')
	msg := c.read_response('a1')!.messages[0]
	assert msg.uid == 5
	assert msg.flags == ['\\Seen']
}

fn test_internal_date() {
	mut c := client_over('* 1 FETCH (INTERNALDATE "17-Jul-1996 02:44:25 -0700")\r\na1 OK done\r\n')
	stamp := c.read_response('a1')!.messages[0].internal_date
	// 1996-07-17 02:44:25 seven hours behind UTC is 09:44:25 UTC.
	assert stamp.unix() == 837596665
}

fn test_search_and_expunge() {
	mut c := client_over('* SEARCH 2 84 882\r\na1 OK done\r\n')
	assert c.read_response('a1')!.numbers == [u32(2), 84, 882]

	// A search that matched nothing sends the keyword with no numbers.
	mut empty := client_over('* SEARCH\r\na1 OK done\r\n')
	assert empty.read_response('a1')!.numbers == []

	// Each expunge renumbers what follows it, which is why 3 appears twice.
	mut e := client_over('* 3 EXPUNGE\r\n* 3 EXPUNGE\r\n* 5 EXPUNGE\r\n* 8 EXISTS\r\na1 OK done\r\n')
	res := e.read_response('a1')!
	assert res.expunged == [u32(3), 3, 5]
	assert res.exists == 8
}

fn test_status_data() {
	mut c := client_over('* STATUS "Work/&AMk-t&AOk-" (MESSAGES 231 UIDNEXT 44292 UNSEEN 3)\r\n' + 'a1 OK done\r\n')
	status := c.read_response('a1')!.statuses[0]
	assert status.name == 'Work/Été'
	assert status.messages == 231
	assert status.uid_next == 44292
	assert status.unseen == 3
	assert status.recent == 0
}

fn test_capability() {
	mut c := client_over('* CAPABILITY IMAP4rev1 STARTTLS AUTH=PLAIN UIDPLUS MOVE\r\na1 OK done\r\n')
	assert c.read_response('a1')!.capabilities == ['IMAP4rev1', 'STARTTLS', 'AUTH=PLAIN', 'UIDPLUS',
		'MOVE']
}

fn test_a_capability_code_on_the_completion_is_read_too() {
	mut c := client_over('a1 OK [CAPABILITY IMAP4rev1 IDLE] Logged in\r\n')
	assert c.read_response('a1')!.capabilities == ['IMAP4rev1', 'IDLE']
}

fn test_a_refusal_becomes_an_error() {
	mut c := client_over('a1 NO [AUTHENTICATIONFAILED] Invalid credentials\r\n')
	c.read_response('a1') or {
		assert err.msg().contains('Invalid credentials')
		mut bad := client_over('a1 BAD Missing argument\r\n')
		bad.read_response('a1') or {
			assert err.msg().contains('Missing argument')
			return
		}
		assert false, 'a BAD completion must not be reported as success'
		return
	}
	assert false, 'a NO completion must not be reported as success'
}

fn test_a_completion_for_another_tag_is_an_error() {
	// Answering the wrong tag means the client and the server disagree about
	// which command is in flight, which nothing later can recover from.
	mut c := client_over('a9 OK done\r\n')
	c.read_response('a1') or {
		assert err.msg().contains('a9')
		return
	}
	assert false, 'a completion for another tag must not be accepted'
}

fn test_an_untagged_bye_closes_the_session() {
	mut c := client_over('* BYE Autologout; idle for too long\r\na1 OK done\r\n')
	c.read_response('a1')!
	assert !c.is_open
}

fn test_a_completion_with_no_text_is_tolerated() {
	// Out of spec, but failing the whole response over a missing pleasantry
	// helps nobody.
	mut c := client_over('a1 OK\r\n')
	assert c.read_response('a1')!.status == .ok
}

fn test_a_bare_lf_line_ending_is_tolerated() {
	mut c := client_over('* 4 EXISTS\na1 OK done\n')
	assert c.read_response('a1')!.exists == 4
}

fn test_a_literal_announcing_more_than_it_sends_is_refused() {
	mut c := client_over('* 1 FETCH (BODY[] {100}\r\nshort)\r\na1 OK done\r\n')
	c.read_response('a1') or { return }
	assert false, 'a truncated literal must not be accepted'
}

fn test_an_absurd_literal_size_is_refused_before_allocating() {
	mut c := client_over('* 1 FETCH (BODY[] {4294967295}\r\n')
	c.read_response('a1') or {
		assert err.msg().contains('limit')
		return
	}
	assert false, 'a literal past the cap must be refused'
}

fn test_esearch_keeps_its_ranges() {
	// RFC 4731 answers with a sequence set rather than a flat list, which is
	// the whole point: a result covering a large mailbox is a few ranges and
	// millions of numbers.
	mut c := client_over('* ESEARCH (TAG "a1") UID MIN 2 COUNT 50000 ALL 1:49999,60000\r\n' + 'a1 OK done\r\n')
	res := c.read_response('a1')!
	assert res.has_set
	assert res.set.str() == '1:49999,60000'
	assert res.set.len() == 2
}

fn test_esearch_handles_an_open_range() {
	// `*` is a list wildcard and so not an atom character, which is why the
	// set needs a reader of its own.
	mut c := client_over('* ESEARCH (TAG "a1") ALL 5:*\r\na1 OK done\r\n')
	assert c.read_response('a1')!.set.str() == '5:*'
}

fn test_a_search_that_matched_nothing_in_the_esearch_form() {
	// RFC 4731 leaves ALL out entirely when nothing matched.
	mut c := client_over('* ESEARCH (TAG "a1") UID COUNT 0\r\na1 OK done\r\n')
	res := c.read_response('a1')!
	assert !res.has_set
	assert res.set.is_empty()
}
