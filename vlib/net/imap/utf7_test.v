module imap

// The vectors below come from RFC 3501 section 5.1.3 and RFC 2152, plus the
// mailbox names real servers hand out. They were cross-checked against
// Python's `utf-7` codec with `/` swapped for `,`, which is the same encoding
// arrived at independently.
fn test_ascii_passes_through() {
	for s in ['INBOX', 'Sent', 'Work/Reports', 'a b c', '~/Mail', 'x!\$%*()[]{}#'] {
		assert utf7_encode(s) == s
		assert utf7_decode(s)! == s
	}
}

fn test_the_shift_character_stands_for_itself() {
	assert utf7_encode('A&B') == 'A&-B'
	assert utf7_decode('A&-B')! == 'A&B'
	assert utf7_encode('&') == '&-'
	assert utf7_decode('&-')! == '&'
	assert utf7_encode('&&') == '&-&-'
	assert utf7_decode('&-&-')! == '&&'
}

fn test_rfc_examples() {
	// The two names RFC 3501 prints, one Japanese and one mixed.
	assert utf7_decode('~peter/mail/&U,BTFw-/&ZeVnLIqe-')! == '~peter/mail/台北/日本語'
	assert utf7_encode('~peter/mail/台北/日本語') == '~peter/mail/&U,BTFw-/&ZeVnLIqe-'
	// RFC 2060's own example of the ampersand escape.
	assert utf7_decode('&-')! == '&'
}

fn test_round_trip() {
	names := [
		'Rapports 2026',
		'Éléments envoyés',
		'Корзина',
		'垃圾桶',
		'受信トレイ',
		'📨 Newsletters',
		'a&b',
		'&&&',
		'Travail/Clients/Société Générale',
		'√±≈',
		'\x01control',
		'ünïcödé/nëstèd/pàth',
	]
	for name in names {
		encoded := utf7_encode(name)
		// The encoded form is always plain printable ASCII, which is what lets
		// a mailbox name travel as a quoted string rather than a literal.
		for ch in encoded {
			assert ch >= 0x20 && ch <= 0x7e, 'encoding ${name} left a raw octet ${ch}'
		}
		assert utf7_decode(encoded)! == name, 'round trip failed for ${name}'
	}
}

fn test_astral_plane_uses_a_surrogate_pair() {
	// U+1F4E8 is above the basic plane, so it takes two UTF-16 units, which is
	// four octets and therefore a longer base64 run.
	encoded := utf7_encode('📨')
	assert encoded == '&2D3c6A-'
	assert utf7_decode(encoded)! == '📨'
}

fn test_one_run_covers_consecutive_non_ascii() {
	// Shifting out and straight back in for each character would be the
	// superfluous shift the RFC tells servers to reject.
	assert utf7_encode('日本語').count('&') == 1
	assert utf7_encode('日a本') == '&ZeU-a&Zyw-'
}

fn test_control_characters_are_encoded() {
	// A tab is below the self-representing range, so it may not be sent bare.
	assert utf7_encode('a\tb') == 'a&AAk-b'
	assert utf7_decode('a&AAk-b')! == 'a\tb'
}

fn test_malformed_input_is_rejected() {
	// A shift that never ends.
	assert fails(fn () ! {
		utf7_decode('&AOk')!})
	// A character outside the base64 alphabet.
	assert fails(fn () ! {
		utf7_decode('&A!k-')!})
	// `/` belongs to standard base64, not this one.
	assert fails(fn () ! {
		utf7_decode('&A/k-')!})
	// A run holding half a UTF-16 unit.
	assert fails(fn () ! {
		utf7_decode('&AAAA-')!})
	// A high surrogate with nothing after it.
	assert fails(fn () ! {
		utf7_decode('&2D0-')!})
	// A low surrogate on its own.
	assert fails(fn () ! {
		utf7_decode('&3Og-')!})
}

fn test_raw_utf8_is_tolerated_on_the_way_in() {
	// Some servers ignore the convention and send UTF-8 directly. Refusing it
	// would hide their mailboxes from a caller for no gain.
	assert utf7_decode('Éléments')! == 'Éléments'
}

// fails reports whether `f` returned an error, which keeps the negative cases
// above to one line each.
fn fails(f fn() !) bool {
	f() or { return true }
	return false
}
