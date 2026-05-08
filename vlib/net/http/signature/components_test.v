// Tests for derived component canonicalization rules from RFC 9421 §2.2.
module signature

fn test_authority_strips_default_https_port() {
	c := Components{
		authority: 'Example.com:443'
		scheme:    'https'
	}
	assert c.derived_value('@authority')! == 'example.com'
}

fn test_authority_strips_default_http_port() {
	c := Components{
		authority: 'example.com:80'
		scheme:    'http'
	}
	assert c.derived_value('@authority')! == 'example.com'
}

fn test_authority_keeps_non_default_port() {
	c := Components{
		authority: 'example.com:8443'
		scheme:    'https'
	}
	assert c.derived_value('@authority')! == 'example.com:8443'
}

fn test_authority_does_not_strip_when_scheme_mismatched() {
	// :80 with https scheme is *not* the default, so it must stay.
	c := Components{
		authority: 'example.com:80'
		scheme:    'https'
	}
	assert c.derived_value('@authority')! == 'example.com:80'
}

fn test_authority_no_port_unchanged() {
	c := Components{
		authority: 'Example.COM'
		scheme:    'https'
	}
	assert c.derived_value('@authority')! == 'example.com'
}

fn test_authority_ipv6_strips_default_port() {
	c := Components{
		authority: '[2001:db8::1]:443'
		scheme:    'https'
	}
	assert c.derived_value('@authority')! == '[2001:db8::1]'
}

fn test_authority_ipv6_keeps_non_default_port() {
	c := Components{
		authority: '[2001:db8::1]:8443'
		scheme:    'https'
	}
	assert c.derived_value('@authority')! == '[2001:db8::1]:8443'
}

fn test_authority_ipv6_no_port_unchanged() {
	c := Components{
		authority: '[2001:db8::1]'
		scheme:    'https'
	}
	assert c.derived_value('@authority')! == '[2001:db8::1]'
}

fn test_authority_no_scheme_keeps_port() {
	// Without a scheme we cannot know which port is the default, so
	// the port is preserved.
	c := Components{
		authority: 'example.com:443'
	}
	assert c.derived_value('@authority')! == 'example.com:443'
}
