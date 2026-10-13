module ini

const sample = '[server]
host = example.com
port: 8080

; a comment
# another comment
[auth]
user = admin
pass =
'

fn test_ini_parse() {
	f := parse_string(sample) or { panic('must parse: ${err}') }
	assert f.sections() == ['server', 'auth']
	assert f.get('server', 'host') or { '' } == 'example.com'
	assert f.get('server', 'port') or { '' } == '8080'
	assert f.get('auth', 'pass') or { 'x' } == ''
	assert f.get_or('server', 'missing', 'dflt') == 'dflt'
	assert f.get('nosuch', 'k') == none
	assert f.get('server', 'nosuch') == none
}

fn test_ini_set_and_encode_roundtrip() {
	mut f := parse_string(sample) or { panic('must parse: ${err}') }
	f.set('server', 'host', 'other.example')
	f.set('new', 'k', 'v')
	assert f.get('server', 'host') or { '' } == 'other.example'
	assert f.sections() == ['server', 'auth', 'new']
	again := parse_string(f.encode()) or { panic('must re-parse: ${err}') }
	assert again.get('server', 'host') or { '' } == 'other.example'
	assert again.get('new', 'k') or { '' } == 'v'
	assert again.sections() == ['server', 'auth', 'new']
}

fn test_ini_errors() {
	parse_string('k = v') or {
		assert err.msg() == 'ini: line 1: key outside any section'
		return
	}
	assert false, 'keys outside sections must error'
	parse_string('[a]\n[a]') or {
		assert err.msg() == 'ini: line 2: duplicate section `a`'
		return
	}
	assert false, 'duplicate sections must error'
	parse_string('[a]\nk = 1\nk = 2') or {
		assert err.msg() == 'ini: line 3: duplicate key `k`'
		return
	}
	assert false, 'duplicate keys must error'
	parse_string('[a]\njust words') or {
		assert err.msg() == 'ini: line 2: missing `=` or `:` separator'
		return
	}
	assert false, 'separator-less lines must error'
	parse_string('[oops') or {
		assert err.msg() == 'ini: line 1: unbalanced section header'
		return
	}
	assert false, 'unbalanced headers must error'
}
