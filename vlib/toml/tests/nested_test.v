import toml

const toml_text = '
[db]
enabled = true

[servers]
  # Indentation (tabs and/or spaces) is allowed but not required
  [servers.alpha]
  ip = "10.0.0.1"
  dc = "eqdc10"

  [servers.beta]
  ip = "10.0.0.2"
  dc = "eqdc10"

  [servers.alpha.tricky]
  ip = "10.0.0.100"

[firewall.rules.limit]
	ip = "10.0.0.101"

	[firewall.rules]
	block = true
'

fn test_parse() {
	toml_doc := toml.parse(toml_text) or { panic(err) }
	// dump(toml_doc.ast)
	// assert false

	assert toml_doc.value('db.enabled') or { false }.bool() == true
	// TODO make this work
	assert toml_doc.value('servers.alpha.ip') or { '' }.string() == '10.0.0.1'
	assert toml_doc.value('servers.alpha.dc') or { '' }.string() == 'eqdc10'

	assert toml_doc.value('servers.beta.ip') or { '' }.string() == '10.0.0.2'
	assert toml_doc.value('servers.beta.dc') or { '' }.string() == 'eqdc10'

	assert toml_doc.value('servers.alpha.tricky.ip') or { '' }.string() == '10.0.0.100'
	assert toml_doc.value('firewall.rules.limit.ip') or { '' }.string() == '10.0.0.101'
	assert toml_doc.value('firewall.rules.block') or { false }.bool() == true
}
