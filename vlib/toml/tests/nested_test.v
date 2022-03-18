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
	toml_doc := toml.parse_text(toml_text) or { panic(err) }
	// dump(toml_doc.ast)
	// assert false

	assert toml_doc.value('db.enabled').bool()
	// TODO make this work
	assert toml_doc.value('servers.alpha.ip').string() == '10.0.0.1'
	assert toml_doc.value('servers.alpha.dc').string() == 'eqdc10'

	assert toml_doc.value('servers.beta.ip').string() == '10.0.0.2'
	assert toml_doc.value('servers.beta.dc').string() == 'eqdc10'

	assert toml_doc.value('servers.alpha.tricky.ip').string() == '10.0.0.100'
	assert toml_doc.value('firewall.rules.limit.ip').string() == '10.0.0.101'
	assert toml_doc.value('firewall.rules.block').bool() == true
}
