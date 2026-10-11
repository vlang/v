module vmod

import strings

fn quote(input string) string {
	if input.contains("'") {
		return '"' + input + '"'
	}
	return "'" + input + "'"
}

fn encode_array(mut b strings.Builder, input []string) {
	if input.join('').len > 60 {
		b.writeln('[')
		for item in input {
			b.write_string('\t\t')
			b.write_string(quote(item))
			b.writeln(',')
		}
		b.writeln('\t]')
	} else {
		mut quoted := []string{}
		for item in input {
			quoted << quote(item)
		}
		b.write_string('[')
		b.write_string(quoted.join(', '))
		b.writeln(']')
	}
}

// encode serializes manifest metadata, including catalog and workspaces.
pub fn encode(manifest Manifest) string {
	mut b := strings.new_builder(512)
	b.writeln('Module {')
	b.write_string('\tname: ')
	b.writeln(quote(manifest.name))
	if manifest.base_url != '' {
		b.write_string('\tbase_url: ')
		b.writeln(quote(manifest.base_url))
	}
	b.write_string('\tdescription: ')
	b.writeln(quote(manifest.description))
	b.write_string('\tversion: ')
	b.writeln(quote(manifest.version))
	b.write_string('\tlicense: ')
	b.writeln(quote(manifest.license))
	if manifest.repo_url != '' {
		b.write_string('\trepo_url: ')
		b.writeln(quote(manifest.repo_url))
	}
	if manifest.author != '' {
		b.write_string('\tauthor: ')
		b.writeln(quote(manifest.author))
	}
	b.write_string('\tdependencies: ')
	encode_array(mut b, manifest.dependencies)
	if manifest.catalog.len > 0 {
		b.writeln('\tcatalog: {')
		for key in manifest.catalog.keys().sorted() {
			b.writeln('\t\t' + quote(key) + ': ' + quote(manifest.catalog[key]))
		}
		b.writeln('\t}')
	}
	if manifest.workspaces.len > 0 {
		b.write_string('\tworkspaces: ')
		encode_array(mut b, manifest.workspaces)
	}
	for key, values in manifest.unknown {
		b.write_string('\t')
		b.write_string(key)
		b.write_string(': ')
		encode_array(mut b, values)
	}
	b.write_string('}')
	return b.str()
}
