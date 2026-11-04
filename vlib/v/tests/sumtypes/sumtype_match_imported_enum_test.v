import match_colors as colors

type ImportedMatchColor = colors.First | colors.Second | string

fn test_match_sum_storage_keeps_imported_enum_shorthand() {
	mut values := map[int]ImportedMatchColor{}
	for k in 1 .. 3 {
		values[k] = match k {
			1 { colors.Second.red }
			else { .blue }
		}
		value := values[k] or { panic('missing imported enum') }
		match value {
			colors.Second {
				assert value == if k == 1 { colors.Second.red } else { colors.Second.blue }
			}
			else {
				assert false
			}
		}
	}
}

type ImportedColorAlias = colors.Second

type AliasedImportedMatchColor = ImportedColorAlias | colors.Second | string

fn test_match_sum_storage_keeps_imported_enum_type_alias() {
	mut values := map[int]AliasedImportedMatchColor{}
	for k in 1 .. 3 {
		values[k] = match k {
			1 { ImportedColorAlias(colors.Second.red) }
			else { colors.Second.blue }
		}
		value := values[k] or { panic('missing aliased enum') }
		match value {
			ImportedColorAlias {
				assert k == 1
			}
			colors.Second {
				assert k == 2
			}
			else {
				assert false
			}
		}
	}
}
