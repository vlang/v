import maps
import x.json2

pub type Locale = string

pub struct ApplicationCommandOptionChoice {
pub:
	name               string
	name_localizations ?map[Locale]string
}

pub fn ApplicationCommandOptionChoice.parse(j json2.Any) !ApplicationCommandOptionChoice {
	match j {
		map[string]json2.Any {
			return ApplicationCommandOptionChoice{
				name:               j['name']! as string
				name_localizations: if m := j['name_localizations'] {
					maps.to_map[string, json2.Any, Locale, string](m as map[string]json2.Any,
						fn (k string, v json2.Any) (Locale, string) {
						return k, v as string
					})
				} else {
					none
				}
			}
		}
		else {
			return error('expected application command option choice to be object, got ${j.type_name()}')
		}
	}
}

fn test_main() {
	var := ApplicationCommandOptionChoice.parse({
		'name':               json2.Any('foo')
		'name_localizations': {
			'name': json2.Any('foo')
		}
	})!
	assert dump(var) == var
}
