import x.json2

pub fn maybe_map[T, X](a []T, f fn (T) !X) ![]X {
	mut r := []X{cap: a.len}
	for v in a {
		r << f(v)!
	}
	return r
}

pub enum ApplicationCommandOptionType {
	sub_command = 1
	sub_command_group
	string
	integer
	boolean
	user
	channel
	role
	mentionable
	number
	attachment
}

pub struct ApplicationCommandOption {
pub:
	typ         ApplicationCommandOptionType @[required]
	name        string                       @[required]
	description string                       @[required]
	options     ?[]ApplicationCommandOption
}

pub fn ApplicationCommandOption.parse(j json2.Any) !ApplicationCommandOption {
	match j {
		map[string]json2.Any {
			return ApplicationCommandOption{
				typ:         unsafe { ApplicationCommandOptionType(j['type']!.int()) }
				name:        j['name']! as string
				description: j['description']! as string
				options:     if a := j['options'] {
					maybe_map(a as []json2.Any, fn (k json2.Any) !ApplicationCommandOption {
						return ApplicationCommandOption.parse(k)!
					})!
				} else {
					none
				}
			}
		}
		else {
			return error('expected application command option to be object, got ${j.type_name()}')
		}
	}
}

fn test_main() {
	assert ApplicationCommandOption.parse({
		'type':        json2.Any(3)
		'name':        'foo'
		'description': 'This is my first command.'
	}) or {
		assert false, 'Should not return error: ${err}'
		return
	} == ApplicationCommandOption{
		typ:         .string
		name:        'foo'
		description: 'This is my first command.'
	}
}
