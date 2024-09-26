import x.json2

pub struct PartialEmoji {
pub:
	id       ?int
	name     string
	animated bool
}

pub fn PartialEmoji.parse(j json2.Any) !PartialEmoji {
	match j {
		map[string]json2.Any {
			return PartialEmoji{
				id:       if s := j['id'] {
					if s !is json2.Null {
						?int(s.int())
					} else {
						none
					}
				} else {
					none
				}
				name:     j['name']! as string
				animated: if b := j['animated'] {
					b as bool
				} else {
					false
				}
			}
		}
		else {
			return error('expected partial emoji to be object, got ${j.type_name()}')
		}
	}
}

pub enum ButtonStyle {
	primary = 1
	secondary
	success
	danger
	link
}

pub struct Button {
pub:
	style     ButtonStyle = .secondary
	label     ?string
	emoji     ?PartialEmoji
	custom_id ?string
	url       ?string
	disabled  ?bool
}

pub fn Button.parse(j json2.Any) !Button {
	match j {
		map[string]json2.Any {
			return Button{
				style:     unsafe { ButtonStyle(j['style']!.int()) }
				label:     if s := j['label'] {
					?string(s as string)
				} else {
					none
				}
				emoji:     if o := j['emoji'] {
					dump(PartialEmoji.parse(o)!)
					?PartialEmoji(PartialEmoji.parse(o)!)
				} else {
					none
				}
				custom_id: if s := j['custom_id'] {
					?string(s as string)
				} else {
					none
				}
				url:       if s := j['url'] {
					?string(s as string)
				} else {
					none
				}
				disabled:  if b := j['disabled'] {
					?bool(b as bool)
				} else {
					none
				}
			}
		}
		else {
			return error('expected button to be object, got ${j.type_name()}')
		}
	}
}

fn test_main() {
	assert Button.parse({
		'style': json2.Any(2)
	}) or { panic('That case should not return error: ${err}') } == Button{
		style: .secondary
	}
}
