struct Papa {
	fam_name string
}

pub struct Child {
	Papa
pub mut:
	activity Activity = Fun.roll
	age      u8       = 2
}

type Activity = Fun | Other

pub enum Fun {
	run
	roll
	jump
}

pub struct Other {}

// Same struct without embedding just works.
pub struct Human {
	fam_name string
pub mut:
	activity Activity = Fun.roll
	age      u8       = 2
}

fn test_embed_struct_field_default_value() {
	c := Child{}
	println(c.activity)
	assert c.activity == Activity(Fun.roll)
	h := Human{}
	println(h.activity)
	assert h.activity == Activity(Fun.roll)
}
