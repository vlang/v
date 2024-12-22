module models

pub type UserTags = string

pub fn (ut UserTags) to_list() []string {
	return ut.split(',')
}

pub fn (ut UserTags) add(tags ...string) UserTags {
	return ut + ',' + tags.join(',')
}

pub type Genders = string

pub fn (g Genders) to_list() []string {
	return g.split(',')
}

pub fn (g Genders) add(genders ...string) Genders {
	return g + ',' + genders.join(',')
}

@[table: 'InstantGamesScraping']
pub struct InstantGamesScraping {
pub:
	id                   int @[primary; sql: serial]
	price                f64
	price_old            ?f64
	discount             ?int
	review_general       string
	qtde_review          int
	images               []InstantGamesImage @[fkey: 'parent_id']
	title                string
	about                string
	activating_plataform string
	genders              Genders
	tags                 UserTags
	studio               string
	publisher            string
}
