module models

pub enum TypeInstantGamesImage {
	full
	thumbnail
}

@[table: 'InstantGamesImages']
pub struct InstantGamesImage {
pub mut:
	id         int @[primary; sql: serial]
	parent_id  int
	image_url  string
	type_image TypeInstantGamesImage
}
