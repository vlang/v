module orm_selective_import_entities

pub struct Remote {
pub mut:
	id   int @[primary; sql: serial]
	name string
}
