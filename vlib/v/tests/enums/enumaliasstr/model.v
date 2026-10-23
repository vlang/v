module enumaliasstr

pub enum Kind {
	one
}

pub type MethodAlias = Kind

// str returns the alias-specific representation.
pub fn (value MethodAlias) str() string {
	return 'custom imported alias'
}

pub type AliasChain = MethodAlias
pub type AliasDeep = AliasChain

pub struct Holder {
pub:
	value AliasDeep
}
