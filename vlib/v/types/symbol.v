module types

// SymbolId is the identity of a resolved declaration name: the id of its
// spelling in the global interner (see flat.source_type_id), shared by every
// checker, fork and worker of the process. Zero denotes no symbol.
pub type SymbolId = u32
