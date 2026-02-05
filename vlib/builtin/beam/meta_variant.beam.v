module builtin

// VariantData holds information about a sum type variant
// Used by compile-time reflection ($for variant in SumType.variants)
pub struct VariantData {
pub:
	typ int
}
