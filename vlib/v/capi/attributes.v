module capi

// CApiSkip excludes a public function from the generated C surface.
@[attribute]
pub struct CApiSkip {}

// CApiName overrides the generated C symbol name for a function.
@[attribute]
pub struct CApiName {
pub:
	name string
}

// CApiExport includes a private function in the generated C surface.
@[attribute]
pub struct CApiExport {}
