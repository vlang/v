struct RequiredBase {
	a int @[required]
}

struct RequiredDerived {
	RequiredBase
}

struct EmbeddedInner {}

struct EmbeddedRefBase {
pub:
	inner &EmbeddedInner
}

struct EmbeddedRefDerived {
	EmbeddedRefBase
}

fn test_embedded_literal_initializes_required_field() {
	d := RequiredDerived{
		RequiredBase: RequiredBase{
			a: 1
		}
	}
	assert d.a == 1
}

fn test_embedded_value_initializes_required_field() {
	base := RequiredBase{
		a: 2
	}
	d := RequiredDerived{
		RequiredBase: base
	}
	assert d.a == 2
}

fn test_promoted_field_initializes_required_field() {
	d := RequiredDerived{
		a: 3
	}
	assert d.a == 3
}

fn test_embedded_literal_initializes_reference_field() {
	inner := &EmbeddedInner{}
	d := EmbeddedRefDerived{
		EmbeddedRefBase: EmbeddedRefBase{
			inner: inner
		}
	}
	assert d.inner == inner
}
