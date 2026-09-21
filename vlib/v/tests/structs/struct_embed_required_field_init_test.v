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

struct ShadowInner {}

struct ShadowBase {
	n int @[required]
	p &ShadowInner
}

struct ShadowMiddle {
	ShadowBase
}

struct ShadowOuter {
	ShadowBase
	ShadowMiddle
}

fn test_same_named_embeds_keep_distinct_initializers() {
	inner := &ShadowInner{}
	d := ShadowOuter{
		ShadowBase:   ShadowBase{
			n: 1
			p: inner
		}
		ShadowMiddle: ShadowMiddle{
			ShadowBase: ShadowBase{
				n: 2
				p: inner
			}
		}
	}
	assert d.ShadowBase.n == 1
	assert d.ShadowMiddle.n == 2
}
