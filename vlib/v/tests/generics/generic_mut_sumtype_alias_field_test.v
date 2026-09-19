type GenericMutSum = int | string

type GenericMutSumAlias = GenericMutSum

struct GenericMutSumHolder {
mut:
	value GenericMutSumAlias
}

fn set_generic_mut_sum[T](mut value T) {
	$if T.unaliased_typ is $sumtype {
		value = T(42)
	}
}

fn set_generic_mut_sum_field[T](mut value T) {
	$for field in T.fields {
		set_generic_mut_sum(mut value.$(field.name))
	}
}

fn test_generic_mut_sumtype_alias_field() {
	mut holder := GenericMutSumHolder{
		value: GenericMutSumAlias('before')
	}
	set_generic_mut_sum_field(mut holder)
	assert holder.value == GenericMutSumAlias(42)
}
