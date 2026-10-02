struct OwnerReviewHost {
	offset int
}

struct OwnerReviewPadding {
	marker int
}

fn (h OwnerReviewHost) first[T](xs []T) T {
	return xs[0]
}

fn (h OwnerReviewHost) label[T](x T) string {
	return '${h.offset}:${x}'
}

struct OwnerReviewWrapper {
	OwnerReviewPadding
	OwnerReviewHost
	padding int
}

struct OwnerReviewDeep {
	OwnerReviewWrapper
	padding int
}

struct OwnerReviewBox[T] {
	item T
}

fn (b OwnerReviewBox[T]) paired[U](u U) string {
	return '${b.item}/${u}'
}

struct OwnerReviewGenericWrapper[T] {
	OwnerReviewBox[T]
}

fn test_promoted_generic_method_values_bind_the_declaring_embedded_receiver() {
	direct := OwnerReviewHost{ offset: 3 }
	direct_value := direct.label[string]
	assert direct_value('direct') == '3:direct'
	wrapper := OwnerReviewWrapper{
		padding:            99
		OwnerReviewPadding: OwnerReviewPadding{ marker: 91 }
		OwnerReviewHost:    OwnerReviewHost{ offset: 7 }
	}
	first := wrapper.first[int]
	assert first([42]) == 42
	assert wrapper.first[int]([43]) == 43
	label := wrapper.label[string]
	assert label('value') == '7:value'
	deep := OwnerReviewDeep{
		padding:            123
		OwnerReviewWrapper: wrapper
	}
	deep_label := deep.label[int]
	assert deep_label(42) == '7:42'
}

fn test_promoted_generic_receiver_parameters_are_fixed_by_the_embedded_instance() {
	direct := OwnerReviewBox[string]{ item: 'direct' }
	direct_value := direct.paired[int]
	assert direct_value(41) == 'direct/41'
	wrapper := OwnerReviewGenericWrapper[string]{
		OwnerReviewBox: OwnerReviewBox[string]{ item: 'bound' }
	}
	value := wrapper.paired[int]
	assert value(42) == 'bound/42'
}
