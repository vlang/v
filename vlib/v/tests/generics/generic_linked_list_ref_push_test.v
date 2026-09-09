import datatypes

interface PoolView {
	uid       u64
	view_type PoolViewType
mut:
	content []PoolView
}

enum PoolViewType {
	container
	text
}

struct PoolSpan {
	text string
}

struct PoolContainerView implements PoolView {
	uid       u64
	view_type PoolViewType = .container
mut:
	content []PoolView
}

struct PoolTextView implements PoolView {
	uid       u64
	view_type PoolViewType = .text
mut:
	content []PoolView
}

fn (mut cv PoolContainerView) reset_fields() {
	cv.content = []PoolView{}
}

fn (mut tv PoolTextView) reset_fields() {
	tv.content = []PoolView{}
}

struct PoolViewPool {
mut:
	spans      datatypes.LinkedList[PoolSpan]           = datatypes.LinkedList[PoolSpan]{}
	containers datatypes.LinkedList[&PoolContainerView] = datatypes.LinkedList[&PoolContainerView]{}
	texts      datatypes.LinkedList[&PoolTextView]      = datatypes.LinkedList[&PoolTextView]{}
	in_use     map[u64]bool
}

fn (mut vp PoolViewPool) seed_spans() {
	vp.spans.push(PoolSpan{'span'})
}

fn (mut vp PoolViewPool) reclaim_container_view(mut cv PoolContainerView) {
	if cv.uid !in vp.in_use {
		return
	}
	cv.reset_fields()
	vp.in_use.delete(cv.uid)
	vp.containers.push(cv)
}

fn (mut vp PoolViewPool) reclaim_text_view(mut tv PoolTextView) {
	if tv.uid !in vp.in_use {
		return
	}
	tv.reset_fields()
	vp.in_use.delete(tv.uid)
	vp.texts.push(tv)
}

fn test_generic_linked_list_ref_push_with_mut_params() {
	mut vp := PoolViewPool{}
	vp.seed_spans()

	mut cv := PoolContainerView{
		uid: 1
	}
	vp.in_use[cv.uid] = true
	vp.reclaim_container_view(mut cv)

	mut tv := PoolTextView{
		uid: 2
	}
	vp.in_use[tv.uid] = true
	vp.reclaim_text_view(mut tv)

	assert vp.spans.len() == 1
	assert vp.containers.len() == 1
	assert vp.texts.len() == 1
}
