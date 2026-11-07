import builtin.closure

interface RetainedAnimation {
	id string
mut:
	stopped bool
}

struct RetainedCallback implements RetainedAnimation {
	id       string
	callback fn (mut RetainedCallback, mut AnimationOwner) @[required]
mut:
	stopped bool
}

struct AnimationOwner {
mut:
	animations map[string]RetainedAnimation
	lifetime   closure.Lifetime
	value      int
}

fn (mut owner AnimationOwner) add(mut animation RetainedAnimation) {
	owner.animations[animation.id] = animation
}

enum RetainedArgumentKind {
	constructor
	returned_value
	local
	local_fn_value
	local_generic
}

fn returned_callback(seed int) RetainedCallback {
	return RetainedCallback{
		id:       'callback'
		callback: fn [seed] (mut animation RetainedCallback, mut owner AnimationOwner) {
			owner.value = seed
			animation.stopped = true
		}
	}
}

fn retain_callback(mut owner AnimationOwner, mut animation RetainedAnimation) {
	owner.add(mut animation)
}

fn retain_callback_with_tag[T](mut owner AnimationOwner, mut animation RetainedAnimation, tag T) {
	assert tag == 37
	owner.add(mut animation)
}

fn check_animation_aliases(mut owner AnimationOwner, mut animation RetainedCallback) {
	mut retained := owner.animations['callback'] or { panic('missing animation') }
	retained.stopped = true
	assert animation.stopped
	animation.stopped = false
	assert !retained.stopped
}

fn add_retained_callback(mut owner AnimationOwner, kind RetainedArgumentKind) {
	owner.lifetime.frame(fn [mut owner, kind] () {
		owner.lifetime.suspend(fn [mut owner, kind] () {
			seed := 37
			if kind == .local_fn_value {
				mut callback_argument := returned_callback(seed)
				retain := retain_callback
				retain(mut owner, mut callback_argument)
				check_animation_aliases(mut owner, mut callback_argument)
			} else if kind == .local_generic {
				mut generic_argument := returned_callback(seed)
				retain_callback_with_tag(mut owner, mut generic_argument, seed)
				check_animation_aliases(mut owner, mut generic_argument)
			} else if kind == .local {
				mut animation := RetainedCallback{
					id:       'callback'
					callback: fn [seed] (mut animation RetainedCallback, mut owner AnimationOwner) {
						owner.value = seed
						animation.stopped = true
					}
				}
				owner.add(mut animation)
				check_animation_aliases(mut owner, mut animation)
			} else if kind == .returned_value {
				owner.add(mut returned_callback(seed))
			} else {
				owner.add(mut RetainedCallback{
					id:       'callback'
					callback: fn [seed] (mut animation RetainedCallback, mut owner AnimationOwner) {
						owner.value = seed
						animation.stopped = true
					}
				})
			}
		}) or { panic(err) }
	}) or { panic(err) }
}

@[noinline]
fn overwrite_animation_stack(seed int) int {
	mut values := [256]int{}
	for i in 0 .. values.len {
		values[i] = seed + i
	}
	return values[127]
}

fn check_retained_callback(kind RetainedArgumentKind) {
	mut owner := AnimationOwner{
		lifetime: closure.new_lifetime()
	}
	add_retained_callback(mut owner, kind)
	owner.lifetime.frame(fn () {}) or { panic(err) }
	owner.lifetime.reclaim(1) or { panic(err) }
	gc_collect()
	assert overwrite_animation_stack(11) == 138
	mut animation := owner.animations['callback'] or { panic('missing animation') }
	match mut animation {
		RetainedCallback {
			animation.callback(mut animation, mut owner)
		}
		else {
			assert false, 'unexpected animation type'
		}
	}
	assert owner.value == 37
	assert animation.stopped
	owner.animations.clear()
	owner.lifetime.dispose() or { panic(err) }
}

fn test_mutable_interface_retains_constructor_payload() {
	check_retained_callback(.constructor)
}

fn test_mutable_interface_retains_original_local_payload() {
	check_retained_callback(.local)
}

fn test_mutable_interface_retains_returned_payload() {
	check_retained_callback(.returned_value)
}

fn test_mutable_interface_fn_value_retains_original_local_payload() {
	check_retained_callback(.local_fn_value)
}

fn test_mutable_interface_generic_call_retains_original_local_payload() {
	check_retained_callback(.local_generic)
}
