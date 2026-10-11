module ssa

import v.flat

const pending_loop_label_marker = '__v_pending_loop_label:'

struct LoopTargets {
	label           string
	break_target    BlockID
	continue_target BlockID
	has_body_post   bool
}

fn (mut b Builder) take_pending_loop_label() string {
	label := b.pending_loop_label
	b.pending_loop_label = ''
	return label
}

fn (mut b Builder) push_loop_targets(label string, node flat.Node, body_start int, break_target BlockID, continue_target BlockID) {
	mut target := continue_target
	mut has_body_post := false
	if label.len > 0 {
		for i in body_start .. node.children_count {
			child := b.a.child_node(&node, i)
			if child.kind == .label_stmt && child.value == '${label}_continue' {
				// The transformer can move the post step into the body. Continue
				// must reach that step before returning to the loop condition.
				target = b.m.add_block(b.cur_func, 'loop_continue_${label}')
				has_body_post = true
				break
			}
		}
	}
	b.loop_targets << LoopTargets{
		label:           label
		break_target:    break_target
		continue_target: target
		has_body_post:   has_body_post
	}
}

fn (mut b Builder) build_loop_control(label string, is_continue bool) {
	for i := b.loop_targets.len - 1; i >= 0; i-- {
		loop := b.loop_targets[i]
		if label.len == 0 || loop.label == label {
			target := if is_continue { loop.continue_target } else { loop.break_target }
			b.emit1(.jmp, b.void_type, ValueID(target))
			return
		}
	}
}

fn (mut b Builder) build_loop_label(name string) bool {
	if name.starts_with(pending_loop_label_marker) {
		mut label := name
		for label.starts_with(pending_loop_label_marker) {
			label = label[pending_loop_label_marker.len..]
		}
		b.pending_loop_label = label
		return true
	}
	if b.loop_targets.len > 0 {
		loop := b.loop_targets.last()
		if loop.has_body_post && name == '${loop.label}_continue' {
			if !b.current_block_terminated() {
				b.emit1(.jmp, b.void_type, ValueID(loop.continue_target))
			}
			b.cur_block = loop.continue_target
			return true
		}
	}
	return false
}
