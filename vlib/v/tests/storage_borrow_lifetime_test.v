module main

struct Entry {
	value int
}

struct Tree {
	entries []Entry
}

struct Payload {
	pointer &int
}

struct Holder {
	inner Payload
}

fn extract(copy Payload) &int { return copy.pointer }

fn (holder &Holder) copy_payload() &int { return extract(holder.inner) }

fn payload_snapshot(value int) &int {
	local := value
	holder := Holder{ inner: Payload{ pointer: &local } }
	return holder.copy_payload()
}

fn (tree &Tree) index(node &Entry) int { return node.value }

fn (tree &Tree) child_node(node &Entry) &Entry {
	id := tree.index(node)
	return unsafe { &tree.entries[id] }
}

fn (tree &Tree) type_reference(node &Entry) &Entry {
	if node.value == 0 { return unsafe { &tree.entries[0] } }
	return unsafe { &tree.entries[1] }
}

fn (tree &Tree) copied_payload(node &Entry) &Entry {
	_ = node.value
	return tree.child_node(node)
}

fn identity(node &Entry) &Entry { return node }

fn inline_address(node &Entry) &int { return &node.value }

fn snapshot_lookup(tree &Tree, node Entry) &Entry { return tree.child_node(&node) }

fn snapshot_type(tree &Tree, node Entry) &Entry { return tree.type_reference(&node) }

fn snapshot_identity(node Entry) &Entry { return identity(&node) }

struct AuditPayload {
mut:
	pointer &int
}

struct AuditEnvelope {
mut:
	inner AuditPayload
}

struct AuditSink {
mut:
	pointer &int = unsafe { nil }
}

fn audit_extract(copy AuditPayload) &int { return copy.pointer }

fn audit_extract_optional(copy AuditPayload) ?&int { return copy.pointer }

fn audit_extract_result(copy AuditPayload) !&int { return copy.pointer }

fn audit_storage(input &AuditEnvelope) &int { return audit_extract(input.inner) }

fn audit_storage_optional(input &AuditEnvelope) ?&int { return audit_extract_optional(input.inner) }

fn audit_storage_result(input &AuditEnvelope) !&int { return audit_extract_result(input.inner) }

fn audit_storage_field(input &AuditEnvelope) &&int { return &input.inner.pointer }

fn audit_store_payload(input &AuditEnvelope, mut sink AuditSink) &int {
	sink.pointer = input.inner.pointer
	return sink.pointer
}

fn audit_direct(value int) &int {
	local := value
	payload := AuditEnvelope{ inner: AuditPayload{ pointer: &local } }
	return audit_storage(&payload)
}

fn audit_payload_assignment(value int) &int {
	local := value
	mut payload := AuditEnvelope{ inner: AuditPayload{ pointer: unsafe { nil } } }
	payload.inner.pointer = &local
	return audit_storage(&payload)
}

fn audit_optional(value int) ?&int {
	local := value
	payload := AuditEnvelope{ inner: AuditPayload{ pointer: &local } }
	return audit_storage_optional(&payload)
}

fn audit_result(value int) !&int {
	local := value
	payload := AuditEnvelope{ inner: AuditPayload{ pointer: &local } }
	return audit_storage_result(&payload)
}

fn audit_field(value int) &&int {
	local := value
	payload := AuditEnvelope{ inner: AuditPayload{ pointer: &local } }
	return audit_storage_field(&payload)
}

fn audit_mutable_store(value int, mut sink AuditSink) &int {
	local := value
	payload := AuditEnvelope{ inner: AuditPayload{ pointer: &local } }
	return audit_store_payload(&payload, mut sink)
}

struct AuditCallback {
	audit_storage fn (&AuditEnvelope) &AuditEnvelope
}

fn audit_callback_identity(input &AuditEnvelope) &AuditEnvelope { return input }

fn audit_callback_escape(value int) &AuditEnvelope {
	local := value
	payload := AuditEnvelope{ inner: AuditPayload{ pointer: &local } }
	callback := AuditCallback{ audit_storage: audit_callback_identity }
	return callback.audit_storage(&payload)
}

fn stack_churn(value int) int {
	mut values := [256]int{}
	for i in 0 .. values.len { values[i] = value + i }
	return values[127]
}

fn test_storage_borrow_keeps_escaping_payloads_alive() {
	tree := Tree{ entries: [Entry{ value: 0 }, Entry{ value: 1 }] }
	for i in 0 .. 200 {
		direct := audit_direct(i)
		optional := audit_optional(i) or { panic('missing optional') }
		result := audit_result(i) or { panic(err) }
		field := audit_field(i)
		mut sink := AuditSink{}
		stored := audit_mutable_store(i, mut sink)
		written := audit_payload_assignment(i)
		callback := audit_callback_escape(i)
		copied := payload_snapshot(i)
		identity_value := snapshot_identity(Entry{ value: i })
		assert stack_churn(i) == i + 127
		assert *direct == i
		assert *optional == i
		assert *result == i
		assert **field == i
		assert *stored == i
		assert *sink.pointer == i
		assert *written == i
		assert *callback.inner.pointer == i
		assert *copied == i
		assert identity_value.value == i
		assert snapshot_lookup(&tree, Entry{ value: 1 }).value == 1
		assert snapshot_type(&tree, Entry{ value: 0 }).value == 0
	}
}
