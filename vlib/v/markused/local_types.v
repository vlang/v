module markused

import v.flat

fn (c &CallCollector) new_local_types() flat.TypeTextBindings {
	return flat.TypeTextBindings{ texts: c.local_type_texts }
}

fn (c &CallCollector) new_ident_types() flat.NodeTypeTextBindings {
	return flat.NodeTypeTextBindings{ texts: c.local_type_texts }
}

fn (c &CallCollector) source_type_text_id(text string) u32 {
	mut table := c.local_type_texts
	return table.intern(text)
}
