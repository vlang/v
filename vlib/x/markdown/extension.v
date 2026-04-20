// Copyright 2026 The V Language. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module markdown

// Extension is the interface implemented by markdown extensions.
// An extension configures the Markdown processor by enabling parser and
// renderer features.
pub interface Extension {
	// extend is called once when the extension is registered with a Markdown processor.
	extend(mut m Markdown)
}

// TableExt adds GitHub Flavored Markdown table support (| col | col |).
pub struct TableExt {}

// extend implements Extension for TableExt.
pub fn (_ TableExt) extend(mut m Markdown) {
	m.opts.tables = true
}

// StrikethroughExt adds GFM strikethrough support (~~text~~).
pub struct StrikethroughExt {}

// extend implements Extension for StrikethroughExt.
pub fn (_ StrikethroughExt) extend(mut m Markdown) {
	m.opts.strikethrough = true
}

// LinkifyExt adds autolink support for bare URLs and email addresses.
pub struct LinkifyExt {}

// extend implements Extension for LinkifyExt.
pub fn (_ LinkifyExt) extend(mut m Markdown) {
	m.opts.linkify = true
}

// TaskListExt adds GFM task list item support (- [ ] / - [x]).
pub struct TaskListExt {}

// extend implements Extension for TaskListExt.
pub fn (_ TaskListExt) extend(mut m Markdown) {
	m.opts.task_list = true
}

// FootnoteExt adds footnote support ([^label] references and [^label]: definitions).
pub struct FootnoteExt {}

// extend implements Extension for FootnoteExt.
pub fn (_ FootnoteExt) extend(mut m Markdown) {
	m.opts.footnotes = true
}

// TypographerExt replaces ASCII punctuation sequences with Unicode typographic
// equivalents: -- en dash, --- em dash, ... ellipsis, and smart quotes.
pub struct TypographerExt {}

// extend implements Extension for TypographerExt.
pub fn (_ TypographerExt) extend(mut m Markdown) {
	m.opts.typographer = true
}

// DefinitionListExt adds Pandoc-style definition list support.
pub struct DefinitionListExt {}

// extend implements Extension for DefinitionListExt.
pub fn (_ DefinitionListExt) extend(mut m Markdown) {
	m.opts.definition_list = true
}

// table returns a TableExt extension value.
pub fn table() TableExt {
	return TableExt{}
}

// strikethrough returns a StrikethroughExt extension value.
pub fn strikethrough() StrikethroughExt {
	return StrikethroughExt{}
}

// linkify returns a LinkifyExt extension value.
pub fn linkify() LinkifyExt {
	return LinkifyExt{}
}

// task_list returns a TaskListExt extension value.
pub fn task_list() TaskListExt {
	return TaskListExt{}
}

// footnote returns a FootnoteExt extension value.
pub fn footnote() FootnoteExt {
	return FootnoteExt{}
}

// typographer returns a TypographerExt extension value.
pub fn typographer() TypographerExt {
	return TypographerExt{}
}

// definition_list returns a DefinitionListExt extension value.
pub fn definition_list() DefinitionListExt {
	return DefinitionListExt{}
}

// gfm returns the core GitHub Flavored Markdown extensions:
// TableExt, StrikethroughExt, LinkifyExt, and TaskListExt.
pub fn gfm() []Extension {
	return [
		Extension(TableExt{}),
		StrikethroughExt{},
		LinkifyExt{},
		TaskListExt{},
	]
}
