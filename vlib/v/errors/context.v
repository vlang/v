module errors

import v.token
import strings

// ErrorContext provides additional context for error messages,
// including details, suggestions, and related information.
pub struct ErrorContext {
mut:
	details      []string
	suggestions  []string
	related_info []string
	call_stack   []CallStackItem
}

// new_context creates a new ErrorContext instance
pub fn new_context() &ErrorContext {
	return &ErrorContext{
		details:      []string{}
		suggestions:  []string{}
		related_info: []string{}
		call_stack:   []CallStackItem{}
	}
}

// add_detail adds a detail message to the error context
pub fn (mut c ErrorContext) add_detail(detail string) {
	c.details << detail
}

// add_suggestion adds a suggestion message to the error context
pub fn (mut c ErrorContext) add_suggestion(suggestion string) {
	c.suggestions << suggestion
}

// add_related adds related information to the error context
pub fn (mut c ErrorContext) add_related(info string) {
	c.related_info << info
}

// add_call_stack_item adds a call stack item to the error context
pub fn (mut c ErrorContext) add_call_stack_item(item CallStackItem) {
	c.call_stack << item
}

// format formats the error context into a single string
pub fn (c &ErrorContext) format() string {
	mut parts := []string{}

	if c.details.len > 0 {
		parts << c.format_details()
	}

	if c.suggestions.len > 0 {
		parts << c.format_suggestions()
	}

	if c.related_info.len > 0 {
		parts << c.format_related()
	}

	return parts.join('\n')
}

// format_details formats the details section
fn (c &ErrorContext) format_details() string {
	return 'Details: ${c.details.join('\n         ')}'
}

// format_suggestions formats the suggestions section
fn (c &ErrorContext) format_suggestions() string {
	return 'Suggestion: ${c.suggestions.join('\n             ')}'
}

// format_related formats the related information section
fn (c &ErrorContext) format_related() string {
	return 'Related: ${c.related_info.join('\n         ')}'
}

// has_details returns true if the context has any details
pub fn (c &ErrorContext) has_details() bool {
	return c.details.len > 0
}

// has_suggestions returns true if the context has any suggestions
pub fn (c &ErrorContext) has_suggestions() bool {
	return c.suggestions.len > 0
}

// has_related returns true if the context has any related information
pub fn (c &ErrorContext) has_related() bool {
	return c.related_info.len > 0
}

// has_call_stack returns true if the context has a call stack
pub fn (c &ErrorContext) has_call_stack() bool {
	return c.call_stack.len > 0
}

// clear clears all context information
pub fn (mut c ErrorContext) clear() {
	c.details = []
	c.suggestions = []
	c.related_info = []
	c.call_stack = []
}

// clone creates a deep copy of the error context
pub fn (c &ErrorContext) clone() ErrorContext {
	return ErrorContext{
		details:      c.details.clone()
		suggestions:  c.suggestions.clone()
		related_info: c.related_info.clone()
		call_stack:   c.call_stack.clone()
	}
}

// CallStackManager manages a call stack for error reporting
pub struct CallStackManager {
mut:
	stack []CallStackItem
}

// new_call_stack_manager creates a new CallStackManager instance
pub fn new_call_stack_manager() &CallStackManager {
	return &CallStackManager{
		stack: []CallStackItem{}
	}
}

// push pushes a new call stack item onto the stack
pub fn (mut c CallStackManager) push(file_path string, pos token.Pos) {
	c.stack << CallStackItem{
		file_path: file_path
		pos:       pos
	}
}

// pop removes the most recent call stack item from the stack
pub fn (mut c CallStackManager) pop() {
	if c.stack.len > 0 {
		c.stack = c.stack[..c.stack.len - 1]
	}
}

// current returns the current call stack
pub fn (c &CallStackManager) current() []CallStackItem {
	return c.stack.clone()
}

// size returns the current size of the call stack
pub fn (c &CallStackManager) size() int {
	return c.stack.len
}

// clear clears the entire call stack
pub fn (mut c CallStackManager) clear() {
	c.stack = []
}

// format_call_stack formats the call stack into a readable string
pub fn (c &CallStackManager) format_call_stack() string {
	if c.stack.len == 0 {
		return ''
	}

	mut builder := strings.new_builder(1024)
	for i, item in c.stack {
		builder.write_string('${i + 1}. ${item.file_path}:${item.pos.line_nr + 1}:${item.pos.col + 1}')
		if i < c.stack.len - 1 {
			builder.write_string('\n')
		}
	}

	return builder.str()
}
