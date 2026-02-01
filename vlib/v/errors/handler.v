module errors

import v.token
import v.pref

// ErrorHandler is the interface that all error handlers must implement
pub interface ErrorHandler {
	// Core reporting methods
	report(message CompilerMessage, kind MessageKind)

	// Context management
	add_detail(detail string)
	add_suggestion(suggestion string)
	add_related(info string)

	// Query methods
	should_abort() bool
	error_count() int
	warning_count() int
	notice_count() int
	has_errors() bool
	has_warnings() bool
	has_notices() bool

	// Get methods
	get_errors() []ErrorMessage
	get_warnings() []WarningMessage
	get_notices() []NoticeMessage

	// Reset
	reset()
}

// DefaultErrorHandler provides a default implementation of ErrorHandler
pub struct DefaultErrorHandler {
pub mut:
	errors       []ErrorMessage
	warnings     []WarningMessage
	notices      []NoticeMessage
	context      &ErrorContext
	pref         &pref.Preferences
	deduplicator &ErrorDeduplicator
	call_stack   []CallStackItem
	should_abort bool
	reporter     Reporter // default reporter when message.reporter is .unknown
}

// new_error_handler creates a new DefaultErrorHandler instance
pub fn new_error_handler(pref_ &pref.Preferences, reporter Reporter) &DefaultErrorHandler {
	if reporter == .unknown {
		print_backtrace()
	}
	return &DefaultErrorHandler{
		errors:       []ErrorMessage{}
		warnings:     []WarningMessage{}
		notices:      []NoticeMessage{}
		context:      new_context()
		pref:         pref_
		deduplicator: new_deduplicator()
		call_stack:   []CallStackItem{}
		should_abort: false
		reporter:     reporter
	}
}

// report is the main method for reporting errors, warnings, and notices
pub fn (mut h DefaultErrorHandler) report(message CompilerMessage, kind MessageKind) {
	// 1. Convert message kind based on preferences
	actual_kind := h.convert_message_kind(kind)

	// 2. Skip warnings if skip_warnings is enabled
	if actual_kind == .warning && h.pref.skip_warnings {
		return
	}

	// Skip notices if skip_notes is enabled
	if actual_kind == .notice && h.pref.skip_notes {
		return
	}

	// 3. Check deduplication
	if !h.deduplicator.should_report(actual_kind, message.file_path, message.pos, message.message) {
		return
	}

	// 4. Check message limit
	if h.pref.message_limit >= 0 {
		if h.error_count() + h.warning_count() + h.notice_count() >= h.pref.message_limit {
			h.should_abort = true
			return
		}
	}

	// 5. Apply context details to message
	details := h.format_details()
	call_stack := if message.call_stack.len > 0 {
		message.call_stack
	} else {
		h.call_stack.clone()
	}

	// 5. reporter
	if message.reporter == .unknown {
		unsafe {
			message.reporter = h.reporter
		}
	}

	// 6. Store the message
	match actual_kind {
		.error {
			h.errors << ErrorMessage{
				message:    message.message
				file_path:  message.file_path
				pos:        message.pos
				reporter:   message.reporter
				details:    details
				call_stack: call_stack
			}
			// If fatal errors mode is enabled, display and exit immediately
			if h.pref.fatal_errors {
				show_compiler_message('error:', message)
				h.should_abort = true
			}
		}
		.warning {
			h.warnings << WarningMessage{
				message:    message.message
				file_path:  message.file_path
				pos:        message.pos
				reporter:   message.reporter
				details:    details
				call_stack: call_stack
			}
		}
		.notice {
			if !h.pref.skip_notes {
				h.notices << NoticeMessage{
					message:    message.message
					file_path:  message.file_path
					pos:        message.pos
					reporter:   message.reporter
					details:    details
					call_stack: call_stack
				}
			}
		}
	}

	// 6. Display the message (if not in fatal mode and not in silent mode)
	if !h.pref.fatal_errors && h.pref.output_mode != .silent {
		kind_str := kind_to_string(actual_kind)
		show_compiler_message(kind_str, message)
	}
}

// convert_message_kind converts a message kind based on preferences
fn (h &DefaultErrorHandler) convert_message_kind(kind MessageKind) MessageKind {
	match kind {
		.error {
			return .error
		}
		.warning {
			// Skip warnings if skip_warnings is enabled
			if h.pref.skip_warnings {
				return .warning
			}
			// Convert warnings to errors in prod mode or if warns_are_errors is set
			if h.pref.warns_are_errors || h.pref.is_prod {
				return .error
			}
		}
		.notice {
			if h.pref.notes_are_errors {
				return .error
			}
			if h.pref.skip_notes {
				return .notice // Will be filtered out in report()
			}
		}
	}
	return kind
}

// format_details formats the accumulated details from the context
fn (h &DefaultErrorHandler) format_details() string {
	if h.context.has_details() {
		return h.context.format_details()
	}
	return ''
}

// add_detail adds a detail message to the current error context
pub fn (mut h DefaultErrorHandler) add_detail(detail string) {
	h.context.add_detail(detail)
}

// add_suggestion adds a suggestion message to the current error context
pub fn (mut h DefaultErrorHandler) add_suggestion(suggestion string) {
	h.context.add_suggestion(suggestion)
}

// add_related adds related information to the current error context
pub fn (mut h DefaultErrorHandler) add_related(info string) {
	h.context.add_related(info)
}

// push_call_stack pushes a call stack item
pub fn (mut h DefaultErrorHandler) push_call_stack(file_path string, pos token.Pos) {
	h.call_stack << CallStackItem{
		file_path: file_path
		pos:       pos
	}
}

// pop_call_stack pops the most recent call stack item
pub fn (mut h DefaultErrorHandler) pop_call_stack() {
	if h.call_stack.len > 0 {
		h.call_stack = h.call_stack[..h.call_stack.len - 1]
	}
}

// should_abort returns true if error processing should abort
pub fn (h &DefaultErrorHandler) should_abort() bool {
	return h.should_abort
}

// error_count returns the number of errors reported
pub fn (h &DefaultErrorHandler) error_count() int {
	return h.errors.len
}

// warning_count returns the number of warnings reported
pub fn (h &DefaultErrorHandler) warning_count() int {
	return h.warnings.len
}

// notice_count returns the number of notices reported
pub fn (h &DefaultErrorHandler) notice_count() int {
	return h.notices.len
}

// has_errors returns true if any errors have been reported
pub fn (h &DefaultErrorHandler) has_errors() bool {
	return h.errors.len > 0
}

// has_warnings returns true if any warnings have been reported
pub fn (h &DefaultErrorHandler) has_warnings() bool {
	return h.warnings.len > 0
}

// has_notices returns true if any notices have been reported
pub fn (h &DefaultErrorHandler) has_notices() bool {
	return h.notices.len > 0
}

// get_errors returns all errors
pub fn (h &DefaultErrorHandler) get_errors() []ErrorMessage {
	return h.errors
}

// get_warnings returns all warnings
pub fn (h &DefaultErrorHandler) get_warnings() []WarningMessage {
	return h.warnings
}

// get_notices returns all notices
pub fn (h &DefaultErrorHandler) get_notices() []NoticeMessage {
	return h.notices
}

// clear_context clears the current error context
pub fn (mut h DefaultErrorHandler) clear_context() {
	h.context.clear()
}

// reset clears all errors, warnings, notices, and state
pub fn (mut h DefaultErrorHandler) reset() {
	h.errors = []
	h.warnings = []
	h.notices = []
	h.context.clear()
	h.deduplicator.reset()
	h.call_stack = []
	h.should_abort = false
}

// kind_to_string converts a MessageKind to its string representation
pub fn kind_to_string(kind MessageKind) string {
	match kind {
		.error { return 'error' }
		.warning { return 'warning' }
		.notice { return 'notice' }
	}
}

// string_to_kind converts a string to a MessageKind
pub fn string_to_kind(s string) !MessageKind {
	match s {
		'error' { return .error }
		'warning' { return .warning }
		'notice' { return .notice }
		else { return error('invalid message kind: ${s}') }
	}
}
