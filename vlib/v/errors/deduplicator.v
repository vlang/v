module errors

import v.token

// MessageKind represents the type of message being reported
pub enum MessageKind {
	error
	warning
	notice
}

// ErrorDeduplicator prevents duplicate error/warning/notice messages
// from being reported multiple times at the same location.
pub struct ErrorDeduplicator {
mut:
	error_lines   map[string]bool
	warning_lines map[string]bool
	notice_lines  map[string]bool
}

// new_deduplicator creates a new ErrorDeduplicator instance
pub fn new_deduplicator() &ErrorDeduplicator {
	return &ErrorDeduplicator{
		error_lines:   map[string]bool{}
		warning_lines: map[string]bool{}
		notice_lines:  map[string]bool{}
	}
}

// should_report determines whether a message should be reported.
// Returns false if a message with the same kind, file path, position,
// and message content has already been reported.
pub fn (mut d ErrorDeduplicator) should_report(kind MessageKind, file_path string, pos token.Pos, msg string) bool {
	// Create a unique key for this message
	key := d.make_key(file_path, pos, msg)

	match kind {
		.error {
			if key in d.error_lines {
				return false
			}
			d.error_lines[key] = true
		}
		.warning {
			if key in d.warning_lines {
				return false
			}
			d.warning_lines[key] = true
		}
		.notice {
			if key in d.notice_lines {
				return false
			}
			d.notice_lines[key] = true
		}
	}
	return true
}

// make_key creates a unique key for deduplication based on file path, position, and message
fn (d &ErrorDeduplicator) make_key(file_path string, pos token.Pos, msg string) string {
	// Use file path, line number, column, and message to create a unique key
	// Column is included to handle multiple errors on the same line
	return '${file_path}:${pos.line_nr}:${pos.col}:${msg}'
}

// reset clears all deduplication state
pub fn (mut d ErrorDeduplicator) reset() {
	d.error_lines.clear()
	d.warning_lines.clear()
	d.notice_lines.clear()
}

// error_count returns the number of unique errors that have been reported
pub fn (d &ErrorDeduplicator) error_count() int {
	return d.error_lines.len
}

// warning_count returns the number of unique warnings that have been reported
pub fn (d &ErrorDeduplicator) warning_count() int {
	return d.warning_lines.len
}

// notice_count returns the number of unique notices that have been reported
pub fn (d &ErrorDeduplicator) notice_count() int {
	return d.notice_lines.len
}
