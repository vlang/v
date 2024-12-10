module testing

import time

pub enum MessageKind {
	compile_begin // sent right before *each* _test.v file compilation, the resulting status is not known yet, but the _test.v file itself is
	compile_end   // sent right after *each* _test.v file compilation, the message contains the output of that compilation
	cmd_begin     // sent right before *each* _test.v file execution, the resulting status is not known yet, but the _test.v file itself is
	cmd_end       // sent right after *each* _test.v file execution, the message contains the output of that execution

	ok   // success of a _test.v file
	fail // failed _test.v file, one or more assertions failed
	skip // the _test.v file was skipped for some reason
	info // a generic information message, detailing the actions of the `v test` program (some tests could be repeated for example, and the details are sent with an .info status)

	cannot_compile // when the _test.v file compiled with errors

	sentinel // send just once after all executions are done; it signals that the reporting/printing thread should stop the loop and exit
}

pub struct LogMessage {
pub:
	kind    MessageKind   // see the MessageKind declaration above
	file    string        // the _test.v file that the message is about
	when    time.Time     // when was the message sent (messages are sent by the execution threads at the *end* of each event)
	flow_id string        // the messages of each thread, producing LogMessage, will have all the same unique flow_id. Messages by other threads will have other flow_id. If you use VJOBS=1 to serialise the execution, then all messages will have the same flow_id.
	took    time.Duration // the duration of the event, that this message describes
	message string        // the actual message text; the result of the event, that the message describes; most reporters could ignore this, since it could be reconstructed by the other fields
}

pub interface Reporter {
mut:
	session_start(message string, mut ts TestSession)        // called once per test session, in the main thread, suitable for setting up supporting infrastructure.
	session_stop(message string, mut ts TestSession)         // called once per test session, in the main thread, after everything else, suitable for summaries, creating .xml reports, uploads etc.
	worker_threads_start(files []string, mut ts TestSession) // called once per test session, in the main thread, right before all the worker threads start
	worker_threads_finish(mut ts TestSession)                // called once per test session, in the main thread, right after all the worker threads finish

	report(index int, log_msg LogMessage) // called once per each message, that will be shown (ok/fail/skip etc), only in the reporting thread.
	report_stop()                         // called just once after all messages are processed, only in the reporting thread, but before stop_session.

	// TODO: reconsider, whether the next methods, should be kept for all reporters, or just moved inside the normal reporter, to simplify the interface
	progress(index int, message string)
	update_last_line(index int, message string)
	update_last_line_and_move_to_next(index int, message string)
	message(index int, message string)
	divider() // called to show a long ---------- horizontal line; can be safely ignored in most reporters; used in the main thread.
	list_of_failed_commands(cmds []string) // called after all testing is done, to produce a small summary that only lists the failed commands, so that they can be retried manually if needed, without forcing the user to scroll and find them.
}
