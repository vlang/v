module notify

// new returns an error when the current platform has no notifier backend.
pub fn new() !FdNotifier {
	return error('os.notify is not supported on this platform')
}
