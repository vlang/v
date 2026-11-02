// vtest vflags: -w
// Deliberate deprecated-type coverage must also run in suites using -W.
import sync

fn test_legacy_signed_tls() ! {
	value := isize(-42)
	mut tls := sync.new_tls(value)!
	assert tls.get()! == value
	tls.set(-123)!
	assert tls.get()! == -123
	tls.destroy()!
}
