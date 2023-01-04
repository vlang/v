module os

fn signal_str(signal Signal) string {
	mut result := signal.str().to_upper()
	result = 'SIG${result}'
	return result
}

fn signal_from_str(str JS.String) Signal {
	s := string(str)
	return match s {
		'SIGHUP' {
			Signal.hup
		}
		'SIGINT' {
			Signal.int
		}
		'SIGQUIT' {
			Signal.quit
		}
		'SIGILL' {
			Signal.ill
		}
		'SIGTRAP' {
			Signal.trap
		}
		'SIGABRT' {
			Signal.abrt
		}
		'SIGBUS' {
			Signal.bus
		}
		'SIGFPE' {
			Signal.fpe
		}
		'SIGKILL' {
			Signal.kill
		}
		'SIGUSR1' {
			Signal.usr1
		}
		'SIGSEGV' {
			Signal.segv
		}
		'SIGUSR2' {
			Signal.usr2
		}
		'SIGPIPE' {
			Signal.pipe
		}
		'SIGALRM' {
			Signal.alrm
		}
		'SIGTERM' {
			Signal.term
		}
		'SIGSTKFLT' {
			Signal.stkflt
		}
		'SIGCHLD' {
			Signal.chld
		}
		'SIGCONT' {
			Signal.cont
		}
		'SIGSTOP' {
			Signal.stop
		}
		'SIGTSTP' {
			Signal.tstp
		}
		'SIGTTIN' {
			Signal.ttin
		}
		'SIGTTOU' {
			Signal.ttou
		}
		'SIGURG' {
			Signal.urg
		}
		'SIGXCPU' {
			Signal.xcpu
		}
		'SIGXFSZ' {
			Signal.xfsz
		}
		'SIGVTALRM' {
			Signal.vtalrm
		}
		'SIGPROF' {
			Signal.prof
		}
		'SIGWINCH' {
			Signal.winch
		}
		'SIGPOLL' {
			Signal.poll
		}
		'SIGPWR' {
			Signal.pwr
		}
		'SIGSYS' {
			Signal.sys
		}
		else {
			panic('unknown signal: ${s}')
		}
	}
}

// signal will assign `handler` callback to be called when `signum` signal is received.
//
// # Behaviour on different backends:
// - NodeJS: Will use `process.on` and add `handler` to the listeners list for `signum` to happen
// - Browser: Will use `window.addEventListener` for handling signal
//
// TODO: Add signal events implementation for browser backend
pub fn signal_opt(signum Signal, handler SignalHandler) ?SignalHandler {
	signame := signal_str(signum)
	_ := signame
	$if js_node {
		#$process.on(signame.str,function (sig) { handler(os__signal_from_str(sig));});

		return handler
	} $else $if js_browser {
		#let event = new CustomEvent(signame.str, {detail: signum});
		#window.addEventListener(signame.str, function (e) { handler(e.detail); });

		return handler
	} $else {
		return error('signal handlers are not supported on bare JS')
	}
}
