// Evaluating a V snippet in process.
//
// This uses the interpreter in `v.eval`, so it needs no build step and no
// temporary file. The interpreter covers a practical subset of V: expressions,
// the common statements, and enough of the standard library to try a call out.
// Anything needing compilation is the job of `v_run`.
module main

import v.eval

// EvalResult is what one snippet produced.
pub struct EvalResult {
pub:
	ok     bool
	stdout string
	// err is the interpreter's message, empty when the snippet ran.
	err string
}

// eval_text evaluates `code` and captures whatever it printed.
pub fn eval_text(ws &Workspace, code string) EvalResult {
	mut e := eval.create()
	e.run_text(code) or {
		return EvalResult{
			ok:     false
			stdout: e.stdout()
			err:    err.msg()
		}
	}
	return EvalResult{
		ok:     true
		stdout: e.stdout()
	}
}
