// Copyright (c) 2019-2020 Ulises Jeremias Cornejo Fandos. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
//
// PLEASE DO NOT MODIFY THE CONTENT OF THIS FILE. If you find potencial errors 
// or want to add new features, create an issue or make a pull request
// in the official VSL repository: https://github.com/ulises-jeremias/vsl
module errno

pub fn str_error(errno Errno) string {
	match errno {
		.success {
			return 'success'
		}
		.failure {
			return 'failure'
		}
		.can_continue {
			return 'the iteration has not converged yet'
		}
		.edom {
			return 'input domain error'
		}
		.erange {
			return 'output range error'
		}
		.efault {
			return 'invalid pointer'
		}
		.einval {
			return 'invalid argument supplied by user'
		}
		.efailed {
			return 'generic failure'
		}
		.efactor {
			return 'factorization failed'
		}
		.esanity {
			return "sanity check failed - shouldn't happen"
		}
		.enomem {
			return 'malloc failed'
		}
		.ebadfunc {
			return 'problem with user-supplied function'
		}
		.erunaway {
			return 'iterative process is out of control'
		}
		.emaxiter {
			return 'exceeded max number of iterations'
		}
		.ezerodiv {
			return 'tried to divide by zero'
		}
		.ebadtol {
			return 'specified tolerance is invalid or theoretically unattainable'
		}
		.etol {
			return 'failed to reach the specified tolerance'
		}
		.eundrflw {
			return 'underflow'
		}
		.eovrflw {
			return 'overflow'
		}
		.eloss {
			return 'loss of accuracy'
		}
		.eround {
			return 'roundoff error'
		}
		.ebadlen {
			return 'matrix/vector sizes are not conformant'
		}
		.enotsqr {
			return 'matrix not square'
		}
		.esing {
			return 'singularity or extremely bad function behavior detected'
		}
		.ediverge {
			return 'integral or series is divergent'
		}
		.eunsup {
			return 'the required feature is not supported by this hardware platform'
		}
		.eunimpl {
			return 'the requested feature is not (yet) implemented'
		}
		.ecache {
			return 'cache limit exceeded'
		}
		.etable {
			return 'table limit exceeded'
		}
		.enoprog {
			return 'iteration is not making progress towards solution'
		}
		.enoprogj {
			return 'jacobian evaluations are not improving the solution'
		}
		.etolf {
			return 'cannot reach the specified tolerance in F'
		}
		.etolx {
			return 'cannot reach the specified tolerance in X'
		}
		.etolg {
			return 'cannot reach the specified tolerance in gradient'
		}
		.eof {
			return 'end of file'
		}
		else {
			return 'unknown error code'
		}
	}
}
