// Copyright (c) 2019-2020 Ulises Jeremias Cornejo Fandos. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
//
// PLEASE DO NOT MODIFY THE CONTENT OF THIS FILE. If you find potencial errors 
// or want to add new features, create an issue or make a pull request
// in the official VSL repository: https://github.com/ulises-jeremias/vsl
module errno

pub enum Errno {
	success = 0
	failure = -1
	can_continue = -2/* iteration has not converged */

	edom = 1/* input domain error, e.g sqrt(-1) */

	erange = 2/* output range error, e.g. exp(1e100) */

	efault = 3/* invalid pointer */

	einval = 4/* invalid argument supplied by user */

	efailed = 5/* generic failure */

	efactor = 6/* factorization failed */

	esanity = 7/* sanity check failed - shouldn't happen */

	enomem = 8/* malloc failed */

	ebadfunc = 9/* problem with user-supplied function */

	erunaway = 10/* iterative process is out of control */

	emaxiter = 11/* exceeded max number of iterations */

	ezerodiv = 12/* tried to divide by zero */

	ebadtol = 13/* user specified an invalid tolerance */

	etol = 14/* failed to reach the specified tolerance */

	eundrflw = 15/* underflow */

	eovrflw = 16/* overflow  */

	eloss = 17/* loss of accuracy */

	eround = 18/* failed because of roundoff error */

	ebadlen = 19/* matrix, vector lengths are not conformant */

	enotsqr = 20/* matrix not square */

	esing = 21/* apparent singularity detected */

	ediverge = 22/* integral or series is divergent */

	eunsup = 23/* requested feature is not supported by the hardware */

	eunimpl = 24/* requested feature not (yet) implemented */

	ecache = 25/* cache limit exceeded */

	etable = 26/* table limit exceeded */

	enoprog = 27/* iteration is not making progress towards solution */

	enoprogj = 28/* jacobian evaluations are not improving the solution */

	etolf = 29/* cannot reach the specified tolerance in F */

	etolx = 30/* cannot reach the specified tolerance in X */

	etolg = 31/* cannot reach the specified tolerance in gradient */

	eof = 32/* end of file */

}
