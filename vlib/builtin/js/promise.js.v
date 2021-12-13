module builtin

pub interface JS.Promise {
	then(onFullfilled JS.Any, onRejected JS.Any)
	catch(onCatch JS.Any) JS.Promise
	finally(callback JS.Any) JS.Promise
}

[use_new]
pub fn JS.Promise.prototype.constructor(JS.Any) JS.Promise
pub fn JS.Promise.reject(JS.Any) JS.Promise
pub fn JS.Promise.resolve(JS.Any) JS.Promise
pub fn JS.Promise.race(JS.Array) JS.Promise

// The Promise object represents the eventual completion (or failure)
// of an asynchronous operation and its resulting value.
pub struct Promise<T> {
mut:
	promise JS.Promise [noinit]
}

pub fn promise_new<T>(executor fn (resolve fn (T), reject fn (JS.Any))) Promise<T> {
	promise := JS.Promise.prototype.constructor(executor)
	return Promise<T>{promise}
}

pub fn (p Promise<T>) then(on_fullfilled fn (T), on_rejected fn (JS.Any)) {
	p.promise.then(on_fullfilled, on_rejected)
}

// catch method returns a Promise and deals with rejected cases only.
pub fn (p Promise<T>) catch(callback fn (error JS.Any)) Promise<T> {
	promise := p.promise.catch(callback)
	return Promise<T>{promise}
}

pub fn (p Promise<T>) finally<U>(callback fn ()) Promise<JS.Any> {
	promise := p.promise.finally(callback)
	return Promise<JS.Any>{promise}
}

// reject<E> returns promise which was rejected because of specified error
pub fn promise_reject(error JS.Any) Promise<JS.Any> {
	promise := JS.Promise.reject(error)
	return Promise<JS.Any>{promise}
}

// resolve<E> returns promise which was resolved with specified value
pub fn promise_resolve<T>(result T) Promise<T> {
	promise := JS.Promise.resolve(result)
	return Promise<T>{promise}
}

// race returns returns a promise that fulfills or rejects as soon as one of
//  the promises in an iterable fulfills or rejects, with the value or reason from that promise.
pub fn promise_race<T>(promises []Promise<T>) Promise<T> {
	promises_ := JS.Array.prototype.constructor()

	for elem in promises {
		promises_.push(elem.promise)
	}

	promise := JS.Promise.race(promises_)
	return Promise<T>{promise}
}

pub fn JS.Promise.all(JS.Array) JS.Promise
pub fn JS.Promise.allSettled(JS.Array) JS.Promise

/*
pub type JsAny = JS.Any

// all takes an iterable of promises as an input, and returns a single Promise that resolves to an array of
// the results of the input promises
pub fn all(array []JS.Promise) Promise<JS.Array, js.promise.JsAny> {
	mut promise := JS.Promise(JS.Any(voidptr(0)))
	#promise = Promise.all(array.arr.arr);

	return Promise<JS.Array,JsAny>{promise}
}
*/
