// vtest vflags: -enable-globals
// The default value of an enum is its first member. When that member's value comes from C,
// V does not know whether it is 0, so every default uses the member: struct fields, fixed
// arrays, the elements of `[]T{len: n}`, the value of a missing map key and globals.
#include <errno.h>
#include <signal.h>
#include <stdio.h>

enum Errno {
	inval = C.EINVAL
	other
}

enum Signal as u8 {
	interrupt = C.SIGINT
	terminate = C.SIGTERM
}

// SEEK_SET is 0: the member is still the default.
enum Whence {
	set = C.SEEK_SET
	cur = C.SEEK_CUR
}

struct Holder {
	e Errno
	s Signal
	w Whence
}

struct FixedHolder {
	errs   [2]Errno
	nested [2][2]Signal
}

__global global_errno Errno
__global global_signal Signal
__global global_errnos [2]Errno
__global global_holder Holder

fn test_member_values_come_from_c() {
	assert int(Errno.inval) == C.EINVAL
	assert int(Errno.other) == C.EINVAL + 1
	assert int(Signal.interrupt) == C.SIGINT
	assert int(Whence.set) == C.SEEK_SET
}

fn test_struct_fields() {
	h := Holder{}
	assert h.e == .inval
	assert h.s == .interrupt
	assert h.w == .set
	f := FixedHolder{}
	assert f.errs == [Errno.inval, .inval]!
	assert f.nested[1] == [Signal.interrupt, .interrupt]!
}

fn test_fixed_arrays() {
	errs := [3]Errno{}
	assert errs == [Errno.inval, .inval, .inval]!
	nested := [2][2]Signal{}
	assert nested[1][1] == .interrupt
	holders := [2]Holder{}
	assert holders[1].e == .inval
	assert holders[1].s == .interrupt
}

fn test_dynamic_arrays() {
	errs := []Errno{len: 2}
	assert errs == [Errno.inval, .inval]
	signals := []Signal{len: 3}
	assert signals[2] == .interrupt
	nested := [][2]Errno{len: 1}
	assert nested[0] == [Errno.inval, .inval]!
	holders := []Holder{len: 2}
	assert holders[1].e == .inval
	assert holders[1].s == .interrupt
}

fn test_missing_map_keys() {
	errs := map[string]Errno{}
	assert errs['missing'] == .inval
	signals := map[int]Signal{}
	assert signals[3] == .interrupt
}

fn test_globals() {
	assert global_errno == .inval
	assert global_signal == .interrupt
	assert global_errnos == [Errno.inval, .inval]!
	assert global_holder.e == .inval
	assert global_holder.s == .interrupt
}
