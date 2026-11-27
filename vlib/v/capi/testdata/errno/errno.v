module errprobe

import v.capi

@[capi.CApiErrno(7)]
pub fn coded() !int {
	return error('coded failure')
}
