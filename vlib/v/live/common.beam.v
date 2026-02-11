// BEAM backend stub for v.live module
// Not applicable on BEAM in C form: V's live reload mechanism is designed for
// the C backend using dlopen/dlsym to swap shared libraries at runtime.
// On BEAM, hot code reloading is a first-class feature of the runtime itself:
//   - code:load_file/1 loads a new version of a module
//   - code:purge/1 removes old versions
//   - Supervisor trees (OTP) manage graceful upgrades
// These stubs provide API compatibility but real BEAM hot reloading would
// use Erlang's native mechanisms, not this V-level abstraction.
module live

pub type FNLinkLiveSymbols = fn (linkcb voidptr)

pub type FNLiveReloadCB = fn (info &LiveReloadInfo)

pub struct LiveReloadInfo {
pub:
	vexe             string
	vopts            string
	original         string
	live_fn_mutex    voidptr
	live_linkfn      FNLinkLiveSymbols = unsafe { nil }
	so_extension     string
	so_name_template string
pub mut:
	monitored_files   []string
	live_lib          voidptr
	reloads           int
	reloads_ok        int
	reload_time_ms    int
	last_mod_ts       i64
	recheck_period_ms int            = 100
	cb_recheck        FNLiveReloadCB = unsafe { nil }
	cb_compile_failed FNLiveReloadCB = unsafe { nil }
	cb_before         FNLiveReloadCB = unsafe { nil }
	cb_after          FNLiveReloadCB = unsafe { nil }
	cb_locked_before  FNLiveReloadCB = unsafe { nil }
	cb_locked_after   FNLiveReloadCB = unsafe { nil }
	user_ptr          voidptr        = unsafe { nil }
}

// info gives user access to program's LiveReloadInfo struct.
// On BEAM: hot code reloading is handled natively by the BEAM VM via
// code:load_file/1 and OTP release handlers. This V-level live reload
// abstraction is not used. Returns an empty struct for API compatibility.
pub fn info() &LiveReloadInfo {
	return &LiveReloadInfo{}
}
