module jobfn_alias_no_args

pub struct Job {
pub:
	f JobFn @[required]
}

// new creates a job using this module's callback signature.
pub fn new(f JobFn) Job {
	return Job{ f: f }
}

// run calls the stored callback.
pub fn (job Job) run() ! {
	job.f()!
}
