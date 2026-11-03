module jobfn_alias_with_args

pub struct Job {
pub:
	f JobFn @[required]
}

// new creates a job using this module's callback signature.
pub fn new(f JobFn) Job {
	return Job{ f: f }
}

// run calls the stored callback with a value.
pub fn (job Job) run(value int) !int {
	return job.f(value)
}
