struct Server {}

fn (server Server) non_mutable_receiver() {
	println(@LOCATION)
}

fn (mut s Server) mutable_receiver() {
	println(@LOCATION)
}

fn (s &Server) reference_receiver() {
	println(@LOCATION)
}

fn test_spawning_threads_with_methods_that_have_mutable_and_non_mutable_receivers() {
	mut server := &Server{}
	t1 := spawn server.non_mutable_receiver()
	t2 := spawn server.mutable_receiver()
	t3 := spawn server.reference_receiver()
	t1.wait()
	t2.wait()
	t3.wait()
}
