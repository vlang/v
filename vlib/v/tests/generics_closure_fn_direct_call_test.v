pub struct App {
}

pub fn (mut app App) register<T>(service T) {
	fn [service] <T>() {
		println(service)
	}()
}

pub struct Service {
}

fn test_generics_closure_fn_direct_call() {
	mut app := App{}
	app.register(Service{})
	assert true
}
