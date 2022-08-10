# To run app
Dockerfile
[docker build]=> Docker image
[docker run]=> Docker container

`sudo docker build -t <name> .`

`sudo docker run --name <container name> --interactive --tty --publish 3001:3001 <name>`

`v run .`

A message like `[Vweb] Running app on http://localhost:3001/` should appear

`exit`

# To implement new bechmarks in v

In `examples/js_dom_draw_bechmark_chart/v_vweb_orm/src/main.v` path
Create a route returning a `Response` struct like:

```v ignore
['/sqlite-memory/:count']
pub fn (mut app App) sqlite_memory(count int) vweb.Result {
	mut insert_stopwatchs := []int{}
	mut select_stopwatchs := []int{}
	mut update_stopwatchs := []int{}

	mut sw := time.new_stopwatch()

	mut db := sqlite.connect(':memory:') or { panic(err) }

	sql db {
		create table Task
	}

	task_model := Task{
		title: 'a'
		status: 'done'
	}

	for i := 0; i < count; i++ {
		sw.start()
		sql db {
			insert task_model into Task
		}
		sw.stop()
		insert_stopwatchs << int(sw.end - sw.start)
	}

	sql db {
		drop table Task
	}

	response := Response{
		insert:	insert_stopwatchs
		@select:select_stopwatchs
		update:	update_stopwatchs
	}
	return app.json(response)
}

```

In `examples/chart/services.v` path
Create a service to request the benchmarks data by http
Decode the info to `FrameworkBenchmarkResponse`
```v ignore
fn typescript_sqlite_memory() ?FrameworkBenchmarkResponse {
	url := 'http://localhost:3000/sqlite-memory/$benchmark_loop_length'
	res := http.get(url) or { panic(err) }
	framework_benchmark_response := json.decode(FrameworkBenchmarkResponse, res.body)?
	return framework_benchmark_response
}
```

In `examples/chart/main.v` path
Create a service to request the benchmarks data by http
Decode the info to `FrameworkBenchmarkResponse`
```v ignore
fn typescript_sqlite_memory() ?FrameworkBenchmarkResponse {
	url := 'http://localhost:3000/sqlite-memory/$benchmark_loop_length'
	res := http.get(url) or { panic(err) }
	framework_benchmark_response := json.decode(FrameworkBenchmarkResponse, res.body)?
	return framework_benchmark_response
}
```
Then, update:
`insert_framework_benchmark_times()`;
`select_framework_benchmark_times()`;
`update_framework_benchmark_times()`.
with the new function



# ROADMAP
02/09/2022
- [ ] select bench (easy)
- [ ] vsql (easy)