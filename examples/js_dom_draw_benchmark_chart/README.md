# JS DOM Benchmark Chart

![image](https://user-images.githubusercontent.com/63821277/186010833-2ea36f3a-4738-4025-9b23-ac62afe74b81.png)

## Running the App

> [!NOTE]
> The following steps require Node.js.
> To install Node, please refer to the [download page](https://nodejs.org/en/download/)
> or the installation via your operating systems [package manager](https://nodejs.org/en/download/package-manager).

The steps below assume that your current directory path is the examples project directory.

```
cd examples/js_dom_draw_benchmark_chart
```

Execute the following commands in separate terminal instances.

Run the Benchmarks Typescript Part

```sh
npm i --prefix typescript_vanilla_typeorm
npm run start:dev --prefix typescript_vanilla_typeorm
```

Run the Benchmarks V Part

```sh
v run v_vweb_orm
```

Run the Chart

```
cd chart/ && v run .
```

## Dockerfile

> [docker build] => Docker image\
> [docker run] => Docker container

```sh
sudo docker build -t <name> .
sudo docker run --name <container name> --interactive --tty --publish 3001:3001 <name>
v run .
# A message like `[Vweb] Running app on http://localhost:3001/` should appear
exit
```

## Implementing New Benchmarks in V

In `v_vweb_orm/src/main.v`, create a route that returns a `Response` struct.

```v ignore
@['/sqlite-memory/:count']
pub fn (mut app App) sqlite_memory(count int) vweb.Result {
	mut insert_stopwatchs := []int{}
	mut select_stopwatchs := []int{}
	mut update_stopwatchs := []int{}

	mut sw := time.new_stopwatch()

	mut db := sqlite.connect(':memory:') or { panic(err) }

	sql db {
		create table Task
	}!

	task_model := Task{
		title: 'a'
		status: 'done'
	}

	for i := 0; i < count; i++ {
		sw.start()
		sql db {
			insert task_model into Task
		} or { []Task{} }
		sw.stop()
		insert_stopwatchs << int(sw.end - sw.start)
	}

	sql db {
		drop table Task
	}!

	response := Response{
		insert: insert_stopwatchs
		@select: select_stopwatchs
		update: update_stopwatchs
	}
	return app.json(response)
}
```

In `chart/main.v`, create a service to request the benchmark data and decode the response as
`FrameworkBenchmarkResponse`.

```v ignore
fn typescript_sqlite_memory() ?FrameworkBenchmarkResponse {
	url := 'http://localhost:3000/sqlite-memory/${benchmark_loop_length}'
	res := http.get(url) or { panic(err) }
	framework_benchmark_response := json.decode(FrameworkBenchmarkResponse, res.body)!
	return framework_benchmark_response
}
```

Then update `insert_framework_benchmark_times()`, `select_framework_benchmark_times()` and
`update_framework_benchmark_times()` to include the `numbers := FrameworkPlatform{` for the newly
added function.

## Roadmap

02/09/2022

- [ ] select bench (easy)
- [ ] vsql (easy)
