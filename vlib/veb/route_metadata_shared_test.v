module veb

import net.http
import net.urllib

struct MetadataContext {
	Context
}

struct MetadataApp {
	Middleware[MetadataContext]
}

@['/items/:id'; get; delete; host: 'example.test']
fn (app &MetadataApp) item(mut ctx MetadataContext, id int) Result {
	return ctx.text(id.str())
}

@['/search'; get; post]
fn (app &MetadataApp) search(mut ctx MetadataContext, query string, page int) Result {
	return ctx.text('${query}:${page}')
}

@['/health'; get]
fn (app &MetadataApp) health(mut ctx MetadataContext) Result {
	return ctx.text('ok')
}

fn test_shared_route_metadata_keeps_arguments_hosts_and_middleware() {
	mut app := MetadataApp{}
	app.route_use('/items/:id', handler: fn (mut ctx MetadataContext) bool { return true })
	app.route_use('/search',
		after:   true
		handler: fn (mut ctx MetadataContext) bool { return true }
	)
	routes := generate_routes[MetadataApp, MetadataContext](app)!
	assert routes.len == 3
	item := routes['item'] or { panic('missing item route') }
	assert item.path == '/items/:id'
	assert item.words == ['items', ':id']
	assert item.methods == [http.Method.get, .delete]
	assert item.host == 'example.test'
	assert item.parameter_names == ['id']
	assert item.middlewares.len == 1
	assert item.after_middlewares.len == 0
	search := routes['search'] or { panic('missing search route') }
	assert search.parameter_names == ['query', 'page']
	assert search.middlewares.len == 0
	assert search.after_middlewares.len == 1
	health := routes['health'] or { panic('missing health route') }
	assert health.parameter_names.len == 0
	assert health.words == ['health']
}

fn test_controller_metadata_flag_preserves_dispatch_and_middleware() {
	mut app := MetadataApp{}
	app.route_use('/items/:id',
		handler: fn (mut ctx MetadataContext) bool {
			ctx.res.header.set_custom('X-Metadata-Test', 'visited') or { panic(err) }
			return true
		}
	)
	ctrl := controller[MetadataApp, MetadataContext]('/api', mut app)!
	$if veb_skip_route_metadata ? {
		assert ctrl.documented_routes.len == 0
	} $else {
		assert ctrl.documented_routes.any(it.path == '/items/:id')
	}
	mut url := urllib.parse_request_uri('/api/items/7')!
	ctx := &Context{
		req: http.Request{
			method: .get
			url:    '/api/items/7'
		}
	}
	completed := ctrl.handler(ctx, mut url, 'example.test')
	assert completed.res.body == '7'
	header_value := completed.res.header.get_custom('X-Metadata-Test') or { '' }
	assert header_value == 'visited'
}
