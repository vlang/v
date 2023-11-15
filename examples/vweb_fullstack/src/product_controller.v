module main

import vweb
import encoding.base64
import json

@['/controller/products'; get]
pub fn (mut app App) controller_get_all_products() vweb.Result {
	token := app.req.header.get_custom('token') or { '' }

	if !auth_verify(token) {
		app.set_status(401, '')
		return app.text('Not valid token')
	}

	jwt_payload_stringify := base64.url_decode_str(token.split('.')[1])

	jwt_payload := json.decode(JwtPayload, jwt_payload_stringify) or {
		app.set_status(501, '')
		return app.text('jwt decode error')
	}

	user_id := jwt_payload.sub

	response := app.service_get_all_products_from(user_id.int()) or {
		app.set_status(400, '')
		return app.text('${err}')
	}
	return app.json(response)
	// return app.text('response')
}

@['/controller/product/create'; post]
pub fn (mut app App) controller_create_product(product_name string) vweb.Result {
	if product_name == '' {
		app.set_status(400, '')
		return app.text('product name cannot be empty')
	}

	token := app.req.header.get_custom('token') or { '' }

	if !auth_verify(token) {
		app.set_status(401, '')
		return app.text('Not valid token')
	}

	jwt_payload_stringify := base64.url_decode_str(token.split('.')[1])

	jwt_payload := json.decode(JwtPayload, jwt_payload_stringify) or {
		app.set_status(501, '')
		return app.text('jwt decode error')
	}

	user_id := jwt_payload.sub

	app.service_add_product(product_name, user_id.int()) or {
		app.set_status(400, '')
		return app.text('error: ${err}')
	}
	app.set_status(201, '')
	return app.text('product created successfully')
}
