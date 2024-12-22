module page

import veb
import json
import scraping.models
import shareds.wcontext
import shareds.components.badge

fn render_instantgaming(data models.InstantGamesScraping, mut ctx wcontext.WsCtx) veb.Result {
	json_data := json.encode(data)

	price := data.price
	discount := data.discount or { 0 }.str()
	old_price := data.price_old or { 0 }.str()
	banner_img := data.images[0] or { models.InstantGamesImage{} }.image_url
	badges_html := badge.list_badge_to_html(data.tags.to_list().map({
		'status': 'none'
		'label':  it
	}))
	save_is := 'instant-gaming'

	return $veb.html('\\view\\card_data_instantgames.html')
}
