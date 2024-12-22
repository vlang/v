module page

import veb
import json
import shareds.wcontext
import scraping.models

fn render_amazom(data models.AmazonScraping, mut ctx wcontext.WsCtx) veb.Result {
	json_data := json.encode(data)

	price_printed := data.price_printed or { 0 }.str()
	price_kindle_ebook := data.price_kindle_ebook or { 0 }.str()
	save_is := 'amazon'

	return $veb.html('\\view\\card_data_amazon.html')
}
