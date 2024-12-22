module main

import veb
import shareds.wcontext
import scraping.page as page_scraping

pub struct Wservice {
	veb.Controller
	veb.StaticHandler
}

fn main() {
	mut wservice := &Wservice{}

	mut ctrl_page_index := page_scraping.PageIndex{}

	wservice.mount_static_folder_at('src/shareds/assets', '/assets')!

	wservice.mount_static_folder_at('src/shareds/components', '/components')!

	wservice.register_controller[page_scraping.PageIndex, wcontext.WsCtx]('/home', mut
		ctrl_page_index)!

	veb.run[Wservice, wcontext.WsCtx](mut wservice, 3030)
}

fn (ws &Wservice) index(mut ctx wcontext.WsCtx) veb.Result {
	return ctx.redirect('/home/en')
}
