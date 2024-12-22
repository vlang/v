module page

import veb
import json
import time
import veb.sse
import net.http
import scraping.models
import shareds.wcontext
import scraping.handles
import scraping.repository
import shareds.components.switch_lang

@['/current_lang']
fn (page &PageIndex) current_lang(mut ctx wcontext.WsCtx) veb.Result {
	ctx.takeover_conn()
	go fn [page] (mut ctx wcontext.WsCtx) {
		mut sse_conn := sse.start_connection(mut ctx.Context)
		for {
			rlock page.current_lang {
				sse_conn.send_message(
					event: 'langName'
					data:  page.current_lang.lang
				) or { continue }
			}
			time.sleep(750 * time.millisecond)
		}
		sse_conn.close()
	}(mut &ctx)

	return veb.no_result()
}

@['/:lng']
fn (page &PageIndex) index(mut ctx wcontext.WsCtx, lng string) veb.Result {
	page.set_lang(mut ctx, lng)

	home := render_home(mut ctx)
	switch_language := switch_lang.construct(ctx.lang)

	if ctx.get_custom_header('render-only-home') or { 'false' } == 'true' {
		return ctx.html(home)
	}

	return $veb.html('\\view\\index.html')
}

@['/scraping'; post]
fn (page &PageIndex) scraping(mut ctx wcontext.WsCtx) veb.Result {
	lng := rlock page.current_lang {
		page.current_lang.abbrev_lang
	}
	page.set_lang(mut ctx, lng)

	data := handles.handle(ctx.form['urlToScraping'], lng) or {
		return $veb.html('\\view\\card_data_fail.html')
	}

	if data is models.AmazonScraping {
		return render_amazom(data, mut ctx)
	} else if data is models.InstantGamesScraping {
		return render_instantgaming(data, mut ctx)
	}

	err := error(veb.tr(lng, 'msg_error_msg_unknown_scrape_type'))
	return $veb.html('\\view\\card_data_fail.html')
}

@['/save/:save_is'; post]
fn (page &PageIndex) save(mut ctx wcontext.WsCtx, save_is string) veb.Result {
	lng := rlock page.current_lang {
		page.current_lang.abbrev_lang
	}
	page.set_lang(mut ctx, lng)

	if save_is == 'amazon' {
		js := json.decode(models.AmazonScraping, ctx.req.data) or {
			return page.modal(mut ctx, veb.tr(lng, 'msg_error_fail_to_save'), 'error: Invalid JSON data')
		}

		repository.RepoAmazon.new(js) or {
			return page.modal(mut ctx, veb.tr(lng, 'msg_error_fail_to_save'), 'error: Data already exists')
		}
	} else if save_is == 'instant-gaming' {
		js := json.decode(models.InstantGamesScraping, ctx.req.data) or {
			return page.modal(mut ctx, veb.tr(lng, 'msg_error_fail_to_save'), 'error: Invalid JSON data')
		}

		repository.RepoInstantGaming.new(js) or {
			return page.modal(mut ctx, veb.tr(lng, 'msg_error_fail_to_save'), 'error: Data already exists')
		}
	}

	return page.modal(mut ctx, veb.tr(lng, 'msg_successfully_saved'), veb.tr(lng, 'msg_all_completed'))
}

fn (page &PageIndex) modal(mut ctx wcontext.WsCtx, title string, content string) veb.Result {
	return $veb.html('\\view\\custom_modal.html')
}

fn (page &PageIndex) set_lang(mut ctx wcontext.WsCtx, lng string) {
	ctx.lang = lng
	ctx.set_cookie(http.Cookie{
		name:  'lang'
		value: '${veb.tr(lng, lng)}"'
	})
	lock page.current_lang {
		page.current_lang.lang = veb.tr(lng, lng)
		page.current_lang.abbrev_lang = lng
	}
}
