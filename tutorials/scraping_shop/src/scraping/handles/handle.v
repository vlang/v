module handles

import veb
import scraping.models
import scraping.handles.amazon
import scraping.handles.instant_gaming

type TypeScrapings = models.AmazonScraping | models.InstantGamesScraping

pub fn handle(url string, lang string) !TypeScrapings {
	return if ['amzn.to', 'amazon'].any(url.contains(it)) {
		scraping_handle := amazon.AmazonHandle{lang}

		return scraping_handle.scraping_amazon(url)!
	} else if url.contains('instant-gaming') {
		scraping_handle := instant_gaming.InstantGamesHandle{lang}

		return scraping_handle.scraping_instantgames(url)!
	} else {
		error(veb.tr(lang, 'msg_error_not_found_handler_from_plataform'))
	}
}
