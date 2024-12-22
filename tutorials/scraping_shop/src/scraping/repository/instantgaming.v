module repository

import shareds.db
import scraping.models

pub struct RepoInstantGaming {}

pub fn RepoInstantGaming.new(model models.InstantGamesScraping) ! {
	con := db.new()!

	sql con {
		insert model into models.InstantGamesScraping
	}!
}
