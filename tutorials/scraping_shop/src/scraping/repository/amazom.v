module repository

import shareds.db
import scraping.models

pub struct RepoAmazon {}

pub fn RepoAmazon.new(model models.AmazonScraping) ! {
	con := db.new()!

	sql con {
		insert model into models.AmazonScraping
	}!
}
