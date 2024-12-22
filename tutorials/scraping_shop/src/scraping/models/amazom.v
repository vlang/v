module models

pub type ThumbLink = string

@[table: 'AmazonScrapings']
pub struct AmazonScraping {
pub:
	price_printed      ?f64
	price_kindle_ebook ?f64
	geral_evaluation   f32
	qtde_evaluation    int
	link               string
	title              string
	author             string
	sinopse            string
	images_links       string
	thumbnails_links   ThumbLink
}
