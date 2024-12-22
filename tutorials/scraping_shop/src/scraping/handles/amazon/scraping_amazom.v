module amazon

import veb
import net.http
import net.html
import net.urllib
import shareds.utils
import scraping.models

pub struct AmazonHandle {
pub:
	lang string
}

pub fn (ah AmazonHandle) scraping_amazon(url string) !models.AmazonScraping {
	resp := http.fetch(
		url:        url
		user_agent: 'Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:88.0) Gecko/20100101 Firefox/88.0'
	)!
	mut doc := html.parse(resp.body)

	images_links, thumbnails_links := ah.get_images_links(mut doc)!
	geral_evaluation, qtde_evaluation := ah.get_avaliacao(mut doc)!

	return models.AmazonScraping{
		link:               ah.get_only_url(url)
		title:              ah.get_title(mut doc) or { '' }
		author:             ah.get_author(mut doc) or { '' }
		sinopse:            ah.get_sinopse(mut doc)!
		price_printed:      ah.get_price_printed(mut doc)
		price_kindle_ebook: ah.get_price_kindle_ebook(mut doc)
		images_links:       images_links[0] or { '' }
		thumbnails_links:   thumbnails_links[0] or { '' }
		geral_evaluation:   geral_evaluation
		qtde_evaluation:    qtde_evaluation
	}
}

fn (ah AmazonHandle) get_only_url(url string) string {
	purl := urllib.parse(url) or { return url }

	return '${purl.scheme}://${purl.host}${purl.path}'
}

fn (ah AmazonHandle) get_avaliacao(mut doc html.DocumentObjectModel) !(f32, int) {
	tags := doc.get_tags_by_attribute_value('id', 'acrPopover')
	tag_span := tags[0] or {
		return error('${veb.tr(ah.lang, 'msg_error_unable_capture_review')} (class: "a-size-base")')
	}.get_tags_by_class_name('a-size-base', 'a-color-base')

	tag_qtde_reviewers := doc.get_tags_by_attribute_value('id', 'acrCustomerReviewText')[0] or {
		return 0, 0
	}

	avaliacao := tag_span[0].content.replace(',', '.').f32()
	qtde_evaluation := tag_qtde_reviewers.content.replace('.', '').int()

	return avaliacao, qtde_evaluation
}

fn (ah AmazonHandle) get_images_links(mut doc html.DocumentObjectModel) !([]string, []models.ThumbLink) {
	mut result_thumbs := []models.ThumbLink{}
	mut result_images := []string{}

	tags := doc.get_tags_by_attribute_value('class', 'imgTagWrapper')

	for tag in tags {
		img_tag := tag.get_tag('img') or { continue }

		result_thumbs << img_tag.attributes['src'] or {
			return error('${veb.tr(ah.lang, 'msg_error_unable_capture_image')} (attrib: "src")')
		}
		result_images << img_tag.attributes['data-old-hires'] or {
			return error('${veb.tr(ah.lang, 'msg_error_unable_capture_image')} (attrib: "data-old-hires")')
		}
	}

	return result_images, result_thumbs
}

fn (ah AmazonHandle) get_price_kindle_ebook(mut doc html.DocumentObjectModel) ?f64 {
	tags := doc.get_tags_by_attribute_value('id', 'tmm-grid-swatch-KINDLE')

	slots_price := tags[0] or { return none }.get_tags_by_attribute_value('class', 'slot-price')

	span_price := slots_price[0] or { return none }.get_tag('span') or { return 0 }

	return utils.only_number(span_price.content)
}

fn (ah AmazonHandle) get_price_printed(mut doc html.DocumentObjectModel) ?f64 {
	tags := doc.get_tags_by_attribute_value('id', 'tmm-grid-swatch-PAPERBACK')

	slots_price := tags[0] or { return none }.get_tags_by_attribute_value('class', 'slot-price')

	span_price := slots_price[0] or { return none }.get_tag('span') or { return 0 }

	return utils.only_number(span_price.content)
}

fn (ah AmazonHandle) get_author(mut doc html.DocumentObjectModel) ?string {
	mut name_authors := ''
	tags := doc.get_tags_by_class_name('singleAuthorSection')

	if tags.len == 0 {
		span_authors := doc.get_tags_by_class_name('author')

		for tag in span_authors {
			if tag.children.len > 0 {
				tags_a := tag.get_tags('a')

				for a in tags_a {
					name_authors += if name_authors.len > 0 {
						', ${a.content}'
					} else {
						'${a.content}'
					}
				}
			}
		}
	} else {
		name_authors = tags[0].get_tag('h2') or { return 'Não identificado' }.content
	}

	return name_authors
}

fn (ah AmazonHandle) get_title(mut doc html.DocumentObjectModel) ?string {
	tags := doc.get_tags_by_attribute_value('id', 'productTitle')
	return if tags.len > 0 {
		tags[0].content.trim_space()
	} else {
		none
	}
}

fn (ah AmazonHandle) get_sinopse(mut doc html.DocumentObjectModel) !string {
	mut tags := doc.get_tags_by_attribute_value('data-a-expander-name', 'book_description_expander')

	if tags.len == 0 {
		tags = doc.get_tags_by_attribute_value('data-a-expander-name', 'book_description')
	}

	sinopse := ah.get_text_content(mut tags[0].children)

	return sinopse
}

fn (ah AmazonHandle) get_text_content(mut tags []&html.Tag) string {
	mut full_content := ''

	for mut tag in tags {
		if tag.class_set.exists('a-expander-prompt') {
			continue
		}
		if tag.name in ['br/', 'span', 'p'] {
			content := tag.content.trim_string_left(' ').trim_string_right(' ')
			if content.len_utf8() > 0 {
				full_content += '${content}\n'
			}
		}

		if tag.children.len > 0 {
			full_content += ah.get_text_content(mut tag.children)
		}
	}

	return full_content
}
