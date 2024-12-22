module instant_gaming

import veb
import net.http
import net.html
import shareds.utils
import scraping.models

pub struct InstantGamesHandle {
pub:
	lang string
}

pub fn (ih InstantGamesHandle) scraping_instantgames(url string) !models.InstantGamesScraping {
	resp := http.fetch(
		url:        url
		user_agent: 'Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:88.0) Gecko/20100101 Firefox/88.0'
	)!
	mut doc := html.parse(resp.body)

	return models.InstantGamesScraping{
		title:                ih.get_title(mut doc)!
		about:                ih.get_about(mut doc)!
		price:                ih.get_price(mut doc)!
		price_old:            ih.get_price_old(mut doc)
		discount:             ih.get_discount_old(mut doc)
		studio:               ih.get_studio(mut doc)!
		publisher:            ih.get_publisher(mut doc)!
		review_general:       ih.get_review(mut doc)!
		qtde_review:          ih.get_qtde_reviews(mut doc)!
		images:               ih.get_images(mut doc)!
		genders:              ih.get_genders(mut doc)!
		tags:                 ih.get_tags(mut doc)!
		activating_plataform: ih.get_activating_plataform(mut doc)!
	}
}

fn (ih InstantGamesHandle) get_activating_plataform(mut doc html.DocumentObjectModel) !string {
	mut content := ''
	mut tags_tags := doc.get_tags_by_class_name('subinfos')[0] or {
		return error('${veb.tr(ih.lang, 'msg_error_title_not_identified')}')
	}

	mut tags_a := tags_tags.get_tags('a')[0]

	for child in tags_a.children {
		if child.content.trim_space().len_utf8() == 0 {
			continue
		}
		content += '${child.content.trim_space()}'
	}

	return content
}

fn (ih InstantGamesHandle) get_tags(mut doc html.DocumentObjectModel) !string {
	mut content := ''
	mut tags_tags := doc.get_tags_by_class_name('users-tags')[0] or {
		return error('${veb.tr(ih.lang, 'msg_error_title_not_identified')}')
	}

	mut tags_a := tags_tags.get_tags('a')#[0..-1]

	for a in tags_a {
		content += '${a.content.trim_space()},'
	}

	return content#[0..-1]
}

fn (ih InstantGamesHandle) get_genders(mut doc html.DocumentObjectModel) !string {
	mut content := ''
	mut genres := doc.get_tags_by_class_name('genres')[0] or {
		return error('${veb.tr(ih.lang, 'msg_error_undetifined_genders')}')
	}

	for tag in genres.get_tags('a') {
		content += '${tag.content.trim_space()}, '
	}

	return content#[0..-2]
}

fn (ih InstantGamesHandle) get_title(mut doc html.DocumentObjectModel) !string {
	title := doc.get_tags_by_class_name('game-title')[0] or {
		return error('${veb.tr(ih.lang, 'msg_error_title_not_identified')}')
	}

	return title.content
}

fn (ih InstantGamesHandle) get_about(mut doc html.DocumentObjectModel) !string {
	mut tags_description := doc.get_tags_by_attribute_value('itemprop', 'description')

	mut description := ih.get_text_content(mut tags_description).trim_space()

	if description.len_utf8() == 0 {
		for tag in tags_description[0].children {
			if tag.content.trim_space().len_utf8() == 0 {
				continue
			}
			description += tag.content.trim_space() + '\n'
		}
	}

	return description
}

fn (ih InstantGamesHandle) get_price(mut doc html.DocumentObjectModel) !f64 {
	mut tags_price := doc.get_tags_by_class_name('total')[0] or {
		return error('${veb.tr(ih.lang, 'msg_error_price_not_identified')}')
	}

	return utils.only_number(tags_price.content)
}

fn (ih InstantGamesHandle) get_price_old(mut doc html.DocumentObjectModel) ?f64 {
	mut tags_price := doc.get_tags_by_class_name('discounted')[0] or { return none }

	return utils.only_number(tags_price.content)
}

fn (ih InstantGamesHandle) get_discount_old(mut doc html.DocumentObjectModel) ?int {
	mut tags_price := doc.get_tags_by_class_name('discounted')[0] or { return none }

	discount_price := int(utils.only_number(tags_price.content))

	return if discount_price == 0 {
		none
	} else {
		discount_price
	}
}

// content="Developers"
fn (ih InstantGamesHandle) get_studio(mut doc html.DocumentObjectModel) !string {
	tag_studio := doc.get_tags_by_attribute_value('content', 'Developers')[0] or {
		return error('${veb.tr(ih.lang, 'msg_error_unidentified_studio')}')
	}

	return tag_studio.content
}

fn (ih InstantGamesHandle) get_publisher(mut doc html.DocumentObjectModel) !string {
	tag_publisher := doc.get_tags_by_attribute_value('content', 'Publishers')[0] or {
		return error('${veb.tr(ih.lang, 'msg_error_undentified_publisher')}')
	}

	return tag_publisher.content
}

fn (ih InstantGamesHandle) get_images(mut doc html.DocumentObjectModel) ![]models.InstantGamesImage {
	tag_image := doc.get_tags_by_class_name('banner')[0] or {
		return error('${veb.tr(ih.lang, 'msg_error_undentified_publisher')}')
	}

	img := tag_image.get_tag('img') or {
		return error('${veb.tr(ih.lang, 'msg_error_unidentified_banner_image')}')
	}

	return [
		models.InstantGamesImage{
			image_url:  img.attributes['data-src'] or {
				return error('${veb.tr(ih.lang, 'msg_error_unidentified_banner_image')}')
			}
			type_image: models.TypeInstantGamesImage.full
		},
	]
}

fn (ih InstantGamesHandle) get_review(mut doc html.DocumentObjectModel) !string {
	tables := doc.get_tags_by_class_name('table')[0] or {
		return error('${veb.tr(ih.lang, 'msg_error_unidentified_review_table')}')
	}

	mut tag_table_cell := tables.get_tags_by_class_name('table-cell')#[-4..][1] or {
		return error('${veb.tr(ih.lang, 'msg_error_unidentified_review_table')} ("table-cell")')
	}

	text := tag_table_cell.get_tag('span') or {
		return error('${veb.tr(ih.lang, 'msg_error_unidentified_review_table')} ("span")')
	}

	return text.content.trim_space()
}

fn (ih InstantGamesHandle) get_qtde_reviews(mut doc html.DocumentObjectModel) !int {
	tables := doc.get_tags_by_class_name('table')[0] or {
		return error('${veb.tr(ih.lang, 'msg_error_unidentified_review_table')}')
	}

	mut tag_table_cell := tables.get_tags_by_class_name('table-cell')#[-4..][1] or {
		return error('${veb.tr(ih.lang, 'msg_error_unidentified_review_table')} ("table-cell")')
	}

	text := tag_table_cell.get_tag('text') or {
		return error('${veb.tr(ih.lang, 'msg_error_unidentified_review_table')} ("text")')
	}

	return int(utils.only_number(text.content))
}

fn (ih InstantGamesHandle) get_text_content(mut tags []&html.Tag) string {
	mut full_content := ''

	for mut tag in tags {
		if tag.name in ['br/', 'span', 'p'] {
			content := tag.content.trim_string_left(' ').trim_string_right(' ')
			if content.len_utf8() > 0 {
				full_content += '${content}\n'
			}
		}

		if tag.children.len > 0 {
			full_content += ih.get_text_content(mut tag.children)
		}
	}

	return full_content
}
