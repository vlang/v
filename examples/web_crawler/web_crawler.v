import net.http
import net.html

fn main() {
	site_url := 'https://news.ycombinator.com'
	resp := http.fetch(
		url:        site_url
		user_agent: 'Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:88.0) Gecko/20100101 Firefox/88.0'
	)!
	mut doc := html.parse(resp.body)
	tags := doc.get_tags_by_attribute_value('class', 'titleline')
	for i, tag in tags {
		el := tag.children[0]
		mut href := el.attributes['href']!
		if !href.starts_with('http') {
			href = '${site_url}/${href}'
		}
		title := el.content
		println('${i + 1:2}. title: ${title:-90s} href: ${href}')
	}
}
