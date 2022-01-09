module html

const (
	html = '<!doctype html>
<html>
<head></head>
<body>
  <div id="1st">
    <div class="bar"></div>
  </div>
  <div id="2nd">
    <div class="foo">
      <a href="https://vlang.io/">V</a>
    </div>
    <div class="bar"></div>
    <a href="https://modules.vlang.io/">vlib</a>
    <div class="bar">
      <div class="bar">
        <p>
        <div>modules</div>
        </p>
        <a href="https://vpm.vlang.io/">vpm</a>
      </div>
    </div>
  </div>
  <div id="3rd"></div>
</body>
</html>'
)

fn test_search_by_tag_type() {
	mut dom := parse(html.html)
	tag := dom.get_tag_by_attribute_value('id', '2nd')[0]
	assert tag.get_tags('div').len == 5
	assert tag.get_tags_by_attribute('href')[2].content == 'vpm'
	assert tag.get_tags_by_attribute_value('class', 'bar').len == 3
}
