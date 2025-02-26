import net.html

fn test_nil() {
	mut doc := html.parse('<body><div class="Box-footer"><div class="truncate">abc</div></div></body>')
	footer := doc.get_tags_by_class_name('Box-footer')[0]
	hrefs := footer.get_tag_by_class_name('Truncate')
	println(hrefs)
	res := '${hrefs}'
	assert res == '&Option(&nil)'
}

fn test_non_nil() {
	mut doc := html.parse('<body><div class="Box-footer"><div class="Truncate">abc</div></div></body>')
	footer := doc.get_tags_by_class_name('Box-footer')[0]
	hrefs := footer.get_tag_by_class_name('Truncate')
	println(hrefs)
	res := '${hrefs}'
	assert res == '&Option(<div class="Truncate" >abc</div>)'
}
