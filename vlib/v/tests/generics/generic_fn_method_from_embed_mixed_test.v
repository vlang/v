// For issue 27786: inside a generic function, a method call on a value of type
// `T` used the embed accessor path (`.Embed`) of the last checked instantiation
// for every other instantiation, producing invalid C code like
// `Category_id(identifiable.Image)`.
struct ID {
	s string
}

interface Identifiable {
	id() ID
}

struct Image {
	image_id ID
}

fn (i Image) id() ID {
	return i.image_id
}

struct SEO {
	seo_id ID
}

fn (s SEO) id() ID {
	return s.seo_id
}

struct ProductImage {
	Image
}

struct CategorySEO {
	SEO
}

struct Category {
	cat_id ID
}

fn (c Category) id() ID {
	return c.cat_id
}

struct InventoryItem {
	item_id ID
}

fn (ii InventoryItem) id() ID {
	return ii.item_id
}

fn make_identifiable_map[T](identifiables []T) map[string]T {
	mut res := map[string]T{}
	for identifiable in identifiables {
		id := identifiable.id()
		res[id.s] = identifiable
	}
	return res
}

fn test_direct_and_embedded_methods_in_generic_fn() {
	c := make_identifiable_map([Category{ID{'cat1'}}])
	assert c.keys() == ['cat1']
	ii := make_identifiable_map([InventoryItem{ID{'item1'}}])
	assert ii.keys() == ['item1']
	pi := make_identifiable_map([ProductImage{Image{ID{'img1'}}}])
	assert pi.keys() == ['img1']
	cs := make_identifiable_map([CategorySEO{SEO{ID{'seo1'}}}])
	assert cs.keys() == ['seo1']
}

struct Base {
mut:
	n int
}

fn (b Base) id() ID {
	return ID{'base${b.n}'}
}

fn (mut b Base) bump() {
	b.n++
}

struct Mid {
	Base
}

struct Wrapper {
	Mid
}

struct Direct {
mut:
	n int
}

fn (d Direct) id() ID {
	return ID{'direct${d.n}'}
}

fn (mut d Direct) bump() {
	d.n += 10
}

fn get_ids[T](items []T) []string {
	mut res := []string{}
	for item in items {
		res << item.id().s
	}
	return res
}

fn bump_all[T](mut items []T) {
	for mut item in items {
		item.bump()
	}
}

fn test_mut_methods_through_nested_embed_in_generic_fn() {
	mut ws := [Wrapper{Mid{Base{1}}}]
	mut ds := [Direct{2}]
	assert get_ids(ws) == ['base1']
	assert get_ids(ds) == ['direct2']
	bump_all(mut ws)
	bump_all(mut ds)
	assert ws[0].n == 2
	assert ds[0].n == 12
	assert get_ids([Base{5}]) == ['base5']
}
