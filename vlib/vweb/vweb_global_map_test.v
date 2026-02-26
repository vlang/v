module vweb

struct VwebGlobalMapContact {
	email string
}

struct VwebGlobalMapTestApp {
	Context
mut:
	contacts map[string]VwebGlobalMapContact @[vweb_global]
}

fn sorted_contact_emails(contacts map[string]VwebGlobalMapContact) []string {
	mut emails := []string{}
	for _, contact in contacts.clone() {
		emails << contact.email
	}
	emails.sort()
	return emails
}

fn test_new_request_app_clones_vweb_global_maps() {
	mut global_app := &VwebGlobalMapTestApp{}
	mut first_request_app := new_request_app(global_app, Context{}, 0)
	first_request_app.contacts['a@example.com'] = VwebGlobalMapContact{'a@example.com'}
	mut second_request_app := new_request_app(global_app, Context{}, 0)
	second_request_app.contacts['b@example.com'] = VwebGlobalMapContact{'b@example.com'}

	assert sorted_contact_emails(first_request_app.contacts) == ['a@example.com']
	assert sorted_contact_emails(second_request_app.contacts) == ['b@example.com']
	assert global_app.contacts.len == 0
}
