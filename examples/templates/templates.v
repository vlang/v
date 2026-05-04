module main

import os
import json

pub struct SiteConfig {
pub mut:
	name      string
	url       string
	branch    string = 'default' // means is the default branch
	pull      bool
	cat       SiteCat
	alias     string
	path_code string
	domains   []string
	descr     string
}

pub enum SiteCat {
	wiki
	data
	web
}

fn data_load() []SiteConfig {
	data := os.read_file(os.resource_abs_path('data.json')) or { panic(err) }
	a := json.decode([]SiteConfig, data) or { panic(err) }
	return a
}

fn filled_in_template() string {
	port_str := '80'
	sites := data_load()
	return $tmpl('template.md')
}

fn main() {
	// A NICE test how to work with the json module
	// data := data_get()
	// data_dump(data)
	b := filled_in_template()
	println(b)
}
