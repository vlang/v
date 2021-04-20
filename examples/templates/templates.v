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

fn data_get() []SiteConfig {
	data := [SiteConfig{
		name: 'www_threefold_io'
		url: 'https://github.com/threefoldfoundation/www_threefold_io'
		branch: 'default'
		pull: false
		cat: .web
		alias: 'tf'
		path_code: '/Users/despiegk/codewww/github/threefoldfoundation/www_threefold_io'
		domains: ['www.threefold.io', 'www.threefold.me']
		descr: 'is our entry point for everyone, redirect to the detailed websites underneith.'
	}, SiteConfig{
		name: 'www_threefold_cloud'
		url: 'https://github.com/threefoldfoundation/www_threefold_cloud'
		branch: 'default'
		pull: false
		cat: .web
		alias: 'cloud'
		path_code: '/Users/despiegk/codewww/github/threefoldfoundation/www_threefold_cloud'
		domains: ['cloud.threefold.io', 'cloud.threefold.me']
		descr: 'for people looking to deploy solutions on top of a cloud, alternative to e.g. digital ocean'
	}, SiteConfig{
		name: 'www_threefold_farming'
		url: 'https://github.com/threefoldfoundation/www_threefold_farming'
		branch: 'default'
		pull: false
		cat: .web
		alias: 'farming'
		path_code: '/Users/despiegk/codewww/github/threefoldfoundation/www_threefold_farming'
		domains: ['farming.threefold.io', 'farming.threefold.me']
		descr: 'crypto & minining enthusiasts, be the internet, know about farming & tokens.'
	}, SiteConfig{
		name: 'www_threefold_twin'
		url: 'https://github.com/threefoldfoundation/www_threefold_twin'
		branch: 'default'
		pull: false
		cat: .web
		alias: 'twin'
		path_code: '/Users/despiegk/codewww/github/threefoldfoundation/www_threefold_twin'
		domains: ['twin.threefold.io', 'twin.threefold.me']
		descr: 'you digital life'
	}, SiteConfig{
		name: 'www_threefold_marketplace'
		url: 'https://github.com/threefoldfoundation/www_threefold_marketplace'
		branch: 'default'
		pull: false
		cat: .web
		alias: 'marketplace'
		path_code: '/Users/despiegk/codewww/github/threefoldfoundation/www_threefold_marketplace'
		domains: ['now.threefold.io', 'marketplace.threefold.io', 'now.threefold.me',
			'marketplace.threefold.me',
		]
		descr: 'apps for community builders, runs on top of evdc'
	}, SiteConfig{
		name: 'www_conscious_internet'
		url: 'https://github.com/threefoldfoundation/www_conscious_internet'
		branch: 'default'
		pull: false
		cat: .web
		alias: 'conscious_internet'
		path_code: '/Users/despiegk/codewww/github/threefoldfoundation/www_conscious_internet'
		domains: ['www.consciousinternet.org', 'eco.threefold.io', 'community.threefold.io',
			'eco.threefold.me', 'community.threefold.me']
		descr: 'community around threefold, partners, friends, ...'
	}, SiteConfig{
		name: 'www_threefold_tech'
		url: 'https://github.com/threefoldtech/www_threefold_tech'
		branch: 'default'
		pull: false
		cat: .web
		alias: 'tech'
		path_code: '/Users/despiegk/codewww/github/threefoldtech/www_threefold_tech'
		domains: ['www.threefold.tech']
		descr: 'cyberpandemic, use the tech to build your own solutions with, certification for TFGrid'
	}, SiteConfig{
		name: 'www_examplesite'
		url: 'https://github.com/threefoldfoundation/www_examplesite'
		branch: 'default'
		pull: false
		cat: .web
		alias: 'example'
		path_code: '/Users/despiegk/codewww/github/threefoldfoundation/www_examplesite'
		domains: ['example.threefold.io']
		descr: ''
	}, SiteConfig{
		name: 'info_threefold'
		url: 'https://github.com/threefoldfoundation/info_foundation_archive'
		branch: 'default'
		pull: false
		cat: .wiki
		alias: 'threefold'
		path_code: '/Users/despiegk/codewww/github/threefoldfoundation/info_foundation_archive'
		domains: ['info.threefold.io']
		descr: 'wiki for foundation, collaborate, what if farmings, tokens'
	}, SiteConfig{
		name: 'info_sdk'
		url: 'https://github.com/threefoldfoundation/info_sdk'
		branch: 'default'
		pull: false
		cat: .wiki
		alias: 'sdk'
		path_code: '/Users/despiegk/codewww/github/threefoldfoundation/info_sdk'
		domains: ['sdk.threefold.io', 'sdk_info.threefold.io']
		descr: 'for IAC, devops, how to do Infrastruture As Code, 3bot, Ansible, tfgrid-sdk, ...'
	}, SiteConfig{
		name: 'info_legal'
		url: 'https://github.com/threefoldfoundation/info_legal'
		branch: 'default'
		pull: false
		cat: .wiki
		alias: 'legal'
		path_code: '/Users/despiegk/codewww/github/threefoldfoundation/info_legal'
		domains: ['legal.threefold.io', 'legal_info.threefold.io']
		descr: ''
	}, SiteConfig{
		name: 'info_cloud'
		url: 'https://github.com/threefoldfoundation/info_cloud'
		branch: 'default'
		pull: false
		cat: .wiki
		alias: 'cloud'
		path_code: '/Users/despiegk/codewww/github/threefoldfoundation/info_cloud'
		domains: ['cloud_info.threefold.io']
		descr: 'how to use the cloud for deploying apps: evdc, kubernetes, planetary fs, ... + marketplace solutions '
	}, SiteConfig{
		name: 'info_tftech'
		url: 'https://github.com/threefoldtech/info_tftech'
		branch: 'default'
		pull: false
		cat: .wiki
		alias: 'tftech'
		path_code: '/Users/despiegk/codewww/github/threefoldtech/info_tftech'
		domains: ['info.threefold.tech']
		descr: ''
	}, SiteConfig{
		name: 'info_digitaltwin'
		url: 'https://github.com/threefoldfoundation/info_digitaltwin.git'
		branch: 'default'
		pull: false
		cat: .wiki
		alias: 'twin'
		path_code: '/Users/despiegk/codewww/github/threefoldfoundation/info_digitaltwin'
		domains: ['twin_info.threefold.io']
		descr: ''
	}]
	return data
}

fn data_dump(data []SiteConfig) {
	a := json.encode_pretty(data)
	os.write_file(os.resource_abs_path('data.json'), a) or { panic(err) }
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
	os.write_file('result.md', b) or { panic(err) }
}
