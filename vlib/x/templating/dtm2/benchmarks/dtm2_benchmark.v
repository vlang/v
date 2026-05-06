module main

import os
import strings
import time
import x.templating.dtm2

const bench_root_name = 'dtm2_benchmark'

struct BenchConfig {
	case_name                 string
	iterations                int
	cold_iterations           int
	placeholder_count         int
	compress_html             bool
	reload_modified_templates bool
	validate_each_iteration   bool
}

fn main() {
	config := BenchConfig{
		case_name:                 os.getenv('DTM2_BENCH_CASE')
		iterations:                env_int('DTM2_BENCH_ITERATIONS', 50000)
		cold_iterations:           env_int('DTM2_BENCH_COLD_ITERATIONS', 500)
		placeholder_count:         env_int('DTM2_BENCH_PLACEHOLDERS', 50)
		compress_html:             env_bool('DTM2_BENCH_COMPRESS_HTML', true)
		reload_modified_templates: env_bool('DTM2_BENCH_RELOAD_MODIFIED_TEMPLATES', false)
		validate_each_iteration:   env_bool('DTM2_BENCH_VALIDATE_EACH_ITERATION', false)
	}
	root := os.join_path(os.vtmp_dir(), '${bench_root_name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	setup_files(root, config)!
	defer {
		os.rmdir_all(root) or {}
	}

	print_config(root, config)
	small := small_placeholders()
	many := many_placeholders(config.placeholder_count)
	include := include_placeholders()

	if should_run(config, 'small_hot') {
		bench_hot('small_hot', root, 'small.html', small, config.iterations, config)
	}
	if should_run(config, 'small_cold') {
		bench_cold('small_cold', root, 'small.html', small, config.cold_iterations, config)
	}
	if should_run(config, 'many_hot') {
		bench_hot('many_hot', root, 'many.html', many, config.iterations, config)
	}
	if should_run(config, 'many_cold') {
		bench_cold('many_cold', root, 'many.html', many, config.cold_iterations, config)
	}
	if should_run(config, 'include_hot') {
		bench_hot('include_hot', root, 'with_include.html', include, config.iterations, config)
	}
	if should_run(config, 'include_cold') {
		bench_cold('include_cold', root, 'with_include.html', include, config.cold_iterations,
			config)
	}
	if should_run(config, 'xml_hot') {
		bench_hot('xml_hot', root, 'feed.xml', small, config.iterations, config)
	}
	if should_run(config, 'xml_cold') {
		bench_cold('xml_cold', root, 'feed.xml', small, config.cold_iterations, config)
	}
}

fn print_config(root string, config BenchConfig) {
	println('DTM2 benchmark')
	println('root: ${root}')
	println('case_name: ${if config.case_name == '' { 'all' } else { config.case_name }}')
	println('iterations: ${config.iterations}')
	println('cold_iterations: ${config.cold_iterations}')
	println('placeholder_count: ${config.placeholder_count}')
	println('compress_html: ${config.compress_html}')
	println('reload_modified_templates: ${config.reload_modified_templates}')
	println('validate_each_iteration: ${config.validate_each_iteration}')
	println('')
}

fn env_int(name string, default_value int) int {
	raw := os.getenv(name)
	if raw == '' {
		return default_value
	}
	value := raw.int()
	if value <= 0 {
		return default_value
	}
	return value
}

fn env_bool(name string, default_value bool) bool {
	raw := os.getenv(name).to_lower()
	if raw == '' {
		return default_value
	}
	if raw in ['1', 'true', 'yes', 'on'] {
		return true
	}
	if raw in ['0', 'false', 'no', 'off'] {
		return false
	}
	return default_value
}

fn should_run(config BenchConfig, label string) bool {
	return config.case_name == '' || config.case_name == 'all' || config.case_name == label
}

fn setup_files(root string, config BenchConfig) ! {
	os.mkdir_all(os.join_path(root, 'partials'))!
	os.write_file(os.join_path(root, 'small.html'),
		'<main>@title <p>@body</p><span>@count</span></main>')!
	mut many := strings.new_builder(config.placeholder_count * 30)
	many.writeln('<dl>')
	for i := 0; i < config.placeholder_count; i++ {
		many.writeln('<dt>${i}</dt><dd>@p_${i}</dd>')
	}
	many.writeln('</dl>')
	os.write_file(os.join_path(root, 'many.html'), many.str())!
	os.write_file(os.join_path(root, 'partials', 'nav.html'), '<nav>@title</nav>')!
	os.write_file(os.join_path(root, 'with_include.html'),
		'<header>@include "partials/nav"</header><main>@body</main>')!
	os.write_file(os.join_path(root, 'feed.xml'),
		'<feed><title>@title</title><entry>@body</entry><count>@count</count></feed>')!
}

fn small_placeholders() map[string]string {
	mut placeholders := map[string]string{}
	placeholders['title'] = 'Small'.clone()
	placeholders['body'] = 'Body <escaped>'.clone()
	placeholders['count'] = '42'
	return placeholders
}

fn many_placeholders(count int) map[string]string {
	mut placeholders := map[string]string{}
	for i := 0; i < count; i++ {
		placeholders['p_${i}'] = 'value ${i}'.clone()
	}
	return placeholders
}

fn include_placeholders() map[string]string {
	mut placeholders := map[string]string{}
	placeholders['title'] = 'Menu'.clone()
	placeholders['body'] = 'Body <escaped>'.clone()
	return placeholders
}

fn bench_hot(label string, root string, template_path string, placeholders map[string]string, iterations int, config BenchConfig) {
	mut manager := new_manager(root, config)
	expected := manager.expand(template_path, placeholders: &placeholders).clone()
	mut last_len := expected.len
	sw := time.new_stopwatch()
	for i in 0 .. iterations {
		rendered := manager.expand(template_path, placeholders: &placeholders)
		if config.validate_each_iteration {
			validate_rendered(label, i, expected, rendered)
		}
		last_len = rendered.len
	}
	elapsed := sw.elapsed()
	validate_rendered(label, iterations, expected, manager.expand(template_path,
		placeholders: &placeholders
	))
	print_result(label, iterations, elapsed, last_len, manager.compiled_template_count())
}

fn bench_cold(label string, root string, template_path string, placeholders map[string]string, iterations int, config BenchConfig) {
	mut expected_manager := new_manager(root, config)
	expected := expected_manager.expand(template_path, placeholders: &placeholders).clone()
	mut last_len := expected.len
	sw := time.new_stopwatch()
	for i in 0 .. iterations {
		mut manager := new_manager(root, config)
		rendered := manager.expand(template_path, placeholders: &placeholders)
		if config.validate_each_iteration {
			validate_rendered(label, i, expected, rendered)
		}
		last_len = rendered.len
	}
	elapsed := sw.elapsed()
	mut final_manager := new_manager(root, config)
	validate_rendered(label, iterations, expected, final_manager.expand(template_path,
		placeholders: &placeholders
	))
	print_result(label, iterations, elapsed, last_len, final_manager.compiled_template_count())
}

fn new_manager(root string, config BenchConfig) &dtm2.Manager {
	return dtm2.initialize(
		template_dir:              root
		compress_html:             config.compress_html
		reload_modified_templates: config.reload_modified_templates
	)
}

fn validate_rendered(label string, iteration int, expected string, rendered string) {
	if rendered != expected {
		eprintln('${label} invalid output at iteration ${iteration}: expected_len=${expected.len} actual_len=${rendered.len}')
		exit(1)
	}
}

fn print_result(label string, iterations int, elapsed time.Duration, last_len int, compiled_count int) {
	ns_per_op := if iterations > 0 { elapsed.nanoseconds() / i64(iterations) } else { i64(0) }
	ops_per_sec := if ns_per_op > 0 { 1_000_000_000.0 / f64(ns_per_op) } else { 0.0 }
	println('${label:-16} iterations=${iterations:8} total_ms=${elapsed.milliseconds():8} ns_per_op=${ns_per_op:10} ops_per_sec=${ops_per_sec:10.1f} last_len=${last_len:6} compiled_templates=${compiled_count}')
}
