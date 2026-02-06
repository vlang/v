#!/usr/bin/env -S v run

import build
import time

// Define variables that can be used to change tasks in the buildscript
const app_name = 'hello'
const program_args = 'World'
const build_dir = 'target'

// Make the build context
mut context := build.context(
	// Set the default task to `release` when no arguments are provided
	default: 'release'
)

// Add a few simple tasks
context.task(name: 'doc', run: |self| system('echo "Nothing to do"'))
context.task(name: 'run', run: |self| system('v run . ${program_args}'))
context.task(name: 'build', run: |self| system('v .'))
context.task(name: 'build.prod', run: |self| system('v -prod -o ${app_name} .'))

// `_` to denote "private" tasks. Nothing stops the user from using it, but
// this tells them that the task is not meant to be used by them.
context.task(
	name: '_mkdirs'
	// The `help` field is displayed in `--tasks` to give a short summary of what the task does.
	help: 'Makes the directories used by the application'
	run:  fn (self build.Task) ! {
		if !exists(build_dir) {
			mkdir_all(build_dir) or { panic(err) }
		}
	}
)

// This task will only run when the `test.txt` file is outdated
context.artifact(
	name: 'test.txt'
	help: 'Generate test.txt'
	run:  fn (self build.Task) ! {
		write_file('test.txt', time.now().str())!
	}
)

// Add a more complex task
context.task(
	name:    'release'
	help:    'Build the app in production mode, generates documentation, and releases the build on Git'
	depends: ['_mkdirs', 'doc', 'test.txt']
	run:     fn (self build.Task) ! {
		system('v -prod -o ${build_dir}/${app_name} .')
		// Pretend we are using Git to publish the built file as a release here.
	}
)

// Run the build context. This will iterate over os.args and each corresponding
// task, skipping any arguments that start with a hyphen (-)
context.run()
