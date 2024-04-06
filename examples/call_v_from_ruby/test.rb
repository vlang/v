require 'bundler/inline'

gemfile do
  source 'https://rubygems.org'
  gem 'ffi'
end

require 'ffi'

# extension for shared libraries varies by platform - see vlib/dl/dl.v
# get_shared_library_extension()
def shared_library_extension
  if Gem.win_platform?
    '.dll'
  elsif RUBY_PLATFORM =~ /darwin/ # MacOS
    '.dylib'
  else
    '.so'
  end
end

module Lib
  extend FFI::Library

  begin
    ffi_lib File.join(File.dirname(__FILE__), 'test' + shared_library_extension)
  rescue LoadError
    abort("No shared library test#{shared_library_extension} found. Check examples/call_v_from_ruby/README.md")
  end

  attach_function :square, [:int], :int
  attach_function :sqrt_of_sum_of_squares, [:double, :double], :double
end

puts "Lib.square(10) result is #{Lib.square(10)}"
raise 'Cannot validate V square().' unless Lib.square(10) == 100

raise 'Cannot validate V sqrt_of_sum_of_squares().' unless \
  Lib.sqrt_of_sum_of_squares(1.1, 2.2) == Math.sqrt(1.1*1.1 + 2.2*2.2)
