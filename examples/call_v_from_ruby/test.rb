require 'bundler/inline'

gemfile do
  source 'https://rubygems.org'
  gem 'ffi'
end

require 'ffi'

module Lib
  extend FFI::Library

  ffi_lib File.join(File.dirname(__FILE__), 'test.so')

  attach_function :square, [:int], :int
  attach_function :sqrt_of_sum_of_squares, [:double, :double], :double
end

puts "Lib.square(10) result is #{Lib.square(10)}"
raise 'Cannot validate V square().' unless Lib.square(10) == 100

raise 'Cannot validate V sqrt_of_sum_of_squares().' unless \
  Lib.sqrt_of_sum_of_squares(1.1, 2.2) == Math.sqrt(1.1*1.1 + 2.2*2.2)
