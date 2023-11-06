module lib <- 'lib.sail'

container baz from 'debian:latest' with [
  lib:foo('a', 'b')
]
