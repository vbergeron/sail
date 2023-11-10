foo(arg) = 'some {arg}'

container baz from 'debian:latest' with
  run '{foo('bar')}'

