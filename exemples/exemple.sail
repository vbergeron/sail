foo(arg): 
  'some {arg}'

build: container from 'debian:latest':
  run '{foo('bar')}'

