foo(name, value) =
  let
     from = '$HOME/{ name }'
     to   = '/some/backup/{ from }'
  in [
    run       'cp { from } { to }'
    defer run 'rm { from }'
    expose    12345
    copy  'foo' to to
  ]