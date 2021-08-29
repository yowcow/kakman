kakman
=========

An OTP application for writing things into files.


BUILD
-----

    $ rebar3 compile


HOW TO USE
----------

### Configure

Have a configuration in sys.config or similar:

```erlang
[
 {kakman, [{log_root, "logs"},
           {handlers, [#{
                         sup_id       => output1_sup,
                         handler_name => output1,
                         file         => "output1.log",
                         rotations    => 5,     %% keep 5 generations
                         maxage       => 60     %% write for 60 seconds, then rotate
                        }
                      ]}
          ]}
].
```

### Write to file

Start `kakman` application, and write to file:

```erlang
kakman:write(output1, <<"binary output">>).
kakman:write(output1, <<"UTF8での出力"/utf8>>).
kakman:write(output1, "list output").
```

### Add a writer while running

Writers can be added while running:

```erlang
kakman:start_writer(
    output3,
    #{ filepath => "logs/output3.log",
       rotations => 0,
       maxage => 10
     }).
```

### Remote a writer while running

Added writers can be removed while running:

```erlang
kakman:stop_writer(output3).
```


WORK IN PROGRESS
----------------

* Testing
* Rotation by written byte length
* Delayed write
* Documentation
