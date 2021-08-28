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
                         rotation     => 0,     %% keep no old output files
                         maxlen       => 1000   %% write current file up to 1000 bytes
                        },
                       #{
                         sup_id       => output2_sup,
                         handler_name => output2,
                         file         => "output2.log",
                         rotation     => 10,    %% keep up to 10 output files
                         maxdur       => 60     %% write to current file up to 60 seconds
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
       rotation => 0,
       maxlen   => 5000
     }).
```

### Remote a writer while running

Added writers can be removed while running:

```erlang
kakman:stop_writer(output3).
```
