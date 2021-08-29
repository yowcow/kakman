-module(kakman).

-export([
         write/2
        ]).

-export_type([
              handler_name/0
             ]).

-type handler_name() :: term().

-spec write(kekman:handler_name(), binary()) -> ok.
write(HandlerName, Content) ->
    gen_server:cast(HandlerName, Content).
