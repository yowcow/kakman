-module(kakman_worker).

-behavior(gen_server).

-export([
         start/2,
         stop/1
        ]).

-export([
         init/1,
         handle_cast/2,
         handle_call/3,
         terminate/2
        ]).

-type config() :: #{
                    filepath  := string(),
                    rotations := integer(),
                    maxage    := integer()
                   }.

-type state() :: #{
                   name      := term(),
                   filepath  := string(),
                   rotations := integer(),
                   fd        := file:io_device(),
                   wlen      := integer(),
                   maxage    := integer(),
                   t0        := integer()
                  }.

-type msg() :: binary() | string().

-type reason() :: normal | shutdown | {shudown, term()} | term().

-spec start(term(), config()) -> {ok, pid()}.
start(HandlerName, Config) ->
    gen_server:start_link(
      {local, HandlerName},
      ?MODULE,
      {HandlerName, Config},
      []
     ).

-spec stop(term()) -> ok.
stop(HandlerName) ->
    gen_server:stop(HandlerName).

-spec init({term(), config()}) -> {ok, state()}.
init(
  {HandlerName,
   #{
     filepath  := FilePath,
     rotations := Rotations,
     maxage   := MaxAge
    } = Config
  }
 ) ->
    logger:info("(~p) init: ~p", [?MODULE, Config]),
    {ok, Fd, Wlen} = case kakman_file:open(FilePath) of
                         {error, Err} ->
                             throw({file_open_failed, Err});
                         Ret ->
                             Ret
                     end,
    {ok, #{
           name      => HandlerName,
           filepath  => FilePath,
           rotations => Rotations,
           fd        => Fd,
           wlen      => Wlen,
           maxage    => MaxAge,
           t0        => epoch()
          }}.

-spec handle_cast(msg(), state()) -> {noreply, state()}.
handle_cast(Msg, State0) ->
    logger:notice("(~p) handle_cast: ~p (state: ~p)", [?MODULE, Msg, State0]),
    {ok, State} = write_log(Msg, State0),
    {noreply, State}.

-spec handle_call(msg(), pid(), state()) -> {reply, term(), state()}.
handle_call(Msg, From, State0) ->
    logger:notice("(~p) handle_call: ~p from ~p (state: ~p)", [?MODULE, Msg, From, State0]),
    {ok, State} = write_log(Msg, State0),
    {reply, {ok, From}, State}.

-spec terminate(reason(), state()) -> ok.
terminate(_, #{ fd := Fd }) ->
    kakman_file:close(Fd).

-spec epoch() -> integer().
epoch() ->
    {Mega, Sec, _} = erlang:timestamp(),
    Mega*1000000 + Sec.

-spec write_log(msg(), state()) -> {ok, state()}.
write_log(
  Msg,
  #{
    maxage := MaxAge,
    t0     := T0
   } = State
 ) ->
    T1 = epoch(),
    #{
      fd := Fd,
      wlen := Wlen
     }
    = NextState
    = case (T0 + MaxAge) < T1 of
          true ->
              #{
                fd := Fd0,
                filepath  := FilePath,
                rotations := Rotations
               } = State,
              {ok, Fd1, Wlen1} = case kakman_file:rotate(Fd0, FilePath, Rotations) of
                                           {error, Err} ->
                                               throw({file_rotation_failed, Err});
                                           Ret ->
                                               Ret
                                       end,
              State#{
                fd => Fd1,
                wlen => Wlen1,
                t0 => T1
               };
          _ ->
              State
      end,
    {ok, Len} = kakman_file:write(Fd, Msg),
    {ok, NextState#{
           wlen => Wlen + Len
          }}.
