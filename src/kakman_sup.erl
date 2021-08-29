%%%-------------------------------------------------------------------
%% @doc kakman top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(kakman_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-type handler_configs() :: [handler_config()].
-type handler_config() :: #{
                            sup_id       := term(),
                            handler_name := term(),
                            file         := string(),
                            rotations    := integer(),
                            maxage       => integer()
                           }.

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{
                 strategy  => one_for_one,
                 intensity => 5,
                 period    => 1
                },
    ChildSpecs = child_specs(
                   application:get_env(kakman, log_root, "log"),
                   application:get_env(kakman, handlers, [])
                  ),
    {ok, {SupFlags, ChildSpecs}}.

-spec child_specs(string(), handler_configs()) -> [supervisor:child_spec()].
child_specs(LogRoot, Configs) ->
    [
     #{
       id => SupID,
       start => {
         kakman_worker,
         start,
         [
          HandlerName,
          create_config(
            [filepath, rotations, maxage],
            LogRoot,
            Config,
            #{}
           )
         ]
        },
       restart => permanent,
       type => worker,
       modules => [kakman_worker]
      } || #{
             sup_id       := SupID,
             handler_name := HandlerName
            } = Config <- Configs
    ].

create_config([], _, _, Acc) ->
    Acc;
create_config(
  [filepath|T],
  LogRoot,
  #{ file := File } = Config,
  Acc
 ) ->
    create_config(
      T,
      LogRoot,
      Config,
      Acc#{ filepath => filename:absname(File, LogRoot) }
     );
create_config(
  [K|T],
  LogRoot,
  Config,
  Acc0
 ) ->
    Acc = case maps:find(K, Config) of
              {ok, V} ->
                  Acc0#{ K => V };
              _ ->
                  Acc0
          end,
    create_config(
      T,
      LogRoot,
      Config,
      Acc
     ).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

child_specs_test_() ->
    Cases = [
             {
              "empty input",
              {"logs", []},
              []
             },
             {
              "with a config",
              {"logs", [
                        #{
                          sup_id => hoge_sup,
                          handler_name => hoge,
                          file => "hoge.log",
                          rotations => 0,
                          maxage => 60
                         }
                       ]},
              [
               #{
                 id => hoge_sup,
                 start => {
                   kakman_worker,
                   start,
                   [
                    hoge,
                    #{
                      filepath => "logs/hoge.log",
                      rotations => 0,
                      maxage => 60
                     }
                   ]
                  },
                 restart => permanent,
                 type => worker,
                 modules => [kakman_worker]
                }
              ]
             }
            ],
    F = fun({Title, {LogRoot, Configs}, Expected}) ->
                Actual = child_specs(LogRoot, Configs),
                {Title, ?_assertEqual(Expected, Actual)}
        end,
    lists:map(F, Cases).

-endif.
