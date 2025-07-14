-module(rebar_elixir_uuid_demo_app).
-behaviour(application).
-export([start/2, stop/1]).
-export([generate_uuid/0]).

start(_StartType, _StartArgs) ->
    io:format("uuid v4: ~p~n", [generate_uuid()]),
    {ok, self()}.

stop(_State) ->
    ok.

generate_uuid() ->
    'Elixir.UUID':uuid4().

