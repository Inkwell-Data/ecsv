%%%-------------------------------------------------------------------
%% @doc ecsv public API
%% @end
%%%-------------------------------------------------------------------

-module(ecsv_app).
-author("Massimo Cesaro <massimo.cesaro@inkwelldata.com>").

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ecsv_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
