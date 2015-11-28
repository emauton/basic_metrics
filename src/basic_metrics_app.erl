%%% @doc The {@link //basic_metrics} application callback module.
%%% This is the entry point for the Basic Metrics
%%% <a href="http://www.erlang.org/doc/design_principles/applications.html">
%%% Erlang/OTP application</a>; see that document for details of the callbacks
%%% defined here.
%%%
%%% Starts the top-level application supervisor and initializes metrics.
%%% Cf. `basic_metrics.app.src'.
-module(basic_metrics_app).
-behaviour(application).
-export([start/2, stop/1]).

%% @private
-spec start(Type :: term(), Args :: term()) ->
    {ok, Pid :: pid()} |
    {error, Reason :: term()}.
start(_, _) ->
    ok = basic_metrics:init(),
    ok = basic_metrics:vm(),
    basic_metrics_sup:start_link().

%% @private
-spec stop(State :: term()) ->
    ok.
stop(_) ->
    ok.
