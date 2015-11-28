%%% @doc The {@link //basic_metrics} supervisor callback module.
%%% This is the top-level
%%% <a href="http://www.erlang.org/doc/design_principles/sup_princ.html">
%%% Erlang/OTP supervisor</a> for the Basic Metrics application.
%%% See the supervisor document for details of the callbacks defined here.
%%%
%%% Supervises periodic processes for metrics.
-module(basic_metrics_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

%% @doc Start the basic_metrics top-level supervisor.
%% Note that we pass empty `Args' to the init/1 callback.
%% @private
-spec start_link() ->
    {ok, pid()} |
    ignore |
    {error, Reason :: term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Initialize the basic_metrics top-level supervisor.
%% @private
-spec init(Args :: list()) ->
    {ok, {{RestartStrategy :: supervisor:strategy(),
           MaxR :: non_neg_integer(),
           MaxT :: non_neg_integer()},
          [supervisor:child_spec()]}} |
    ignore.
init(_Args) ->
    Periodic = {basic_metrics_periodic,
                {basic_metrics_periodic, start_link, []},
                permanent, infinity, worker, [basic_metrics_periodic]},
    {ok, {{one_for_one, 5, 10}, [Periodic]}}.
