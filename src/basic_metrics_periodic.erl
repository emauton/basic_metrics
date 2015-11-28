%%% @doc Periodic monitoring for basic_metrics.
%%%
%%% A simple `gen_server' that periodically updates exometer metrics with
%%% metrics for the node's
%%% <a href="http://ferd.github.io/recon/recon.html#scheduler_usage-1">
%%% scheduler utilization</a>:
%%% <ul>
%%% <li>recon:scheduler_usage/1:
%%%     `recon.scheduler.usage.<schedulerid>'
%%%     <p>The time percentage each scheduler has been running processes, NIFs,
%%%     BIFs, garbage collection, etc. versus time spent idling or trying to
%%%     schedule processes.</p>
%%% </li>
%%% </ul>
%%% and
%%% <a href="http://www.erlang.org/doc/reference_manual/distributed.html">
%%% Erlang distribution</a> sockets:
%%% <ul>
%%% <li>recon:port_info/2:
%%%     `dist.{recv,send}_cnt.<dist_port>', `dist.{recv,send}_oct.<dist_port>'
%%%     <p>Packet counts and bytes, sent and received, per distribution port.
%%%     </p>
%%% </li>
%%% </ul>
-module(basic_metrics_periodic).
-behaviour(gen_server).

%%% API.
-export([start_link/0]).
-ignore_xref([start_link/0]).

%%% gen_server callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%% API implementation.

%% @doc Start the periodic monitor.
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%% gen_server callback implementation.

-record(state, {
                interval :: pos_integer()
               }).
-type state() :: #state{}.

%% @doc Init monitoring interval and loop.
%% @private
-spec init(Args :: term()) ->
    {ok, State :: state(), Timeout :: timeout()}.
init([]) ->
    Interval = 5000,
    lager:info("~p init with interval ~p", [?MODULE, Interval]),
    {ok, #state{interval = Interval}, Interval}.

%% @doc Record metrics and loop.
%% @private
-spec handle_info(timeout, State :: state()) ->
    {noreply, State :: state(), Timeout :: timeout()}.
handle_info(timeout, #state{interval=Interval}=State) ->
    % Scheduler utilization monitoring.
    Usage = recon:scheduler_usage(1000),
    lists:foreach(fun export_scheduler_usage/1, Usage),

    % Distribution port monitoring.
    Ports = [{node_to_exometer_component(N), get_port_data(P)} ||
                {N, P} <- erlang:system_info(dist_ctrl)],
    lists:foreach(fun export_port_data/1, Ports),
    {noreply, State, Interval}.

%% @doc handle_call/3 - unused.
%% @private
-spec handle_call(Request :: term(), From :: term(), State :: state()) ->
    {reply, ok, State :: state()}.
handle_call(_, _, State) ->
    {reply, ok, State}.

%% @doc handle_cast/2 - unused.
%% @private
-spec handle_cast(Msg :: term(), State :: state()) ->
    {noreply, State :: state()}.
handle_cast(_, State) ->
    {noreply, State}.

%% @doc terminate/2 - unused.
%% @private
-spec terminate(Reason :: term(), State :: state()) ->
    ok.
terminate(_, _) ->
    ok.

%% @doc code_change/3 - unused.
%% @private
-spec code_change(Old :: term(), State :: state(), Extra :: term()) ->
    {ok, State :: state()}.
code_change(_, State, _) ->
    {ok, State}.

%%% Support functions.

%%% Functions supporting scheduler utilization monitoring.
export_scheduler_usage({SchedulerId, Usage}) ->
   basic_metrics:gauge([recon, scheduler, usage, SchedulerId], Usage).

%%% Functions supporting distribution port monitoring.

%% Convert a node name atom (e.g. node1@127.0.0.1) to a form suitable
%% for use in a exometer metric name (e.g. node1_127_0_0_1).
node_to_exometer_component(Node) when is_atom(Node) ->
    node_to_exometer_component(atom_to_list(Node), []).

node_to_exometer_component([], Acc) ->
    % Only so many entries in nodes(), so this conversion is "not that dynamic".
    list_to_atom(lists:reverse(Acc));
node_to_exometer_component([H | T], Acc) when H =:= $@;
                                              H =:= $.;
                                              H =:= $- ->
    node_to_exometer_component(T, [$_ | Acc]);
node_to_exometer_component([H | T], Acc) ->
    node_to_exometer_component(T, [H | Acc]).

%% Pull the port information we want to export to exometer out of recon.
get_port_data(Port) ->
   {memory_used, Memory} = recon:port_info(Port, memory_used),
   {_, PortData} = recon:port_info(Port, specific),
   {statistics, Statistics} = lists:keyfind(statistics, 1, PortData),
   Memory ++ lists:filter(fun statistics_filter/1, Statistics).

%% Filter for port statistics we want to export.
statistics_filter({recv_cnt, _}) -> true;
statistics_filter({recv_oct, _}) -> true;
statistics_filter({send_cnt, _}) -> true;
statistics_filter({send_oct, _}) -> true;
statistics_filter(_) -> false.

%% Export port statistics to exometer.
export_port_data({Node, Data}) ->
   lists:foreach(fun export_port_datum/1, [{Node, Datum} || Datum <- Data]).

%% Export a single port statistic to exometer.
export_port_datum({Node, {Key, Value}}) ->
   basic_metrics:gauge([dist, Key, Node], Value).
