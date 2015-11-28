%%% @doc Basic VM metrics.
%%% A "plugin" for basic_metrics to export metrics from `riak' subsystems.
%%%
%%% This is for use in applications that themselves include `riak_api' or
%%% `riak_core' on the 2.x branch where exometer exports are available;
%%% `basic_metrics' does not depend on these applications.
-module(basic_metrics_riak).
-export([api/0, core/1]).
-ignore_xref([api/0, core/1]).

-define(INTERVAL, 5000).

%% @doc Initialize riak_api metrics.
%% Exports exometer statistics from `riak_api' to the graphite reporter.
%% <ul>
%% <li>`riak.riak_api.pbc_connects'
%%     <p>Number of protocol buffer connections.</p>
%% </li>
%% <li>`riak.riak_api.pbc_connects.active'
%%     <p>Number of protocol buffer connections currently active.</p>
%% </li>
%% </ul>
-spec api() ->
    ok.
api() ->
    % Counts of total protobuf API connections and those currently active.
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [riak, riak_api, pbc_connects],
                                   [count],
                                   ?INTERVAL, [], true),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [riak, riak_api, pbc_connects, active],
                                   [value],
                                   ?INTERVAL, [], true).

%% @doc Initialize riak_core metrics for `VNode', e.g. `test_vnode'.
%% Exports exometer statistics from `riak_core' to the graphite reporter.
%% See http://docs.basho.com/riak/latest/dev/references/http/status/ for
%% explations of each stat.
-spec core(VNode :: atom()) ->
    ok.
core(VNode) ->
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [riak, riak_core, ignored_gossip_total],
                                   [value],
                                   ?INTERVAL, [], true),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [riak, riak_core, rings_reconciled],
                                   [count],
                                   ?INTERVAL, [], true),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [riak, riak_core, gossip_received],
                                   [count],
                                   ?INTERVAL, [], true),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [riak, riak_core, handoff_timeouts],
                                   [value],
                                   ?INTERVAL, [], true),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [riak, riak_core, rejected_handoffs],
                                   [value],
                                   ?INTERVAL, [], true),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [riak, riak_core,
                                    dropped_vnode_requests_total],
                                   [value],
                                   ?INTERVAL, [], true),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [riak, riak_core, vnodes_running,
                                    VNode],
                                   [value],
                                   ?INTERVAL, [], true),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [riak, riak_core, converge_delay],
                                   [mean, 50, 75, 95, 99],
                                   ?INTERVAL, [], true),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [riak, riak_core, rebalance_delay],
                                   [mean, 50, 75, 95, 99],
                                   ?INTERVAL, [], true),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [riak, riak_core, vnodeq, VNode],
                                   [mean, median, min, max, total],
                                   ?INTERVAL, [], true),
    ok.
