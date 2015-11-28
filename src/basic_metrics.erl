%%% @doc Basic VM metrics.
%%% Simple wrapper for the `exometer' metrics application to export useful
%%% metrics from an Erlang system.
%%% Handles setting up Graphite reporting and dynamic addition, subscription
%%% and updates for several exometer entry types.
-module(basic_metrics).
-export([init/0, counter/2, gauge/2, histogram/2, vm/0]).
-ignore_xref([init/0, counter/2, gauge/2, histogram/2, vm/0]).

-define(INTERVAL, 5000).

%% @doc Update a counter statistic.
%% If an exometer entry is not already present, create a counter and
%% subscribe to it with `exometer_report_graphite'.
-spec counter(Name :: exometer:name(), Value :: number()) ->
    ok.
counter(Name, Value) ->
    case exometer:update(Name, Value) of
        {error, not_found} ->
            exometer_admin:ensure(Name, counter, []),
            exometer_report:subscribe(exometer_report_graphite,
                                      Name, [value],
                                      ?INTERVAL, [], true),
            exometer:update(Name, Value);
        ok ->
            ok
    end.

%% @doc Update a gauge statistic.
%% If an exometer entry is not already present, create a gauge and
%% subscribe to it with `exometer_report_graphite'.
-spec gauge(Name :: exometer:name(), Value :: number()) ->
    ok.
gauge(Name, Value) ->
    case exometer:update(Name, Value) of
        {error, not_found} ->
            exometer_admin:ensure(Name, gauge, []),
            exometer_report:subscribe(exometer_report_graphite,
                                      Name, [value],
                                      ?INTERVAL, [], true),
            exometer:update(Name, Value);
        ok ->
            ok
    end.

%% @doc Update a histogram statistic.
%% If an exometer entry is not already present, create a histogram and
%% subscribe to it with exometer_report_graphite.
-spec histogram(Name :: exometer:name(), Value :: number()) ->
    ok.
histogram(Name, Value) ->
    case exometer:update(Name, Value) of
        {error, not_found} ->
            exometer_admin:ensure(Name, histogram,
                                  [{module, exometer_histogram}]),
            exometer_report:subscribe(exometer_report_graphite,
                                      Name, [mean, 50, 75, 95, 99],
                                      ?INTERVAL, [], true),
            exometer:update(Name, Value);
        ok ->
            ok
    end.

%% @doc Initialize exometer with Graphite reporting.
-spec init() ->
    ok.
init() ->
    {ok, Name} = inet:gethostname(),
    Host = application:get_env(basic_metrics, host,
                               "carbon.hostedgraphite.com"),
    Port = application:get_env(basic_metrics, port, 2003),
    Key = application:get_env(basic_metrics, key, ""),
    Prefix = application:get_env(basic_metrics, prefix, "basic"),
    ReportOptions = [{connect_timeout, 5000},
                     {prefix, Prefix ++ "." ++ Name},
                     {host, Host},
                     {port, Port},
                     {api_key, Key}],
    ok = exometer_report:add_reporter(exometer_report_graphite, ReportOptions).

%% @doc Initialize basic VM metrics.
%% The following metrics are exported:
%% <ul>
%% <li>erlang:memory/1:
%%     `erlang.memory.{total, processes, system, atom, binary, ets}'
%%     <p>VM memory.
%%     <ul>
%%      <li>total = processes + system.</li>
%%      <li>processes = used by Erlang processes, their stacks and heaps.</li>
%%      <li>system = used but not directly related to any Erlang process.</li>
%%      <li>atom = allocated for atoms (included in system).</li>
%%      <li>binary = allocated for binaries (included in system).</li>
%%      <li>ets = allocated for ETS tables (included in system).</li>
%%     </ul>
%%     </p>
%% </li>
%% <li>recon_alloc:memory/1:
%%     `recon.alloc.memory.{used, allocated, unused, usage}'
%%     <p>Memory actively used by the VM, allocated (should ~match OS
%%     allocation), unused (i.e. allocated - used), and usage
%%     (used / allocated).</p>
%% </li>
%% <li>recon_alloc:memory(allocated_types):
%%     `recon.alloc.memory.types.{binary_alloc, ...}'
%%     <p>Memory reserved by the VM, grouped into different utility
%%     allocators.</p>
%% </li>
%% <li>erlang:system_info/1:
%%     `erlang.system.{process_count, port_count}'
%%     <p>System process and port counts.
%%     <ul>
%%       <li>process_count = current number of processes.</li>
%%       <li>port_count = current number of ports.</li>
%%     </ul>
%%     </p>
%% </li>
%% <li>erlang:statistics/1:
%%     `erlang.statistics.run_queue',
%%     `erlang.gc.{total_coll, rec_wrd}',
%%     `erlang.io.{input, output}'
%%     <p>VM statistics.
%%     <ul>
%%      <li>The number of processes that are ready to run on all available run
%%          queues.</li>
%%      <li>Total garbage collections and words reclaimed.</li>
%%      <li>Total bytes input and output through ports.</li>
%%     </ul>
%%     </p>
%% </li>
%% </ul>
-spec vm() ->
    ok.
vm() ->
    % VM memory.
    ok = exometer:new([erlang, memory],
                      {function, erlang, memory, ['$dp'], value,
                       [total, processes, system, atom, binary, ets]}),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [erlang, memory],
                                   [total, processes, system, atom, binary,
                                    ets], ?INTERVAL, [], true),

	% Recon alloc.
    ok = exometer:new([recon, alloc],
                      {function, recon_alloc, memory, ['$dp'], value,
                       [used, allocated, unused, usage]}),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [recon, alloc],
                                   [used, allocated, unused, usage], ?INTERVAL,
                                   [], true),

	% Recon alloc types.
    ok = exometer:new([recon, alloc, types],
                      {function, recon_alloc, memory,
                       [allocated_types], proplist,
                       [binary_alloc, driver_alloc, eheap_alloc,
                        ets_alloc, fix_alloc, ll_alloc, sl_alloc,
                        std_alloc, temp_alloc]}),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [recon, alloc, types],
                                   [binary_alloc, driver_alloc, eheap_alloc,
                                    ets_alloc, fix_alloc, ll_alloc, sl_alloc,
                                    std_alloc, temp_alloc], ?INTERVAL,
                                   [], true),

	% System process & port counts.
    ok = exometer:new([erlang, system],
                      {function, erlang, system_info, ['$dp'], value,
                       [process_count, port_count]}),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [erlang, system],
                                   [process_count, port_count], ?INTERVAL,
                                   [], true),

	% VM statistics.
    ok = exometer:new([erlang, statistics],
                      {function, erlang, statistics, ['$dp'], value,
                       [run_queue]}),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [erlang, statistics],
                                   [run_queue], ?INTERVAL, [], true),

    ok = exometer:new([erlang, gc],
                      {function, erlang, statistics, [garbage_collection],
                       match, {total_coll, rec_wrd, '_'}}),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [erlang, gc],
                                   [total_coll, rec_wrd], ?INTERVAL, [], true),

    ok = exometer:new([erlang, io],
                      {function, erlang, statistics, [io], match,
                       {{'_', input}, {'_', output}}}),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [erlang, io],
                                   [input, output], ?INTERVAL, [], true).
