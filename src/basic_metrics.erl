%%% @doc A basic VM metrics exporter.
%%% Configures and exports metrics to
%%% <a href="https://www.hostedgraphite.com/">Hosted Graphite</a> using
%%% <a href="https://github.com/Feuerlabs/exometer">exometer</a>.
%%%
%%% Include the `basic_metrics' library application as a dependency and
%%% initialize it with
%%%
%%% `ok = basic_metrics:init("myapp", "my-hostedgraphite-api-key")'
%%%
%%% Rather than using exometer's static configuration options, we build
%%% everything dynamically here. It's a little verbose as a result, but clear
%%% and self-contained. The exports you see here should also provide examples
%%% for your own application metrics.
%%%
%%% This module depends upon and uses
%%% <a href="https://github.com/ferd/recon">recon</a> for additional metrics
%%% useful in production. See also
%%% <a href="http://www.erlang-in-anger.com/">Erlang in Anger</a>.
%%%
%%% The exometer integration is based on Brian Troutwine's notes in
%%% <a href="http://goo.gl/Xo4fWi">
%%% Monitoring with Exometer: An Ongoing Love Story</a>. For lots more detail
%%% on using exometer, see that article and the exometer documentation.
-module(basic_metrics).
-export([init/2]).

-define(INTERVAL, 5000).

%% @doc Setup basic VM metrics reporting to Hosted Graphite.
%% <ul>
%% <li>`Root' forms the first component of your
%%     <a href="http://docs.hostedgraphite.com/#metric-data-format">
%%     metric name hierarchy</a>, e.g.
%%     `<Root>.<hostname>.erlang.statistics.run_queue'. Typically this will be
%%     your application or service name.</li>
%% <li>`ApiKey' is your <a href="http://docs.hostedgraphite.com/#api-key">
%%     Hosted Graphite API key</a>.</li>
%% </ul>
%%
%% The following metrics are exported:
%% <ul>
%% <li>erlang:memory/1:
%%     `erlang.memory.{total, processes, system, atom, binary, ets}'</li>
%% <li>recon_alloc:memory/1:
%%     `recon.alloc.memory.{used, allocated, unused, usage}'</li>
%% <li>recon_alloc:memory(allocated_types):
%%     `recon.alloc.memory.types.{binary_alloc, ...}'</li>
%% <li>erlang:system_info/1:
%%     `erlang.system.{process_count, port_count}'</li>
%% <li>erlang:statistics/1:
%%     `erlang.statistics.run_queue'</li>
%% </ul>
-spec init(Root::string(), ApiKey::string()) ->
    ok.
init(Root, ApiKey) ->
    {ok, Host} = inet:gethostname(),
    ReportOptions = [{connect_timeout, 5000},
                     {prefix, Root ++ "." ++ Host},
                     {host, "carbon.hostedgraphite.com"},
                     {port, 2003},
                     {api_key, ApiKey}],
    ok = exometer_report:add_reporter(exometer_report_graphite, ReportOptions),

    % VM memory.
    % total = processes + system.
    % processes = used by Erlang processes, their stacks and heaps.
    % system = used but not directly related to any Erlang process.
    % atom = allocated for atoms (included in system).
    % binary = allocated for binaries (included in system).
    % ets = allocated for ETS tables (included in system).
    ok = exometer:new([erlang, memory],
                      {function, erlang, memory, ['$dp'], value,
                       [total, processes, system, atom, binary, ets]}),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [erlang, memory],
                                   [total, processes, system, atom, binary,
                                    ets], ?INTERVAL),

    % Memory actively used by the VM, allocated (should ~match OS allocation),
    % unused (i.e. allocated - used), and usage (used / allocated).
    ok = exometer:new([recon, alloc],
                      {function, recon_alloc, memory, ['$dp'], value,
                       [used, allocated, unused, usage]}),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [recon, alloc],
                                   [used, allocated, unused, usage], ?INTERVAL),

    % Memory reserved by the VM, grouped into different utility allocators.
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
                                    std_alloc, temp_alloc], ?INTERVAL),

    % The time percentage each scheduler has been running processes, NIFs,
    % BIFs, garbage collection, etc. versus time spent idling or trying to
    % schedule processes.
    Schedulers = lists:seq(1, erlang:system_info(schedulers)),
    ok = exometer:new([recon, scheduler, usage],
                      {function, recon, scheduler_usage, [1000], proplist, 
                       Schedulers},
                      [{cache, 5000}]),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [recon, scheduler, usage],
                                   Schedulers, ?INTERVAL),

    % process_count = current number of processes.
    % port_count = current number of ports.
    ok = exometer:new([erlang, system],
                      {function, erlang, system_info, ['$dp'], value,
                       [process_count, port_count]}),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [erlang, system],
                                   [process_count, port_count], ?INTERVAL),

    % The number of processes that are ready to run on all available run queues.
    ok = exometer:new([erlang, statistics],
                      {function, erlang, statistics, ['$dp'], value,
                       [run_queue]}),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [erlang, statistics],
                                   [run_queue], ?INTERVAL),

    % TODO: add erlang:statistics(garbage_collection) and
    %           erlang:statistics(io) exports;
    % Small helper functions will do the trick.

    ok.
