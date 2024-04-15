-module(sensor_fusion).

-behavior(application).

-export([set_args/1, set_args/4]).
-export([launch/0, launch_all/0, stop_all/0]).
-export([update_code/2, update_code/3]).
-export([start/2, stop/1]).
% Integrate the movement detection.
-export([realtime/0, realtime/2, realtime_once/0, realtime_once/1, clear_gesture/0]).


% Decide whether or not to print the comments. Remember to change it in your environment.
output_log(Message, Args) ->
    ShowLogs = application:get_env(hera, show_log, false), 
    if 
        ShowLogs -> 
            io:format(Message,Args);
        true -> 
            ok
    end.

output_log_spec(Message, Args) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:now_to_datetime(erlang:timestamp()),
    DisplayedTime = list_to_binary(io_lib:format("~.2.0w:~.2.0w:~.2.0w", [Hour, Min, Sec])),

    ShowLogs = application:get_env(hera, show_log_spec, false), 
    if
        ShowLogs -> 
            if Args == [] ->
                io:format("[~p]: ~p.~n",[DisplayedTime, Message]);
               true -> 
                FullMessage = "[~p]: " ++ Message,
                io:format(FullMessage, [DisplayedTime|Args])
            end;
        true -> 
            ok
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% set the args for the nav and mag modules
set_args(nav) ->
    Cn = nav:calibrate(),
    Cm = mag:calibrate(),
    update_table({{nav, node()}, Cn}),
    update_table({{mag, node()}, Cm});

%% set the args for the nav3 and e11 modules
%% Calibration function for the 3D orientation test.
set_args(nav3) ->
    Cn = nav3:calibrate(),
    R0 = e11:calibrate(element(3, Cn)),
    update_table({{nav3, node()}, Cn}),
    update_table({{e11, node()}, R0}).


%% set the args for the sonar module.
% Any measure above RangeMax [m] will be ignored.
% For e5 to e9, X and Y are the coordinate of the sonar in [m].
% For >= e10, X and Y are the Offset in [m] and Direction (1 or -1).
set_args(sonar, RangeMax, X, Y) ->
    update_table({{sonar, node()}, {RangeMax,X,Y}}).


launch() -> % Calling this function will lead to the launch of the correct function: either a nav or a sonar one. ADDED 2024: or an order board.
    case node_type() of
        order -> % If we have an "order" board, then do not try to launch the measurement.
            ok;
        _ ->
            try launch(node_type()) of
                ok -> % This will be sent if the calibration data is present. Otherwise, red leds.
                    [grisp_led:color(L, green) || L <- [1, 2]],
                    ok
            catch
                error:badarg ->
                    [grisp_led:color(L, red) || L <- [1, 2]],
                    {error, badarg}
            end
    end.


launch_all() ->
    rpc:multicall(?MODULE, launch, []). % rpc:multicall/3 is equivalent to multicall([node()|nodes()], Module, Function, Args, infinity).
% This will start the function 'launch' of the module sensor_fusion in every node connected to the network of boards.


stop_all() ->
    _ = rpc:multicall(application, stop, [hera]),
    _ = rpc:multicall(application, start, [hera]),
    ok.


%% to be called on the source node
update_code(Application, Module) ->
    {ok,_} = c:c(Module),
    {_,Binary,_} = code:get_object_code(Module),
    rpc:multicall(nodes(), ?MODULE, update_code,
        [Application, Module, Binary]).


%% to be called on the destination node
update_code(Application, Module, Binary) ->
    AppFile = atom_to_list(Application) ++ ".app",
    FullPath = code:where_is_file(AppFile),
    PathLen = length(FullPath) - length(AppFile),
    {Path,_} = lists:split(PathLen, FullPath),
    File = Path ++ atom_to_list(Module) ++ ".beam",
    ok = file:write_file(File, Binary),
    c:l(Module).


%%%%%%%%%%%%%%%%%% ADDING THE MOVEMENT DETECTION TO THE NUMERL VERSION OF sensor_fusion %%%%%%%%%%%%%

realtime() ->
    io:format("Start Realtime with 3 seconds between gesture, during 60 seconds~n",[]),
    realtime:start(loop, 2000, 60000).

realtime(Time, Period) ->
    if Period >= 0 ->
        io:format("Start Realtime with ~p seconds between gesture, during ~p seconds~n",[Time, Period]);
    true -> 
        io:format("Start Realtime with ~p seconds for the gesture, indefinitely~n",[Time]) % set Period to a negative number to loop indefinitely
    end,
    realtime:start(loop, Time * 1000, Period * 1000).

realtime_once() ->
    io:format("Start Realtime with 10 seconds for the gesture~n",[]),
    realtime:start(once, 10000, 0). % Last argument not used

realtime_once(Time) ->
    io:format("Start Realtime with ~p seconds for the gesture~n",[Time]),
    realtime:start(once, Time * 1000, 0). % Last argument not used

clear_gesture() ->
    file:write_file("sensor_fusion/lib/sensor_fusion-1.0.0/src/gesture", ""),
    io:format("Clear gesture~n",[]).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(_Type, _Args) ->
    {ok, Supervisor} = sensor_fusion_sup:start_link(),
    init_table(),
    case node_type() of
        nav ->              % This will be detected upon calling node_type().
            _ = grisp:add_device(spi2, pmod_nav); % Adds the device.
        sonar ->
            _ = grisp:add_device(uart, pmod_maxsonar),
            pmod_maxsonar:set_mode(single);
        order ->                                    % At start, launch the supervisor. It is independent of hera.
            _ = hera_sendOrder_sup:start_link();
        _ -> % needed when we use make shell
            _ = net_kernel:set_net_ticktime(8),
            lists:foreach(fun net_kernel:connect_node/1,
                application:get_env(kernel, sync_nodes_optional, []))
    end,
    _ = launch(),
    {ok, Supervisor}.


stop(_State) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

node_type() ->
    Host = lists:nthtail(14, atom_to_list(node())), % Returns the actual node, so here, sensor_fusion@nav_1. It returns AFTER the 14th element, meaning the @.
    IsNav = lists:prefix("nav", Host),              % Detects the nav in the name.
    IsSonar = lists:prefix("sonar", Host),
    IsOrder = lists:prefix("orderCrate", Host),
    if
        IsNav -> nav;
        IsSonar -> sonar;
        IsOrder -> order;   % Need to detect if the board is an order one.
        true -> undefined
    end.


launch(nav) -> % This is the function called by sensor_fusion:launch() when we are working with a nav.
    % Cn = ets:lookup_element(args, {nav, node()}, 2),
    % Cm = ets:lookup_element(args, {mag, node()}, 2),  
    Cn = ets:lookup_element(args, {nav3, node()}, 2),   % Takes the calibration data and puts it in a variable.
    R0 = ets:lookup_element(args, {e11, node()}, 2),
    % {ok,_} = hera:start_measure(nav, Cn),
    % {ok,_} = hera:start_measure(mag, Cm),

    % For debugging purposes.
    output_log("I'm calling hera:start_measure(nav3, Cn)~n",[]),

    {ok,_} = hera:start_measure(nav3, Cn),              % Starts a measure process from Hera. 

    % For debugging purposes.
    output_log("I'm calling hera:start_measure(e11, R0)~n",[]),

    {ok,_} = hera:start_measure(e11, R0),               
    ok;

launch(sonar) ->
    Cs = ets:lookup_element(args, {sonar, node()}, 2),
    {ok,_} = hera:start_measure(sonar, Cs),
    %{ok,_} = hera:start_measure(bilateration, undefined),
    % {ok,_} = hera:start_measure(e10, undefined),
    ok;

launch(_) ->
    ok.


init_table() ->
    args = ets:new(args, [public, named_table]),
    {ResL,_} = rpc:multicall(nodes(), ets, tab2list, [args]),
    L = lists:filter(fun(Res) ->
        case Res of {badrpc,_} -> false; _ -> true end end, ResL),
    lists:foreach(fun(Object) -> ets:insert(args, Object) end, L).


update_table(Object) ->
    _ = rpc:multicall(ets, insert, [args, Object]),
    ok.
