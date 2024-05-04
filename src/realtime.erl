-module(realtime).

-export([start/3]).

% Set to 0 at initialisation. It is used to detect the end of the algorithm.
-record(counter, {value = 0}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Type, Maxtime, Period) ->
    [{_, _,StartTime,_}] = hera_data:get(nav3, sensor_fusion@nav_1),
    case Type of 
    once ->
        io:format("Countdown!~n"),
        Delay = 2,
        countdown(Delay),
        io:format("StartTime : ~p~n", [StartTime]),
        collect_data_over_time(StartTime + Maxtime + Delay*1000); % Delay*1000 for the countdown
    loop ->
        if Period < 0 ->
            grdos(Maxtime, Period); % Keep Period negative to loop indefinitely
        true ->
            grdos(Maxtime, Period + StartTime) % gesture_recognition_division_over_stop
        end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% For the First Method
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

collect_data_over_time(Maxtime) ->
    collect_data_over_time(Maxtime, [], 0).

collect_data_over_time(Maxtime, List, LastT) ->
    [{_, _,Time, Data}] = hera_data:get(nav3, sensor_fusion@nav_1), %[{_, _,Time, Data}]

    if Time > Maxtime ->
        io:format("Done!~nCalculating...~n"),
        classify:classify_new_gesture(List),
        learning(List); % Call the function to ask if the user want to learn the gesture
    true ->
        if Time == LastT ->
            collect_data_over_time(Maxtime, List, Time);
        true ->
            NewList = lists:append(List, [Data]),
            collect_data_over_time(Maxtime, NewList, Time)
        end
    end.

countdown(Count) ->
    case Count of
        0 ->
            io:format("Start!~n");
        _ ->
            io:format("~p~n", [Count]),
            timer:sleep(1000),
            countdown(Count-1)
    end.

learning(List) ->
    case io:get_line("Do you want to learn this gesture? (y/n/ENTER) : ") of
        "y\n" ->
            Name = io:get_line("What is the name of the gesture (use _ for space) : "),
            RemSlash = string:strip(Name, right, $\n),
            NameAtom = list_to_atom(RemSlash),
            learn:learn(List, NameAtom);
        "n\n" ->
            io:format("No learn ");
        "\n" ->
            io:format("No learn ");
        _ ->
            io:format("Unknown~n"),
            learning(List)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% For the Second Method
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

grdos(Maxtime, Period) ->
    AS = learn:av_size(),
    grdos(Maxtime, Period, AS, [], 0, [], 0, 0, x, x, x, Counter = #counter{}). % x for nothing


% ======================= <EXPLANATIONS FOR THE GRDOS FUNCTION> =======================
% TO(TimeOut) : time to stay without moving
% AS(Average Size) : Size of the List where we will do the average (it is too reduce the noise)
% List : List used to detect the stop
% SizeL : Size of the List
% GestureList: List of collected data since last gesture
% LastT : Time of the last data, used to detectif Hera produce a new data
% TSM(Time Since Move) : Last time a movement was detected, if greater than TO, then we have a stop
% LastX, LastY, LastZ : Last gesture for an axis
% grdos => gesture_recognition_division_over_stop
% ======================= </EXPLANATIONS FOR THE GRDOS FUNCTION> =======================

grdos(TO, Period, AS, List, SizeL, GestureList, LastT, TSM, LastX, LastY, LastZ, Counter) ->
    [{_, _,Time, Data}] = hera_data:get(nav3, sensor_fusion@nav_1), % hera_data:get answers back with the structure {Node, Seq, Timestamp, Data}
    if Time > Period andalso Period > 0 ->
        io:format("~n~n~n"), % Just to make it more readable
        io:format("End of Timer!~nCalculating...~n"),
        %classify:classify_new_gesture(GestureList); % Initially: classify the gesture with whatever data is present. Bad for me.
        % New version: at the end of the timer, automatically stop the crate.
        net_adm:ping(sensor_fusion@orderCrate), 
        rpc:call(sensor_fusion@orderCrate, sendOrder, set_state_crate, [stopCrate]);
    true ->
        if Time == LastT ->
            grdos(TO, Period, AS, List, SizeL, GestureList, Time, TSM, LastX, LastY, LastZ, Counter); % Skip if no new data
        true ->
            NewGestureList = lists:append(GestureList, [Data]),
            NewList = lists:append(List, [Data]),
            NewSizeL = SizeL + 1,
            if NewSizeL >= AS ->  % It means we can compute the average. AS = Average Size.
                ListX = csvparser:parse(NewList, 1),
                ListY = csvparser:parse(NewList, 2),
                ListZ = csvparser:parse(NewList, 3),
                PatternX = learn:analyze(ListX),
                PatternY = learn:analyze(ListY),
                PatternZ = learn:analyze(ListZ),
                AvgX = learn:average(PatternX),
                AvgY = learn:average(PatternY),
                AvgZ = learn:average(PatternZ),
                [HX|_] = AvgX,
                [HY|_] = AvgY,
                [HZ|_] = AvgZ,
                if LastX == HX andalso LastY == HY andalso LastZ == HZ -> % If the last gesture is the same as the new one
                    if Time >= TSM + TO ->
                        io:format("~n~n~n~n"), % Just to make it more readable
                        io:format("Stop detected!~n"),
                        {Name, Accuracy} = classify:classify_new_gesture(GestureList),
                        if Accuracy >= 0.7 ->
                            if Name == stopCrate ->
                                NewCounter = Counter#counter{value = Counter#counter.value + 1};
                            true ->
                                NewCounter = Counter#counter{value = 0}
                            end;
                        true ->
                            NewCounter = Counter
                        end,    
                        if NewCounter#counter.value == 3 -> % We need to do it on NewCounter, otherwise, it would take one useless step more to stop.
                            % We put the period at 1, so that it is immediately over at the next call. Reset the counter for good measure.
                            grdos(TO, 1, AS, [], 0, [], Time, Time, LastX, LastY, LastZ, CounterReset = #counter{value = 0}); 
                        true ->
                            grdos(TO, Period, AS, [], 0, [], Time, Time, LastX, LastY, LastZ, NewCounter)
                        end;
                    true -> % too soon, still need to wait
                        grdos(TO, Period, AS, [], 0, GestureList, Time, TSM, LastX, LastY, LastZ, Counter)
                    end;
                true ->
                    NewLastX = HX,
                    NewLastY = HY,
                    NewLastZ = HZ,
                    NewTSM = Time,
                    grdos(TO, Period, AS, [], 0, NewGestureList, Time, NewTSM, NewLastX, NewLastY, NewLastZ, Counter)
                end;
            true ->
                grdos(TO, Period, AS, NewList, NewSizeL, NewGestureList, Time, TSM, LastX, LastY, LastZ, Counter)
            end
        end
    end.