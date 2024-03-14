-module(learn).

-export([to_file/1, learn/2, learn_CSV/2, analyze/1, analyze_CSV/1, regroup/1, average/1, av_size/0]).
-import(csvparser, [parse_CSV/2,  print_list/1]). % can also use csvparser:parse(..) instead of import
-define(AXIS, [x, y, z]).
-define(AV_SIZE, 50). % can be tuned depending on the quality of the results

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Called by realtime.erl
% add a new gesture to the gesture file
learn(List, Name) ->
    % learn for the 3 axis
    learn_axis(List, Name, lists:nth(1, ?AXIS)), %lists:nth(1, ?AXIS) = x
    learn_axis(List, Name, lists:nth(2, ?AXIS)),
    learn_axis(List, Name, lists:nth(3, ?AXIS)).

% CSV : "../measures/bf1.csv"
% example : learn:learn("../measures/bf1.csv", test).
% add a new gesture to the gesture file
learn_CSV(CSV, Name) ->
    % learn for the 3 axis
    learn_axis_CSV(CSV, Name, lists:nth(1, ?AXIS)),
    learn_axis_CSV(CSV, Name, lists:nth(2, ?AXIS)),
    learn_axis_CSV(CSV, Name, lists:nth(3, ?AXIS)).

% analyze the list of acc and determine the pattern FOR REALTIME
analyze(Vector) ->
    analyze(Vector, []).
analyze(Vector, Pattern) ->
    case Vector of
        [] -> Pattern;
        [H|T] ->
            % Rules of patterns
            % It is arbitrary values
            if H < -7 ->
                analyze(T, lists:append(Pattern, [nn])); % nn : negative high
            H < -4 ->
                analyze(T, lists:append(Pattern, [n])); % n : negative low
            H < -2 ->
                analyze(T, Pattern); % Guard Zone
            H > 7 ->
                analyze(T, lists:append(Pattern, [pp])); % pp : positive high
            H > 4 ->
                analyze(T, lists:append(Pattern, [p])); % p : positive low
            H > 2 ->
                analyze(T, Pattern); % Guard Zone
            true ->
                analyze(T, lists:append(Pattern, [o])) % o : zero
            end
    end.

% analyze_CSV the list of acc and determine the pattern
analyze_CSV(Vector) ->
    analyze_CSV(Vector, []).
analyze_CSV(Vector, Pattern) ->
    case Vector of
        [] -> Pattern;
        [H|T] ->
            % Rules of patterns
            {Int_H, _} = string:to_integer(H),
            % It is arbitrary values
            if Int_H < -7 ->
                analyze_CSV(T, lists:append(Pattern, [nn])); % nn : negative high
            Int_H < -4 ->
                analyze_CSV(T, lists:append(Pattern, [n])); % n : negative low
            Int_H < -2 ->
                analyze_CSV(T, Pattern); % Guard Zone
            Int_H > 7 ->
                analyze_CSV(T, lists:append(Pattern, [pp])); % pp : positive high
            Int_H > 4 ->
                analyze_CSV(T, lists:append(Pattern, [p])); % p : positive low
            Int_H > 2 ->
                analyze_CSV(T, Pattern); % Guard Zone
            true ->
                analyze_CSV(T, lists:append(Pattern, [o])) % o : zero
            end
    end.

% regroup the pattern to have the general flow
regroup(Pattern) ->
    regroup(Pattern, []).
regroup(Pattern, Flow) ->
    case Pattern of
        [] -> Flow;
        [H|T] ->
            if T == [] -> % If last element
                regroup(T, lists:append(Flow, [H]));
            true ->
                [HT|_] = T,
                Next = HT, % Head Tail
                if H == Next ->
                    regroup(T, Flow);
                true ->
                    regroup(T, lists:append(Flow, [H]))
                end
            end
    end.

% return a list of average value, each time over AV_SIZE data
average(List) ->
    average(List, ?AV_SIZE, []).
average(List, Size, New_L) ->
    if length(List) =< Size ->
        Av = calculate_av(List),
        lists:append(New_L, [Av]); % Return the list of average
    true ->
        Sub_List = lists:sublist(List, Size),
        Av = calculate_av(Sub_List),
        Next_L = lists:sublist(List, Size + 1, length(List)),
        average(Next_L, Size, lists:append(New_L, [Av]))
    end.

% export the gesture to the file
to_file(Gesture) ->
    file:write_file("sensor_fusion/lib/sensor_fusion-1.0.0/src/gesture", io_lib:fwrite("~p\n", [Gesture]), [append]).

% export AV_SIZE for others modules
av_size() ->
    ?AV_SIZE.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% hardcoded for :  nn, n, pp, p, zero
calculate_av(List) ->
    calculate_av(List, 0, 0, 0, 0, 0).
calculate_av(List, NegH, NegL, PosH, PosL, Zero) ->
    case List of
        [] ->
            if NegL > NegH, NegL > PosH, NegL > PosL, NegL > Zero ->
                n;
            PosL > NegH, PosL > PosH, PosL > NegL, PosL > Zero ->
                p;
            NegH > NegL, NegH > PosH, NegH > PosL, NegH > Zero ->
                nn;
            PosH > NegH, PosH > NegL, PosH > PosL, PosH > Zero ->
                pp;
            true ->
                o
            end;
        [H|T] ->
            case H of
                nn -> calculate_av(T, NegH + 1, NegL, PosH, PosL, Zero);
                n -> calculate_av(T, NegH, NegL + 1, PosH, PosL, Zero);
                pp -> calculate_av(T, NegH, NegL, PosH + 1, PosL, Zero);
                p -> calculate_av(T, NegH, NegL, PosH, PosL + 1, Zero);
                o -> calculate_av(T, NegH, NegL, PosH, PosL, Zero + 1)
            end
    end.

% Called by learn() for a specific axis
learn_axis(List, Name, Axis) ->
    case Axis of
        x ->
            Index = 1;
        y ->
            Index = 2;
        z ->
            Index = 3
    end,
    Vector = csvparser:parse(List, Index),
    Pattern = analyze(Vector),
    Clean_Pat = average(Pattern),
    Flow = regroup(Clean_Pat),
    Gesture = lists:append([Name, Axis], Flow),
    to_file(Gesture).

% Called by learn() for a specific axis
learn_axis_CSV(CSV, Name, Axis) ->
    case Axis of
        x ->
            Index = 3;
        y ->
            Index = 4;
        z ->
            Index = 5
    end,
    Vector = parse_CSV(CSV, Index),
    Pattern = analyze_CSV(Vector),
    Clean_Pat = average(Pattern),
    Flow = regroup(Clean_Pat),
    Gesture = lists:append([Name, Axis], Flow),
    to_file_CSV(Gesture).

% export the gesture to the file
to_file_CSV(Gesture) ->
    file:write_file("gesture", io_lib:fwrite("~p\n", [Gesture]), [append]).