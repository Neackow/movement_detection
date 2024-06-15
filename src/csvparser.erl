-module(csvparser).

-export([parse_CSV/2, parse/2, print_list/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% CSV : "../measures/Anav3_movement_detection@nav_1.csv"
% return a list of the column Index
% For nav_3 acceleration are in 3, 4, 5
parse_CSV(CSV, Index) ->
    Lines = parser(CSV),
    CL = clean_lines(Lines, Index),
    CL.

% Used by realtime.erl
% return a list of the column Index
% For nav_3 acceleration are in 1, 2, 3
parse(List, Index) ->
    CL = clean_lines(List, Index),
    CL.

% return a list of the lines
parser(CSV) ->
    {_, Data} = file:read_file(CSV),
    Rows = string:tokens(binary_to_list(Data), "\n"),
    Records = [string:tokens(Row, ",") || Row <- Rows],
    Records.

% Useful for debugging, print a list
print_list([H|T]) ->
    io:format("H : ~p~n", [H]),
    print_list(T);
print_list([]) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% return a list of the column Index
clean_lines(List, Index) ->
    clean_lines(List, Index, []).
clean_lines(List, Index, AccX) ->
    case List of 
        [] -> AccX;
        [H|T] ->
            Just_AccX_H  = lists:nth(Index, H),
            New_H = lists:append(AccX, [Just_AccX_H]),
            clean_lines(T, Index, New_H)
    end.