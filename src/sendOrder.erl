% Nicolas Isenguerre, 13/04/2024. 

% Module to convert gestures to orders on an object. 
% This V1.0 offers the ability to control a wine crate on wheels, with gestures associated to defined orders.
% This is called by the set_state_crate(MovementDetected), which calls the 'ctrlCrate' version of the handle_call function.
% Extensions of this code are straightforward: either do your own functions and redirect the orders to it, or extend the handle_call procedures
% for more functionalities. E.g.: if the PMOD Ultrawideband comes out, a follower function can be added.
% This could be called via gen_server:call(?MODULE, {follower, WhateverArgument}) and redirect to an appropriate handle_call procedure.

-module(sendOrder).

-behaviour(gen_server).


-export([start_link/0, set_state_crate/1, checkingConnection/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

% At initialisation (see init/1 function): currentVelocity = 100 RPM (in forward direction). 
% prevName: used to forbid unauthorised gesture succession. Set at stopCrate, by default.
% movName: store the movement name as detected by the GRiSP2 board. 
% movMode: this is to deal with submodes. E.g.: if I want to be in 'changeVelocity' mode, then it stores it in this variable. 
% By default: stopCrate and normal, normal being the 'by default' mode.
-record(movState, {currentVelocity, prevName, movName, movMode}).

% At initialisation, this counter is set to 0 and the function verifying that the other node is connected is launched.
-record(counter, {value = 0}).


% =======================================================================
% ========================= <public functions> ==========================
% =======================================================================

start_link() ->
    % Initiates the gen_server. Calls init/1.
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Synchronous call: sets the new state according to the detected movement. This function is called by rpc:call on the other GRiSP2. 
set_state_crate(MovementDetected) -> 
    if(MovementDetected == testRPC) -> % To test the good connection between the two GRiSPs. Useless otherwise. Manual command.
        grisp_led:color(1,white),
        grisp_led:color(2,black);
        true ->
            ok
    end,
    gen_server:call(?MODULE, {ctrlCrate, MovementDetected}).

% Checking if the other board is still answering and available to send orders. This is a safety measure, a guard-rail.
% Has to be public in order to be called by apply_after.
% Also, in parallel, "ping" the Raspberry board to tell it the receiver is still alive.
checkingConnection(Counter) ->
    Connected = net_adm:ping(movement_detection@nav_1),
    if Connected == pang ->
        NewCounter = Counter#counter{value = Counter#counter.value + 1};
    true ->
        NewCounter = Counter#counter{value = 0}
    end,
    
    if NewCounter#counter.value == 1 ->
        i2c_communication:send_data([0,1,0,0,2]), % Stop the crate.
        FinalCounter = Counter#counter{value = 0};
    true ->
        FinalCounter = NewCounter
    end,
    i2c_communication:send_data([1]), % We send a 1, which will be added to a counter in the Rasp.
    timer:apply_after(1000, sendOrder, checkingConnection, [FinalCounter]).

% =======================================================================
% ========================= </public functions> =========================
% =======================================================================


% =======================================================================
% ========================= <private functions> =========================
% =======================================================================

% Structure of Order: [V1, DIR1, V2, DIR2, command_index].
% The command indicator allows the controller to know if it's turning, simply moving forward, etc.
% This could have been simply implemented within the controller by comparing velocities, etc. but since it was needed in changeVelocity mode to try it out,
% it may aswell be reused for simplicity.
% command_index = 0 -> continuous mode, smooth position profile ; 1 -> test velocity in changeVelocity mode ; 2 -> crate-on-wheels stopping ; 3 -> turning around.
order_crate(State) ->
    Order = case State#movState.movMode of
        changeVelocity ->
            case State#movState.movName of
                stopCrate ->   % Stop the crate from whatever it was doing. Reset values to baseline.
                    NewState = State#movState{currentVelocity = 100},
                    [0,1,0,0,2];
                accelerate ->   % When we are changing the velocity, do not move the crate, by default. Just change the state.
                    if State#movState.currentVelocity == 100 ->
                        NewState = State#movState{currentVelocity = 110};
                    State#movState.currentVelocity == 110 ->
                        NewState = State#movState{currentVelocity = 120};
                    State#movState.currentVelocity >= 120 ->
                        io:format("Cannot accelerate further!~n"),
                        NewState = State#movState{currentVelocity = 120};
                    true ->
                        NewState = State#movState{currentVelocity = State#movState.currentVelocity}
                    end,
                    [0,1,0,0,0];
                decelerate ->
                    if State#movState.currentVelocity == 120 ->
                        NewState = State#movState{currentVelocity = 110};
                    State#movState.currentVelocity == 110 ->
                        NewState = State#movState{currentVelocity = 100};
                    State#movState.currentVelocity =< 100 -> % Just in case the sun wants to play with me.
                        io:format("Cannot decelerate further!~n"),
                        NewState = State#movState{currentVelocity = 100};
                    true ->
                        NewState = State#movState{currentVelocity = State#movState.currentVelocity}
                    end,
                    [0,1,0,0,0];
                testingVelocity ->
                    NewState = State,
                    [State#movState.currentVelocity,1,State#movState.currentVelocity,0,1];
                _ ->
                    NewState = State,
                    io:format("Invalid command when in changeVelocity mode.~n"),
                    [0,1,0,0,2] % When order is invalid, automatically set to 0. Send to "slow down to 0", we stop the crate.
            end;
        normal ->
            case State#movState.movName of
                stopCrate ->   % Default command, crate does not move.
                    NewState = State#movState{prevName = stopCrate},
                    [0,1,0,0,2];
                forward ->
                    NewState = State#movState{prevName = forward},
                    [State#movState.currentVelocity,1,State#movState.currentVelocity,0,0];
                backward ->
                    NewState = State#movState{prevName = backward},
                    [State#movState.currentVelocity,0,State#movState.currentVelocity,1,0];
                forwardTurnLeft ->
                    NewState = State#movState{prevName = forward},
                    [80,1,110,0,0]; % When turning, we stay in "continuous" mode, an a dedicated function will adapt the velocities.
                    % The velocites are fixed. This is a design choice, to have a slow turn, to keep as much control on the crate as possible.
                forwardTurnRight ->
                    NewState = State#movState{prevName = forward},
                    [110,1,80,0,0];
                backwardTurnleft ->
                    NewState = State#movState{prevName = backward},
                    [80,0,110,1,0];
                backwardTurnRight ->
                    NewState = State#movState{prevName = backward},
                    [110,0,80,1,0];
                turnAround ->
                    NewState = State#movState{prevName = stopCrate}, 
                    % prevName = stopCrate is necessary to be able to call turnAround multiple times in a row, due to the condition on the gesture.
                    io:format("*briiight eyes* EVERY NOW AND THEN I FALL APART!~n"),
                    [100,1,100,1,3]; % Turn on itself, towards the right. Fixed at 100 RPM, could be less.
                _ -> 
                    NewState = State,
                    io:format("Unknown movement name while in movMode normal.~n"),
                    [0,1,0,0,2]
            end;
        _ ->
            NewState = State,
            io:format("Bad movement mode, crate will not move as a security measure."),
            [0,1,0,0,2]
    end,

    % Send command to the micro-controller.
    i2c_communication:send_data(Order),
    io:format("Order is ~p~n", [Order]),
    io:format("Current state is ~p~n", [NewState]),
    NewState.

% Reads the first 'Nbr' of letters from the Movement variable.
movementComparison(Movement,Nbr) ->
    NewList = atom_to_list(Movement),
    lists:sublist(NewList,1,Nbr).


% =======================================================================
% ======================== </private functions> =========================
% =======================================================================


% =======================================================================
% ======================= <gen_server functions> ========================
% =======================================================================

init([]) ->
    process_flag(trap_exit, true), % Not really useful if I don't need to deal with something when the gen_server goes down.
    % Display message to the console: allows to see if the function is correctly being setup from the shell.
    io:format("Object controller is being setup!~n"),
    % Change LED colors: allow to visually tell if the process has been launched, or not.
    grisp_led:color(1,aqua),
    grisp_led:color(2,yellow),
    % Initialise the counter and launch the function immediately.
    checkingConnection(Counter = #counter{}),
    % Set default state and return {ok, state}. State is the internal state of the gen_server.
    {ok, #movState{currentVelocity = 100, prevName = stopCrate, movName = stopCrate, movMode = normal}}.

handle_call({ctrlCrate, MovementDetected}, From, State = #movState{currentVelocity = CurrentVelocity, movName = MovName, movMode = MovMode}) ->
    Available = i2c_communication:read_data(),
    SuffixMovement = movementComparison(MovementDetected,7),        % This can't be used as a guard. So, define variable outside.
    PreviousSuffix = movementComparison(State#movState.prevName,7), % Only on 7 letters, so as to not double everything.
    if Available == 1 ->
        if MovementDetected == changeVelocity -> 
            NewState = State#movState{prevName = changeVelocity, movName = stopCrate, movMode = changeVelocity}; 
            % When entering changeVelocity mode, stop the crate. It allows easy reset of the changeVelocity mode, in case the user is lost.
            % We set here that the previous move was changeVelocity, only to do it once at entry and same for exitChangeVelocity.
        MovementDetected == exitChangeVelocity ->
            NewState = State#movState{prevName = exitChangeVelocity, movName = stopCrate, movMode = normal};
            % When exiting changeVelocity mode, stop the crate, once again as security measure. 
        SuffixMovement == "forward" ->  % If the current move says "forward"...
            if PreviousSuffix == "backwar" -> % And that now, we would like to go "backward"...
                NewState = State#movState{prevName = forward, movName = stopCrate}; % Stop the crate.
            true ->
                NewState = State#movState{movName = MovementDetected}
            end;
        SuffixMovement == "backwar" -> % Same as priori condition, but the other way around.
            if PreviousSuffix == "forward" ->
                NewState = State#movState{prevName = backward, movName = stopCrate};
            true ->
                NewState = State#movState{movName = MovementDetected}
            end;
        SuffixMovement == "turnAro" -> % For the turnAround rail-guard.
            if PreviousSuffix == "stopCra" ->
                NewState = State#movState{prevName = turnAround, movName = turnAround}; % If previous gesture was stopCrate, it's ok.
            true ->
                NewState = State#movState{prevName = stopCrate, movName = stopCrate}    % If it was anything else, problem. We stop the robot.
            end;
        true ->
            NewState = State#movState{movName = MovementDetected}
        end,
        FinalNewState = order_crate(NewState);
    true -> 
        io:format("The controller is currently unavailable. Please wait.~n"),
        FinalNewState = State
    end,
    {reply, ok, FinalNewState};

handle_call(stop, From, State = #movState{}) ->
    io:format("Handling stop.~n", []),
    {stop, normal, ok, State}.


handle_cast(Request, State) ->
    {reply, ok, State}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.


terminate(normal, State) ->
    io:format("sendOrder: normal termination~n",[]);

terminate(shutdown, State = #movState{currentVelocity=CurrentVelocity,movName=MovName,movMode=MovMode}) ->
    % terminating
    io:format("sendOrder: managing shutdown~n",[]);
    
terminate(Reason, State) ->
    io:format("sendOrder: other termination with reason: ~p~n", [Reason]).

% =======================================================================
% ====================== </gen_server functions> ========================
% =======================================================================