-module(e11).

-behaviour(hera_measure).

-export([calibrate/1]).
-export([init/1, measure/1]).

-define(VAR_Q, 0.001).
-define(VAR_R, 0.01).


% Decide whether or not to print the comments. Remember to change it in your environment.
output_log(Message, Args) ->
    ShowLogs = application:get_env(hera, show_log, false), 
    if 
        ShowLogs -> 
            io:format(Message,Args);
        true -> 
            ok
    end.


% Decide whether or not to print the comments. Remember to change it in your environment.
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

calibrate({MBx,MBy,MBz}) ->
    _ = io:get_line("Place the pmod_nav at 0° then press enter"),
    [Ax,Ay,Az] = calibrate(acc, [out_x_xl, out_y_xl, out_z_xl], 100),
    [Mx,My,Mz] = calibrate(mag, [out_x_m, out_y_m, out_z_m], 10),
    R0 = ahrs([Ax,Ay,-Az], [-(Mx-MBx),My-MBy,-(Mz-MBz)]),
    %io:format("Result for e11 calibration: R0=~p.~n",[[R0]]),
    mat:tr(R0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(R0) ->

    % For debugging purposes.
    %output_log("An e11 process is being init!~n",[]),

    Spec = #{
        name => ?MODULE,
        iter => 10000,
        timeout => 0
    },
    X = mat:matrix([[1],[0],[0],[0]]),
    P = mat:diag([10,10,10,10]),
    State = {hera:timestamp(), X, P, R0},
    {ok, State, Spec}.

% It appears that the ekf is not used here. It is a Kalman filter (linear version) based on quaternion representation.
% As the article it is based on suggests: "[...] a novel linear Kalman filter, suitable for nonlinear attitude estimation, [...]"

measure({T0, X0, P0, R0}) ->

    % For debugging purposes.
    %output_log_spec("e11:measure!~n",[]),

    DataNav = hera_data:get(nav3, sensor_fusion@nav_1),
    T1 = hera:timestamp(),
    Nav = [Data || {_,_,Ts,Data} <- DataNav, T0 < Ts, T1-Ts < 500],
    if
        length(Nav) == 0 ->
            %output_log_spec("e11:measure finished with undefined~n",[]),
            {undefined, {T0, X0, P0, R0}};
        true ->
            {Acc, Gyro, Mag} = process_nav(Nav),

            R1 = ahrs(Acc, Mag),
            Quat = dcm2quat(mat:'*'(R1, R0)),   % This is never truly explained in the TFEs. The way I get it: we get the orientation by multiplying the original calibration orientation
                                                % and the current orientation matrix, in DCM format. 
            % {ok, Quat, {T1, X0, P0, R0}} % acc_mag only

            Dt = (T1-T0)/1000,
            [Wx,Wy,Wz] = Gyro,

            % Regarding the sign: it is the opposite of what the article it is based on says. 
            % According to Sébastien Kalbusch, he has found experimentally that -Omega was better than the Omega from the article.
            Omega = mat:matrix([
                [0,Wx,Wy,Wz],
                [-Wx,0,-Wz,Wy],
                [-Wy,Wz,0,-Wx],
                [-Wz,-Wy,Wx,0]
            ]),

            F = mat:'+'(mat:eye(4), mat:'*'(0.5*Dt, Omega)),
            Q = mat:diag([?VAR_Q,?VAR_Q,?VAR_Q,?VAR_Q]), % Not squared ? Indeed, because the value is already sigma_Q².
            H = mat:eye(4),
            Z = mat:tr(Quat),
            R = mat:diag([?VAR_R,?VAR_R,?VAR_R,?VAR_R]),
            {Xp, Pp} = kalman:kf_predict({X0,P0}, F, Q),

            {X1, P1} = case qdot(Z, Xp) > 0 of % This is to get the shortest path for the estimation.
                true ->
                    kalman:kf_update({Xp, Pp}, H, R, Z);
                false ->
                    kalman:kf_update({mat:'*'(-1,Xp), Pp}, H, R, Z)
            end,

            % {X1, P1} = {Xp, Pp}, % gyro only
            
            Values = unit(mat:to_array(X1)),
            X1Norm = mat:matrix([[X] || X <- Values]),
            %output_log_spec("e11:measure finished with ok and values ~p ~n",[Values]),
            {ok, Values, {T1, X1Norm, P1, R0}}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

qdot(Z, Xp) ->
    [Q11, Q12, Q13, Q14] = mat:to_array(Z),
    [Q21, Q22, Q23, Q24] = mat:to_array(Xp),
    Q11*Q21 + Q12*Q22 + Q13*Q23 + Q14*Q24.


process_nav([Nav]) ->
    {Acc, Next} = lists:split(3, Nav),
    {Gyro, Mag} = lists:split(3, Next),
    {Acc, Gyro, Mag}.


unit(V) ->
    Norm = math:sqrt(lists:sum([X*X || X <- V])),
    [X/Norm || X <- V].


scale(List, Factor) ->
    [X*Factor || X <- List].


calibrate(Comp, Registers, N) ->
    Data = [list_to_tuple(pmod_nav:read(Comp, Registers))
        || _ <- lists:seq(1,N)],
    {X, Y, Z} = lists:unzip3(Data),
    [lists:sum(X)/N, lists:sum(Y)/N, lists:sum(Z)/N].


% Acc, Mag : arrays
% returns a mat matrix
% This basically returns the DCM matrix, or something close to that form, depending on when it is called.
ahrs(Acc, Mag) ->
    Down = unit([-A || A <- Acc]),
    East = unit(cross_product(Down, unit(Mag))),
    North = unit(cross_product(East, Down)),
    mat:tr(mat:matrix([North, East, Down])).


cross_product([U1,U2,U3], [V1,V2,V3]) -> 
    [U2*V3-U3*V2, U3*V1-U1*V3, U1*V2-U2*V1].


% returns mat matrix
% Eq. 2.15 of TFE Sébastien Kalbusch.
dcm2quat(R) ->
    [R11,R12,R13,R21,R22,R23,R31,R32,R33] = mat:to_array(R),
    Q12 = 0.25*(1+R11+R22+R33),
    Q1 = math:sqrt(Q12),
    V = [
        4*Q12,
        R32-R23,
        R13-R31,
        R21-R12
    ],
    mat:matrix([scale(V, (0.25/Q1))]).
