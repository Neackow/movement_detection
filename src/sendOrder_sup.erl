-module(sendOrder_sup).
-behaviour(supervisor).
 
-export([start_link/0]).
-export([init/1]).


start_link() ->
  supervisor:start_link({local,?MODULE}, ?MODULE, []).

init(_Args) ->
    SupFlags = #{
      strategy => one_for_one, 
      intensity => 1, 
      period => 5
    },
    SendOrder = #{
      id => sendOrder,
      start => {sendOrder, start_link, []},
      restart => transient,
      shutdown => 5000,
      type => worker,
      modules => [sendOrder]
    },
    I2C = #{
        id => i2c_communication,
        start => {i2c_communication, start_link, []}
    },

    ChildSpecs = [SendOrder,I2C],  
    {ok, {SupFlags, ChildSpecs}}.

