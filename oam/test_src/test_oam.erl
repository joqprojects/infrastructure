%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_oam).
%% --------------------------------------------------------------------
%% Include files 
%% --------------------------------------------------------------------
%%  -include("").
%-include_lib("eunit/include/eunit.hrl").
-include("services/include/tcp.hrl").
-include("services/include/dns.hrl").
%% --------------------------------------------------------------------
%-export([start/0]).
-export([s1/1,
	s2/1]).

%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: Application
%% Description:
%% Returns: non
%% ------------------------------------------------------------------

%% --------------------------------------------------------------------
%% 1. Initial set up
%% -------------------------------------------------------------------
s2(N)->
    start(N).

start(0)->	       
    ok;
start(N) ->	
    if_log:call(debug,['#################################',?MODULE,?LINE,'N=',N]),
    R0=if_dns:call("oam",oam,start_app,["subtract","1.0.0"]),
   % if_log:call(debug,[?MODULE,?LINE,'started subtract=',R0]),
    %timer:sleep(100),
    R1=if_dns:call("subtract",subtract,sub,[20,22]),
   if_log:call(debug,[?MODULE,?LINE,'REEEEEEESSSUUUUULLLTTTTTTAAAT',R1]),

  %  if_log:call(debug,[?MODULE,?LINE,'20-22=',R1]),
  %  {ok,[]}=if_dns:call("oam",oam,start_app,["subtract","1.0,0"]),
  %  42=if_dns:call("adder",subtract,sub,[100,58]),
 
 %  {ok,[]}=if_dns:call("oam",oam,start_app,["calc_app","1.0,0"]),
  % {ok,[]}=if_dns:call("oam",oam,stop_app,["calc_app","1.0,0"]),
%    42=if_dns:call("adder",adder,add,[20,22]),
    R3=if_dns:call("oam",oam,stop_app,["subtract","1.0.0"]),
   if_log:call(debug,[?MODULE,?LINE,'STOOOOPPEEEED ',R3]),
  % timer:sleep(100), 
%   {ok,[]}=if_dns:call("oam",oam,stop_app,["subtract","1.0,0"]),
  %  io:format("~p~n",[{?MODULE,?LINE,if_dns:call("adder",adder,add,[20,22])}]),
    start(N-1).

s1(0)->
    finished_s1;
s1(N)->
   if_log:call(debug,['#################################',?MODULE,?LINE,'N=',N]),
    R0=if_dns:call("oam",oam,start_app,["calc_app","1.0.0"]),
   % if_log:call(debug,[?MODULE,?LINE,'started subtract=',R0]),
    %timer:sleep(100),
    R1=if_dns:call("adder",adder,add,[20,22]),
    R2=if_dns:call("divider",divider,divi,[420,10]),    
   if_log:call(debug,[?MODULE,?LINE,'REEEEEEESSSUUUUULLLTTTTTTAAAT',R1,R2]),
  %  {ok,[]}=if_dns:call("oam",oam,start_app,["subtract","1.0,0"]),
  %  42=if_dns:call("adder",subtract,sub,[100,58]),
 
 %  {ok,[]}=if_dns:call("oam",oam,start_app,["calc_app","1.0,0"]),
  % {ok,[]}=if_dns:call("oam",oam,stop_app,["calc_app","1.0,0"]),
%    42=if_dns:call("adder",adder,add,[20,22]),
    R3=if_dns:call("oam",oam,stop_app,["calc_app","1.0.0"]),
   if_log:call(debug,[?MODULE,?LINE,'STOOOOPPEEEED ',R3]),
  % timer:sleep(100), 
%   {ok,[]}=if_dns:call("oam",oam,stop_app,["subtract","1.0,0"]),
  %  io:format("~p~n",[{?MODULE,?LINE,if_dns:call("adder",adder,add,[20,22])}]),
    s1(N-1).


deploy_adder_test()->
    ok.


stop_test()->    
    spawn(fun()->kill_session() end),
    ok.
kill_session()->
    timer:sleep(1000),
    erlang:halt(),
    ok.
    
