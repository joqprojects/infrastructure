%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_vim).
%% --------------------------------------------------------------------
%% Include files 
%% --------------------------------------------------------------------
%%  -include("").
-include_lib("eunit/include/eunit.hrl").
-include("../include/tcp.hrl").
-include("../include/dns.hrl").
%% --------------------------------------------------------------------
-export([]).

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
%% --------------------------------------------------------------------
start_test_Tabort()->
    vim:app_start(?VIM_IP,?VIM_PORT,"vim","1.0.0").

%% Requires that nfvi_1-nfvi_4 are started 
all_nfvis_service_1_test()->
    AvailibleNFVI=if_dns:call("vim",vim,get_all_instances,[]),
    [[{init_args,[{public_ip,_},
		  {public_port,_},
		  {service_id,"nfvi"},
		  {vsn,"1.0.0"}]},
      {zone,_},
      {capabilities,_}],
     [{init_args,[{public_ip,_},
		  {public_port,_},
		  {service_id,"nfvi"},
		  {vsn,"1.0.0"}]},
      {zone,_},
      {capabilities,_}]]=AvailibleNFVI,
    
ok.

check_capa_zone_1_test()->
    [[{public_ip,"localhost"},{public_port,40002}]]=if_dns:call("vim",vim,check_capa_zone,[[],["stugan.attic"]]),
    [[{public_ip,"localhost"},{public_port,_40001}]
    ]=if_dns:call("vim",vim,check_capa_zone,[[disc],[]]),
    
    ok.

ports_1_test()->
    AvailiblePorts=if_dns:call("vim",vim,availible_ports,[]),
  %  io:format("~p~n",[AvailiblePorts]),
    [30010,30011,30012,30013,30014,30015,30016,30017,30018,30019,30020,30021,
     30022,30023,30024,30025,30026,30027,30028,30029,30030,30031,30032,30033,
     30034,30035,30036,30037,30038,30039,30040,30041,30042,30043,30044,30045,
     30046,30047,30048,30049,30050,30051,30052,30053,30054,30055,30056,30057,
     30058,30059,30060,30061,30062,30063,30064,30065,30066,30067,30068,30069,
     30070,30071,30072,30073,30074,30075,30076,30077,30078,30079,30080,30081,
     30082,30083,30084,30085,30086,30087,30088,30089,30090,30091,30092,30093,
     30094,30095,30096,30097,30098,30099,30100,30101,30102,30103,30104,30105,
     30106,30107,30108,30109,30110]=AvailiblePorts,
    
    ok.


start_services_1_test()->
    {ok,"adder","1.0.0","localhost",30010}=if_dns:call("vim",vim,start_service,["adder","1.0.0",[disc],[]]),
    {error,[vim,_Line,'service already started',
	    [[{ip_addr,"localhost"},
	      {port,_},
	      {service_id,"adder"},
	      {vsn,"1.0.0"}]]]}=if_dns:call("vim",vim,start_service,["adder","1.0.0",[],[]]),
					 
    [      30011,30012,30013,30014,30015,30016,30017,30018,30019,30020,30021,
     30022,30023,30024,30025,30026,30027,30028,30029,30030,30031,30032,30033,
     30034,30035,30036,30037,30038,30039,30040,30041,30042,30043,30044,30045,
     30046,30047,30048,30049,30050,30051,30052,30053,30054,30055,30056,30057,
     30058,30059,30060,30061,30062,30063,30064,30065,30066,30067,30068,30069,
     30070,30071,30072,30073,30074,30075,30076,30077,30078,30079,30080,30081,
     30082,30083,30084,30085,30086,30087,30088,30089,30090,30091,30092,30093,
     30094,30095,30096,30097,30098,30099,30100,30101,30102,30103,30104,30105,
     30106,30107,30108,30109,30110
    ]=if_dns:call("vim",vim,availible_ports,[]),
    42=if_dns:call("adder",adder,add,[22,20]),
    ok.

stop_adder_test()->
     ok=if_dns:call("vim",vim,stop_service,["localhost",30010]),
    {error,[vim,_Line,'service not exists',"localhost",30010]}=if_dns:call("vim",vim,stop_service,["localhost",30010]),
    ok.

stop_services_1_test_glurk()->
    ok=if_dns:call("vim",vim,stop_service,["localhost",30010]),
    {error,[vim,_Line,'service not exists',"localhost",30010]}=if_dns:call("vim",vim,stop_service,["localhost",30010]),
    [            30012,30013,30014,30015,30016,30017,30018,30019,30020,30021,
     30022,30023,30024,30025,30026,30027,30028,30029,30030,30031,30032,30033,
     30034,30035,30036,30037,30038,30039,30040,30041,30042,30043,30044,30045,
     30046,30047,30048,30049,30050,30051,30052,30053,30054,30055,30056,30057,
     30058,30059,30060,30061,30062,30063,30064,30065,30066,30067,30068,30069,
     30070,30071,30072,30073,30074,30075,30076,30077,30078,30079,30080,30081,
     30082,30083,30084,30085,30086,30087,30088,30089,30090,30091,30092,30093,
     30094,30095,30096,30097,30098,30099,30100,30101,30102,30103,30104,30105,
     30106,30107,30108,30109,30110,
		 30010]=if_dns:call("vim",vim,availible_ports,[]),
    ok=if_dns:call("vim",vim,stop_service,["localhost",30011]),
    if_dns:call("vim",vim,stop_service,["localhost",30012]),
    
    ok.

stop_test()->    
    spawn(fun()->kill_session() end),
    ok.
kill_session()->
    timer:sleep(1000),
    erlang:halt(),
    ok.
    