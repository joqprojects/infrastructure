%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(repo_lib).
 


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("services/include/dns.hrl").
%% --------------------------------------------------------------------

%% External exports
-compile(export_all).

%-export([load_start_node/3,stop_unload_node/3
%	]).


%% ====================================================================
%% External functions
%% ====================================================================

boot()->
    repo:app_start(?REPO_PUBLIC_IP,?REPO_PUBLIC_PORT,?REPO_LOCAL_IP,?REPO_LOCAL_PORT,"repo","1.0.0").

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
create_service_artifact(ServiceId,Vsn,RepoInfo,DbaseId)->
    {tar_file,{TarName,Binary}}=lists:keyfind(tar_file,1,RepoInfo),
    R1=dbase_dets:create({tar_file,ServiceId,Vsn},{TarName,Binary},DbaseId),

    {app_file,{_AppBaseName,_AppVsn,AppInfo}}=lists:keyfind(app_file,1,RepoInfo),
    R2=dbase_dets:create({app_file,ServiceId,Vsn},AppInfo,DbaseId),

    {josca_file,{_JoscaBaseName,_JoscaVsn,JoscaInfo}}=lists:keyfind(josca_file,1,RepoInfo),
    R3=repo_lib:create_josca(ServiceId,Vsn,JoscaInfo,DbaseId),
    {R1,R2,R3}.

create_josca(AppId,Vsn,JoscaInfo,DbaseId)->
    {R,_}=dbase_dets:create({josca_file,AppId,Vsn},JoscaInfo,DbaseId),
    R.

read_service_artfact(ServiceId,Vsn,DbaseId)->    
    TarInfo=dbase_dets:read({tar_file,ServiceId,Vsn},DbaseId),
    AppInfo=dbase_dets:read({app_file,ServiceId,Vsn},DbaseId),
    JoscaInfo= dbase_dets:read({josca_file,ServiceId,Vsn},DbaseId),
    [{tar_file,TarInfo},{app_file,AppInfo},{josca_file,JoscaInfo}].

read_service_app_file(ServiceId,Vsn,DbaseId)->
    R = case dbase_dets:read({app_file,ServiceId,Vsn},DbaseId)of
	    []->
		[];
	    [{_AppKey,AppInfo}]->
		AppInfo
	end,
    R.	

read_josca_file(Id,Vsn,DbaseId)-> %can be both app or service
    R = case dbase_dets:read({josca_file,Id,Vsn},DbaseId) of
	    []->
		[];
	    [{_JoscaKey,JoscaInfo}]->
		JoscaInfo
	end,
    R.	
read_service_tar_file(ServiceId,Vsn,DbaseId)->
    R = case dbase_dets:read({tar_file,ServiceId,Vsn},DbaseId) of
	    []->
		[];
	    [{_TarKey,TarInfo}]->
		TarInfo
	end,
    R.

delete_service_artifact(ServiceId,Vsn,DbaseId)->
    {R1,_}=dbase_dets:delete({tar_file,ServiceId,Vsn},DbaseId),
    {R2,_}=dbase_dets:delete({app_file,ServiceId,Vsn},DbaseId),
    {R3,_}= dbase_dets:delete({josca_file,ServiceId,Vsn},DbaseId),
    {R1,R2,R3}.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
delete_app_artifact(AppId,Vsn,DbaseId)->
    {R,_}= dbase_dets:delete({josca_file,AppId,Vsn},DbaseId),
    R.

%%------------- Catalog --------------------------------------------------
%%
%%
%%---------------------------------------------------------------------------
add_to_catalog(Id,Vsn,DbaseId)->
    Reply=case dbase_dets:read(service_catalog,DbaseId) of
	      []-> % No entry yet
		  dbase_dets:update(service_catalog,[{Id,Vsn}],DbaseId);
	      [{service_catalog,CatalogList}]->
		  case lists:member({Id,Vsn},CatalogList) of
		      false->
			  NewCatalog=[{Id,Vsn}|CatalogList],
			  dbase_dets:delete(service_catalog,DbaseId),
			  dbase_dets:update(service_catalog,NewCatalog,DbaseId);
		      true ->
			  {ok,[?MODULE,?LINE,'already exists',{Id,Vsn}]}
		  end;
	      Err ->
		  {error,[?MODULE,?LINE,Err]}
	  end,
    Reply.

remove_from_catalog(Id,Vsn,DbaseId)->
    Reply=case dbase_dets:read(service_catalog,DbaseId) of
	      []-> % No entry yet
		  {error,[?MODULE,?LINE,'service_catalog has no entires']};
	      [{service_catalog,CatalogList}]->
		  case lists:member({Id,Vsn},CatalogList) of
		      false->
			  {error,[?MODULE,?LINE,'doesnt exists',{Id,Vsn}]};
		      true ->
			  NewCatalog=lists:delete({Id,Vsn},CatalogList),
			  dbase_dets:delete(service_catalog,DbaseId),
			  dbase_dets:update(service_catalog,NewCatalog,DbaseId)
		  end;
	      Err ->
		  {error,[?MODULE,?LINE,Err]}
	  end,
    Reply.

member(Id,Vsn,DbaseId) ->
    Reply=case dbase_dets:read(service_catalog,DbaseId) of
	      []-> % No entry yet
		  {error,[?MODULE,?LINE,'service_catalog has no entires']};
	      [{service_catalog,CatalogList}]->
		  lists:member({Id,Vsn},CatalogList);
	      Err ->
		  {error,[?MODULE,?LINE,Err]}
	  end,
    Reply.

read_catalog(DbaseId)->
    Reply=case dbase_dets:read(service_catalog,DbaseId) of
	      []-> % No entry yet
		  {error,[?MODULE,?LINE,'service_catalog has no entires']};
	      [{service_catalog,CatalogList}]->
		  CatalogList;
	      Err ->
		  {error,[?MODULE,?LINE,Err]}
	  end,
    Reply.
