%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(repo_cmn).

% 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%-include("/include/tcp.hrl").
-include("services/include/dns.hrl").
%% --------------------------------------------------------------------
%% External exports
-compile([export_all]).
%-export([user_create_service_tar_file/3,
%	 unix_untar/2
%	]).


%% ====================================================================
%% External functions
%% ====================================================================

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: The function requires directory strucuture 
%%          ServiceId/ebin/*.beam , *.app , *.josca
%%          
%% 
%% ------------------------------------------------------------------
user_create_service_tar_file(ServiceId,SrcDir)->
    % Check the .app 
    AppBaseName=ServiceId++".app",
    AppFile=filename:join([SrcDir,AppBaseName]),
    ServiceIdAtom=list_to_atom(ServiceId),
    {ok,AppInfo}=file:consult(AppFile),
    [{application,ServiceIdAtom,Info}]=AppInfo,
    {vsn,AppVsn}=lists:keyfind(vsn,1,Info),  
   % {ok,AppBinary}=file:read_file(AppFile),

    % Get josca file
    JoscaBaseName=ServiceId++".josca",
    JoscaFile=filename:join([SrcDir,JoscaBaseName]),
    {ok,JoscaInfo}=file:consult(JoscaFile),
    {vsn,JoscaVsn}=lists:keyfind(vsn,1,JoscaInfo),  
 %   {ok,JoscaBinary}=file:read_file(JoscaFile),

    %create tar file
% New code
    ServiceEbin="service_ebin",
    case filelib:is_dir(ServiceEbin) of
	true->
	    os:cmd("rm -r "++ServiceEbin);
	false->
	    ok
    end,
    ok=file:make_dir(ServiceEbin),
    os:cmd("cp "++SrcDir++"/* "++ServiceEbin),

    TarBaseName=ServiceId++"-"++AppVsn++".tar",
    {ok,TarBaseName}=unix_tar(TarBaseName,ServiceEbin),
    {ok,TarFileBinary}=file:read_file(TarBaseName),
    ok=file:delete(TarBaseName),
    os:cmd("rm -r "++ServiceEbin),
% end new code
  %  RepoInfo=[{tar_file,{TarBaseName,TarFileBinary}},{app_file,{AppBaseName,AppVsn,AppBinary}},{josca_file,{JoscaBaseName,JoscaVsn,JoscaBinary}}],
    RepoInfo=[{tar_file,{TarBaseName,TarFileBinary}},{app_file,{AppBaseName,AppVsn,AppInfo}},{josca_file,{JoscaBaseName,JoscaVsn,JoscaInfo}}],
   
    % send the info to repo 
  Reply = case if_dns:call("repo",repo,create_service_artifact,[ServiceId,AppVsn,RepoInfo]) of
	      {ok,ok,ok}->
		  case if_dns:call("repo",repo,add_to_catalog,[ServiceId,AppVsn]) of
		      {ok,object_updated}->
			  ok;
		      {error,Err}->
			  {error,[?MODULE,?LINE,Err]}
		  end;
	      Err ->
		  {error,[?MODULE,?LINE,Err]}
	  end,
    
   Reply.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% {board_object,BoardId},[{brd_ipaddr,IpAddr},{brd_port,BrdPort},{worker_port,WorkerPort}]
%% ------------------------------------------------------------------

unix_tar(BaseFileName,Dir)->
    BaseDir=filename:basename(Dir),
    case filelib:is_dir(BaseDir) of
	true->
	    os:cmd("tar -czf "++BaseFileName++" "++Dir);
	false->
%	    io:format("~p~n",[{?MODULE,?LINE,BaseFileName,Dir,BaseDir}]),
	    []=os:cmd("cp -R "++Dir++" "++"."),
	    []=os:cmd("tar -czf "++BaseFileName++" "++BaseDir),
	    []=os:cmd("rm -r "++BaseDir)
	    
    end,
    case filelib:is_file(BaseFileName) of
	true->
	    Reply={ok,BaseFileName};
	 false->
	    Reply={error,[?MODULE,?LINE,failed_to_create_tar_file,BaseFileName]}
    end,
    Reply.

unix_untar(BaseFileName,TarBinary,Dir)->
    file:write_file(BaseFileName,TarBinary),
    os:cmd("tar -xzf "++BaseFileName++" -C "++Dir),
    file:delete(BaseFileName),
    ok.



%tar -czf web.tar web
%tar -xzvf archive.tar.gz
