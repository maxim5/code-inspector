%% interface authorization 
%%--------------------------------------------------------------------------------
-module(gnosus_authorization).
 
%% API
-export([
     authorize/1
]).
 
%% include
-include_lib("gnosus.hrl").
-include_lib("models.hrl").

%%================================================================================
-define(NOAUTHENTICATE_ROUTES, [web_index, web_register]).
-define(ADMIN_ROUTES, [web_admin, web_user_add, web_user]).
-define(HOST_ROUTES, [web_host, web_host_user_register, web_host_user_add]).

%%================================================================================
authorize(Module) ->
    case lists:member(Module, ?NOAUTHENTICATE_ROUTES) of
        false -> case is_authenticated(Module) of 
                     ok -> case is_admin(Module) of
                               ok -> case has_host(Module) of
                                         ok -> ok;
                                         Result -> Result
                                     end;
                                Result -> Result
                           end;
                     Result -> Result
                end;  
        true -> ok
    end.
                        
%%================================================================================
is_admin(Module) ->
    case lists:member(Module, ?ADMIN_ROUTES) of
        false -> ok;
        true ->
            case wf:user() of
                #users{uid=Uid, role=Role} -> 
                    case Role of
                        admin ->
                            gnosus_logger:message({admin_authorized, [Module, Uid]}),                     
                            ok;
                        _ -> 
                            admin_authorization_failed(Module, Uid)
                    end;
                #client_users{jid=Jid} ->
                    admin_authorization_failed(Module, Jid)
            end                    
    end.

%%--------------------------------------------------------------------------------
admin_authorization_failed(Module, Uid) ->
    gnosus_logger:alarm({admin_authorization_failed, [Module, Uid]}),                     
    wf:logout(),
    [wf:redirect(?LOGIN), wf:flash("access denied")].

%%--------------------------------------------------------------------------------
is_authenticated(Module) ->
    case wf:user() of
        undefined -> 
            gnosus_logger:warning({authentication_authorization_failed, Module}),                     
            wf:redirect(?LOGIN);
        _User -> ok
    end.   

%%--------------------------------------------------------------------------------
has_host(Module) ->
    case lists:member(Module, ?HOST_ROUTES) of
        false -> ok;
        true ->
            #users{uid=Uid} = wf:user(),
            Hosts = wf:session(hosts),            
            ReqHost = wf:get_path_info(),
            case lists:member(ReqHost, Hosts) of
                true -> ok;
                _ -> 
                    gnosus_logger:alarm({host_authorization_failed, [Module, Uid]}),                     
                    wf:logout(),
                    [wf:redirect(?LOGIN), wf:flash("access to requested host denied")]
            end
            
    end.
    