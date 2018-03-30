%%
%% Simple implementation of file service behaviour.
%%
-module(markserv_file).

-behaviour(file_service).

-export([init/1]).
-export([upload/2]).


%%
%% Initialize simple file service
%%
init(Config) -> Config.

%%
%% Combines url
%%
combine_url(Base, Value) ->
    case string:substr(Base, string:len(Base), 1) of
        "/" -> lists:flatten([Base, Value]);
        _   -> string:join([Base, Value], "/")
    end.

%%
%% Uploads file into service
%%      Returns {failed, Reason} on failure and {ok, FileUrl} on success
%%
upload(FileData, {FilePath, BaseUrl}) ->
    {T1, T2, T3} = now(),
    FileName = io_lib:format("~p-~p-~p", [T1, T2, T3]),
    FullPath = filename:join(FilePath, FileName),
    case file:open(FullPath, [write, binary]) of
        {ok, IoDevice} ->
            case file:write(IoDevice, FileData)  of
                ok ->
                    file:close(IoDevice),
                    {ok, combine_url(BaseUrl, FileName)};
                
                {error, Reason} ->
                    error_logger:warning_msg(Reason),
                    {failed, Reason}
            end;
        
        {error, Reason} ->
            error_logger:warning_msg(Reason),
            {failed, Reason}
    end.

