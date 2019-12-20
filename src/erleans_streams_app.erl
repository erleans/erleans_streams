%%%-------------------------------------------------------------------
%% @doc erleans_streams public API
%% @end
%%%-------------------------------------------------------------------

-module(erleans_streams_app).

-behaviour(application).

-export([start/2,
         stop/1]).

-include_lib("kernel/include/logger.hrl").

start(_StartType, _StartArgs) ->
    {ok, Pid} = erleans_streams_sup:start_link(),
    init_providers(),

    %% streams manager needs the providers fully initialized first
    %% so we have to do it after post_init_providers
    erleans_streams_sup:start_partitions_sup(),

    {ok, Pid}.

stop(_State) ->
    ok.

%% Internal functions

init_providers() ->
    Providers = erleans_config:get(stream_providers, []),
    maps:map(fun(ProviderName, Config) ->
                    case init_provider(ProviderName, Config) of
                        ok ->
                            ok;
                        %% handle crash here so application stops
                        {ok, _} ->
                            ok;
                        {error, Reason} ->
                            ?LOG_ERROR("failed to initialize provider ~s: reason=~p", [ProviderName, Reason]),
                            ok
                    end
                end, Providers).

init_provider(Name, Opts) ->
    erleans_provider_sup:start_child(Name, Opts).
