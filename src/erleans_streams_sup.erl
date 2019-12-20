%%%----------------------------------------------------------------------------
%%% Copyright Tristan Sloughter 2019. All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%----------------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% @doc erleans streams supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erleans_streams_sup).

-behaviour(supervisor).

-export([start_link/0,
         start_partitions_sup/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


start_partitions_sup() ->
    supervisor:start_child(?SERVER, #{id => erleans_partitions_sup,
                                      start => {erleans_partitions_sup, start_link, []},
                                      restart => permanent,
                                      type => supervisor,
                                      shutdown => 5000}).

init([]) ->
    %% if there are no stream providers configured there is no need to start these
    case erleans_config:get(stream_providers, []) of
        [] ->
            ignore;
        _ ->
            SupFlags = #{strategy => one_for_one,
                         intensity => 5,
                         period => 10},
            ChildSpecs = [#{id => erleans_stream_broker,
                            start => {erleans_stream_broker, start_link, []},
                            restart => permanent,
                            type => worker,
                            shutdown => 5000},
                          #{id => erleans_stream_agent_sup,
                            start => {erleans_stream_agent_sup, start_link, []},
                            restart => permanent,
                            type => supervisor,
                            shutdown => 5000},
                          #{id => erleans_stream_manager,
                            start => {erleans_stream_manager, start_link, []},
                            restart => permanent,
                            type => worker,
                            shutdown => 5000}],
            {ok, {SupFlags, ChildSpecs}}
    end.

%%====================================================================
%% Internal functions
%%====================================================================
