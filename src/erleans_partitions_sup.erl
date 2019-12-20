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

%%%----------------------------------------------------------------------------
%% @doc supervisor for processes that depend on the consistent hash partitions.
%% @end
%%%----------------------------------------------------------------------------

-module(erleans_partitions_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => rest_for_one,
                 intensity => 5,
                 period => 10},
    ChildSpecs = [#{id => erleans_partitions,
                    start => {erleans_partitions, start_link, []},
                    restart => permanent,
                    type => worker,
                    shutdown => 5000},
                  #{id => erleans_streams_sup,
                    start => {erleans_streams_sup, start_link, []},
                    restart => permanent,
                    type => supervisor,
                    shutdown => 5000}],
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
