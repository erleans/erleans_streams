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
%%%
%%% @doc
%%% @end
%%% ---------------------------------------------------------------------------
-module(erleans_streams).

-export([get_stream/2,
         get_stream/3]).

-include("erleans_streams.hrl").

-type stream_ref() :: #{topic           := term(),
                        stream_module   := module(),
                        stream_provider := atom(),
                        fetch_interval  := integer()}.

-export_type([stream_ref/0]).

-spec get_stream(module(), term()) -> stream_ref().
get_stream(StreamProvider, Topic) ->
    get_stream(StreamProvider, Topic, ?INITIAL_FETCH_INTERVAL).

-spec get_stream(module(), term(), pos_integer()) -> stream_ref().
get_stream(StreamProvider, Topic, FetchInterval) ->
    StreamConfig = proplists:get_value(StreamProvider, erleans_config:get(stream_providers, [])),
    StreamModule = proplists:get_value(module, StreamConfig),
    #{topic => Topic,
      stream_module => StreamModule,
      stream_provider => StreamProvider,
      fetch_interval => FetchInterval}.
