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

%%% ---------------------------------------------------------------------------
%%% @doc
%%% @end
%%% ---------------------------------------------------------------------------
-module(erleans_streams_provider_vonnegut).

-behaviour(erleans_stream_provider).

-export([init/2,
         fetch/1,
         produce/1]).

-include("erleans_streams.hrl").

init(_Name, _Args) ->  %% TODO: fix before PR to actually honor these
    application:ensure_all_started(vonnegut),
    timer:sleep(1000),
    ok = vg_client_pool:start(),
    ok.

fetch(TopicOffsets) ->
    [case (catch vg_client:fetch(Topic, Offset)) of
         {ok, #{Topic := #{0 := #{record_batches := [],
                                  high_water_mark := _HWM}}}} ->
             {Topic, {Offset, []}};
         {ok, #{Topic := #{0 := #{record_batches := Sets,
                                  high_water_mark := _HWM}}}} ->
             {IDs, Records} = lists:unzip([decode_fetch(Set) || Set <- Sets]),
             MaxOffset = lists:max(IDs),
             {Topic,
              {MaxOffset + 1, Records}};
         {error, _} = E ->
             E;
         {'EXIT', _} ->
             {error, {Topic, not_found}}
     end
     || {Topic, Offset} <- TopicOffsets].

produce(TopicRecordSets) ->
    [case vg_client:produce(Topic, RecordSet) of
         {ok, TopicOffset} ->
             TopicOffset;
         {error, _} = E ->
             E
     end
     || {Topic, RecordSet} <- TopicRecordSets].

%%%% internal


decode_fetch(#{offset := ID,
               value := Record}) ->
    {ID, Record}.
