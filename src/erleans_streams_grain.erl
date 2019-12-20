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
-module(erleans_streams_grain).

-export([subscribe/2,
         subscribe/3,
         unsubscribe/2]).

subscribe(StreamProvider, Topic) ->
    %% 0 is going to be the most common initial token, but subscribe/3
    %% allows the token to be overridden when it is not 0, or not an
    %% integer.
    subscribe(StreamProvider, Topic, 0).

subscribe(StreamProvider, Topic, SequenceToken) ->
    StreamRef = erleans_streams:get_stream(StreamProvider, Topic),
    MyGrain = get(grain_ref),
    erleans_stream_manager:subscribe(StreamRef, MyGrain, SequenceToken).

unsubscribe(StreamProvider, Topic) ->
    StreamRef = erleans_streams:get_stream(StreamProvider, Topic),
    MyGrain = get(grain_ref),
    erleans_stream_manager:unsubscribe(StreamRef, MyGrain).
