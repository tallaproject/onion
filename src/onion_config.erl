%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Application Configuration API
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_config).

%% API.
-export([get_string/2,
         get_string/3,

         get_integer/2,
         get_integer/3,

         get_boolean/2,
         get_boolean/3,

         get_value/2,
         get_value/3]).

-spec get_string(Application, Key) -> Result | not_found
    when
        Application :: atom(),
        Key         :: atom(),
        Result      :: string().
get_string(Application, Key) ->
    get_string(Application, Key, not_found).

-spec get_string(Application, Key, Default) -> Result | not_found
    when
        Application :: atom(),
        Key         :: atom(),
        Default     :: term(),
        Result      :: string().
get_string(Application, Key, Default) ->
    case get_value(Application, Key, Default) of
        Default ->
            Default;

        Value when is_list(Value) ->
            Value;

        Value when is_binary(Value) ->
            binary_to_list(Value);

        Value when is_integer(Value) ->
            integer_to_list(Value);

        Value ->
            error({invalid_string, Value})
    end.

-spec get_integer(Application, Key) -> Result | not_found
    when
        Application :: atom(),
        Key         :: atom(),
        Result      :: integer().
get_integer(Application, Key) ->
    get_integer(Application, Key, not_found).

-spec get_integer(Application, Key, Default) -> Result | not_found
    when
        Application :: atom(),
        Key         :: atom(),
        Default     :: term(),
        Result      :: integer().
get_integer(Application, Key, Default) ->
    case get_value(Application, Key, Default) of
        Default ->
            Default;

        Value when is_integer(Value) ->
            Value;

        Value when is_list(Value) ->
            list_to_integer(Value);

        Value when is_binary(Value) ->
            binary_to_integer(Value);

        Value ->
            error({invalid_string, Value})
    end.

-spec get_boolean(Application, Key) -> Result | not_found
    when
        Application :: atom(),
        Key         :: atom(),
        Result      :: boolean().
get_boolean(Application, Key) ->
    get_boolean(Application, Key, not_found).

-spec get_boolean(Application, Key, Default) -> Result | not_found
    when
        Application :: atom(),
        Key         :: atom(),
        Default     :: term(),
        Result      :: boolean().
get_boolean(Application, Key, Default) ->
    case get_value(Application, Key, Default) of
        Default ->
            Default;

        Value when is_boolean(Value) ->
            Value;

        Value when is_integer(Value) ->
            case Value of
                0 ->
                    false;
                _ ->
                    true
            end;

        Value when is_list(Value) ->
            case Value of
                "true" ->
                    true;
                _ ->
                    false
            end;

        Value when is_binary(Value) ->
            case Value of
                <<"true">> ->
                    true;
                _ ->
                    false
            end;

        Value ->
            error({invalid_boolean, Value})
    end.

-spec get_value(Application, Key) -> Result | not_found
    when
        Application :: atom(),
        Key         :: atom(),
        Result      :: term().
get_value(Application, Key) when is_atom(Application), is_atom(Key) ->
    get_value(Application, Key, not_found).

-spec get_value(Application, Key, Default) -> Result
    when
        Application :: atom(),
        Key         :: atom(),
        Default     :: term(),
        Result      :: term().
get_value(Application, Key, Default) when is_atom(Application), is_atom(Key) ->
    gproc:get_env(l, Application, Key, [{os_env, atom_to_env(Application, Key)},
                                        app_env,
                                        {default, Default}]).

%% @private
-spec atom_to_env(Application, Atom) -> string()
    when
        Application :: atom(),
        Atom        :: atom().
atom_to_env(Application, Atom) when is_atom(Application), is_atom(Atom) ->
    string:to_upper(lists:flatten([atom_to_list(Application), "_", atom_to_list(Atom)])).
