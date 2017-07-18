%%%
%%% Copyright (c) 2017 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Lasse Grinderslev Andersen <lasse@etableret.dk>
%%% @doc Definitions file for the lexer used to tokenize Tor directory documents. according to
%%%
%%%  The overall document format is specified in
%%%  https://gitweb.torproject.org/torspec.git/tree/dir-spec.txt section 1.2:
%%% 
%%%     NL = The ascii LF character (hex value 0x0a).
%%%     Document ::= (Item | NL)+
%%%     Item ::= KeywordLine Object*
%%%     KeywordLine ::= Keyword NL | Keyword WS ArgumentChar+ NL
%%%     Keyword = KeywordChar+
%%%     KeywordChar ::= 'A' ... 'Z' | 'a' ... 'z' | '0' ... '9' | '-'
%%%     ArgumentChar ::= any printing ASCII character except NL.
%%%     WS = (SP | TAB)+
%%%     Object ::= BeginLine Base64-encoded-data EndLine
%%%     BeginLine ::= "-----BEGIN " Keyword "-----" NL
%%%     EndLine ::= "-----END " Keyword "-----" NL
%%% 
%%%     The BeginLine and EndLine of an Object must use the same keyword.
%%%
%%% @end
%%% -----------------------------------------------------------

%% We have included all definitions above for completeness.
%% Note that we have to deviate from the spec in a few places since
%% Tor itself does not obey the overall format in a few instances.
%% See the comments below.

Definitions.

NL  = \n
TAB = \t
SP = \s
WS = ({SP}|{TAB})+

%%% The spec is not followed here. ARGUMENTCHAR should be all ASCII except '\n', i.e. [\x00-\x09\x0B-\x7F]+. 
%%% However, the 'contact' item of server-descriptors allow non-ascii characters
%%% (probably utf-8/latin1 characters). Thus, we allow all non-ascii bytes as well.
ARGUMENTCHAR  = [\x00-\x09\x0B-\xFF]
ARGUMENTCHARS = {ARGUMENTCHAR}+
BASE64_DATA   = [A-Za-z0-9+/=\n]
KEYWORDCHAR   = [A-Za-z0-9\-] 
KEYWORD       = {KEYWORDCHAR}+

%%% The spec is not followed here! There should not be a SP before the NL in KEYWORD_LINE_NO_ARGS.
%%% However, the "'pr' SP Entries NL" item of, e.g., micro-descriptors produces 'pr\s\n' if no entries are present:
KEYWORD_LINE_NO_ARGS_PR = {KEYWORD}{SP}
KEYWORD_LINE_NO_ARGS    = {KEYWORD}
KEYWORD_LINE_ARGS       = {KEYWORD}{WS}{ARGUMENTCHARS}

%%% We have to include spaces such that '-----BEGIN RSA PUBLIC KEY-----'
%%% is captured. This does not obey the spec as described above.
BEGIN_LINE   = \-\-\-\-\-BEGIN\s({KEYWORD}|{SP})+\-\-\-\-\-
END_LINE     = \-\-\-\-\-END\s({KEYWORD}|{SP})+\-\-\-\-\-
OBJECT       = {BEGIN_LINE}{BASE64_DATA}+{END_LINE}

KEYWORD_LINE = {KEYWORD_LINE_NO_ARGS}|{KEYWORD_LINE_ARGS}
ITEM         = {KEYWORD_LINE}{OBJECT}*
DOCUMENT     = ({ITEM}|{NL})+

%%% Auxilliary items not used to describe the spec
%%% Used for '@tags' used in cached-* files produced by Tor
D        = [0-9]
INT      = {D}+
YEAR     = {D}{D}{D}{D}
MONTH    = [0-1][1-9]
DAY      = [0-3][1-9]
DATE     = {YEAR}\-{MONTH}\-{DAY}
TIME     = {INT}\:{INT}\:{INT}


Rules.

{OBJECT}                             : {token, {object, TokenLine, extract_object(TokenChars)}}.
{KEYWORD_LINE_NO_ARGS}               : {token, {keyword, TokenLine, erlang:list_to_atom(TokenChars)}}.
{KEYWORD_LINE_NO_ARGS_PR}            : {token, {keyword, TokenLine, erlang:list_to_atom(string:strip(TokenChars))}}.
{KEYWORD_LINE_ARGS}                  : {token, {keyword_ws_arguments, TokenLine, split_keyword_and_args(TokenChars)}}.
{NL}                                 : skip_token.


Erlang code.

-spec split_keyword_and_args(KeywordArgs) -> {Keyword, Args}
    when
      KeywordArgs :: string(),
      Keyword     :: atom(),
      Args        :: binary().
split_keyword_and_args(KeywordArgs) ->
    Keyword = string:sub_word(KeywordArgs, 1),
    Args = string:sub_string(KeywordArgs, string:len(Keyword) + 2),
    {erlang:list_to_atom(Keyword), erlang:list_to_binary(Args)}.


-spec extract_object(Object) -> {ObjectName, ObjectData}
    when
      Object     :: string(),
      ObjectName :: binary(),
      ObjectData :: binary().
extract_object(Object) ->
    [BeginLine | Rest] = string:tokens(Object, "\n"),
    EndLine =  lists:last(Rest),

    %% Extract object name and verify that it is equal in both lines, according to spec.
    ObjectName = object_name(BeginLine, begin_line),
    ObjectName = object_name(EndLine, end_line),

    DataLines = lists:droplast(Rest),
    ObjectData = string:join(DataLines, ""),
    {erlang:list_to_binary(ObjectName), erlang:list_to_binary(ObjectData)}.


-spec object_name(Line, LineType) -> Name
    when
      Line     :: string(),
      LineType :: end_line | begin_line,
      Name     :: string().
object_name(Line, end_line) ->
    string:substr(Line, 10, string:len(Line) - 14);

object_name(Line, begin_line) ->
    string:substr(Line, 12, string:len(Line) - 16).
