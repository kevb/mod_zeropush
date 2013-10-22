%%%----------------------------------------------------------------------

%%% File    : mod_zeropush.erl
%%% Author  : Adam Duke <adam@zeropush.com>
%%% Purpose : Forward offline messages to ZeroPush
%%% Created : 12 Feb 2012 by Adam Duke <adam@zeropush.com>
%%%
%%%
%%% Copyright (C) 2012   Adam Duke
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_zeropush).
-author('adam@zeropush.com').

-behaviour(gen_mod).

-export([start/2,
	 init/2,
	 stop/1,
	 send_notice/3]).

-define(PROCNAME, ?MODULE).

-include("ejabberd.hrl").
-include("jlib.hrl").

start(Host, Opts) ->
    ?INFO_MSG("Starting mod_zeropush", [] ),
    register(?PROCNAME,spawn(?MODULE, init, [Host, Opts])),  
    ok.

init(Host, _Opts) ->
    inets:start(),
    ssl:start(),
    ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, send_notice, 10),
    ok.

stop(Host) ->
    ?INFO_MSG("Stopping mod_zeropush", [] ),
    ejabberd_hooks:delete(offline_message_hook, Host,
			  ?MODULE, send_notice, 10),
    ok.

send_notice(From, To, Packet) ->
    Type = xml:get_tag_attr_s("type", Packet),
    Body = xml:get_path_s(Packet, [{elem, "body"}, cdata]),
    Token = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, auth_token, [] ),
    Sound = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, sound, [] ),
    PostUrl = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, post_url, [] ),

    if (Type == "chat") and (Body /= "") ->
	      Sep = "&",
        Post = [
          "alert=", url_encode(Body), Sep,
					"badge=", url_encode("+1"), Sep,
          "sound=", Sound, Sep,
          "channel=", To#jid.luser, Sep,
          "info[from]=", From#jid.luser, Sep,
          "auth_token=", Token],
        ?INFO_MSG("Sending post request to ~s with body \"~s\"", [PostUrl, Post]),
        httpc:request(post, {PostUrl, [], "application/x-www-form-urlencoded", list_to_binary(Post)},[],[]),
        ok;
      true ->
        ok
    end.


%%% The following url encoding code is from the yaws project and retains it's original license.
%%% https://github.com/klacke/yaws/blob/master/LICENSE
%%% Copyright (c) 2006, Claes Wikstrom, klacke@hyber.org
%%% All rights reserved.
url_encode([H|T]) when is_list(H) ->
    [url_encode(H) | url_encode(T)];
url_encode([H|T]) ->
    if
        H >= $a, $z >= H ->
            [H|url_encode(T)];
        H >= $A, $Z >= H ->
            [H|url_encode(T)];
        H >= $0, $9 >= H ->
            [H|url_encode(T)];
        H == $_; H == $.; H == $-; H == $/; H == $: -> % FIXME: more..
            [H|url_encode(T)];
        true ->
            case integer_to_hex(H) of
                [X, Y] ->
                    [$%, X, Y | url_encode(T)];
                [X] ->
                    [$%, $0, X | url_encode(T)]
            end
     end;

url_encode([]) ->
    [].

integer_to_hex(I) ->
    case catch erlang:integer_to_list(I, 16) of
        {'EXIT', _} -> old_integer_to_hex(I);
        Int         -> Int
    end.

old_integer_to_hex(I) when I < 10 ->
    integer_to_list(I);
old_integer_to_hex(I) when I < 16 ->
    [I-10+$A];
old_integer_to_hex(I) when I >= 16 ->
    N = trunc(I/16),
    old_integer_to_hex(N) ++ old_integer_to_hex(I rem 16).

