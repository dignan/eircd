-module(eircd_irc_messages).
-export([rpl_welcome/2]).
-export([rpl_topic/4]).
-export([err_nicknameinuse/2]).
-export([err_nosuchnick/3]).
-export([pong/1]).
-export([ping/1]).
-export([nick/4]).
-export([join/4]).
-export([privmsg/5]).

rpl_welcome(ServerName, Nick) ->
    rpl_numeric(ServerName, 1, [Nick], [<<"Welcome to the Internet Relay Network ">>, Nick]).

rpl_topic(ServerName, Nick, Channel, Topic) ->
    rpl_numeric(ServerName, 332, [Nick, Channel], Topic).

err_nicknameinuse(ServerName, Nick) ->
    rpl_numeric(ServerName, 433, [<<"*">>, Nick], <<"Nickname is already in use.">>).

err_nosuchnick(ServerName, Nick, Target) ->
    rpl_numeric(ServerName, 401, [Nick, Target], <<"No such nick/channel.">>).

pong(Token) -> {<<"PONG">>, [Token]}.

ping(Token) -> {<<"PING">>, [], Token}.

nick(OldNick, User, Address, Nick) -> {get_prefix(OldNick, User, Address), <<"NICK">>, [], Nick}.

privmsg(FromNick, FromUser, FromAddress, To, MessageText) ->
    {get_prefix(FromNick, FromUser, FromAddress), <<"PRIVMSG">>, [To], MessageText}.

join(Nick, User, Address, Channel) ->
    {get_prefix(Nick, User, Address), <<"JOIN">>, [Channel], undefined}.

get_prefix(Nick, User, Address) -> [Nick, <<"!">>, User, <<"@">>, eircd_utils:get_ip_address_string(Address)].

rpl_numeric(ServerName, Number, Args, Trailing) ->
    {ServerName, string:right(integer_to_list(Number), 3, $0), Args, Trailing}.
