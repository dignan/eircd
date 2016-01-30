-module(eircd_irc_messages).
-export([rpl_welcome/2]).
-export([rpl_topic/4]).
-export([err_nicknameinuse/2]).
-export([err_nosuchnick/3]).
-export([err_nosuchchannel/2]).
-export([err_notonchannel/3]).
-export([rpl_motdstart/2]).
-export([rpl_motd/3]).
-export([rpl_motdend/2]).
-export([rpl_nomotd/2]).
-export([pong/1]).
-export([ping/1]).
-export([nick/4]).
-export([join/4]).
-export([part/5]).
-export([topic/5]).
-export([privmsg/5]).

rpl_welcome(ServerName, Nick) ->
    rpl_numeric(ServerName, 1, [Nick], [<<"Welcome to the Internet Relay Network ">>, Nick]).

rpl_topic(ServerName, Nick, Channel, Topic) ->
    rpl_numeric(ServerName, 332, [Nick, Channel], Topic).

rpl_motdstart(ServerName, Nick) ->
    rpl_numeric(ServerName, 375, [Nick], [<<"- ">>, ServerName, <<"Message of the Day -">>]). 

rpl_motd(ServerName, Nick, Line) ->
    rpl_numeric(ServerName, 372, [Nick], [<<"- ">>, Line]).

rpl_motdend(ServerName, Nick) ->
    rpl_numeric(ServerName, 376, [Nick], <<"End of /MOTD command.">>).

rpl_nomotd(ServerName, Nick) ->
    rpl_numeric(ServerName, 422, [Nick], <<"MOTD File is missing.">>).

err_nicknameinuse(ServerName, Nick) ->
    rpl_numeric(ServerName, 433, [<<"*">>, Nick], <<"Nickname is already in use.">>).

err_nosuchnick(ServerName, Nick, Target) ->
    rpl_numeric(ServerName, 401, [Nick, Target], <<"No such nick/channel.">>).

err_nosuchchannel(ServerName, Channel) ->
    rpl_numeric(ServerName, 403, [Channel], <<"No such channel">>).

err_notonchannel(ServerName, Nick, Channel) ->
    rpl_numeric(ServerName, 442, [Nick, Channel], <<"You're not on that channel">>).

pong(Token) -> {<<"PONG">>, [Token]}.

ping(Token) -> {<<"PING">>, [], Token}.

nick(OldNick, User, Address, Nick) -> {get_prefix(OldNick, User, Address), <<"NICK">>, [], Nick}.

privmsg(FromNick, FromUser, FromAddress, To, MessageText) ->
    {get_prefix(FromNick, FromUser, FromAddress), <<"PRIVMSG">>, [To], MessageText}.

join(Nick, User, Address, Channel) ->
    {get_prefix(Nick, User, Address), <<"JOIN">>, [Channel], undefined}.

part(Nick, User, Address, Channel, PartMessage) ->
    {get_prefix(Nick, User, Address), <<"PART">>, [Channel], PartMessage}.

topic(Nick, User, Address, Channel, Topic) ->
    {get_prefix(Nick, User, Address), <<"TOPIC">>, [Channel], Topic}.

get_prefix(Nick, User, Address) -> [Nick, <<"!">>, User, <<"@">>, eircd_utils:get_ip_address_string(Address)].

rpl_numeric(ServerName, Number, Args, Trailing) ->
    {ServerName, string:right(integer_to_list(Number), 3, $0), Args, Trailing}.
