-module(eircd_irc_protocol).
-behaviour(gen_server).
-behaviour(ranch_protocol).
-include("eircd.hrl").

-export([start_link/4]).
-export([send_message/2]).
-export([init/1, init/4, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    socket,
    transport,
    ref,
    buffer = <<>>,
    messages,
    fsm,
    servername,
    max_queue_len
}).

start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

send_message(Pid, ParsedMessage) -> gen_server:cast(Pid, {irc, ParsedMessage}).

init([]) -> {ok, undefined}.

init(Ref, Socket, Transport, _Opts) ->
    true = gproc:reg({p, l, {module, ?MODULE}}),
    {ok, ServerName} = application:get_env(eircd, servername),
    {ok, MaxQueueLen} = application:get_env(eircd, max_queue_len),
    ok = proc_lib:init_ack({ok, self()}),
    ok = Transport:setopts(Socket, [{active, once}]),
    erlang:send_after(1000, self(), process_message),
    {ok, {Address, _}} = inet:peername(Socket),
    {ok, Pid} = eircd_irc_protocol_fsm_sup:start_child(self(), Address),
    link(Pid),
    lager:info("New client: ~p", [Address]),
    gen_server:enter_loop(?MODULE, [], #state{
        ref = Ref,
        socket = Socket,
        transport = Transport,
        messages = queue:new(),
        fsm = Pid,
        servername = ServerName,
        max_queue_len = MaxQueueLen
    }).

handle_call(_Request, _From, State) ->
    {reply, {error, undefined}, State}.

handle_cast({irc, ParsedMessage}, State) ->
    send(encode_message(ParsedMessage), State),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(process_message, State=#state{fsm=Fsm, messages=Messages, max_queue_len=MaxQueueLen}) ->
    case queue:len(Messages) > MaxQueueLen of
        true -> exit(flood);
        false -> ok
    end,
    State2 = case queue:out(Messages) of
        {{value, Message}, Messages2} ->
            gen_fsm:send_event(Fsm, {irc, decode_message(Message)}),
            State#state{messages = Messages2};
        {empty, _} ->
            State
    end,
    erlang:send_after(1000, self(), process_message),
    {noreply, State2};
handle_info({tcp, _Socket, Data}, State=#state{socket=Socket, transport=Transport, buffer=Buffer, messages=Messages}) ->
    Buffer2 = if Buffer =:= <<>> -> Data;
                  true -> <<Buffer/binary, Data/binary>>
              end,
    {Messages2, Rest} = receive_lines(Buffer2, Messages),
    ok = Transport:setopts(Socket, [{active, once}]),
    {noreply, State#state{buffer = Rest, messages = Messages2}};
handle_info({tcp_closed, _Port}, #state{transport=Transport, socket=Socket}=State) ->
    Transport:close(Socket),
    {stop, exit, State};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

receive_lines(Buffer, Queue) ->
    case binary:split(Buffer, <<"\r\n">>) of
        [Rest] ->
            {Queue, Rest};
        [Line, Rest] ->
            receive_lines(Rest, queue:in(Line, Queue))
    end.

send(Data, #state{transport=Transport, socket=Socket}) ->
    lager:info("Sending ~p", [Data]),
    Transport:send(Socket, Data).

encode_message({Command, Args}) ->
    [Command, <<" ">>, eircd_utils:join_list(Args, <<" ">>), <<"\n">>];
encode_message({Command, Args, Trailing}) ->
    [Command, <<" ">>, eircd_utils:join_list(Args, <<" ">>), <<" :">>, Trailing, <<"\n">>];
encode_message({Prefix, Command, Args, undefined}) ->
    [<<":">>, Prefix, <<" ">>, Command, <<" ">>, eircd_utils:join_list(Args, <<" ">>), <<"\n">>];
encode_message({Prefix, Command, Args, Trailing}) ->
    [<<":">>, Prefix, <<" ">>, Command, <<" ">>, eircd_utils:join_list(Args, <<" ">>), <<" :">>, Trailing, <<"\n">>].

decode_message(Line) ->
    {Prefix, Rest} = get_prefix(Line),
    {Command, Args, Trailing} = get_command(Rest),
    {Prefix, Command, Args, Trailing}.

get_prefix(Line) ->
    case binary:first(Line) =:= $: of
        true ->
            Rest = binary:part(Line, {1, byte_size(Line)-1}),
            case binary:split(Rest, <<" ">>) of
                [Prefix, Rest2] -> {Prefix, Rest2};
                _ -> throw(bad_message)
            end;
        false ->
            {undefined, Line}
    end.

get_command(Line) ->
    case binary:match(Line, <<" :">>) of
        nomatch ->
            [Command|Args] = binary:split(Line, <<" ">>),
            {Command, [split_args(A) || A <- Args], undefined};
        _ ->
            case binary:split(Line, <<" :">>) of
                [Args, Trailing] ->
                    [Command|Args2] = binary:split(Args, <<" ">>, [global]),
                    {Command, [split_args(A) || A <- Args2], Trailing};
                _ ->
                    throw(bad_message)
            end
    end.

split_args(Args) ->
    case binary:match(Args, <<",">>) of
        nomatch -> Args;
        _ -> binary:split(Args, <<",">>, [global])
    end.
