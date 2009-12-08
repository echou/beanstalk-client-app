-module(beanstalk_proto, [Host, Port]).

-author('echou327@gmail.com').

-export([connect/0, send/1, recv/1]).

-include("beanstalk.hrl").

-record(packet, {line=[], % raw command line
                 command,
                 body= <<>>, % raw body
                 body_size=0}).

-define(SOCK_OPTS, [{active, true}, binary, {packet, raw}]).
-define(KEY_SOCKET, beanstalk_sock).
-define(KEY_STATUS, beanstalk_status).
-define(KEY_PACKET, beanstalk_packet).

connect() ->
    ?PRINT("connect(~p, ~p)", [Host, Port]),
    {ok, Sock} = gen_tcp:connect(Host, Port, ?SOCK_OPTS, 2000),
    put(?KEY_SOCKET, Sock),
    put(?KEY_STATUS, line),
    put(?KEY_PACKET, #packet{}),
    ok.


send(Req) ->
    erlang:port_command(get(?KEY_SOCKET), encode_req(Req)).

recv(Bin) ->
    {Status, Packet, Ps} =  parse_data(get(?KEY_STATUS), Bin, get(?KEY_PACKET), []),
    put(?KEY_STATUS, Status),
    put(?KEY_PACKET, Packet),
    Ps.


% internal functions

parse_data(Status, <<>>, P, Acc) ->
    {Status, P, lists:reverse(Acc)};

parse_data(Status, Data, P, Acc) ->
    case parse_fsm(Status, Data, P) of
        {done, Rest, P1} ->
            parse_data(line, Rest, #packet{}, [process_packet(P1)|Acc]);
        {Status1, Rest1, P1} ->
            parse_data(Status1, Rest1, P1, Acc)
    end.


parse_fsm(line, <<"\r\n", Rest/binary>>, #packet{line=Line}=P) ->
    Command = parse(string:tokens(lists:reverse(Line), " ")),
    case has_body(Command) of
        {true, Size} ->
            {body, Rest, P#packet{line=[], command=Command, body_size=Size, body= <<>>}};
        false ->
            {done, Rest, P#packet{line=[], command=Command}}
    end;

parse_fsm(line, <<C, Rest/binary>>, #packet{line=Line}=P) ->
    parse_fsm(line, Rest, P#packet{line=[C|Line]});


parse_fsm(body, <<"\r\n", Rest/binary>>, #packet{body_size=0}=P) ->
    {done, Rest, P};

parse_fsm(body, Data, #packet{body_size=SizeLeft, body=Body}=P) when byte_size(Data) =< SizeLeft ->
    {body, <<>>, P#packet{body_size=SizeLeft-byte_size(Data), body= <<Body/binary, Data/binary>>}};

parse_fsm(body, Data, #packet{body_size=SizeLeft, body=Body}=P) ->
    <<B:SizeLeft/binary, Rest/binary>> = Data,
    {body, Rest, P#packet{body_size=0, body= <<Body/binary, B/binary>>}}.
                  

has_body({reserved, _, Size}) ->
    {true, Size};
has_body({found, _, Size}) ->
    {true, Size};
has_body({ok, Size}) ->
    {true, Size};
has_body(_) ->
    false.

process_packet(#packet{command=Command, body=Body}) ->
    case Command of
        {reserved, Id, _} ->
            {reserved, Id, Body};
        {found, Id, _} ->
            {found, Id, Body};
        {ok, _} ->
            {ok, Body};
        _ ->
            Command
    end.

    
% internal functions

encode_item(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
encode_item(Int) when is_integer(Int) ->
    integer_to_list(Int);
encode_item(V) ->
    V.

encode_line([H], Line) ->
    ["\r\n", encode_item(H)|Line];
encode_line([H|T], Line) ->
    encode_line(T, [" ", encode_item(H)|Line]).

encode(Items) ->
    lists:reverse(encode_line(Items, [])).

encode_req({put, Data, Params}) ->
    Pri = proplists:get_value(pri, Params, 0),
    Delay = proplists:get_value(delay, Params, 0),
    TTR = proplists:get_value(ttr, Params, 60),
    [ encode(['put', Pri, Delay, TTR, iolist_size(Data)]), Data, "\r\n"];
encode_req(Tuple) when is_tuple(Tuple) ->
    encode(tuple_to_list(Tuple));
encode_req(Atom) when is_atom(Atom) ->
    encode([Atom]);
encode_req(List) ->
    encode(List).


parse(["OUT_OF_MEMORY"]) ->
    out_of_memory;
parse(["INTERNAL_ERROR"]) ->
    internal_error;
parse(["DRAINING"]) ->
    draining;
parse(["BAD_FORMAT"]) ->
    bad_format;
parse(["UNKNOWN_COMMAND"]) ->
    unknown_command;
parse(["EXPECTED_CRLF"]) ->
    expected_crlf;
parse(["JOB_TOO_BIG"]) ->
    job_too_big;
parse(["DEADLINE_SOON"]) ->
    deadline_soon;
parse(["TIMED_OUT"]) ->
    timed_out;
parse(["DELETED"]) ->
    deleted;
parse(["NOT_FOUND"]) ->
    not_found;
parse(["RELEASED"]) ->
    released;
parse(["BURIED"]) ->
    buried;
parse(["TOUCHED"]) -> 
    touched;
parse(["NOT_IGNORED"]) ->
    not_ignored;
parse(["INSERTED", ID]) ->
    {inserted, list_to_integer(ID)};
parse(["BURIED", ID]) ->
    {buried, list_to_integer(ID)};
parse(["WATCHING", ID]) ->
    {watching, list_to_integer(ID)};
parse(["KICKED", Count]) ->
    {kicked, list_to_integer(Count)};
parse(["USING", Tube]) ->
    {using, Tube};
parse(["RESERVED", ID, Size]) ->
    {reserved, list_to_integer(ID), list_to_integer(Size)};
parse(["FOUND", ID, Size]) ->
    {found, list_to_integer(ID), list_to_integer(Size)};
parse(["OK", Size]) ->
    {ok, list_to_integer(Size)};
parse(_) ->
    unknown.
