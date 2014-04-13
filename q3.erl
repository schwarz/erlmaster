-module(q3).
-export([start/1]).

start(Port) ->
  case gen_udp:open(Port, [{active, true}, binary]) of
    {ok, Socket} ->
      loop(Socket, #{}),
      gen_udp:close(Socket);
    {error, _} = Error ->
      {stop, Error}
  end.

% TODO Move the case into another function
% TODO Purge inactive servers (timeout)
loop(Socket, Db) ->
  receive
    {udp, Socket, Host, Port, Bin} ->
      case Bin of
        <<255, 255, 255, 255, "getservers", _/bits>> ->
          getserversResponse(Socket, Host, Port, maps:keys(Db)),
          loop(Socket, Db);
        <<255, 255, 255, 255, "heartbeat flatline", 10>> ->
          loop(Socket, maps:remove({Host, Port}, Db));
        <<255, 255, 255, 255, "heartbeat", _/bits>> ->
          case maps:is_key({Host, Port}, Db) of
            true ->
              loop(Socket, maps:update({Host, Port}, erlang:localtime(), Db));
            false ->
              Nonce = term_to_binary(nonce(9)),
              gen_udp:send(Socket, Host, Port, <<255, 255, 255, 255, "getchallenge", 32, Nonce/binary, 10>>),
              gen_udp:send(Socket, Host, Port, <<255, 255, 255, 255, "getstatus", 32, Nonce/binary, 10>>),
              loop(Socket, Db)
          end;
        <<255, 255, 255, 255, "statusResponse", 10, _/bits>> ->
          loop(Socket, maps:put({Host, Port}, erlang:localtime(), Db));
        _ ->
          % TODO should log other packets
          loop(Socket, Db)
      end
  end.

nonce(Digits) ->
  Nonce = [ random:uniform(10) - 1 || _ <- lists:seq(1, Digits) ],
  [ integer_to_list(Int) || Int <- Nonce ].

splitMin(N, List) ->
  lists:split(min(length(List), N), List).

% 112 servers per packet
getserversResponse(Socket, Host, Port, Servers) ->
  {Current, Next} = splitMin(112, Servers),
  Payload = <<255, 255, 255, 255, "getserversResponse", 10, 0, 92,
            << <<A, B, C, D, P:16, 92>> || {{A,B,C,D},P} <- Current >>/binary >>,
  case Next of
    [] ->
      io:fwrite(<<"sending last packet\n">>),
      gen_udp:send(Socket, Host, Port, <<Payload/binary, "EOF">>);
    [_] ->
      io:fwrite(<<"sending regular packet\n">>),
      gen_udp:send(Socket, Host, Port, <<Payload/binary, "EOT">>),
      getserversResponse(Socket, Host, Port, Next)
  end.
