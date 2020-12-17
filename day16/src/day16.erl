-module(day16).

%% API exports
% -export([main/1]).
-compile(export_all).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    Path = lists:nth(1, Args),
    Lines = utils:file_lines(Path),
    [FieldsStrs, MyTicketStrWithHeader, NearbyTicketsStrsWithHeader|_] = utils:split_list(Lines, ""),
    MyTicketStr = lists:last(MyTicketStrWithHeader),
    NearbyTicketsStrs = lists:nthtail(1, NearbyTicketsStrsWithHeader),
    Fields = lists:map(fun parse_field/1, FieldsStrs),
    NearbyTickets = lists:map(fun parse_ticket/1, NearbyTicketsStrs),
    InvalidNumbersPerTicket = lists:map(fun(T) -> validate_ticket(T, Fields) end, NearbyTickets),
    InvalidNumbers = lists:append(InvalidNumbersPerTicket),
    io:format("~p~n", [lists:sum(InvalidNumbers)]),
    ValidTickets = lists:filter(fun(T) -> validate_ticket(T, Fields) =:= [] end, NearbyTickets),
    FieldsInOrder = guess_field_order(Fields, ValidTickets),
    io:format("~p~n", [FieldsInOrder]),
    % rest solved by hand!
    % 101 departure time
    % 179 zone
    % 193 arrival track
    % 103 departure date
    % 53  departure platform
    % 89  departure station
    % 181 wagon
    % 139 class
    % 137 arrival station
    % 97  route
    % 61  departure location
    % 71  duration
    % 197 departure track
    % 59  type
    % 67  arrival location
    % 173 row
    % 199 price
    % 211 seat
    % 191 arrival platform
    % 131 train
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

parse_field(FieldStr) ->
    [Name, RangesStr|_] = string:split(FieldStr, ": "),
    RangesStrs = string:split(RangesStr, " or "),
    Ranges = lists:map(fun(RangeStr) ->
                               [Min, Max|_] = string:split(RangeStr, "-"),
                               {list_to_integer(Min), list_to_integer(Max)}
                       end, RangesStrs),
    {Name, Ranges}.

parse_ticket(TicketStr) ->
    lists:map(fun list_to_integer/1, string:split(TicketStr, ",", all)).

validate_ticket(Ticket, Fields) ->
    lists:filter(fun(N) -> not validate_number(N, Fields) end, Ticket).

validate_number(Number, Fields) ->
    lists:any(fun(Field) -> validate_number_with_field(Number, Field) end, Fields).

validate_number_with_field(Number, {_, Ranges}) ->
    lists:any(fun({Min, Max}) -> (Number >= Min) and (Number =< Max) end, Ranges).

guess_field_order(Fields, Tickets) ->
    lists:map(fun(I) ->
                      Fs = lists:filter(fun(F) ->
                                           lists:all(fun(N) -> validate_number_with_field(N, F) end, slice(I, Tickets))
                                   end, Fields),
                      lists:map(fun({Name, _}) -> Name end, Fs)
              end, lists:seq(1, length(Fields))).

slice(I, Lists) ->
    lists:map(fun(L) -> lists:nth(I, L) end, Lists).
