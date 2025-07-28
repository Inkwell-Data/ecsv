% This file is part of ecsv released under the MIT license.
% See the LICENSE file for more information.

-module(ecsv_parser).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").
-author("Massimo Cesaro <massimo.cesaro@inkwelldata.com>").

-include("ecsv.hrl").

-export([start_parsing/1, start_parsing/2]).

-define(EMPTY_STRING, []).

%% @doc start parsing a csv stream and send the result to ResultPid
start_parsing(ResultPid) ->
    DefaultOptions = default_options(),
    start_parsing(ResultPid, DefaultOptions).

start_parsing(ResultPid, Options) ->
    ready(ResultPid, Options, undefined).

% -----------------------------------------------------------------------------

default_options() ->
    #ecsv_opts{ }.

% the ready state is the initial one and also the most common state
% through the parsing
ready(ResultPid, Options, ExpectedFieldCount) ->
    ready(ResultPid, Options, ExpectedFieldCount, [], []).
ready(ResultPid, Options, ExpectedFieldCount, ParsedCsv, CurrentValue) ->
    Delimiter = Options#ecsv_opts.delimiter,
    receive
        {eof} ->
            NewLine = lists:reverse([lists:reverse(CurrentValue) | ParsedCsv]),
            validate_and_send_line(ResultPid, NewLine, ExpectedFieldCount),
            send_eof(ResultPid);
        {char, Char} when (Char == $") ->
            % pass an empty string to in_quotes as we do not want the
            % preceeding characters to be included, only those in quotes
            in_quotes(ResultPid, Options, ExpectedFieldCount, ParsedCsv, ?EMPTY_STRING, Char);
        {char, Char} when Char == Delimiter ->
            ready(
                ResultPid, Options, ExpectedFieldCount,
                [lists:reverse(CurrentValue) | ParsedCsv], ?EMPTY_STRING);
        {char, Char} when Char == $\n ->
            % a new line has been parsed: time to send it back
            NewLine = lists:reverse([lists:reverse(CurrentValue) | ParsedCsv]),
            NewExpectedFieldCount = validate_and_send_line(ResultPid, NewLine, ExpectedFieldCount),
            ready(ResultPid, Options, NewExpectedFieldCount, [], ?EMPTY_STRING);
        {char, Char} when Char == $\r ->
            % ignore line feed characters
            ready(ResultPid, Options, ExpectedFieldCount, ParsedCsv, CurrentValue);
        {char, Char} ->
            ready(ResultPid, Options, ExpectedFieldCount, ParsedCsv, [Char | CurrentValue])
    end.

% the in_quotes state adds all chars it receives to the value string until
% it receives a char matching the initial quote in which case it moves to
% the skip_to_delimiter state.
in_quotes(ResultPid, Options, ExpectedFieldCount, ParsedCsv, CurrentValue, QuoteChar) ->
    receive
        {eof} ->
            NewLine = lists:reverse([lists:reverse(CurrentValue) | ParsedCsv]),
            validate_and_send_line(ResultPid, NewLine, ExpectedFieldCount),
            send_eof(ResultPid);
        {char, Char} when Char == QuoteChar ->
            skip_to_delimiter(
                ResultPid, Options, ExpectedFieldCount,
                [lists:reverse(CurrentValue) | ParsedCsv]);
        {char, Char} ->
            in_quotes(ResultPid, Options, ExpectedFieldCount, ParsedCsv, [Char | CurrentValue], QuoteChar)
    end.

% the skip_to_delimiter awaits chars which will get thrown away, when a
% value delimiter is received the machine moves to the ready state again.
skip_to_delimiter(ResultPid, Options, ExpectedFieldCount, ParsedCsv) ->
    receive
        {eof} ->
            NewLine = lists:reverse(ParsedCsv),
            validate_and_send_line(ResultPid, NewLine, ExpectedFieldCount),
            send_eof(ResultPid);
        {char, Char} when Char == Options#ecsv_opts.delimiter ->
            ready(ResultPid, Options, ExpectedFieldCount, ParsedCsv, ?EMPTY_STRING);
        {_} ->
            skip_to_delimiter(ResultPid, Options, ExpectedFieldCount, ParsedCsv)
    end.

% ----------------------------------------------------------------------------

validate_and_send_line(ResultPid, Line, undefined) ->
    % First line - set the expected field count
    FieldCount = length(Line),
    send_line(ResultPid, Line),
    FieldCount;
validate_and_send_line(ResultPid, Line, ExpectedFieldCount) ->
    case length(Line) of
        ExpectedFieldCount ->
            send_line(ResultPid, Line),
            ExpectedFieldCount;
        ActualFieldCount ->
            send_line_exception(ResultPid, Line, {fields_count, ActualFieldCount}),
            ExpectedFieldCount
    end.

send_line(ResultPid, NewLine) ->
    ResultPid ! {newline, NewLine}.

send_line_exception(ResultPid, NewLine, {ExceptionType, Reason}) ->
    ResultPid ! {newline_exception, NewLine, {ExceptionType, Reason}}.

send_eof(ResultPid) ->
    ResultPid ! {done}.