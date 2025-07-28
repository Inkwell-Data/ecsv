% This file is part of ecsv released under the MIT license.
% See the LICENSE file for more information.

-module(ecsv).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").
-author("Massimo Cesaro <massimo.cesaro@inkwelldata.com>").

-export([process_csv_file_with/2, process_csv_string_with/2]).
-export([process_csv_file_with/3, process_csv_string_with/3]).
-export([process_csv_file_with/4, process_csv_string_with/4]).
-export([process_csv_binary_with/2, process_csv_binary_with/3, process_csv_binary_with/4]).

%% @doc parse a csv file and process each parsed row with the RowFunction
process_csv_file_with(IoDevice, RowFunction) ->
    process_csv_file_with(IoDevice, RowFunction, fun default_exception_handler/3, []).

%% @doc parse a csv string and process each parsed row with the RowFunction
process_csv_string_with(String, RowFunction) ->
    process_csv_string_with(String, RowFunction, fun default_exception_handler/3, []).

%% @doc parse a csv file and process each parsed row with the RowFunction
%% and the initial state InitState
process_csv_file_with(IoDevice, RowFunction, InitState) ->
    process_csv_file_with(IoDevice, RowFunction, fun default_exception_handler/3, InitState).

%% @doc parse a csv string and process each parsed row with the RowFunction
%% and the initial state InitState
process_csv_string_with(String, RowFunction, InitState) ->
    process_csv_string_with(String, RowFunction, fun default_exception_handler/3, InitState).

%% @doc parse a csv file with row processing and exception handling
process_csv_file_with(IoDevice, RowFunction, ExceptionHandler, InitState) ->
    do_it(IoDevice, RowFunction, ExceptionHandler, stream_from_file, InitState).

%% @doc parse a csv string with row processing and exception handling
process_csv_string_with(String, RowFunction, ExceptionHandler, InitState) ->
    do_it(String, RowFunction, ExceptionHandler, stream_from_string, InitState).

process_csv_binary_with(Bin, RowFunction) ->
    process_csv_binary_with(Bin, RowFunction, fun default_exception_handler/3, []).
process_csv_binary_with(Bin, RowFunction, InitState) ->
    process_csv_binary_with(Bin, RowFunction, fun default_exception_handler/3, InitState).
process_csv_binary_with(Bin, RowFunction, ExceptionHandler, InitState) ->
    do_it(Bin, RowFunction, ExceptionHandler, stream_from_binary, InitState).

%
% Internal API
%

default_exception_handler(NewLine, {ExceptionType, Reason}, State) ->
    error({csv_parsing_exception, ExceptionType, Reason, NewLine}).

do_it(Stream, RowFunction, ExceptionHandler, StreamFunctionName, InitState) ->
    % prepare the processes
    ProcessingPid = self(),
    ParsingPid = spawn(ecsv_parser, start_parsing, [ProcessingPid]),
    _ReadingPid = spawn(ecsv_reader, StreamFunctionName, [Stream, ParsingPid]),

    % let's go!
    loop(RowFunction, ExceptionHandler, InitState).

loop(RowFunction, ExceptionHandler, State) ->
    receive
        % ignore empty row
        {newline, [[]]} ->
            loop(RowFunction, ExceptionHandler, State);
        % process a new row
        {newline, NewLine} ->
            NewState = RowFunction(NewLine, State),
            loop(RowFunction, ExceptionHandler, NewState);
        % handle parsing exception
        {newline_exception, NewLine, {ExceptionType, Reason}} ->
            NewState = ExceptionHandler(NewLine, {ExceptionType, Reason}, State),
            loop(RowFunction, ExceptionHandler, NewState);
        % the parsing is done, time to stop
        {done} ->
            {ok, State}
    end.