#ecsv 0.3

**2011 (c) Nicolas R Dufour <nicolas.dufour@nemoworld.info>**

**ecsv** is a simple Erlang CSV parser able to read a file or string and sending back to an erlang process events when a line is parsed.

ecsv is under MIT. See NOTICE file for more details.

##Requirements

* Erlang/OTP R13/R14
* GNU Make

##Design

Ecsv is using a stateful parser in which you have to:

- create a initial state with options and a processing function
- provide a flow of character to the main parsing function ([*parse_with_character*])

##How to use it

Create a function that will accept 2 arguments:

- an element which can be
  - {eof} if the flow had ended
  - {newline, NewLine} for each parsed line
- a current state (defaulted to [])

Example: how to count the lines:

    MyFun = fun(_NewLine, Counter) ->
        % NewLine contains an array of strings
        Counter + 1.

Then call ecsv with the default state set to `0`:

    {ok, IoDevice} = file:open("/path/to/my.csv", [read]),
    {ok, FinalCounter} = ecsv:process_csv_file_with(IoDevice, MyFun, 0)

`FinalCounter` will have the number of parsed lines.

Take a look at the examples in the directory `examples`. A basic benchmark accepting one argument as the csv filename will parse it and display the number of parsed lines.

##Notes

This parser is based on the blog post written by *Andy Till* located
here [http://andrewtill.blogspot.com/2009/12/erlang-csv-parser.html](http://andrewtill.blogspot.com/2009/12/erlang-csv-parser.html).

This parser supports well formed csv files which are

- a set of lines ending with a `\n`
- each line contains a set of fields separated with a comma (`,`)
- each field value can be enclosed with single (`'`) or double quote (`"`)
- each field value can be empty
- any `\r` is ignored

Example:

    SC_Group_ID,SC_Group_Desc,SC_GroupCommod_ID,SC_GroupCommod_Desc,SC_Geography_ID,SortOrder,SC_GeographyIndented_Desc,SC_Commodity_ID,SC_Commodity_Desc,SC_Attribute_ID,SC_Attribute_Desc,SC_Unit_ID,SC_Unit_Desc,Year_ID,SC_Frequency_ID,SC_Frequency_Desc,Timeperiod_ID,Timeperiod_Desc,Amount
    2,"Supply and use",9,"Barley",1,0.800,"United States",1,"Barley",34,"Imports, trade year",7,"1,000 metric tons",1960,3,"Annual",114,"MY Oct-Sep",248
    2,"Supply and use",9,"Barley",1,0.800,"United States",1,"Barley",34,"Imports, trade year",7,"1,000 metric tons",1961,3,"Annual",114,"MY Oct-Sep",326

Please note:

- This parser has no failsafe mechanism if the file is badly formed!
  But the line `a,,,,,\n` is perfectly fine.
- This parser doesn't allow a return (`\n`) in a field value!


## Updates


handle parsing exceptions by adding an exception handler function parameter and updating the loop function to process exception messages. Here's the updated code:


1. Added new functions with ExceptionHandler parameter:
   - `process_csv_file_with/4`
   - `process_csv_string_with/4`
   - `process_csv_binary_with/4`

2. Updated existing functions to provide default exception handler:
   - All existing functions now pass `fun default_exception_handler/2` as the exception handler
   - The default handler raises an error when encountering exceptions

3. Added `default_exception_handler/2` function:
   - Provides default behavior of raising an error
   - Can be overridden by users with custom handlers

4. Modified `do_it/5` to accept ExceptionHandler:
   - Passes the handler to the loop function

5. Updated `loop/3` to handle exception messages:
   - Added case for `{newline_exception, NewLine, {ExceptionType, Reason}}`
   - Calls ExceptionHandler with the exception details and current state
   - Continues processing with the new state returned by the handler

The exception handler function should have the signature:
```erlang
   ExceptionHandler(NewLine, {ExceptionType, Reason}, State) -> NewState
```

The default_exception_handler will accept and include the NewLine in the error:
   ```erlang
   default_exception_handler(NewLine, {ExceptionType, Reason}, State) ->
       error({csv_parsing_exception, ExceptionType, Reason, NewLine}).
   ```


Example usage with a custom exception handler that includes the malformed line:

```erlang
% Custom exception handler that logs the bad line and continues
MyExceptionHandler = fun(NewLine, {fields_count, ActualCount}, State) ->
    io:format("Bad line (~p fields): ~p~n", [ActualCount, NewLine]),
    % Optionally, you could:
    % - Fix the line and add to state
    % - Count errors in state
    % - Skip the line by returning original state
    State
end,

% Process file with custom exception handling
{ok, FinalState} = ecsv:process_csv_file_with(
    File, 
    fun process_row/2, 
    MyExceptionHandler,
    InitialState
).
```

This implementation gives the exception handler full context about the parsing issue:
1. The malformed line itself (NewLine)
2. The type of exception (ExceptionType)
3. Additional details (Reason)
4. The current processing state

The handler can then make informed decisions about how to handle the malformed line while maintaining the processing state.