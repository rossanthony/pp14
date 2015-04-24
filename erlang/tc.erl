%% Programming Paradigms 2014-15, Erlang Assignment: Message passing
%% Message passing program that can perform temperature conversions.
%% @author Ross Anthony <ross@velo33.com>
%% To test run `tc:init().` 
-module(tc).
-compile(export_all).

% Temperature Converter Actor
converter() ->
    receive
	{ConvertToFahrenheit, C} ->
	    disp ! {C, round((C*9/5)+32)},
	    converter();
	{ConvertToCelsius, F} ->
	    disp ! {round((F-32)*5/9), F},
        converter();
	_ ->
	    io:format("Unknown message received.~n"),
	    converter()
    end.

% Display Actor: wait for Temperature messages
display() ->
    receive
	{C, F} ->
	    io:format("The temperature is ~pC / ~pF~n", [C,F]),
	    display();
	_ ->
	    io:format("Unknown message received.~n"),
	    display()
    end.

% Controller: for the purposes of testing the following initialiser 
% acts as a controller, by spawning processes for both the converter
% and display actors and sending some test messages to the converter
init() ->
    DispPid = spawn(fun tc:display/0),
    % Register a reference to the display process, so it can be sent messages from the converter.
    % register(Name, Pid)	Associates the name Name, an atom, with the process Pid.
    register(disp, DispPid),
    ConvPid = spawn(fun tc:converter/0),
    % Fire off a series of temp conversion requests to the converter process
    ConvPid ! {ConvertToCelsius, 40},
    ConvPid ! {ConvertToCelsius, 77},
    ConvPid ! {ConvertToCelsius, 0},
    ConvPid ! {ConvertToCelsius, -1},
    ConvPid ! {ConvertToFahrenheit, 10},
    ConvPid ! {ConvertToFahrenheit, 25}.
    ConvPid ! {ConvertToFahrenheit, 0}.
    ConvPid ! {ConvertToFahrenheit, -15}.
    ConvPid ! {ConvertToFahrenheit, 'abc'}.