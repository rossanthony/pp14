%% Programming Paradigms 2014-15, Erlang Assignment: Message passing
%% Message passing program that can perform temperature conversions.
%% @author Ross Anthony <ross@velo33.com>
%% To test   tc:init(). 
-module(tc).
-compile(export_all).

converter() ->
    receive
	{convertToFahrenheit, C} ->
	    disp ! {C, round((C*9/5)+32)},
	    converter();
	{convertToCelsius, F} ->
	    disp ! {round((F-32)*5/9), F},
        converter();
	_ ->
	    io:format("Unknown message received.~n"),
	    converter()
    end.

display() ->
    receive
	{C, F} ->
	    io:format("The temperature is ~pC / ~pF~n", [C,F]),
	    display();
	_ ->
	    io:format("Unknown message received.~n"),
	    display()
    end.

init() ->
    DispPid = spawn(fun tc:display/0),
    register(disp, DispPid),
    ConvPid = spawn(fun tc:converter/0),
    % Fire off a series of temp conversion requests to the converter process
    ConvPid ! {convertToCelsius, 40},
    ConvPid ! {convertToFahrenheit, 15},
    ConvPid ! {convertToCelsius, 77},
    ConvPid ! {convertToCelsius, 90},
    ConvPid ! {convertToFahrenheit, 25}.