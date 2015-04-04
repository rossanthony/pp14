-module(tc).
-compile(export_all).

converter() ->
    receive
	{convertToFahrenheit, C} ->
	    disp ! {c2f, C, round((C*9/5)+32)},
	    converter();
	{convertToCelsius, F} ->
	    disp ! {f2c, round((F-32)*5/9), F},
            converter();
	_ ->
	    io:format("Unknown message received.~n"),
	    converter()
    end.

display() ->
    receive
	{c2f, C, F} ->
	    io:format("~p C == ~p F~n", [C,F]),
	    display();
	{f2c, C, F} ->
	    io:format("~p F == ~p C~n", [F,C]),
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
