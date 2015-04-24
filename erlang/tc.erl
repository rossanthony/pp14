% Programming Paradigms 2014-15, Erlang Assignment: Message passing
% Message passing program that can perform temperature conversions.
% @author Ross Anthony <ross@velo33.com>
% Compile
%	c(tc).
% Test
%	tc:controller().

-module(tc).
-compile(export_all).


% Temperature Converter Actor: performs conversion and issues a 
% message to the display actor

converter() ->
    receive
	{"ConvertToFahrenheit", C} ->
		if 
			is_number(C) -> disp ! {C, round((C*9/5)+32)};
			not is_number(C) -> disp ! {invInput, C}
		end,
	    converter();
	{"ConvertToCelsius", F} ->
	    disp ! {round((F-32)*5/9), F},
        converter();
	_ ->
	    io:format("Unknown message received.~n"),
	    converter()
    end.


% Display Actor: waits for temperature messages to output

display() ->
    receive
	{invInput, C} ->
	    io:format("Invalid input: ~p ~n", [C]),
	    display();
	{C, F} ->
	    io:format("~p C <=> ~p F ~n", [C,F]),
	    display();
	_ ->
	    io:format("Unknown message received.~n"),
	    display()
    end.


% Controller: for the purposes of testing the following  acts as a controller, 
% spawning processes for both the converter and display actors and sending some 
% test messages to the converter

controller() ->

    % Spawn processes for the converter and display actors:
    ConvPid = spawn(fun tc:converter/0),
    DispPid = spawn(fun tc:display/0),

    % Register a reference to the display process, so it can be sent messages from the converter.
    % register(Name, Pid)  Associates the name Name, an atom, with the process Pid.
    register(disp, DispPid),

    % Fire off a series of temp conversion requests to the converter process
    ConvPid ! {"ConvertToFahrenheit", 'abc'},
    ConvPid ! {"ConvertToCelsius", 65},
    ConvPid ! {"ConvertToCelsius", 77.9},
    ConvPid ! {"ConvertToCelsius", 0},
    ConvPid ! {"ConvertToCelsius", -1},
    ConvPid ! {"ConvertToFahrenheit", 10},
    ConvPid ! {"ConvertToFahrenheit", 25},
    ConvPid ! {"ConvertToFahrenheit", 0}. 

% why does the last message {"ConvertToFahrenheit", 0} output raw as the 2nd line out?
% e.g.
%        Invalid input: abc 
%        18 C <=> 65 F 
%        {"ConvertToFahrenheit",0}
%        26 C <=> 77.9 F 
%        -18 C <=> 0 F 
%        -18 C <=> -1 F 
%        10 C <=> 50 F 
%        25 C <=> 77 F 
%        0 C <=> 32 F 
