/*
 * pr1_shreyas-jayanna.pl
 * v1.0
 * Date: 07/02/2014
 *
 * This file implements an expert system for climate control. 
 */
 
/* Dynamic predicates */
:- dynamic temp_median/1.
:- dynamic temp_mean/1.
:- dynamic humid_median/1.
:- dynamic humid_mean/1.
:- dynamic current_temp/1.
:- dynamic outdoor_temp/1.
:- dynamic current_humid/1.

/* Default values for Td and humidity */
default_values(Td,H):-
	Td is 46.36,
	H is 40.

/* Set outdoor temperature */
set_outdoor_temp(Outdoor_temp):-
	outdoor_temp(X), Y is X, retract(outdoor_temp(X)),
	asserta(outdoor_temp(Outdoor_temp)).

/* Get outdoor temperature */
get_outdoor_temp(Out_temp):-
	outdoor_temp(X),
	Out_temp is X.

/* Various predicates to be called to infrom the user of the decision the expert system has made */
t1:-
	nl,
	write('Turning off AC till desired temperature is reached. Heater may be turned to acheive the desired temperature').

t2:-
	nl,
	write('It will be turned on again when the temperature changes by 3F.').

t3:-
	nl,
	write('Turning off AC till indoor temperature equalizes with outside temperature. Heater will be turned on once the temperatures equalize.').

t4:-
	nl,
	write('Turnig on AC till desired temperature is reached. AC will be turned off when desired temperature is reached.').

t5:-
	nl,
	write('Turning on heater till desired temperature is reached. Heater will be turned off once the desired temperature is reached. It will be turned on when the temperature drops by 3F.').


/* To set new indoor temperature */
setNewTemp(T):-
	nl,
	current_temp(X),
	retract(current_temp(X)),
	asserta(current_temp(T)),
	write('Setting temperature to '),
	format('~2f',T),
	write('F'), nl.

/* Predicate to get confirmation from the user while resetting the humidity conditions */
h1(T):-
	nl,
	write('Temperature has to be increased by '),
	write(T),
	write('F. Is this okay? (Y/N)'),
	read(Response).

/* Predicate to get confirmation from the user while resetting the humidity conditions */
h2(T):-
	nl,
	write('Temperature has to be decreased by '),
	write(T),
	write('F. Is this okay? (Y/N)'),
	read(Response).

/* To check if the indoor temperature is less than outdoor temperature */
indoorTemp_L_outdoorTemp(T):-
	outdoor_temp(X),
	(T < X) -> true; false.

/* To check if the desired temperature is greater than the current indoor temperature*/
desiredTemp_G_currentTemp(T):-
	current_temp(X),
	(T > X) -> true; false.

/* To check if the desired temperature is lesser than the current indoor temperature*/
desiredTemp_L_currentTemp(T):-
	current_temp(X),
	(T < X) -> true; false.

/* To check if the desired temperature is lesser than or equal to the outdoor temperature*/
desiredTemp_LE_outdoorTemp(T):-
	outdoor_temp(X),
	(T =< X) -> true; false.

/* To check if the desired temperature is greater than the outdoor temperature*/
desiredTemp_G_outdoorTemp(T):-
	outdoor_temp(X),
	(T > X) -> true; false.

/* To check if the mean is greater than the median */
mean_G_median(Mean,Median):-
	(Mean > Median) -> true; false.

/* To check if the mean is lesser than or equal to the median */
mean_LE_median(Mean,Median):-
	(Mean =< Median) -> true; false.

/* To check if the mean is greater than  or equal to the median */
mean_GE_median(Mean,Median):-
	(Mean >= Median) -> true; false.

/* To check if the indoor temperature is greater than or equal to the outdoor temperature */
indoorTemp_GE_outdoorTemp(T):-
	outdoor_temp(X),
	(T >= X) -> true; false.

/* To check if the mean is less than the median */
mean_L_median(Mean,Median):-
	(Mean < Median) -> true; false.

/* To check if the desired temperature is less than or equal to the outdoor temperature */
newTemp_LE_outdoorTemp(T):-
	outdoor_temp(X),
	(T =< X) -> true; false.

/* To check if the new temperature is greater than the outdoor temperature */
newTemp_G_outdoorTemp(T):-
	outdoor_temp(X),
	(T > X) -> true; false.

/* To convert temperature from faranheit to celcius */
faranheit_2_celcius(F,C):-
	C is ((F - 32) * (5/9)).

/* To convert temperature from celcius to faranheit  */
celcius_2_faranheit(C,F):-
	F is ((C * (9/5)) + 32).

/* The following are the various cases of facts that will be evaluated by evaluating the rules defined in them.
 * Each of these cases are evaluated until one of them succeed.
 */
case1a(T):-
	indoorTemp_L_outdoorTemp(T),
	temp_mean(Mean), temp_median(Median),
	mean_LE_median(Mean,Median),
	
	desiredTemp_G_currentTemp(Mean),
	desiredTemp_LE_outdoorTemp(Mean),
	setNewTemp(Mean),
	t1, t2,
	start.

case1b(T):-
	indoorTemp_L_outdoorTemp(T),
	temp_mean(Mean), temp_median(Median),
	mean_G_median(Mean,Median),
	
	desiredTemp_G_currentTemp(Median),
	desiredTemp_LE_outdoorTemp(Median),
	setNewTemp(Median),
	t1, t2,
	start.

case2a(T):-
	indoorTemp_L_outdoorTemp(T),
	temp_mean(Mean), temp_median(Median),
	mean_GE_median(Mean,Median),
	
	desiredTemp_G_currentTemp(Mean),
	desiredTemp_G_outdoorTemp(Mean),
	setNewTemp(Mean),
	t3, t2,
	
	start.

case2b(T):-
	indoorTemp_L_outdoorTemp(T),
	temp_mean(Mean), temp_median(Median),
	mean_GE_median(Mean,Median),
	
	desiredTemp_G_currentTemp(Median),
	desiredTemp_G_outdoorTemp(Median),
	setNewTemp(Median),
	t3, t2,
	
	start.

case3(T):-
	indoorTemp_L_outdoorTemp(T),
	temp_mean(Mean), temp_median(Median),
	mean_G_median(Mean,Median),
	
	desiredTemp_L_currentTemp(Mean),
	setNewTemp(Mean),
	t4,t2,
	
	start.

case4(T):-
	indoorTemp_L_outdoorTemp(T),
	temp_mean(Mean), temp_median(Median),
	mean_LE_median(Mean,Median),
	
	desiredTemp_L_currentTemp(Median),
	setNewTemp(Median),
	t4,t2,
	
	start.

case5(T):-
	indoorTemp_GE_outdoorTemp(T),
	temp_mean(Mean), temp_median(Median),
	mean_GE_median(Mean,Median),
	
	desiredTemp_G_currentTemp(Median),
	setNewTemp(Median),
	t5,
	
	start.

case6(T):-
	indoorTemp_GE_outdoorTemp(T),
	temp_mean(Mean), temp_median(Median),
	mean_L_median(Mean,Median),
	
	desiredTemp_G_currentTemp(Mean),
	setNewTemp(Mean),
	t5,
	
	start.

case7(T):-
	indoorTemp_GE_outdoorTemp(T),
	temp_mean(Mean), temp_median(Median),
	mean_GE_median(Mean,Median),
	desiredTemp_L_currentTemp(Median),
	newTemp_LE_outdoorTemp(Median),
	setNewTemp(Median),
	t4, t2,
	
	start.

case8(T):-
	indoorTemp_GE_outdoorTemp(T),
	temp_mean(Mean), temp_median(Median),
	mean_L_median(Mean,Median),
	
	desiredTemp_L_currentTemp(Mean),
	newTemp_G_outdoorTemp(Mean),
	setNewTemp(Mean),
	t5,
	
	start.

/* Predicate to be called to inform the user about the temperature alteration */
informNewTemp(H):-
	write('Temperature will be altered by +/- 2F to achieve the desired humidity of '),
	write(H),
	write('%.').

/* Predicate to be called to inform the user about the new humidity value that will be set */
setHumid(H):-
	write('Humidity set to '),
	write(H),
	write('%. Temperature will be altered accordingly.').

/* Mergesort to sort the input values */
mergesort([],[]).    		% An output of sorting an empty list is another empty list
mergesort([A],[A]).  		% List with only one element is the same list when sorted
mergesort([A,B|R],S) :-		% recursively call mergesort
   split([A,B|R],L1,L2),	% split the list into two halves
   mergesort(L1,S1),		% recursive calls to mergesort
   mergesort(L2,S2),
   merge(S1,S2,S).

/* To split the passed list into two halves*/
split([],[],[]).			
split([A],[A],[]).
split([A,B|R],[A|Ra],[B|Rb]) :-  split(R,Ra,Rb).

/* To merge the two passed lists while sorting the values before merging */
merge(A,[],A).
merge([],B,B).
merge([A|Ra],[B|Rb],[A|M]) :-  A =< B, merge(Ra,[B|Rb],M).
merge([A|Ra],[B|Rb],[B|M]) :-  A > B,  merge([A|Ra],Rb,M).

/* Predicate to calculate the mean */
calculate_mean(List,Mean):-
	length(List, Len),
	sum(List, Sum),
	Mean is Sum / Len.

sum([], 0).
sum([H|T], Sum) :-
	sum(T, Temp),
	Sum is Temp + H.

/* Predicate to calculate the median */
calculate_median(List,Median):-
	median_rec(List, List, Median).

median_rec([_], [Z|_], Z).
median_rec([_,_], [X,Y|_], (X+Y)/ 2).
median_rec([_,_|[X3|List]], [_|Remainder], Median) :-
	median_rec([X3|List], Remainder, Median).


/* Predicates to calulate the mean and median of the temperature values */
calculate_temp([],Mean,Median):-
	Mean is 0,
	Median is 0.

/* Predicate to  calculate the mean and median and then evaluate the different cases */
calculate_temp(List,Mean,Median):-
	mergesort(List,Sorted),
	calculate_mean(Sorted,Mean),
	calculate_median(Sorted,Median),

	remove_if_exists(1,Prev_mean),
	asserta(temp_mean(Mean)),

	remove_if_exists(2,Prev_median),
	asserta(temp_median(Median)),

	current_temp(X),

	not(case1a(X)),
	not(case1b(X)),
	not(case2a(X)),
	not(case2b(X)),
	not(case3(X)),
	not(case4(X)),
	not(case5(X)),
	not(case6(X)),
	not(case7(X)),
	case8(X).


/* To remove a dynamic predicate if it exists */
remove_if_exists(V,Y):-
	(V == 1) -> ((temp_mean(X)) -> Y is X, retract(temp_mean(X)); Y is -999);
	(V == 2) -> ((temp_median(X)) -> Y is X, retract(temp_median(X)); Y is -999);
	(V == 3) -> ((humid_mean(X)) -> Y is X, retract(humid_mean(X)); Y is -999);
	(V == 4) -> ((humid_median(X)) -> Y is X, retract(humid_median(X)); Y is -999).


/* Predicates to calulate the mean and median of the humidity values*/
calculate_humid([],Mean,Median):-
	Mean is 0,
	Median is 0.

/* To calculate the new humidity value to be set */
calculate_humid(List,Mean,Median):-
	mergesort(List,Sorted),

	calculate_mean(Sorted,Mean),
	calculate_median(Sorted,Median),

	remove_if_exists(3,Prev_mean),
	asserta(humid_mean(Mean)),

	remove_if_exists(4,Prev_median),
	asserta(humid_median(Median)),

	(Mean =< Median) -> (Humid is Mean; Humid is Median),	
	
	current_humid(X),
	retract(current_humid(X)),
	asserta(current_humid(Humid)),
	write('Humidity is now set to '),
	format('~2f',[Humid]),
	write('%'),
	nl,
	
	(X < Humid) -> (write('Humidifier is now turned on'), nl;
					write('Dehumidifier is now turned on'), nl),
					
	start.
	

/* To compute the relative humidity value from the given temperature value */
verify_humidity(T,Humid) :-
	default_values(Td,H),
	Humid is 100*(((112 - (0.1 * T) + Td) / (112 + (0.9*T))) ^ 8).

/* To compute the temperature value from the given relative humidity value */
verify_temp(Humid,Temp) :-
	default_values(Td,H),
	Temp is (Td - (112 * (Humid/100) ^ (1/8)) + 112) / ((0.9 * (Humid/100) ^ (1/8)) + 0.1).

/* To take input from the user for the new desired temperature value(s) */
ask_Temp:-
	nl,
	write('Current temperature is '),
	current_temp(X),
	write(X), write('F'),
	nl,
	write('Enter desired temperature(s) ([72.3<,..,74.5>]): '),
	read(Response),
	calculate_temp(Response,Mean,Median).


/* To take input from the user for the new desired humidity value(s) */
ask_Humid:-
	write('Enter desired humidities (30-50) in %  ([30.5<,46.9,..,49.2>]): '),
	read(Response),
	calculate_humid(Response,Mean,Median).

/* To reset the outdoor temperature */
reset_outdoor_temp:-
	nl,
	write('How much is outdoor temperature? '),
	read(Temp),
	set_outdoor_temp(Temp),
	nl,
	write('Outdoor temperature set to '),
	write(Temp),
	write('F'),
	nl,
	start.


/* Predicate which asks the user, The user has to enter yes or y if it is relevant character, else type anything else */
start:-
	nl, nl,
	write('Reset
	     1. Temperature
	     2. Humidity
	     3. Set outdoor temperature'),
	nl,
	write('Select 1, 2 or 3 from the options or type q to turn off the system: '),

	read(Response),
	nl,

	/* Call respective predicate to perform action chosen by the user */
	not((Response == 1) -> ask_Temp; false),
	not((Response == 2) -> ask_Humid; false),
	not((Response == 3) -> reset_outdoor_temp; false),
	not((Response == quit ; Response == q) -> abort; true).

/* The point of execution of the expert system */
go:-
	nl,
	write('How much is outdoor temperature? '), read(Out_temp), asserta(outdoor_temp(Out_temp)),
	write('What is the current indoor temperature? '), read(In_temp), asserta(current_temp(In_temp)),
	verify_humidity(In_temp,Humid),
	write('Current indoor humidity is '),
	format('~2f', [Humid]),
	write('%'),
	asserta(current_humid(Humid)),
	nl,
	start.

/* End of pr1_shreyas-jayanna.pl*/