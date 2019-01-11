%..............................................................................%
%.............................tower/3..........................................%
%..............................................................................%

%length and domain are used to flip arguments 
length_list(N, L) :-
        length(L, N).

domain(N,L):-
        fd_domain(L,1,N).

%fill in list of lists with constraints
fill_list(N,T):-
        fd_all_different(T),
        maplist(domain(N), T).

%https://stackoverflow.com/questions/4280986/how-to-
%	transpose-a-matrix-in-prolog
%transpose matrix to verify each column has unique element.
transpose([], []).
transpose([H|T], L):-
    transpose(H, [H|T], L).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

%%IDEA ON C:
%1.T is list of lists, so check_row evaluates each list element.
%2.Sides are lists, each element is the accumlative when there is
%	element value in a row of T that is greater than current
%	element.
%3.All Sides element has at least 1 block shown.
%4.Reverse all the list of lists to evaluate the opposite sides.

reverse_matrix([],[]).
reverse_matrix([H|T], [H_R|T_R]):-
	reverse(H,H_R),
	reverse_matrix(T,T_R).

check_elem([],Hd,_):- Hd=1.
check_elem([H|T], Acc, Prev):-
	(Prev #< H ->
	check_elem(T, Acc1, H),
	Acc #= Acc1+1
	;
	check_elem(T, Acc, Prev)).


check_row([],_).
check_row([[T_hd|T_tl]|TRest], [Hd|Tl]):-
	check_elem(T_tl, Hd, T_hd),
	check_row(TRest,Tl).


check(T,Ts,Top,Bottom,Left,Right):- 
	check_row(T, Left),
	check_row(Ts, Top),
	reverse_matrix(T, T_R),
	reverse_matrix(Ts, Ts_R),
        check_row(T_R,Right),
        check_row(Ts_R, Bottom).
	
tower(N, T, C):-
	%list of lists 
	length(T,N),
	maplist(length_list(N), T),
	maplist(fill_list(N), T),
	transpose(T, Ts),
	maplist(fill_list(N),Ts),
	maplist(fd_labeling, T),
	%Getting C lists
	C = counts(Top,Bottom, Left, Right),
	length(Top, N),
	length(Bottom, N),
	length(Left, N),
	length(Right, N),
	check(T,Ts,Top,Bottom,Left,Right).

%..............................................................................%
%.........................plain_tower/3........................................%
%..............................................................................%




%Fill list with 1 to N
list_1toN(X,X,L) :-
	L = [X].
list_1toN(H,N,[H|T]) :-
	X is H+1,
	list_1toN(X,N, T).

%each element must be unique
list_check([]).
list_check([H|T]):-
	\+(member(H,T)),
	list_check(T).	

lists_check([]).
lists_check([H|T]):-
	list_check(H),
	lists_check(T).



%Idea on plain_tower/3
%1.Have T as list of lists
%2.With the list that contains 1 to N, use permutation
%	to have all lists contain 1 to N.
%3.Transpose to verify the columns to have 1 to N.
%	This is checked with lists_check/1

plain_tower(N,T,C):-
	length(T,N),
	maplist(length_list(N),T),
	list_1toN(1,N,L),
	maplist(permutation(L),T),
	transpose(T,Ts),
	lists_check(Ts),
	%this part will be same as tower/3
	C = counts(Top,Bottom, Left, Right),
        length(Top, N),
        length(Bottom, N),
        length(Left, N),
        length(Right, N),
	check(T,Ts,Top,Bottom,Left,Right).



%.............................................................................%
%.................................Stats.......................................%
%.............................................................................%

%http://www.gprolog.org/manual/html_node/gprolog048.html
%statistics/2 to measure the SinceStart and SinceLast time.
%for number N, it must be 4 or less since greater than 4 is way too
% 	slow for plain_tower/3.

tower_time(A):-
	statistics(cpu_time,_),
	tower(4, T, counts([2,1,2,4],[2,4,2,1],[2,3,1,2],[3,2,2,1])),
	statistics(cpu_time,[_,A]).

plain_tower_time(B):-
	statistics(cpu_time,_),
        plain_tower(4, T, counts([2,1,2,4],[2,4,2,1],[2,3,1,2],[3,2,2,1])),
        statistics(cpu_time,[_,B]).

speedup(Ratio):-
	plain_tower_time(B),
	tower_time(A),
	Ratio is B/A.


%.............................................................................%
%.........................ambiguous/4.........................................%
%.............................................................................%


ambiguous(N,C,T1,T2):-
	tower(N,T1,C),
	tower(N,T2,C),
	\+ (T1 = T2).












