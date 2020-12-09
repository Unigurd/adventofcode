:- set_prolog_flag(double_quotes, chars).
?- use_module(library(clpfd)).
:- op(920,fy, *).
*_.

filename_numbers(Filename,Contents) :-
    open(Filename,read,File),
    read_string(File,_,Str),
    close(File),
    split_string(Str,"\n","\n",StrList),
    maplist(number_string,Contents,StrList).

pair((A-B)) --> skip, single(A), skip, single(B).
three((A-B-C)) --> skip, single(A), skip, single(B), skip, single(C).
skip --> [].
skip --> [_], skip.
single(A) --> [A].

task1(X,Y,Z) :-
    X \== Y,
    X + Y #= 2020,
    Z #= X * Y.

solution1(X,Y,Z) :-
    filename_numbers('1.input',L),
    pair((X-Y), L, _),
    task1(X,Y,Z).

task2(X,Y,Z,R) :-
    X \== Y,
    X \== Z,
    Y \== Z,
    X + Y + Z #= 2020,
    R #= X * Y * Z.

solution2(X,Y,Z,R) :-
    filename_numbers('1.input',L),
    three((X-Y-Z), L, _),
    task2(X,Y,Z,R).

