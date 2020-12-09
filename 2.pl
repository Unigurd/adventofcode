:- set_prolog_flag(double_quotes, chars).
?- use_module(library(clpfd)).
[parse.pl]
:- op(920,fy, *).
*_.
:- op(920,fy, #).
# P :-
    trace,
    call(P),
    nodebug.

filename_chars(Filename,Chars) :-
    open(Filename,read,File),
    read_string(File,_,Str),
    string_chars(Str,Chars),
    close(File).

parse_fully(DCG, Res, List) :-
    call(DCG,Res,List,[]).

digit_char(0,'0').
digit_char(1,'1').
digit_char(2,'2').
digit_char(3,'3').
digit_char(4,'4').
digit_char(5,'5').
digit_char(6,'6').
digit_char(7,'7').
digit_char(8,'8').
digit_char(9,'9').
parse_number(Num) --> parse_number_help(Num, 0).

parse_number_help(Num, Acc, [C|Cs], Rest) :-
    digit_char(D,C),
    NewAcc #= Acc * 10 + D,
    parse_number_help2(Num,NewAcc,Cs,Rest).

parse_number_help2(Num, Num) --> [].
parse_number_help2(Num, Acc, [C|Cs], Rest) :-
    digit_char(D,C),
    NewAcc #= Acc * 10 + D,
    parse_number_help2(Num,NewAcc,Cs,Rest).

non_newline([])     --> [].
non_newline([C|Cs]) --> [C], {C \= '\n'}, non_newline(Cs).

line((Min-Max-Char-Str)) -->
    parse_number(Min),
    "-",
    parse_number(Max),
    " ",
    [Char],
    ": ",
    non_newline(Str).

lines([]) --> [].
lines([Line|Lines]) --> line(Line), ['\n'], lines(Lines).

list_index_elm([E|_],0,E).
list_index_elm([_|Es],I,R) :-
    NewI #= I - 1,
    list_index_elm(Es,NewI,R).

check((A-B-C-S)) :-
    Ap #= A - 1,
    Bp #= B - 1,
    list_index_elm(S,Ap,C),
    list_index_elm(S,Bp,C).
check((A-B-C-S),(C-X-Y)) :-
    Ap #= A - 1,
    Bp #= B - 1,
    list_index_elm(S,Ap,X),
    list_index_elm(S,Bp,Y).


task1((Min-Max-Char-Str)) :-
    include(=(Char),Str,Filtered),
    length(Filtered,Len),
    Min #=< Len,
    Len #=< Max.

solution1(N) :-
    filename_chars('2.input',Str),
    lines(Lines, Str, []),
    include(task1,Lines,ValidLines),
    length(ValidLines,N).

task2((Min-Max-Char-Str)) :-
    NewMin #= Min - 1,
    NewMax #= Max - 1,
    list_index_elm(Str,NewMin,X),
    list_index_elm(Str,NewMax,Y),
    task2_help(Char,X,Y).

task2_help(C,C,Y) :- C \= Y.
task2_help(C,X,C) :- C \= X.

solution2(N) :-
    filename_chars('2.input',Str),
    lines(Lines, Str, []),
    include(task2,Lines,ValidLines),
    length(ValidLines,N).
