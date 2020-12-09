:- set_prolog_flag(double_quotes, chars).
?- use_module(library(clpfd)).
?- [parse].
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

person([A]) --> lowercase(A).
person(As1) -->
    lowercase(A),
    person(As0),
    {ord_union([A],As0,As1)}.

group(_,A) --> person(A), whitespace.
group(Op,A2) --> person(A0), whitespace, group(Op,A1),
              {call(Op,A0,A1,A2)}.

groups(_,[]) --> [].
groups(Op,[G|Gs]) --> group(Op,G), groups2(Op,Gs).

groups2(_,[],[],[]).
groups2(Op,Gs) --> whitespace, groups(Op,Gs).


list_num_lengthsum(L,N,M) :-
    length(L,Len),
    M #= N + Len.

solution1(N) :-
    filename_chars('6.input',Chars),
    groups(ord_union,Gs,Chars,[]),
    foldl(list_num_lengthsum,Gs,0,N).

solution2(N) :-
    filename_chars('6.input',Chars),
    groups(ord_intersection,Gs,Chars,[]),
    foldl(list_num_lengthsum,Gs,0,N).

