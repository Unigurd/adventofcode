:- set_prolog_flag(double_quotes, chars).
?- use_module(library(clpfd)).
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

list_index_elm([E|_],0,E).
list_index_elm([_|Es],I,R) :-
    NewI #= I - 1,
    list_index_elm(Es,NewI,R).

list_index_tail([],_,[]).
list_index_tail([E|Es],0,[E|Es]).
list_index_tail([_|Es],Idx,Res) :-
    Idx #> 0, % Could only be checked once
    NewIdx #= Idx - 1,
    list_index_tail(Es, NewIdx, Res).

non_newline([])     --> [].
non_newline([C|Cs]) --> [C], {C \= '\n'}, non_newline(Cs).

lines([]) --> [].
lines([Line|Lines]) --> non_newline(Line), ['\n'], lines(Lines).

tree_num('.',0).
tree_num('#',1).

task1(_, _, _, [], _, Res, Res).
task1(Mod, Right, Down, [List|Lists], Idx, Acc, Res) :-
    list_index_elm(List,Idx,Elm),
    tree_num(Elm,Num),
    NewAcc #= Acc + Num,
    NewIdx #= (Idx + Right) mod Mod,
    list_index_tail([List|Lists], Down, Tail),
    task1(Mod,Right,Down, Tail, NewIdx, NewAcc, Res).

solution1(Res) :-
    filename_chars('3.input',Str),
    lines(Lines, Str, []),
    Lines = [L|_],
    length(L,Mod),
    task1(Mod,3,1,Lines,0,0,Res).

solution2(Res) :-
    filename_chars('3.input',Str),
    lines(Lines, Str, []),
    Lines = [L|_],
    length(L,Mod),
    task1(Mod,1,1,Lines,0,0,A),
    task1(Mod,3,1,Lines,0,0,B),
    task1(Mod,5,1,Lines,0,0,C),
    task1(Mod,7,1,Lines,0,0,D),
    task1(Mod,1,2,Lines,0,0,E),
    Res #= A * B * C * D * E.
