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


row(0)    --> "F".
row(1)    --> "B".
column(0) --> "L".
column(1) --> "R".

boardingpass((R-C)) -->
    parse_times(row,7,R),
    parse_times(column,3,C).

boardingpasss([]) --> [].
boardingpasss([BP|BPs]) -->
    boardingpass(BP),
    "\n",
    boardingpasss(BPs).

bin_space(L,Res) :- bin_space(L,0,Res).
bin_space([],Res,Res).
bin_space([Placement|Ps],Acc,Res) :-
    NewAcc #= Acc * 2 + Placement,
    bin_space(Ps,NewAcc,Res).

bin_space_tuple((R-C),Res) :-
    bin_space(R,RRes),
    bin_space(C,CRes),
    Res = (RRes-CRes).

row_col_seatID(R,C,SID) :-
    SID #= R * 8 + C.

boardingpass_SID((Rs-Cs),SID) :-
    bin_space(Rs,0,R),
    bin_space(Cs,0,C),
    row_col_seatID(R,C,SID).

validrow(R) :-
    0 #=< R,
    R #=< 127.

validcol(C) :-
    0 #=< C,
    C #=< 7.

neq_2d((L0-R0),(L1-R1)) :-
    L0 #\= L1,
    *R0 #\= R1.

tuple_left_right((L-R),L,R).

solution1(N) :-
    filename_chars('5.input',Chars),
    boardingpasss(BPs,Chars,[]),
    maplist(boardingpass_SID,BPs,SIDs),
    max_list(SIDs,N).

solution2(N) :-
    filename_chars('5.input',Chars),
    boardingpasss(BPs,Chars,[]),
    maplist(boardingpass_SID,BPs,SIDs),
    N in 0..1023,
    Nminus1 #= N - 1,
    Nplus1 #= N + 1,
    *sort(SIDs,SSIDs),
    *maplist(writeln,SSIDs),
    exclude(#\=(N),SIDs,[]),
    include(#=(Nminus1),SIDs,[Nminus1]),
    include(#=(Nplus1),SIDs,[Nplus1]).

test(R,C,N) :-
    boardingpass((Rs-Cs),"BBFFBBFRLL",[]),
    bin_space(Rs,0,R),
    bin_space(Cs,0,C),
    N #= R * 8 + C.


