digit('0') --> ['0'].
digit('1') --> ['1'].
digit('2') --> ['2'].
digit('3') --> ['3'].
digit('4') --> ['4'].
digit('5') --> ['5'].
digit('6') --> ['6'].
digit('7') --> ['7'].
digit('8') --> ['8'].
digit('9') --> ['9'].

hexdigit('0') --> ['0'].
hexdigit('1') --> ['1'].
hexdigit('2') --> ['2'].
hexdigit('3') --> ['3'].
hexdigit('4') --> ['4'].
hexdigit('5') --> ['5'].
hexdigit('6') --> ['6'].
hexdigit('7') --> ['7'].
hexdigit('8') --> ['8'].
hexdigit('9') --> ['9'].
hexdigit('a') --> ['a'].
hexdigit('b') --> ['b'].
hexdigit('c') --> ['c'].
hexdigit('d') --> ['d'].
hexdigit('e') --> ['e'].
hexdigit('f') --> ['f'].

lowercase(a) --> [a].
lowercase(b) --> [b].
lowercase(c) --> [c].
lowercase(d) --> [d].
lowercase(e) --> [e].
lowercase(f) --> [f].
lowercase(g) --> [g].
lowercase(h) --> [h].
lowercase(i) --> [i].
lowercase(j) --> [j].
lowercase(k) --> [k].
lowercase(l) --> [l].
lowercase(m) --> [m].
lowercase(n) --> [n].
lowercase(o) --> [o].
lowercase(p) --> [p].
lowercase(q) --> [q].
lowercase(r) --> [r].
lowercase(s) --> [s].
lowercase(t) --> [t].
lowercase(u) --> [u].
lowercase(v) --> [v].
lowercase(w) --> [w].
lowercase(x) --> [x].
lowercase(y) --> [y].
lowercase(z) --> [z].

uppercase('A') --> ['A'].
uppercase('B') --> ['B'].
uppercase('C') --> ['C'].
uppercase('D') --> ['D'].
uppercase('E') --> ['E'].
uppercase('F') --> ['F'].
uppercase('G') --> ['G'].
uppercase('H') --> ['H'].
uppercase('I') --> ['I'].
uppercase('J') --> ['J'].
uppercase('K') --> ['K'].
uppercase('L') --> ['L'].
uppercase('M') --> ['M'].
uppercase('N') --> ['N'].
uppercase('O') --> ['O'].
uppercase('P') --> ['P'].
uppercase('Q') --> ['Q'].
uppercase('R') --> ['R'].
uppercase('S') --> ['S'].
uppercase('T') --> ['T'].
uppercase('U') --> ['U'].
uppercase('V') --> ['V'].
uppercase('W') --> ['W'].
uppercase('X') --> ['X'].
uppercase('Y') --> ['Y'].
uppercase('Z') --> ['Z'].

letter(L) --> lowercase(L).
letter(U) --> uppercase(U).

alphanumeric(L) --> letter(L).
alphanumeric(D) --> digit(D).

whitespace --> [' '].
whitespace --> ['\n'].
whitespace --> ['\t'].
whitespace --> ['\r'].

many(_,[])      --> [].
many(P,[C|Cs])  --> call(P,C), many(P,Cs).
many1(P,[C|Cs]) --> call(P,C), many(P,Cs).

option(P,_,R) --> call(P,R).
option(_,R,R) --> [].

parse_times(_,0,[]) --> [].
parse_times(P,N,[R|Rs]) -->
    call(P,R),
    {NewN #= N - 1},
    parse_times(P,NewN,Rs).

num_hexchar(0,'0').
num_hexchar(1,'1').
num_hexchar(2,'2').
num_hexchar(3,'3').
num_hexchar(4,'4').
num_hexchar(5,'5').
num_hexchar(6,'6').
num_hexchar(7,'7').
num_hexchar(8,'8').
num_hexchar(9,'9').
num_hexchar(10,'a').
num_hexchar(11,'b').
num_hexchar(12,'c').
num_hexchar(13,'d').
num_hexchar(14,'e').
num_hexchar(15,'f').
num_hexchar(10,'A').
num_hexchar(11,'B').
num_hexchar(12,'C').
num_hexchar(13,'D').
num_hexchar(14,'E').
num_hexchar(15,'F').

num_decchar(0,'0').
num_decchar(1,'1').
num_decchar(2,'2').
num_decchar(3,'3').
num_decchar(4,'4').
num_decchar(5,'5').
num_decchar(6,'6').
num_decchar(7,'7').
num_decchar(8,'8').
num_decchar(9,'9').

sign(neg)  --> ['-'].
sign(pos)  --> ['+'].

dec_help(pos,D,Acc,NextAcc) :-
    num_hexchar(N,D),
    NextAcc #= Acc * 10 + N.
dec_help(neg,D,Acc,NextAcc) :-
    num_hexchar(N,D),
    NextAcc #= Acc * 10 - N.

parse_decint(Num) -->
    option(sign,pos,R),
    many1(digit,DigitStr),
    {foldl(dec_help(R),DigitStr,0,Num)}.

parse_decnat(Num) -->
    many1(digit,DigitStr),
    {foldl(dec_help(pos),DigitStr,0,Num)}.
