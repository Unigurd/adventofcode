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

last([E],E).
last([_|Es],E) :-
    last(Es,E).

fieldchar(F) --> alphanumeric(F).
fieldchar('#') --> ['#'].

field((Header-Value)) --> many1(fieldchar,Header), [':'], many1(fieldchar,Value).

document([Field|Fields]) --> field(Field), whitespace, document2(Fields).

document2([],[],[]).
document2([]) --> whitespace.
document2([Field|Fields]) --> field(Field), whitespace, document2(Fields).
%lastdocument([]) --> [].
%lastdocument([Field|Fields]) --> field(Field), whitespace, lastdocument(Fields).

documents([]) --> [].
documents([Doc|Docs]) --> document(Doc), documents(Docs).
%documents([Doc]) --> lastdocument(Doc).

requiredfields(["byr","iyr","eyr","hgt","hcl","ecl","pid"]).

wrap(Term,=(Term)).
fst((A-_),A).


% byr (Birth Year) - four digits; at least 1920 and at most 2002.
validate(("byr"-YearStr)) :-
    length(YearStr,4),
    parse_decnat(Year,YearStr,[]),
    1920 #=< Year,
    Year #=< 2002.

% iyr (Issue Year) - four digits; at least 2010 and at most 2020.
validate(("iyr"-YearStr)) :-
    length(YearStr,4),
    parse_decnat(Year,YearStr,[]),
    2010 #=< Year,
    Year #=< 2020.

% eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
validate(("eyr"-YearStr)) :-
    length(YearStr,4),
    parse_decnat(Year,YearStr,[]),
    2020 #=< Year,
    Year #=< 2030.

% hgt (Height) - a number followed by either cm or in:
%     If cm, the number must be at least 150 and at most 193.
%     If in, the number must be at least 59 and at most 76.
validate(("hgt"-HeightStr)) :-
    parse_decnat(Height,HeightStr,Unit),
    validateHeight(Height,Unit).

% hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
validate(("hcl"-['#'|Color])) :-
    many(hexdigit,Ds,Color,[]),
    length(Ds,6).

% ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
validate(("ecl"-"amb")).
validate(("ecl"-"blu")).
validate(("ecl"-"brn")).
validate(("ecl"-"gry")).
validate(("ecl"-"grn")).
validate(("ecl"-"hzl")).
validate(("ecl"-"oth")).

% pid (Passport ID) - a nine-digit number, including leading zeroes.
validate(("pid"-PID)) :-
    length(PID,9),
    many(digit,_,PID,[]).

% cid (Country ID) - ignored, missing or not.
% No validation

validateHeight(Height,"cm") :-
    150 #=< Height,
    Height #=< 193.
validateHeight(Height,"in") :-
    59 #=< Height,
    Height #=< 76.

task1(Doc) :-
    requiredfields(Reqs),
    maplist(fst,Doc,Headers),
    maplist(wrap,Headers,Ready),
    foldl(exclude,Ready,Reqs,[]).

task2cmp((Elm-Val),Elm) :-
    validate((Elm-Val)).

exclude2(Elm,Acc,NewAcc) :-
    exclude(task2cmp(Elm),Acc,NewAcc).

task2(Doc) :-
    requiredfields(Reqs),
    foldl(exclude2,Doc,Reqs,[]).

solution1(N) :-
    filename_chars('4.input',Chars),
    documents(Docs,Chars,[]),
    include(task1,Docs,ValidDocs),
    length(ValidDocs,N).

interestingcmp(Elm,(Elm-_)).

interestingdoc(D) :-
    exclude(interestingcmp("cid"),D,E),
    length(E,7).

solution2(N) :-
    filename_chars('4.input',Chars),
    documents(Docs,Chars,[]),
    *length(Docs,N),
    %last(Docs,N),
    include(task2,Docs,ValidDocs),
    length(ValidDocs,N),
    *maplist(mywrite,ValidDocs),
    *include(interestingdoc,ValidDocs,Bla),
    *maplist(sort,Bla,SortedVDocs),
    *maplist(mywrite,SortedVDocs),
    *length(SortedVDocs,N).

mywrite(L) :-
    maplist(writeln,L),
    writeln("").
