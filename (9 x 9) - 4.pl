for(N,N,N):- !.
for(I,N,I):- I < N.
for(I ,N ,X):- I < N ,I1 is I + 1,for(I1 ,N,X).

size(9 ,9).

valid(cell(X ,Y)) :- size(N ,M) ,X > 0 ,Y > 0 , X =< N ,Y =< M.

next(cell(X ,Y) ,Ret) :- NX is X + 1 ,Ret = cell(NX ,Y) ,valid(Ret).
next(cell(X ,Y) ,Ret) :- NX is X - 1 ,Ret = cell(NX ,Y) ,valid(Ret).
next(cell(X ,Y) ,Ret) :- NY is Y + 1 ,Ret = cell(X ,NY) ,valid(Ret).
next(cell(X ,Y) ,Ret) :- NY is Y - 1 ,Ret = cell(X ,NY) ,valid(Ret).

getNeighbours(cell(X ,Y) ,L):- findall(N ,next(cell(X ,Y) ,N) ,L).

printcell(I ,J) :- black(I ,J) ,cell(I ,J ,V) ,write('B(') ,write(V) ,write(')') ,!.
printcell(I ,J) :- white(I ,J) ,cell(I ,J ,V) ,write('W(') ,write(V) ,write(')') ,!.
printcell(I ,J) :- cell(I ,J ,V) ,write(' (') ,write(V) ,write(')') ,!.
print(N ,M) :- for(1 ,N ,I),nl , for(1 ,M ,J) ,printcell(I ,J) ,write(' '),fail.
print:- size(N ,M) ,\+print(N ,M) ,nl.

:-dynamic(vis/1).
:- dynamic(black / 2).
:- dynamic(white /2).


cell(1,1,8). cell(1,2,4). cell(1,3,9). cell(1,4,8). cell(1,5,5). cell(1,6,7). cell(1,7,6). cell(1,8,3). cell(1,9,5). 
cell(2,1,7). cell(2,2,1). cell(2,3,2). cell(2,4,4). cell(2,5,3). cell(2,6,9). cell(2,7,8). cell(2,8,5). cell(2,9,3). 
cell(3,1,8). cell(3,2,9). cell(3,3,8). cell(3,4,1). cell(3,5,7). cell(3,6,8). cell(3,7,2). cell(3,8,5). cell(3,9,6). 
cell(4,1,9). cell(4,2,2). cell(4,3,4). cell(4,4,8). cell(4,5,8). cell(4,6,1). cell(4,7,8). cell(4,8,2). cell(4,9,7). 
cell(5,1,3). cell(5,2,8). cell(5,3,1). cell(5,4,5). cell(5,5,7). cell(5,6,6). cell(5,7,6). cell(5,8,4). cell(5,9,3). 
cell(6,1,1). cell(6,2,7). cell(6,3,3). cell(6,4,6). cell(6,5,5). cell(6,6,2). cell(6,7,7). cell(6,8,1). cell(6,9,4). 
cell(7,1,3). cell(7,2,7). cell(7,3,5). cell(7,4,6). cell(7,5,1). cell(7,6,4). cell(7,7,9). cell(7,8,1). cell(7,9,2). 
cell(8,1,2). cell(8,2,5). cell(8,3,8). cell(8,4,8). cell(8,5,6). cell(8,6,3). cell(8,7,3). cell(8,8,7). cell(8,9,5). 
cell(9,1,5). cell(9,2,2). cell(9,3,8). cell(9,4,7). cell(9,5,9). cell(9,6,4). cell(9,7,1). cell(9,8,6). cell(9,9,8). 



clear:- retractall(vis(_)).

dfs(cell(X ,Y)) :- \+vis(cell(X ,Y)) ,assert(vis(cell(X ,Y))) ,getNeighbours(cell(X ,Y) ,L) ,member(cell(NX ,NY) ,L) ,\+black(NX ,NY) ,\+dfs(cell(NX, NY)) ,fail.

cntBlack(Cnt) :- findall(black(X ,Y),black(X ,Y) ,L) ,length(L ,Cnt).

unShadded(I ,J ,V) :- cell(I ,J ,V) ,\+black(I ,J).

uniqueWhiteRegion:- clear ,size(N ,M) , unShadded(I ,J ,_) ,!
,\+dfs(cell(I ,J)) ,findall(X ,vis(X) ,L) ,length(L ,Len)
,cntBlack(B) ,Len =:= (N * M) - B.

hasBlack(L) :- member(cell(X ,Y) ,L) ,black(X ,Y).

checkNeighbours([]) :- !.
checkNeighbours([H|T]) :- getNeighbours(H ,N) ,\+hasBlack(N) ,checkNeighbours(T).

blackDontTouch:- findall(cell(X ,Y) ,black(X ,Y) ,L) ,checkNeighbours(L).

checkRow(I) :- unShadded(I ,J1 ,V1) ,unShadded(I ,J2 ,V2) ,J1 \= J2 ,V1 = V2 ,!.
checkColumn(J) :- unShadded(I1 ,J ,V1) ,unShadded(I2 ,J ,V2) ,I1 \= I2 ,V1 = V2 ,!.

checkRows:- size(N ,_) ,for(1 ,N ,I) ,checkRow(I) ,!.
checkColumns:- size(_,M) ,for(1 ,M ,J) ,checkColumn(J) ,!.

solved:- \+checkRows ,\+checkColumns.

clearColor:- retractall(white(_ ,_)) ,retractall(black(_ ,_)).

setBlackRow(I ,J) :- size(N ,_) ,cell(I ,J ,Val) ,for(1 ,N ,K) ,cell(I ,K ,V1) ,V1 = Val ,K \= J ,setBlack(I ,K) ,!.
setBlackColumn(I ,J) :- size(_ ,M) ,cell(I ,J ,Val) ,for(1 ,M ,K) ,cell(K ,J ,V1) ,V1 = Val ,K\= I ,setBlack(K ,J) ,!.

setWhite(I ,J) :- white(I ,J) ,! ,fail.
setWhite(I ,J) :- black(I ,J) ,!.
setWhite(I ,J) :- \+black(I ,J) ,assert(white(I ,J)) ,\+setBlackRow(I ,J),setBlackColumn(I ,J).
setBlack(I ,J) :- black(I ,J) ,! ,fail.
setBlack(I ,J) :- white(I ,J) ,!.
setBlack(I ,J) :- \+white(I ,J) ,assert(black(I ,J)) ,getNeighbours(cell(I ,J) ,L) ,member(cell(NX ,NY) ,L) ,setWhite(NX ,NY) ,!.

inferXYXPatternRow:- write('infering Pattern in Rows') ,nl,cell(I ,J ,V) ,J1 is J + 2 ,cell(I ,J1 ,Val) ,V = Val ,J2 is J + 1 ,setWhite(I ,J2) ,!.
inferXYXPatternColumn:- write('infering Pattern in Columns') ,nl,cell(I ,J ,V) ,I1 is I + 2 ,cell(I1 ,J ,Val) ,V = Val ,I2 is I + 1 ,setWhite(I2 ,J) ,!.

inferCorner1A:- cell(1 ,1 ,V) ,cell(1 ,2 ,V1) ,cell(2 ,1 ,V2) ,V = V1 ,V = V2 ,setBlack(1 ,1).
inferCorner2A:- size(N ,M) ,M1 is M - 1 ,N1 is N - 1 ,cell(N ,M ,V) ,cell(N ,M1,V1) ,cell(N1 ,M ,V2) ,V = V1 ,V = V2 ,setBlack(N ,M).
inferCorner3A:- size(N ,_) ,N1 is N - 1 ,cell(N ,1 ,V) ,cell(N ,2 ,V1) ,cell(N1 ,1 ,V2) ,V = V1 ,V = V2 ,setBlack(N ,1).
inferCorner4A:- size(_ ,M) ,M1 is M - 1 ,cell(1 ,M ,V) ,cell(1 ,M1 ,V1) ,cell(2 ,M ,V2) ,V = V1 ,V = V2 ,setBlack(1 ,M).

inferCorner1B:- cell(1 ,1 ,V) ,cell(1 ,2 ,V1) ,cell(2 ,1 ,V2) ,cell(2 ,2 ,V3) ,V = V1 ,V2 = V3 ,setBlack(1 ,1) ,setBlack(2 ,2).
inferCorner2B:- size(N ,M) ,M1 is M - 1 ,N1 is N - 1 ,cell(N ,M ,V) ,cell(N ,M1,V1) ,cell(N1 ,M ,V2) ,cell(N1 ,M1 ,V3) ,V = V2 ,V1 = V3 ,setBlack(N ,M) ,setBlack(N1 ,M1).
inferCorner3B:- size(N ,_) ,N1 is N - 1 ,cell(N ,1 ,V) ,cell(N ,2 ,V1) ,cell(N1 ,1 ,V2) ,cell(N1 ,2 ,V3) ,V = V2 ,V3 = V1 ,setBlack(N ,1) ,setBlack(N1 ,2).
inferCorner4B:- size(_ ,M) ,M1 is M - 1 ,cell(1 ,M ,V) ,cell(1 ,M1 ,V1) ,cell(2 ,M ,V2) ,cell(2 ,M1 ,V3) ,V = V2 ,V1 = V3 ,setBlack(1 ,M) ,setBlack(2 ,M1).

inferCorners:- write('infering corners') ,nl,\+inferCorner1A ,\+inferCorner2A ,\+inferCorner3A ,\+inferCorner4A ,\+inferCorner1B,\+inferCorner2B,\+inferCorner3B,\+inferCorner4B.


:- dynamic(forceBackTrack / 0).
:- dynamic(returnSolve / 0).

copy(L) :- member(X ,L) ,\+assert(X).
backTrack(L1 ,L2) :- clearColor ,write('backtracking') ,nl ,\+copy(L1) ,\+copy(L2).

try(I ,J ,_ ,_) :- write('try black at ') ,write(I) ,write(' ') ,write(J) ,nl,\+setBlack(I ,J) ,uniqueWhiteRegion ,! ,fail.
try(I ,J ,Black ,White) :- backTrack(Black ,White) ,write('try white at ') ,write(I) ,write(' ') ,write(J) ,nl ,\+setWhite(I ,J) ,!,fail.
try(_ ,_ ,_ ,_) :- assert(returnSolve).

check(Black ,White):- forceBackTrack ,backTrack(Black ,White) ,retract(forceBackTrack).
check(_,_).

solve(_ ,_):- solved ,! ,print ,nl.

solve(Black ,White) :- cell(I ,J ,V) ,\+returnSolve ,check(Black ,White) ,repeated(I ,J ,V) ,\+black(I ,J) ,\+white(I ,J) ,print,\+try(I ,J ,Black ,White) 
,findall(black(X ,Y) ,black(X ,Y) ,L),findall(white(XX ,YY) ,white(XX ,YY) ,L1) ,solve(L ,L1) ,!.
solve(_ ,_) :- returnSolve ,retract(returnSolve),assert(forceBackTrack) ,! ,fail.
solve(_ ,_) :- \+solved ,assert(forceBackTrack) ,fail.
solve:- clearColor ,inferCorners ,print,\+inferXYXPatternRow ,print ,\+inferXYXPatternColumn,print ,write('------------------------------\n') ,findall(black(X ,Y) ,black(X ,Y) ,B)
,findall(white(I ,J) ,white(I ,J) ,W) ,solve(B ,W) ,!.

repeatedRow(I ,J ,V) :- cell(I ,K ,V1) ,K \= J ,V = V1 ,\+black(I ,K).
repeatedColumn(I ,J ,V) :- cell(K ,J ,V1) ,K \= I ,V = V1 ,\+black(K ,J).
repeated(I ,J ,V) :- repeatedColumn(I ,J ,V);repeatedRow(I ,J ,V).









