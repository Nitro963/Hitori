for(N,N,N):- !.
for(I,N,I):- I < N.
for(I ,N ,X):- I < N ,I1 is I + 1,for(I1 ,N,X).

size(20 ,20).

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

cell(1,1,7). cell(1,2,10). cell(1,3,16). cell(1,4,11). cell(1,5,2). cell(1,6,9). cell(1,7,2). cell(1,8,3). cell(1,9,14). cell(1,10,14). cell(1,11,17). cell(1,12,19). cell(1,13,4). cell(1,14,4). cell(1,15,13). cell(1,16,17). cell(1,17,13). cell(1,18,15). cell(1,19,5). cell(1,20,11). 
cell(2,1,1). cell(2,2,18). cell(2,3,13). cell(2,4,19). cell(2,5,9). cell(2,6,8). cell(2,7,2). cell(2,8,1). cell(2,9,7). cell(2,10,5). cell(2,11,15). cell(2,12,7). cell(2,13,3). cell(2,14,4). cell(2,15,9). cell(2,16,3). cell(2,17,11). cell(2,18,11). cell(2,19,16). cell(2,20,16). 
cell(3,1,9). cell(3,2,19). cell(3,3,6). cell(3,4,11). cell(3,5,7). cell(3,6,16). cell(3,7,14). cell(3,8,2). cell(3,9,3). cell(3,10,20). cell(3,11,11). cell(3,12,5). cell(3,13,12). cell(3,14,11). cell(3,15,15). cell(3,16,4). cell(3,17,18). cell(3,18,3). cell(3,19,14). cell(3,20,10). 
cell(4,1,5). cell(4,2,6). cell(4,3,9). cell(4,4,11). cell(4,5,12). cell(4,6,8). cell(4,7,17). cell(4,8,17). cell(4,9,4). cell(4,10,8). cell(4,11,18). cell(4,12,8). cell(4,13,15). cell(4,14,14). cell(4,15,12). cell(4,16,19). cell(4,17,13). cell(4,18,18). cell(4,19,2). cell(4,20,1). 
cell(5,1,20). cell(5,2,4). cell(5,3,11). cell(5,4,9). cell(5,5,7). cell(5,6,17). cell(5,7,13). cell(5,8,5). cell(5,9,1). cell(5,10,15). cell(5,11,19). cell(5,12,15). cell(5,13,7). cell(5,14,10). cell(5,15,12). cell(5,16,11). cell(5,17,2). cell(5,18,8). cell(5,19,18). cell(5,20,11). 
cell(6,1,18). cell(6,2,12). cell(6,3,4). cell(6,4,17). cell(6,5,15). cell(6,6,20). cell(6,7,11). cell(6,8,7). cell(6,9,14). cell(6,10,1). cell(6,11,11). cell(6,12,9). cell(6,13,18). cell(6,14,13). cell(6,15,2). cell(6,16,6). cell(6,17,3). cell(6,18,18). cell(6,19,19). cell(6,20,17). 
cell(7,1,11). cell(7,2,13). cell(7,3,3). cell(7,4,16). cell(7,5,18). cell(7,6,2). cell(7,7,20). cell(7,8,15). cell(7,9,7). cell(7,10,3). cell(7,11,2). cell(7,12,13). cell(7,13,8). cell(7,14,19). cell(7,15,2). cell(7,16,1). cell(7,17,17). cell(7,18,11). cell(7,19,2). cell(7,20,9). 
cell(8,1,4). cell(8,2,8). cell(8,3,10). cell(8,4,3). cell(8,5,15). cell(8,6,14). cell(8,7,1). cell(8,8,4). cell(8,9,9). cell(8,10,13). cell(8,11,12). cell(8,12,2). cell(8,13,13). cell(8,14,18). cell(8,15,19). cell(8,16,2). cell(8,17,6). cell(8,18,18). cell(8,19,20). cell(8,20,6). 
cell(9,1,10). cell(9,2,16). cell(9,3,5). cell(9,4,20). cell(9,5,1). cell(9,6,15). cell(9,7,9). cell(9,8,9). cell(9,9,18). cell(9,10,19). cell(9,11,6). cell(9,12,10). cell(9,13,6). cell(9,14,11). cell(9,15,7). cell(9,16,13). cell(9,17,12). cell(9,18,3). cell(9,19,17). cell(9,20,4). 
cell(10,1,5). cell(10,2,14). cell(10,3,8). cell(10,4,1). cell(10,5,13). cell(10,6,4). cell(10,7,12). cell(10,8,7). cell(10,9,11). cell(10,10,16). cell(10,11,9). cell(10,12,11). cell(10,13,17). cell(10,14,19). cell(10,15,3). cell(10,16,5). cell(10,17,16). cell(10,18,19). cell(10,19,1). cell(10,20,12). 
cell(11,1,4). cell(11,2,7). cell(11,3,7). cell(11,4,15). cell(11,5,11). cell(11,6,6). cell(11,7,19). cell(11,8,12). cell(11,9,11). cell(11,10,2). cell(11,11,19). cell(11,12,4). cell(11,13,16). cell(11,14,10). cell(11,15,18). cell(11,16,20). cell(11,17,9). cell(11,18,11). cell(11,19,8). cell(11,20,5). 
cell(12,1,3). cell(12,2,17). cell(12,3,5). cell(12,4,6). cell(12,5,5). cell(12,6,11). cell(12,7,15). cell(12,8,19). cell(12,9,8). cell(12,10,18). cell(12,11,1). cell(12,12,12). cell(12,13,3). cell(12,14,15). cell(12,15,10). cell(12,16,10). cell(12,17,3). cell(12,18,13). cell(12,19,9). cell(12,20,13). 
cell(13,1,10). cell(13,2,5). cell(13,3,9). cell(13,4,18). cell(13,5,20). cell(13,6,4). cell(13,7,10). cell(13,8,18). cell(13,9,16). cell(13,10,10). cell(13,11,7). cell(13,12,12). cell(13,13,6). cell(13,14,12). cell(13,15,1). cell(13,16,2). cell(13,17,8). cell(13,18,7). cell(13,19,3). cell(13,20,19). 
cell(14,1,14). cell(14,2,3). cell(14,3,18). cell(14,4,2). cell(14,5,16). cell(14,6,5). cell(14,7,12). cell(14,8,13). cell(14,9,1). cell(14,10,9). cell(14,11,16). cell(14,12,11). cell(14,13,20). cell(14,14,11). cell(14,15,10). cell(14,16,8). cell(14,17,17). cell(14,18,7). cell(14,19,4). cell(14,20,17). 
cell(15,1,1). cell(15,2,20). cell(15,3,19). cell(15,4,4). cell(15,5,17). cell(15,6,8). cell(15,7,16). cell(15,8,19). cell(15,9,19). cell(15,10,6). cell(15,11,17). cell(15,12,8). cell(15,13,16). cell(15,14,7). cell(15,15,4). cell(15,16,11). cell(15,17,1). cell(15,18,12). cell(15,19,13). cell(15,20,18). 
cell(16,1,11). cell(16,2,12). cell(16,3,19). cell(16,4,2). cell(16,5,8). cell(16,6,18). cell(16,7,9). cell(16,8,14). cell(16,9,19). cell(16,10,17). cell(16,11,11). cell(16,12,19). cell(16,13,1). cell(16,14,5). cell(16,15,20). cell(16,16,5). cell(16,17,10). cell(16,18,8). cell(16,19,6). cell(16,20,14). 
cell(17,1,19). cell(17,2,19). cell(17,3,17). cell(17,4,13). cell(17,5,4). cell(17,6,12). cell(17,7,3). cell(17,8,12). cell(17,9,15). cell(17,10,10). cell(17,11,14). cell(17,12,1). cell(17,13,18). cell(17,14,15). cell(17,15,8). cell(17,16,2). cell(17,17,4). cell(17,18,5). cell(17,19,16). cell(17,20,7). 
cell(18,1,16). cell(18,2,11). cell(18,3,7). cell(18,4,18). cell(18,5,4). cell(18,6,3). cell(18,7,14). cell(18,8,7). cell(18,9,7). cell(18,10,15). cell(18,11,1). cell(18,12,3). cell(18,13,10). cell(18,14,9). cell(18,15,6). cell(18,16,17). cell(18,17,19). cell(18,18,2). cell(18,19,12). cell(18,20,2). 
cell(19,1,7). cell(19,2,15). cell(19,3,12). cell(19,4,14). cell(19,5,3). cell(19,6,19). cell(19,7,4). cell(19,8,10). cell(19,9,17). cell(19,10,17). cell(19,11,20). cell(19,12,16). cell(19,13,1). cell(19,14,18). cell(19,15,14). cell(19,16,9). cell(19,17,1). cell(19,18,2). cell(19,19,7). cell(19,20,8). 
cell(20,1,13). cell(20,2,3). cell(20,3,16). cell(20,4,8). cell(20,5,10). cell(20,6,2). cell(20,7,10). cell(20,8,4). cell(20,9,10). cell(20,10,16). cell(20,11,3). cell(20,12,18). cell(20,13,19). cell(20,14,8). cell(20,15,17). cell(20,16,12). cell(20,17,12). cell(20,18,6). cell(20,19,2). cell(20,20,20). 


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
