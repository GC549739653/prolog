%GUOCHANG
%z5140582
%----------------------------------111
neg(X):-%Is it less than 0
    X<0.
pos(X):-%Is it greater than 0?
    X>=0.
sumsq_neg([],0).%basecase
sumsq_neg([Head | Tail], Sum) :-%less than 0,it's calculated,and then recursive.
    neg(Head),
    sumsq_neg(Tail, S),
    Sum is S+ Head * Head.
sumsq_neg([Head | Tail], Sum) :-%Greater than 0, direct recursion, no calculation.
    pos(Head),
    sumsq_neg(Tail, Sum).

%----------------------------------222
%likes(mary, apple).
%likes(mary, pear).
%likes(mary, grapes).
%likes(tim, mango).
%likes(tim, apple).
%likes(jane, apple).
%likes(jane, mango).

%all_like([],_).%basecase
%all_like([H|T],F):-%Everyone on the left likes the fruit.
    %likes(H,F),
    %all_like(T,F).

like_all(_,[]).%basecase
like_all(P,[X|Y]):-%Decide if the person likes all the fruit in the list.
    likes(P,X),
    like_all(P,Y).

all_like_all([],_).%basecase

all_like_all(_,[]).%basecase

all_like_all([H|T],[X|Y]):-%Using like_all function to everyone in the list.
    like_all(H,[X|Y]),
    all_like_all(T,[X|Y]).
%-----------------------------------------------3
%gre(X,Y):-
    %X>=Y.
%append([], L, L).
%append([X|L1], L2, [X|L]):-append(L1, L2, L)
%sqrt_table(0,0,_).
sqrt_table(N,M,Result):-
    N>M,%So let's see if N is greater than M.
    N>=0,
    M>=0,
    X is sqrt(M),%now we have X
    Y=[[M,X]],
    S is M+1,
    sqrt_table(N,S,Resultt),
    append(Resultt,Y,Result).%add Y into the list.
sqrt_table(N,M,Result):-%When this is equal to the situation, it's over.
    N=M,
    N>=0,
    M>=0,
    SqrtN is sqrt(N),
    Result = [[N,SqrtN]].

%sqrt_table(N,M,[[N,NS]|Result]):-
 %   N>M,
  %  N>0,
   % M>0,
    %NS is sqrt(N),
    %S is N-1,
    %sqrt_table(S,M,Result).
%sqrt_table(N,N,[[N,Result]]):-
	%Result is sqrt(N).
%--------------------------------------------4444
chop_up([], []).%basecase
chop_up([Head|Tail],[[Head,NewlistNextHead]|NewlistTail]):-
%If it is a growth, choose the choosefunction operation, select the maximum number of growth sequences, and select the remaining list.
  is_increase(Head, Tail),
  choosefunction(Tail,NewlistNextHead,TTail),
  chop_up(TTail, NewlistTail).
chop_up([Head|Tail], [NewlistHead|NewlistTail]) :-
%If it's not growing, use the previous list of growth sequences as head and recurse on the rest of the list.
  not(is_increase(Head, Tail)),
  NewlistHead is Head,
  chop_up(Tail,NewlistTail).
  %append(X,Tail,[Head|Tail]),
  %print(X),
  %print(Tail),
  %print([Head|Tail]),
  %append(H,X,NewList),
  %print(NewList),
  %append([NewList],Tail,New),
  %print(New),
  %chop_up(New, _).


is_increase(Number, [Head|_]) :-
  Number =:= Head -1.%Let's see if the next one is plus 1.
choosefunction([Head|Tail],Next,TTail):-
    %If yes, record the large number and continue recursion.
    is_increase(Head,Tail),
    choosefunction(Tail,Next,TTail).
choosefunction([Head|Tail],Head,Tail):-%Judge, if not, stop
    not(is_increase(Head,Tail)).
choosefunction(First,Second,_):-
    %If there is only one number left, just pick it up.
    length(First,1),
    Second is First.




%------------------------------------------------5555
tree_eval(_Number, tree(empty,X,empty), X) :-
    number(X).%judge X if it is a number

tree_eval(Number, tree(empty,X,empty), Number ) :-
    X =z.%put z into X.


% recursive branches of tree
tree_eval(Value, tree(tree(Lleft,Lop,Lright),Op,tree(Rleft,Rop,Rright)), Eval ) :-
    tree_eval(Value, tree(Lleft,Lop,Lright), LeftEval ),% rec subtree1
    tree_eval(Value, tree(Rleft,Rop,Rright), RightEval ),% rec subtree2
    Expression=..[Op,LeftEval ,RightEval ],% calculate...
    Eval  is Expression.
