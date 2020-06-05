% Source is arith.pl from Class Material.
 %--------------------------------------------
  % nat(N) :- N is a natural number
  nat(0).
  nat(s(X)) :- nat(X).

%-----------------------------------------------

  % add(X,Y,Z) :-
  %    X,Y,Z are natural numbers 
  %    such that  Z = X+Y

  add(0, X, X). % :- nat(X).
  add(s(X), Y, s(Z)) :- add(X,Y,Z).

 %--------------------------------------------

  % leq(X,Y) :-
  %   X,Y are natural numbers 
  %    such that X is less or equal than Y

  leq(0,_).
  leq(s(X),s(Y)) :- leq(X,Y).

% alternative
  leq2(X,Y) :- add(X,_,Y).

%----------------------------------------------

  % lt(X,Y) :-
  %   X,Y are natural numbers 
  %    such that X is less than Y

  lt(0,s(_)).
  lt(s(X),s(Y)) :- lt(X,Y).

% alternative
  lt2(X,Y) :- add(X,s(_),Y).

%-----------------------------------------------
  % times(X,Y,Z) :-
  %    X,Y,Z are natural numbers 
  %    such that  Z = X*Y

  
  times(0,_X,0).% :- nat(X).
  times(s(X),Y,Z) :- lt(X,Z),leq(Y,Z),times(X,Y,W), add(W,Y,Z).
 
% --------------------------------------------
% member(X,Y)
% X is is one of the element in the list Y return true\false.

member(X,[X|_Y]).
member(X,[_|Xs]) :- member(X,Xs).

%-----------------------------------------------
%-----------------------------------------------
% ----------------------------------------------
% All the predicate above we learn in the class.
%-----------------------------------------------
%-----------------------------------------------
%-----------------------------------------------

% Task 1: Unary Suare Root
% unary_sqrt(+,-)
% 
%?- unary_sqrt(nonat,A).
%false.
% 
%?- unary_sqrt(s(s(s(s(0)))),A).
%A = s(s(0));
%false.

%?- unary_sqrt(0,A).
%A = 0;
%false.

%?- unary_sqrt(s(0),A).
%A = s(0);
%false.

%?- unary_sqrt(s(s(0)),A).
%A = s(s(0));
%false.

unary_sqrt(N,K) :- times(K,K,N), nat(K).   % K*K = N

% The predicate is work in mode inary_sqrt(-,+), because the definition of 
% the predicate times(+,+,-) that first chack if lt(X,Z). and leq(Y,Z).
% and prolog searching depth first.

unary_sqrt_rev(N,K) :- unary_sqrt(N,K).

% ----------------------------------------------
% ----------------------------------------------

%Task 2: unary_divisor(N,K).
% unary_divisor(+,-)ץ


%?- unary_divisor(s(s(s(s(s(s(s(s(s(s(s(s(0)))))))))))), X).
%X = s(0) ;
%X = s(s(0)) ;
%X = s(s(s(0))) ;
%X = s(s(s(s(0)))) ;
%X = s(s(s(s(s(s(0)))))) ;
%X = s(s(s(s(s(s(s(s(s(s(s(s(0)))))))))))) ;
%false ;

%% divisors of 5: 1 and 5
%?- unary_divisor(s(s(s(s(s(0))))), X).
%X = s(0) ;
%X = s(s(s(s(s(0))))) ;
%2
%false
unary_divisor(0,s(N)) :- nat(N).
unary_divisor(K,N) :- nat(K),lt(0,K),times(N,R,K),nat(R).

%The parediciate is working in mode unary_divisor(-,+) but there is infinity 
%numbers, for every unary number X, X is divisor of (X,X*2,X*3,X*4,X*5........),
%so the parediciate get into infinity loop. (infinity answers).
%also because of my changes in the deffinition of the parediciate times 
%(the same explanation of Task 1) the parediciate working in that way. 

unary_divisor_rev(K,N) :- unary_divisor(N,K).

% ----------------------------------------------
%-----------------------------------------------
%Task 3: is_binary(B).
% is_binary(+).

%?- is_binary([]).
%true.

%?- is_binary([0,1,0,0,1]).
%true.

%?- is_binary([1,0,0,1,0]).
%false.

%?- is_binary(X).
%false.

%?- is_binary([1,1,X,1]).
%false.

is_binary_sim([]).
is_binary_sim([0|Xs]) :- is_binary_0_sim(Xs).
is_binary_sim([1|Xs]) :- is_binary_1_sim(Xs).


is_binary_1_sim([]).
is_binary_1_sim([0|Xs]) :- is_binary_0_sim(Xs).
is_binary_1_sim([1|Xs]) :- is_binary_1_sim(Xs).

is_binary_0_sim([0|Xs]) :- is_binary_0_sim(Xs).
is_binary_0_sim([1|Xs]) :- is_binary_1_sim(Xs).

%----- The explain of is_binary_sim at the PDF file

is_binary(X) :- nonvar(X), append([],[],X).

is_binary([X|Xs]) :- nonvar(X),X is 0,
      is_binary_0(Xs).

is_binary([X|Xs]) :- nonvar(X),X is 1,
      is_binary_1(Xs).

is_binary_1(X) :- nonvar(X),append([],[],X).
is_binary_1([X|Xs]) :- nonvar(X), X is 1,
        is_binary_1(Xs).
is_binary_1([X|Xs]) :- nonvar(X), X is 0,
        is_binary_0(Xs).

is_binary_0([X|Xs]) :- nonvar(X), X is 1,
        is_binary_1(Xs).
is_binary_0([X|Xs]) :- nonvar(X), X is 0,
        is_binary_0(Xs).



%-----------------------------------------------
% ----------------------------------------------

%Task 4: binary_plus(X,Y,Z).
% binary_plus(+,+,-).

%?- binary_plus([1,0,1], [1,1,1], C).
%C = [0, 0, 1, 1] ;

%?- binary_plus_rev(A, B, [1,0,1]).
%A = [1, 0, 1],
%B = [] ;
%A = [],
%B = [1, 0, 1] ;
%A = [1],
%B = [0, 0, 1] ;
%A = [0, 0, 1],
%B = [1] ;
%A = [0, 1],
%B = [1, 1] ;
%A = [1, 1],
%B = [0, 1] ;
%false



%first we chack if X and Y is binary number, then start the calculation of X+Y when the carry is 0.
binary_plus(X,Y,Z) :- is_binary(X),is_binary(Y),binary_plus_carry(X,Y,Z,0).
%base case when the carry is 0.
binary_plus_carry([],[],[],0) .
binary_plus_carry([],[Ys|Yn],[Ys|Yn],0) .
binary_plus_carry([Xs|Xn],[],[Xs|Xn],0) .
%All the posible given X and Y when the carry is 0.
binary_plus_carry([0|Xn],[0|Yn],[0|Zn],0) :-  binary_plus_carry(Xn,Yn,Zn,0).
binary_plus_carry([1|Xn],[0|Yn],[1|Zn],0) :-  binary_plus_carry(Xn,Yn,Zn,0).
binary_plus_carry([0|Xn],[1|Yn],[1|Zn],0) :-  binary_plus_carry(Xn,Yn,Zn,0).
binary_plus_carry([1|Xn],[1|Yn],[0|Zn],0) :-  binary_plus_carry(Xn,Yn,Zn,1).
%base case when the carry is 1.
binary_plus_carry([],[],[1],1).
binary_plus_carry([],[Ys|Yn],Zn,1) :- binary_plus_carry([1],[Ys|Yn],Zn,0).
binary_plus_carry([Xs|Xn],[],Zn,1) :- binary_plus_carry([Xs|Xn],[1],Zn,0).
%All the posible given X and Y when the carry is 1.
binary_plus_carry([0|Xn],[0|Yn],[1|Zn],1) :-  binary_plus_carry(Xn,Yn,Zn,0).
binary_plus_carry([1|Xn],[0|Yn],[0|Zn],1) :-  binary_plus_carry(Xn,Yn,Zn,1).
binary_plus_carry([0|Xn],[1|Yn],[0|Zn],1) :-  binary_plus_carry(Xn,Yn,Zn,1).
binary_plus_carry([1|Xn],[1|Yn],[1|Zn],1) :-  binary_plus_carry(Xn,Yn,Zn,1).


%The progrem is not wonrk correctly in the mode binary_plus(-,-,+),
%because first for binary_plus(X,Y,[0,1]) for example I check if is_binary(X) and is_binary(Y).
%and my definition for is_binary(+) is that is_binary(X) or is_binary(Y) is not list of binary number 
%and the predicate will return 'false'.

%binary_plus_rev(-,-,+)
%I used all the predicate of binary_plus(+,+,-).

binary_plus_rev(X,Y,Z) :- is_binary(Z),binary_plus_carry(X,Y,Z,0),is_binary(X),is_binary(Y).

%-----------------------------------------------
% ----------------------------------------------

%Task 5:binary_palindrom(B).
%binary_palindrom(-)

%?- Binary = [X1, X2, X3],
%binary_palindrome(Binary).
%Binary = [1, 0, 1];
%Binary = [1, 1, 1];
%false

%?- binary_palindrome(X).
%X = [] ;
%X = [1] ;
%X = [1, 1] ;
%X = [1, 0, 1] ;
%X = [1, 1, 1] ;
%4
%X = [1, 0, 0, 1] ;
%X = [1, 1, 1, 1] ;
%X = [1, 0, 0, 0, 1] ;
%X = [1, 0, 1, 0, 1] ;
%. . .

%will use the reverse(+,-) predicate to build a list of veribale that represent a palindrom 
%and the is_binary(+) will repalce the varibales in 0 and 1.

binary_palindrom(B) :- reverse(B,B),
                      is_binary_sim(B).

%reverse(B1,B2) B2 is the list the create from the reverese of B1
%reverse(+,-).

reverse(B1,B2) :- reverse_my(B1,B2,[]).
reverse_my([],B2,B2) .
reverse_my([Bs|Bn],B2,Acc) :- reverse_my(Bn,B2,[Bs|Acc]).

% ----------------------------------------------
% ----------------------------------------------

%Task 6:binary_palindrom_sum(+,-,-,-).
%binary_palindrom_sum(N,A,B,C).

%?- N = [0, 0, 1, 1],
%binary_palindrome_sum(N, A, B, C).

%A = [],
%B = [1, 1],
%C = [1, 0, 0, 1] ;
%A = [],
%B = [1, 0, 1],
%C = [1, 1, 1] ;
%false

%First the predicat we get A,B,C binary_number, by using binary_plus_rev
%then check if thay palindrom, then check 0<=A<=B<=C

binary_palindrom_sum(N,A,B,C) :- binary_plus_rev(B1,C,N),
                                 binary_plus_rev(A,B,B1),
                                 reverse(C,C),
                                 reverse(B,B),
                                 reverse(A,A),
                                 beq(B,A),
                                 beq(C,B).


%beq(A,B) 
% A,B binary palindrom number,
%  such that A is big or equal then B
%beq(+,+)             
beq(_,[]).
beq([0|_],[1]).
beq([1|As],[0|Bs]) :- lbeq(As,Bs).
beq([1|As],[1|Bs]) :- beq(As,Bs),!.
beq([0|As],[0|Bs]) :- beq(As,Bs),!.


%lbeq(A,B) 
% A,B binary number, such that A length is big or equal then B
%lbeq(+,+)
lbeq(_,[]).
lbeq([_|As],[_|Bs]) :- lbeq(As,Bs).



% ----------------------------------------------
% ----------------------------------------------

%Task 7: is_prime(N)
%is_prime(+)

%?- is_prime(2).
%true.

%?- is_prime(22).
%false.

%?- is_prime(26233793190084349893096347).
%true.

%?- is_prime(28769375361317015373302983).
%false.

is_prime(Num,X) :- between(2,X,N),0 is Num mod N, !.

is_prime(X) :- sqrt(X,X1),floor(X1,X2),X3 is X2+1,not(is_prime(X,X3)).
is_prime(2).


% ----------------------------------------------
% ----------------------------------------------

%Task 8: right_prime(N)
%right_prime(+).

%?- right_prime(7393).
%true.

%?- right_prime(937).
%false.

%?- right_prime_gen(N).
%N = 2 ;
%N = 23 ;
%N = 233 ;
%N = 239 ;
%N = 29 ;
%N = 293 ;
%N = 3 ;
%N = 31 ;
%. . .


right_prime(N) :-   N > 1,
            N =< 73939133,
            is_prime(N),
                    N1 is div(N,10),
                    right_prime(N1).
right_prime(0) .

    
% by using the predicate between I send all the 
%number from 2 (the lowest prime number)
% to 73939133 (the highest right number) to the 
%predicate right_prime to check the if he is right_prime.
    
right_prime_gen(N) :- between(2,73939133,N),
              right_prime(N).
    