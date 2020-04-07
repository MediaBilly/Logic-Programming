:- set_flag(print_depth,1000).

% Functions discussed in course
first_column([], [], []).
first_column([[X|Row] | M], [X|Xs], [Row|Rest]) :- first_column(M, Xs, Rest).

distribute1(_,[],[]).
distribute1(X,[V|Vs],[[X|V]|Out]):-
    distribute1(X,Vs,Out).

distribute([],_,[]).
distribute([X|Xs],Vs,Out):-
    distribute1(X,Vs,Out1),
    distribute(Xs,Vs,Out2),
    append(Out1,Out2,Out).

cart_prod([],[[]]).
cart_prod([X|Rest],CP):-
    cart_prod(Rest,CP1),
    distribute(X,CP1,CP).

matr_transp([[X]|Y],[[X|Z]]):-flatten(Y,Z).
matr_transp(X,[Col|Rest_Cols]):-
    first_column(X,Col,Rest),
    matr_transp(Rest,Rest_Cols).

% Returns the dot_product of 2 vextors
dot_product([],[],0).
dot_product([X|RestX],[Y|RestY],P):-
    dot_product(RestX,RestY,P2),
    P is P2 + X * Y.

% Multiplies a specific row with a matrix
multiply_row_with_matrix(X,[[Y]|Z],[P]):-
    flatten([[Y]|Z],W),
    dot_product(X,W,P).
multiply_row_with_matrix(X,Y,[P|Rest]):-
    first_column(Y,Col,RestY),
    dot_product(X,Col,P),
    multiply_row_with_matrix(X,RestY,Rest).

matr_mult([X],Y,[M]):-multiply_row_with_matrix(X,Y,M).
matr_mult([X|Rest_Rows],Y,[M|Rest]):-
    multiply_row_with_matrix(X,Y,M),
    matr_mult(Rest_Rows,Y,Rest).

% Fuctions to remove nth element from a list(counting from 1)
drop_nth_element([_|Rest],N,N,Rest).
drop_nth_element([X|T],N,I,[X|Rest]):-
    K is I + 1,
    drop_nth_element(T,N,K,Rest).

remove_nth_element(X,N,L):-
    drop_nth_element(X,N,1,L).

% Implements recursive sub-determinant calculation step of laplace determinant algorithn
subdet([],_,_,_,0).
subdet([X|RestX],M,Col,Sign,D):-  
    matr_transp(M,Mtr),
    remove_nth_element(Mtr,Col,SubMatr),
    matr_det(SubMatr,SubD),
    NewCol is Col + 1,
    NewSign is Sign * -1,
    subdet(RestX,M,NewCol,NewSign,D2),
    D is D2 + X*SubD*Sign.

matr_det([[X,Y],[Z,W]],D):-
    D is X*W - Y*Z.
matr_det([X|Rest],D):-
    length(X, L),L > 2,
    subdet(X,Rest,1,1,D).
