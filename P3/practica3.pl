/*Ejercicio 1*/
pertenece(E, [E|_]).
pertenece(E, [_|Xs]) :- pertenece(E, Xs).
pertenece(E, [Xs|E]) :- pertenece(E, Xs).

/*Ejercicio 2*/
concatena([], L, L).
concatena([X|L1], L2, [X|L3]) :- concatena(L1, L2, L3).

invierte([], []).
invierte([E|Xs], L) :- invierte(Xs, Y), concatena(Y, [E], L).

/*Ejercicio 3*/
/*INORDER*/
inorder(tree(I, nil, nil), [I]).
inorder(tree(I, L, nil), [L1|I]) :- inorder(L, L1).
inorder(tree(I, nil, R), [I|L1]) :- inorder(R, L1).
inorder(tree(I, L, R), Ls) :- inorder(L, Y), inorder(R, Z), 
    concatena(Y, [I], A), concatena(A, Z, Ls).

/*PREORDER*/
preorder(tree(I, nil, nil), [I]).
preorder(tree(I, L, nil), [I|L1]) :- preorder(L, L1).
preorder(tree(I, nil, R), [I|L1]) :- preorder(R, L1).
preorder(tree(I, L, R), Ls) :- preorder(L, Y), preorder(R, Z), 
    concatena([I], Y, A), concatena(A, Z, Ls).

/*POSORDER*/
postorder(tree(I, nil, nil), [I]).
postorder(tree(I, L, nil), Ls) :- postorder(L, L1), concatena(L1, [I], Ls).
postorder(tree(I, nil, R), Ls) :- postorder(R, L1), concatena(L1, [I], Ls).
postorder(tree(I, L, R), Ls) :- postorder(L, Y), postorder(R, Z), 
    concatena(Y, Z, A), concatena(A, [I], Ls).


/*Ejercicio 4*/
insertar(E,Xs,1,[E|Xs]).
insertar(E,[F|Xs],N,[F|L1]):- M is N-1 ,insertar(E,Xs,M,L1).

/*Ejercicio 5*/
extract([X|L], X, L).
extract([E|L],X,[E|L1]):- extract(L,X,L1).

/*Ejercicio 6*/
man_pref(juan, [maria, carmen, pilar]).
man_pref(pedro, [carmen, maria, pilar]).
man_pref(mario, [maria, carmen, pilar]).
woman_pref(maria, [juan, pedro, mario]).
woman_pref(carmen, [pedro, juan, mario]).
woman_pref(pilar, [juan, pedro, mario]).

/*Ejercicio 7*/
pos(X, [X|_], 1).
pos(X, [_|R], N) :- pos(X, R, M),  N is M + 1.
unstable(M1-W1, M) :- pertenece(M2-W2, M), man_pref(M1, M_pref), 
    woman_pref(W2, W_pref), pos(W2, M_pref, W2_pos), 
    pos(W1, M_pref, W1_pos), W2_pos<W1_pos, pos(M2, W_pref, M2_pos), 
    pos(M1, W_pref, M1_pos), M1_pos<M2_pos.

/*Ejercicio 8*/
smp([],[], M, M).
smp(FreeMan, [W|FreeWoman], MIn, MOut) :-
    woman_pref(W, Mp), /*Cogemos su lista de preferencias*//*Cogemos su favorito*/
    pertenece(M, Mp),
    not(unstable(M-W, MIn)), /*Vemos que no sea inestable*/
    extract(FreeMan, M, FreeMan2),
    smp(FreeMan2, FreeWoman, [M-W|MIn], MOut).
    


