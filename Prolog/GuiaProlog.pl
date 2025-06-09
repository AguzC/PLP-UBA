%Comentarios con '#', y para cargar el archivo "consult('GuiaProlog')." 
% comentario o %! documentacion
%?- make. para recargar cambios de archivos.
%?- consulta. para evaluar una consulta.
%; para ver la siguiente solucion.
%. para dejar de listar soluciones.
%CTRL+D o ?- halt. para salir del interprete.
%?- E. quiz´as para reırse.
%:- use_module(archivo). para importar un archivo dentro de otro.
 % comentario o %! documentaci´on
%?- help(predicado). para ver la documentacion de un predicado
 

%Ejercicio 1:

%! padre(+X,+Y)
% Algo hace, asi se documenta esta gilada.

padre(jesu,juan).
padre(jesu,pedro).
padre(juan, carlos).
padre(juan, luis).
padre(carlos, daniel).
padre(carlos, diego).
padre(luis, pablo).
padre(luis, manuel).
padre(luis, ramiro).
padre(pepe,tete).
padre(tete,lele).
padre(lele,dede).

%! abuelo(?X,?Y)
% X abuelo de Y
abuelo(X,Y) :- padre(X,Z), padre(Z,Y).

%! hermano(?X,?Y)
% X hermano de Y
hermano(X,Y) :- padre(A,X), padre(A,Y), X \= Y.

%! descendiente(?X,?Y)
% X descendiente de Y
descendiente(X,Y) :- padre(Y,X).
descendiente(X,Y) :- padre(Z,X), descendiente(Z,Y).

%! ancestro(?X,?Y)
%Diria que si pones ancestro antes que padre, tonces siempre quiere 
%encontrar un ancestro que llegue al caso base y despues que cumpla
%la condicion de padre....?
ancestro(X, Y) :- padre(X,Y).
ancestro(X, Y) :- padre(X,Z), ancestro(Z,Y).

%Ejercicio 2:

%Si le cambias el orden, los resultados salen al reves, veamos el ejemplo vecino(5, Y, [5,6,5,3]) 
%entiendo que lo que pasa es: va a hacer un DFS hasta llegar a [], tonces no entra en ninguno,
%y despues vuelve a [3], que tampoco cumple la segunda, despues llega a [5,3], la cual si cumple,
%por lo que devuelve 3, y asi, al cambiarlo, la forma en la que busca DFS cambia(?,kinda foldr y
%foldl, maybe....?, aclaracion, si lo dejamos como esta, primero entra con la lista entera en el
%primer hecho, por lo que ve que cumple con Y = 6.
vecino(X, Y, [X|[Y|_]]).
vecino(X, Y, [_|Ls]) :- vecino(X, Y, Ls).

%Ejercicio 3:

natural(0).
natural(suc(X)) :- natural(X).

%menorOIgual(0,X) como viene rompe todoooo. Entiendo que busca un Y que si le sacas el suc() cumpla,
%y pasa que ta sacando infinitamente suc's. Un programa se puede colgar sin soluciones si busca una 
%solucion de una forma recursiva que nunca llegue a un caso base, como el caso anterior, siempre 
%saca suc y nada lo detiene.
%menorOIgual(X, suc(Y)) :- menorOIgual(X, Y). Este es el que funcaba mal y se colgaba con 0.
%menorOIgual(X,X) :- natural(X).
%menorOIgual(X,Y) -> X <= Y?
%MOI(0,X) es "dame todos los x tq 0 <= X", asiq da 0 y naturales mayores a 0...
%MOI(X,0) es "dame todos los x tq x <= 0", da 0 y nada, ningun natural es menor
%MOI(X,Y) es "dame todos los x,y tq x <= y" asiq daria todos los naturales para 
%ambos, aunq nunca vamos a ver a X distinto de 0, solo va a subir Y, porq la forma
%en la que definimos, esta primero MOI(0,X), asiq va a buscar todos los X que cumplan
%la primer def, asiq nunca llegamos al siguiente.

%! menorOIgual(?X,?Y)
menorOIgual(0,X) :- natural(X). %Esto seria, si tenes un 0 y cualquier natural, se cumple 0 <= X.
menorOIgual(suc(X),suc(Y)) :- menorOIgual(X,Y).

menorOIgual2(X,X) :- natural(X).
menorOIgual2(X, suc(Y)) :- menorOIgual2(X, Y).

%Ejercicio 4:

%!juntar(?Lista1,?Lista2,?Lista3)
%juntar(L1,L2,L3) Tiene exito si L3 es L1 ++ L2, esto es exactamente lo mismo que append que ta hecho.
%Tonces, +L1,+L2,+L3 --> Me dice si L3 es L1 ++ L2
% +L1,+L2,-L3 --> Me da la instanciacion de L3 tq L1 ++ L2
% Y ahora, si falta alguna de las primeras, me da posibles formas de combinar para llegar a la ultima.
juntar([],L,L).
juntar([X|L1],L2,[X|L3]) :- juntar(L1,L2,L3). 
 

%Ejercicio 5:
%Definir los preds usando append

%! last(?L,?U)
%Donde U es el ultimo elemento de la lista L
%So freaking magico, U es el ultimo elemento de L si L es CualquierCosa ++ [U].
last(L, U) :- append(_, [U], L).

%! reverse(+L, ?R)
%donde R contiene los mismos elementos que L, pero en orden inverso.
%Ejemplo: reverse([a,b,c], [c,b,a]).
%Si ponemos las condiciones al reves y hacemos reverse(+L1,-L2), despues de encontrar
%la unica solucion, se traba.... intentemos entender porq.
%Yo se que append, si lo llamas como append(-X,+Y,-Z), va a devolver las instanciaciones 
%de X e Z tq Y sea el final, asiq, si ponemos primero la condicion del append, cuando se 
%haga el DFS, se va a meter y va a encontrar las infinitas soluciones donde X es vacio y
%le va agregando elementos, y Y es X ++ Y, asiq solo va a ver que tiene infinitas sols.
%Ahora, si ponemos reverse(L1,L3) antes, va a buscar un L3 que cumpla, tonces, cuando 
%lo tenga, va a tener una instanciacion de L3, asiq el append pasa a ser +X,+Y,-Z diria,
%por lo que va a ponerse a buscar en resultados finitos teoricamente.

reverse([],[]).
reverse([X|L1],L2) :- reverse(L1,L3), append(L3,[X],L2).

%! i. prefijo(?P, +L)
%P es prefijo de la lista L

prefijo([],[]).
prefijo(L1,L2) :- append(L1,_,L2).

%! sufijo(?S, +L)
%donde S es sufijo de la lista L.

sufijo([],[]).
sufijo(L1,L2) :- append(_,L1,L2).

%! sublista(?S, +L) 
%donde S es sublista de L
%SL es sublista de L si: Existe SL2 tq cualquier cosa ++ SL2 = L y SL ++ cualquier cosa = SL2 y SL != [].
%Osea, es subLIsta si existe SL2 tq esta otra lista sea sufijo de L y la sublista SL sea prefijo de esta Otra.
%Me gusta pensarlo como: SL2 al ser sufijo, son todos los sufijos de L, tonces tenes todas las listas donde 
%le vas sacando 0 o mas elementos desde el fondo, y despues, se tiene que cumplir, que para ese sufijo, la 
%sublista que tenes sea prefijo, asiq, al sufijo entero, que no saco todavia nada, le sacas uno a uno todos 
%sus prefijos, despues, al sufijo sin el ultimo le sacas todos, y asi, maravilloso y magico.
%IMPORTANTE, si no pones que tu sublista no unifica con [] (Unificar es un == sintactico), tonces se rompe,
%PREGUNTA PORQ CORNO SE ROMPEEEEEE.............

sublista([],_).
sublista(SL, L) :-
	append(_, SL2, L), % sufijo (SL2,L)
	append(SL, _, SL2),% prefijo(SL,SL2)   
	SL \= [].
