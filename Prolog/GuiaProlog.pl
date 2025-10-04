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

%Ejercicio 3.5:

longitud([],0).
longitud([_|XS],Y) :- longitud(XS,Z),
					   Y is Z + 1.
						

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
reverse([X|L],L2) :- reverse(L,L3), append(L3,[X],L2).

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
%Update, se rompia porque cuando comentaba para probar sin la ultima condicion no taba cambiando de lugar 
%el punto '.', asiq solo me tiraba la primer linea del sublista, la ultima condicion es para que la lista 
%vacia no aparezca como solucion multiples veces por la regla.

sublista([],_).
sublista(SL, L) :-
	append(_, SL2, L), % sufijo (SL2,L)
	append(SL, _, SL2),% prefijo(SL,SL2)   
	SL \= [].

%Subconjunto /= Sublista, la sublista solo puede tener combinaciones con elementos contiguos, por ejemplo,
%si L = [1,2,3,4], [1,4] no es una sublista de L pero si un subcojunto.
%La diferencia cuando lo pensas es que la sublista es dar caso a caso la lista con y la lista sin un 
%elemento, mientras que la sublista al ser contigua, la pdoes ver con el predicado de arriba.




%!pertenece(?X, +L)
% X pertence a L?, y en prolog esta definido como "member"
%Aca toda la magia sale de sublista jaja.

pertenece(X,XS) :- sublista([X],XS).

%Otra def que no depende de sublista, kinda better.
pertenece2(X, [X| _]).
pertenece2(X, [_| XS]) :- pertenece(X, XS).

%Ejercicio 6:

%!aplanar(+Xs, -Ys)
%es verdadero sii Ys contiene los elementos de todos los niveles de
%Xs, en el mismo orden de aparición. Los elementos de Xs son enteros, átomos o nuevamente listas, de modo que
%Xs puede tener una profundidad arbitraria. Por el contrario, Ys es una lista de un solo nivel de profundidad.
%Ejemplos:
%?- aplanar([a, [3, b, []], [2]], L).→ L=[a, 3, b, 2]
%?- aplanar([[1, [2, 3], [a]], [[[]]]], L).→ L=[1, 2, 3, a]
%queda en veremos por ahora

/* aplanar([],YS).
aplanar([X|XS],YS) :- pertenece2(X,YS), aplanar(XS,YS).
aplanar([[X]|XS],YS) :-  append(X,XS), XS = L3 , aplanar(L3,YS). 
*/



%Ejercicio 7:
%!interseccion(+L1, +L2, -L3)
%L3 es la intersección sin repeticiones de las listas L1 y L2, 
%respetando en L3 el orden en que aparecen los elementos en L1.

interseccion([],_,[]).
interseccion([X|L1],L2,L3) :- not(member(X,L2)), interseccion(L1,L2,L3).
interseccion([X|L1],L2,[X|L3]) :- member(X,L2), interseccion(L1,L2,L3).


%!partir(N, L, L1, L2)
%L1 tiene los N primeros elementos de L, y L2 el resto. Si L tiene menos de N
%elementos el predicado debe fallar. ¾Cuán reversible es este predicado? Es decir, ¾qué parámetros pueden
%estar indefinidos al momento de la invocación?
%partir +N, +L, -L1 Y -L2 funca
%partir -N, +L, -L1 Y -L2 si el len va adelante, se cuelga porq, despues de dar las soluciones para 
%todas las particiones de la lista con long de 0 a len(L), busca si existe otra lista de mas longitud que 
%cumpla, lo cual no va a pasar, pero si lo hacemos al reves, el append no es un generador infinito, asi
%que funca, pero creo que hace mas inferencias, nop, es peor poner el len adelante, cada solucion distinta
%cuesta una inferencia mas que la anterior, seems pretty lineal to me, mientras que la del append es constante.
%partir -N, -L, -L1 Y -L2 no se cuelga, se pone a generar infinitamente en tiempo lineal, despues del primero 
%solo le toma 6 inferencias cada uno nuevo.

partir(N,L,L1,L2) :- append(L1,L2,L), length(L1,N).

%Version mas recursiva, pero esta no es reversible en N, necesita eso instanciado para N2 is N-1.
partir2(0, L, [], L).
partir2(N, [X| XS], [X | L1], L2) :- N2 is N-1, partir(N2, XS, L1, L2).

%!borrar(+ListaOriginal, +X, -ListaSinXs)
%que elimina todas las ocurrencias de X de la lista ListaOriginal.
%Extra: CADA VEZ QUE LEAS ESTO ACORDATE QUE TE TOMO 20 MINUTOS MAS PORQ LA FORMA RECURSIVA LA TABAS
%HACIENDO CON NOMBRES DE VAR DISTINTOS Y POR ESO NO DABA, GIL.

borrar([],_,[]).
borrar([X|LI],X,LF) :- borrar(LI,X,LF). %Aca elimino de LF el X que aparece en LI
borrar([Y|LI],X,[Y|LF]) :- Y \= X, borrar(LI,X,LF). %Aca ignoro el Y si es /= de X, asiq es parte 
													%del resultado final.

%!sacarDuplicados(+L1, -L2)
%que saca todos los elementos duplicados de la lista L1.

sacarDuplicados([],[]).
sacarDuplicados([X|XS],RES) :- member(X,XS), sacarDuplicados(XS,RES).
sacarDuplicados([X|XS],[X|RES]) :- not(member(X,XS)), sacarDuplicados(XS,RES).

%Mucho mas linda y declarativa, viva el dueño.
sacarDuplicados2([], []).
sacarDuplicados2([X | L1], [X | L2]) :- borrar(L1, X, L3), sacarDuplicados(L3, L2).

%!permutación(+L1, ?L2).
%Pretty self explanatory


permutacion([], []).
permutacion([H|T], P) :- permutacion(T, Q), insertar(H, Q, P).
%Okey, analicemos la magia negra de Brian, la permutacion de la lista vacia es la misma, perfecto,
%ahora, si tengo al menos un elemento, P es la permutacion si, existe un Q que sea la permutacion
%de T y P es el resultado de insertar el elemento que tenemos en Q

%!insertar(+X,+L,-RES)
%verdadero si res es alguna instancia de la lista L con X en alguna posicion.

insertar(X,L,RES) :- append(L1,L2,L) ,append(L1,[X|L2],RES).

%!reparto(+L, +N, -LListas)
%que tenga éxito si LListas es una lista de N listas (N ≥ 1) de cualquier
%longitud - incluso vacías - tales que al concatenarlas se obtiene la lista L.


reparto([], 0, []).
reparto(L, N, [X | LListas]) :- N > 0, append(X, XS, L), N2 is N-1, reparto(XS, N2, LListas).
%Si N > 0, Existe XS tq X ++ XS = L, N2 es N-1, y la cola de la original es el reparto 
%de XS con N2. I guess la mejor forma de pensar reparto es ignorando la parte de que queres 
%generar todas las posibles listas que dan la lista y concentrarte en hacer el resultado, 
%aca lo que plantea es ir recursivamente diciendo que la primer lista de LListas apendeada
%con una lista XS da la lista L, de todo junto, asiq tenemos X ++ XS = L, y ahora, XS cumple
%que es la lista de donde se hace el reparto de la cola, asiq va diciendo que L se forma 
%recursivamente con las posibles listas de LListas.

%Ejercicio 8:
%!parteQueSuma(+L,+S,-P)

parteQueSuma(L,S,P) :- subConjunto(L,P), sumlist(P,S).

subConjunto([],[]).
subConjunto([_|L],SL) :- subConjunto(L,SL). 
subConjunto([X|L],[X|SL]) :- subConjunto(L,SL).

%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

%Ejercicio 11:
%Arboleda Binaria

%!vacío(+Arbol) 
vacio(nil).

%!raiz(+Arbol,-R)
raiz(bin(_,V,_),V).

%!altura(+Arbol,-H)
altura(nil,0).
altura(bin(D,_,I),H) :- altura(D,Hm1), altura(I,Hm2), H is 1 + max(Hm1,Hm2).

%!cantNodos(+Arbol,-N)
cantNodos(nil,0).
cantNodos(bin(I,_,D),N) :- cantNodos(I,N1), cantNodos(D,N2), N is N1 + N2 + 1.


%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

%Ejercicio 12:

%!inorder(+AB,-Lista)
%que tenga éxito si AB es un árbol binario y Lista la lista de sus nodos según el
%recorrido inorder.

inorder(nil,[]).
inorder(bin(I,R,D),RES) :- inorder(I,IX), inorder(D,DX), append(IX,[R|DX],RES).
%Importante: 1. Por favor acordate que orden implica inorder (nodo mas a la izquierda
%primero), 2. En un binario, va a poner toda la rama izquierda, despues la raiz en el
%medio y despues la derecha, por eso queda asi esta cosa.

preorder(nil,[]).
preorder(bin(I,R,D),RES) :- preorder(I,IX), preorder(D,DX), append([R|IX],DX,RES).
%preorder es arriba abajo izquierda derecha(?

postorder(nil,[]).
postorder(bin(I,R,D),RES) :- postorder(I,IX), postorder(D,DX), append(DX,[R],DXR), append(IX,DXR,RES).
%postorder es abajo izquierda a derecha arriba...????

%Repaso:
%Inorder: Aplastas el arbol desde arriba y queda en una lista, de izquierda a derecha.
%Preorder: Tomas la raiz y vas bajando al mas de la izquierda, y asi seguis.
%Postorder: De abajo a arriba, izquierda a derecha, termina una rama y sigue a la otra
%subiendo, kinda weird.

%!arbolConInorder(+Lista,-AB)
%versión inversa del predicado anterior.


arbolConInorder([],nil).
arbolConInorder(L,bin(I,R,D)):- append(LI,[R|LD],L), arbolConInorder(I,LI), arbolConInorder(D,LD).
%Se puede leer como que, existen 2 listas, LI y LD, que aparte tienen un elemento entre ellas, R,
%talq, si las combinas dan la lista que quiero hacer un arbolito, tonces, decimos que nuesrto arbol
%cumple el pred si R de la mitad es la Raiz del arbol, y lo mismo recursivamente para los otros.

%!aBB(+T) 
%que será verdadero si T es un árbol binario de búsqueda.
%Si no me falla la memoria, es que cada nodo puede tener 2 hijos, que aca lo cumple el tipo de arbol
%que tamos usando, y que Raiz(I) <= Raiz < Raiz(D)

/* abb(nil).
abb(bin(I,R,D)) :- abb(I), abb(D), inorder(I,L1), inorder(D,L2), max_list(L1,R), min_list(L2,R). */

/* aBB(nil).
aBB(bin(Izq, R, Der)) :- aBB(Izq), aBB(Der), inorder(Izq, L1), inorder(Der, L2), maximo(L1, R), minimo(L2, R). */

%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

%Ejercicio 14:
%!cuadradoSemiMagico(+N, -XS)
%El predicado debe ir devolviendo matrices (utilizando la representación antes mencionada), que sean cuadrados semi-mágicos de dimensión N*N. Dichas
%matrices deben devolverse de manera ordenada: primero aquellas cuyas filas suman 0, luego 1, luego 2,
%etc. No es necesario utilizar la técnica Generate & Test.

%Un semimagico es una matriz cuadrada de naturales donde todas las filas suman lo mismo, el magico hace que tambien las columnas sumen igual.

cuadradoSemiMagico(N,M) :- desde(0,I), generarMatrizCuadradaSumaI(N,I,M).

generarMatrizCuadradaSumaI(N,I,M) :- length(M,N), todaFilaSumaI(N,I,M).

todaFilaSumaI(_,_,[]).
todaFilaSumaI(N,I,[X|XS]) :- todaFilaSumaI(N,I,XS),listaQueSuma(I,N,X).

%Tengo que ponerle valores a las filas de la matriz, aparte de generarla, tengo que encajarle numeritos.
%Tonces, si tengo una lista de longitud N, y quiero que sume I, tengo que hacer que la lista genere los
%distintos numeros subiendo no linealmente, sino ocupando el plano, porque asi es la vida.
%tonces tengo que generar una lista kinda: [0,0,...,0], [0,0,....,1], [0,1,....,0], tipo no subiendo uno 
%solo, sino todos... 
%Solucion, nos copiamos el codigo de Octokerbs, que maestro la verdad, este codigo te va dando las listas
%que suman S de longitud N, 
%Consigue un X entre 0 y S objetivo, S2 es la diferencia entre S-X, el objetivo menos el x conseguido, y
%despues a N le resta uno para bajar la long del resto de la lista, y asi hace que lo que qeuda lo tengan
%que sumar el resto, de esta forma toda la lista tiene que sumar un objetivo, es la generalizacion de 
%generar pares esto, generar pares hacia que lo mismo para 2 elementos no mas.
%Instancia de a uno los numeros que tienen que dar el objetivo, y despues vas fijandote el siguiente.

% listaQueSuma(+S, +N, ?XS)
listaQueSuma(0, 0, []).
listaQueSuma(S, N, [X | XS]) :- N > 0, between(0, S, X), S2 is S-X, N2 is N-1, listaQueSuma(S2, N2, XS).

%B, hay que hacer que sea full magico el cuadrado, 2 opciones, o vemos como se transpone la matriz, o 
%vemos como hacer la suma de los elementos en las columnas.
%Conseguir columna de M, sabemos que es cuadrada, la columna tiene N elementos, puedo usar iesimo para 
%conseguir esos elementos, al final esto puede ser transponer al mismto tiempo, metemos eso en listas.

%!columnaJ(+MATRIZ,+LONGITUD,+LONGITUD,+COLUMNA,-RES), M matriz, N longitud de la matriz, asi no la calcula todo el tiempo,
%N2 tambien long matriz, pero esta es la que itera, J es la columna que queres sacar, XS la columna. 

columnaJ(_,_,0,_,[]).
columnaJ(M,N,N2,J,[X|XS]) :- D is N - N2, N3 is N2 - 1, iesimo(D,M,LISTA), iesimo(J,LISTA,X), columnaJ(M,N,N3,J,XS).

%Ahora que podemos sacar las columnas, para el cuadrado magico necesitamos que las filas sumen algo, que eso ya lo hace
%el semimagico, y despues hacemos que pase que todas las filas suman esa cantidad.

cuadradoMagico(N,M) :- cuadradoSemiMagico(N,M), last(M,FILA), sumlist(FILA,I), transponer(M,N,N,T), todaFilaSumaI(N,I,T).
%No use que la transpuesta sea un cuadrado semi magico tambien porque cuadradoSemiMagico no es reversible en M.

transponer(_,_,0,[]).
transponer(M,N,N2,[T|XS]) :- D is N - N2, N3 is N2 - 1, columnaJ(M,N,N,D,T), transponer(M,N,N3,XS).   

%Ejercicio de la guia de resolucion, era ver no mas que asi se pasaba todo.
r(alan).
j(alan).
pr(a).

i(X) :- r(X),pl(Y),res(X,Y).

res(X,Y) :- r(X), j(X), pr(Y).

pl(X) :- pr(X).


memberP(X,[X|_]).
memberP(X,[_|XS]) :- memberP(X,XS).

esSublista(_,[]).
esSublista(L,[X|XS]) :- memberP(X,L), esSublista(L,XS).

%Okey, esta bazofia se cuelga con +L -XS porque esta definicion esa diciendo que la lista 
%que cumple es una donde el primer elemento pertenece a la lista L, y asi sigue, siempre 
%va a agregarle el elemento que pertenezca a L en la siguiente posicion y no para mas.

naturala(cero). 
naturala(suc(X)) :- naturala(X). 

mayorOIguala(suc(X),Y) :- mayorOIguala(X, Y).
mayorOIguala(X,X) :- naturala(X).


%La consulta de si 2 es mayor o igual a 1 (ambos escritos con suc(..)) falla, porque cuando le 
%pasas para que consulte suc(suc(N)) y suc(cero), las primeras 2 veces, resuelve con la primer
%regla llegando a mayorOIgual(N,suc(cero)), y despues se fija si unifica N con suc(X), y despues
%si X unifica con suc(X) y asi se va metiendo con el dfs, va viendo que puede seguir unificando
%sacando suc y no sale de la primer condicion, nunca llega a la segunda.

analfabeto(X) :- vivo(X), noSabeLeer(X). 

%noSabeLeer(X) :- mesa(X).
noSabeLeer(X) :- delfin(X).

vivo(X) :- delfin(X). 

inteligente(flipper). 

delfin(flipper).

%inteligente(alan).



preorder2(nil,[]).
preorder2(bin(I,R,D),[R|L]) :- append2(LI,LD,L), preorder2(I,LI), preorder2(D,LD).

append2([],YS,YS).
append2([X|XS],YS,[X|L]) :- append(XS,YS,L).



natural3(0).
natural3(succ(N)) :- natural(N).

mayor3(succ(X),0) :- natural3(X).
mayor3(succ(X),succ(Y)) :- mayor3(X,Y).

parPositivo(X,Y) :- mayor3(X, 0), mayor3(Y, 0).


reduce(ap(ap(const,X),_), X).
reduce(ap(id,X), X).
reduce(ap(ap(ap(flip,F),X),Y), ap(ap(F,Y),X)).
reduce(ap(M,_), ap(M,M1)) :- reduce(M, M1).


listaDeNat([]).
listaDeNat([X|XS]) :- natural(X), listaDeNat(XS).





caminoDesde(P, C) :- desde(1,K), kCaminoDesde(K, P, C).

kCaminoDesde(1, P, [P]).
kCaminoDesde(K, P, C) :- K > 1, Km1 is K - 1, kCaminoDesde(Km1, P, Cp), append(_, [Last], Cp), mover(Last, Pp), append(Cp, [Pp], C).


mover((X, Y), (X, Yp)) :- Yp is Y + 1.
mover((X, Y), (X, Yp)) :- Yp is Y - 1.
mover((X, Y), (Xp, Y)) :- Xp is X + 1.
mover((X, Y), (Xp, Y)) :- Xp is X - 1.


caminoDesde2(POS,RES) :- desde(1,LONG), caminoDesdeLongN(LONG,POS,RES).

caminoDesdeLongN(1,(X,Y),[(X,Y)]).
caminoDesdeLongN(N,POS,RES) :- 
	N > 1, Nm1 is N - 1, 
	caminoDesdeLongN(Nm1,POS,NewRes), %Voy sacandole elementos a la lista hasta llegar al caso base
	append(_,[NewPos],NewRes), %Despues, desde el caso base en adelante, voy sacando el ultimo de
	posValida(NewPos,OtroPos), %los resultados anteriores, y desde esos calculo el movimiento legal.
	append(NewRes,[OtroPos],RES). %La nueva pos valida la meto al final y digo que el RES es lo que 
							%tenia antes junto con el nuevo al final.
							   


posValida((X,Y),(X1,Y)) :- X > 0, X1 is X - 1. 
posValida((X,Y),(X,Y1)) :- Y > 0, Y1 is Y - 1. 
posValida((X,Y),(X1,Y)) :- X >= 0, X1 is X + 1. 
posValida((X,Y),(X,Y1)) :- Y >= 0, Y1 is Y + 1. 


%Idea, generar sublistas y despues quedarse con las que esten por debajo de C
%Problema, manipular los objetos de alguna manera, pasarlos de predicados separados
%a una lista. Si x cumple ser objeto, pertenece a la lista.


%IDEA, EL CONTADOR DE VISITADOS SOLO TIENE MIEMBROS DE LA LISTA A LA QUE AGREGAS

%listaObjetos([],-XS)

/* listaObjetos(_,[]).
listaObjetos(VISITADOS,[X|XS]) :- 
	objeto(X,Y,Z), 
	not(member(X,VISITADOS)), 
	append(VISITADOS,[X],V2),
	listaObjetos(V2,XS). */

%ayudaaaaaaaaaaaaaaaaaaaaa




rama2(nil,[]).
rama2(bin(I,R,_),[R|XS]) :- rama2(I,XS).
rama2(bin(_,R,D),[R|XS]) :- rama2(D,XS). 


aplanar([],[]).
aplanar([X|XS],[X|L]) :- aplanar(XS,L).
aplanar([[X]|XS],L) :- aplanar([X|XS],L).


interseccion2([],_,[]).
interseccion2([X|XS],YS,RS) :- not(member(X,YS)), interseccion2(XS,YS,RS).
interseccion2([X|XS],YS,[X|RS]) :- member(X,YS), interseccion2(XS,YS,RS).


partir3(N,L,L1,L2) :- append(L1,L2,L), length(L1,N).


borrar3([],_,[]).
borrar3([X|XS],X,YS) :- borrar3(XS,X,YS).
borrar3([Y|XS],X,[Y|YS]) :- Y \= X, borrar3(XS,X,YS).


sacarDuplicados3([],[]).
sacarDuplicados3([X|XS],YS) :- sacarDuplicados3(XS,YS), member(X,YS).
sacarDuplicados3([X|XS],[X|YS]) :- sacarDuplicados3(XS,YS), not(member(X,YS)).

permutacion3([],[]).
permutacion3([L|LS],P) :- permutacion3(LS,P1), insertar3(P1,L,P).



insertar3(L,X,RES) :- append(L1,L2,L), append(L1,[X|L2],RES).


reparto3([],0,[]).
reparto3(L,N,[X|LL]) :- N > 0, Nm1 is N-1, reparto3(L1,Nm1,LL), append(X,L1,L).
%Habria que cambiar de lugar reparto con append para que despues de dar todas las sol
%no se cuelgue

repartoSinVacias(L,LL) :- length(L,N), between(1,N,K), reparto(L,K,LL), not(member([],LL)).

sublista3(_,[]).
sublista3(L,P) :- append(L1,_,L), append(_,P,L1), P \= [].

subcojunto3([],[]).
subcojunto3([_|XS],YS) :- subcojunto3(XS,YS).
subcojunto3([X|XS],[X|YS]) :- subcojunto3(XS,YS).


parteQueSuma3(L,VALOR,RES) :- subcojunto3(L,RES), sumlist(RES,VALOR).  

intercalar3(L,[],L).
intercalar3([],L,L).
intercalar3([X|XS],[Y|YS],[X|[Y|RS]]) :- intercalar3(XS,YS,RS).


vacio3(nil).

raiz3(bin(_,R,_),R).

altura3(nil,0).
altura3(bin(I,_,D),K):- altura3(I,KI), altura3(D,KD), K is (max(KI,KD) + 1).

%     bin(bin(bin(nil,1,nil),2,nil),3,bin(bin(bin(nil,1,nil),2,nil),3,nil))

%     bin(bin(bin(nil,1,nil),2,nil),3,bin(nil,4,bin(nil,5,nil)))

cantNodos3(nil,0).
cantNodos3(bin(I,_,D),K) :- cantNodos3(I,KI), cantNodos3(D,KD), K is (1 + KI + KD).


inorder3(nil,[]).
inorder3(bin(I,R,D),RES) :- inorder3(I,II),inorder3(D,ID), append(II,[R|ID],RES). 


arbolConInorder3([],nil).
arbolConInorder3(L, bin(I,R,D)) :- append(LI,[R|LD],L), arbolConInorder3(LI,I), arbolConInorder3(LD,D).


abb3(nil).
abb3(bin(nil,_,nil)).
abb3(bin(I,R,D)) :-
	inorder3(I,II), max_list(II,MI), R >= MI,
	inorder3(D,ID), min_list(ID,MD), R < MD,
	abb3(I), abb3(D).



aBBInsertar3(X,nil,bin(nil,X,nil)).
aBBInsertar3(X,bin(I,R,D),bin(IN,R,D))  :- X < R, aBBInsertar3(X,I,IN).
aBBInsertar3(X,bin(I,R,D),bin(I,R,DN))  :- X >= R, aBBInsertar3(X,D,DN).

iesimo3(N,L,I) :- append(L1,[I|_],L), length(L1,N).

generarParesQueSuman3(S,(X,Y)) :- between(1,S,X), Y is S - X. 

coprimos3(X,Y) :- desde(1,N), generarParesQueSuman3(N,(X,Y)), gcd(X,Y) =:= 1.

esTriangulo3(tri(A,B,C)) :- A < B + C, B < A + C, C < B + A.
						

corteMasParejo3(L,L1,L2) :- append(L1,L2,L), not(hayMasParejo3(L,L1,L2)).

hayMasParejo3(L,L1,L2) :- sum_list(L1,L1SUMA), sum_list(L2,L2SUMA), append(XS,YS,L),
	sum_list(XS,XSSUMA), sum_list(YS,YSSUMA), L1 \= XS, L2 \= YS, 
	abs(L1SUMA-L2SUMA) > abs(XSSUMA-YSSUMA).




esPrimo(N) :- N > 1, not(tieneUnDivisorNoTrivial(N)).

tieneUnDivisorNoTrivial(N) :- N1 is N-1, between(2, N1, D), 0 =:= N mod D.


/* proximoPoderoso(X,M) :- Xp1 is X + 1, esPoderoso(Xp1), M is Xp1.
proximoPoderoso(X,N) :- Xp1 is X + 1, not(esPoderoso(Xp1)), proximoPoderoso(Xp1,N).

esPoderoso(Z) :- divisoresPrimos(Z,XS), mapElevar(XS,XXS) ,todosDividen(XXS,Z).

divisoresPrimos(1,[]).
divisoresPrimos(Z,[X|XS]) :- Z \= 1,listaNatHastaN(Z,) , esPrimo(X), Z mod X =:= 0, divisoresPrimos(Z,XS).

divPrimosN(N,XS) :- listaHastaN(N,XXS), filtrarNoPrimos(XXS,XS).

listaHastaN(0,[]).
listaHastaN(X,[X|XS]) :- X \= 0, Xm1 is X - 1, listaHastaN(Xm1,XS).


filtrarNoPrimos([],[]).
filtrarNoPrimos([NP|XS],PS) :- not(esPrimo(P)), filtrarNoPrimos(XS,PS).
filtrarNoPrimos([P|XS],[P|PS]) :- esPrimo(P), filtrarNoPrimos(XS,PS). */

%bin(bin(nil,0,nil),1,bin(nil,2,bin(nil,3,nil)))

ramas3(nil,[]).
ramas3(bin(nil,R,nil),[R]).
ramas3(bin(I,R,_),[R|XS]) :- I \= nil ,ramas3(I,XS).
ramas3(bin(_,R,D),[R|XS]) :- D \= nil ,ramas3(D,XS).


ramaMasLarga3(A,R) :- ramas3(A,R), not((ramas3(A,T), T \= R, length(T,TL),length(R,RL), TL > RL)).


ramaUnicaDeLongN(A,N,R) :- ramas3(A,R), length(R,N), not((ramas3(A,T),length(T,N), T \= R)).



generarCapicuas3(L):- desde(1,L1), length(L, L1), esCapicua3(L).

esCapicua3(L):- reverse(L,L), sonNumeros3(L).

sonNumeros3([]).
sonNumeros3([X|XS]):- between(1, 9, X), sonNumeros3(XS).


objeto(1,50,10). % objeto(ID,PESO,VALOR)
objeto(2,75,15). 
objeto(3,60,5).
objeto(4,10,1).


mochila(_,[]).
mochila(N,[Id|L]):-objeto(Id,P,_), P=<N, NMP is N - P, mochila(NMP,L), sort([Id|L],[Id|L]).

mejorMochila(C,L):- mochila(C,L),sumaDeVals(L,SL) ,not((mochila(C,L1),sumaDeVals(L1,SL1), SL1 > SL)).

sumaDeVals([],0).
sumaDeVals([H|T],N):- sumaDeVals(T,Nrec), objeto(H,_,V), N is V + Nrec.


/* 
pruebaSinDups(L,[X|L2]) :- L2 is L, sort(L2,L2).
*/






%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

%Ejercicios clase Rafa:

%!iesimo(+I, +L, -X)

iesimo2(0,[X|_],X).
iesimo2(I,[_|XS],Y) :- I2 is I - 1, iesimo(I2,XS,Y).
%Esta no es reversible porque sino tengo I, no puedo evaluar el I - 1, las aritmeticas necesitan
%tar instanciadas.

iesimo(I,L,X) :- append(L1,[X|_],L), length(L1,I).
%Esta es reversible en I porque no necesita la instanciacion previa, aca el append genera las listas
%que cumplen y despues se fija que tengan la longitud I, que en este caso lo busca el legnth, le cae
%una lista instancia y quiere saber su longitud.

desde1(X, X).
desde1(X, Y) :- N is X+1, desde1(N, Y).
%Se cuelga con +X +Y porq siempre le suma 1

desde(X,X).
desde(X,Y) :-var(Y), N is X+1, desde(N, Y).
desde(X,Y) :- nonvar(Y), X =< Y.
%Okey, lo que hace este predicado choto es, si le pasas +X +Y te dice si X <= Y, 
%y si pasas +X -Y, lo que va a hacer es dar los Y talq empieza desde X y sigue
%sumando 1, X, X+1,X+2... etc, esto lo hace porque, la linea de var(Y) primero
%que nada indica que tiene que ser una variable no instanciada, asi nos metemos
%en el caso de -Y, despues, si le pasas desde(10,Y). lo que va a hacer es primero
%ver la primer linea, que dice desde(X,X)., tonces va a unificar 10 con Y, asi 
%ese resultado funca, y despues en la siguiente linea pregunta desde(11,X)., 
%Lo cual va a repetir lo anterior, de esta forma, va sumando a partir de la 
%X que le pasaste al inicio, la primera linea hace que unifique lo que le pases
%y la segunda que suba en 1. Un ejemplo medio falopa, la primer linea va a hacer 
%que lo primero que le pases como X sea devuelto, ej:
%2 ?- desde("hola",Y). --> Y = "hola" ;, unifico Y con Hola, si le das a seguir
%explota porq quiere sumarle 1 a hola.

%!pmq(+X,-Y)
%Numeros naturales menores o iguales a X.

pmq(X,Y) :- between(0,X,Y), 0 =:= Y mod 2.
%Importante de esto, si solo decis x >= y, no funca, tas intentando evaluar algo 
%no instanciado, el op aritmetico explota, por eso usas el between para producir
%valores.

%!coprimos(-X,-Y).
%Queres generar infinitos pares coprimos, 2 numeros son coprimos sii gcd(X,Y) = 1,
%y lo importante del ejercicio es que queremos una generacion "diagonal", que vayan
%subiendo los valores de ambos, recorrer el plano en diagonal, no que siempre se 
%se quede buscando los coprimos del primer numero.
%Vamos a generar los pares X Y de cierta forma, que era con rangos
%Diria que asigno algo al primero y despues genero los que estan entre
%0 y ese, y despues le sumo 1 a ese?.
% X := 1, between(0,X,Y) 

coprimos(X,Y) :- generarPares(X,Y), 1 =:= gcd(X,Y).

%!generarPares(-X,-Y).
generarPares(X,Y) :- desde(0,N), between(0,N,X), between(0,N,Y),  N =:= X + Y.
%Equivalente a: 0 <= N, 0 <= X <= N, 0 <= Y <= X, N = X + Y;
%Equivalente Haskell:
%puntosDelPlano :: [Punto]
%puntosDelPlano = [(x, y) | k <- [0..], x <- [0..k], y<- [0..k], x + y == k]
%Otro equivalente Haskell 
%puntosDelPlano :: [Punto]
%puntosDelPlano = [(x,k-x) | k<-[0..], x<-[0..k]]


%!corteMasParejo(+L,-L1,-L2)
%donde L es una lista de numeros, y L1 y L2 representan el corte mas parejo posible de L
%respecto a la suma de sus elementos (predicado sumlist/2). Puede haber mas de un resultado.

%La idea iba por decir que uno es el corte mas parejo si no hay otro mas parejo?.....


corteMasParejo(L,L1,L2) :- append(L1,L2,L), not(hayCorteMasParejo(L1,L2,L)).

hayCorteMasParejo(L1,L2,L) :- append(L3,L4,L), 
sumlist(L1,S1), sumlist(L2,S2),
sumlist(L3,S3), sumlist(L4,S4),
abs(S1-S2) > abs(S3-S4).

%IMPORTANTISIMO de esto, ir por la negacion, esto es declaratividad en su maximo 
%esplendor, se lee perfectamente en LPO, tener el mejor candidato de algo es 
%decir que no existe nadie mejor.

maximo(L,X) :- member(X,L), not(hayOtroMaximo(L,X)).

hayOtroMaximo(L,X) :- member(Y,L), Y > X.

minimo(L,X) :- member(X,L), not(hayOtroMinimo(L,X)).

hayOtroMinimo(L,X) :- member(Y,L), Y < X.

%Esto seria la version mas declarativa de maximo de una lista, le decis que 
%no existe otro mas grande, en vez de la forma recursiva de Haskell.

%!proximoPrimo(+N,-P).
%Dado un natural N, te da el proximo primo que venga.


/* 
esPrimo1(X) :- Y is X-1 , between(2,Y,Z), 0 =\= X mod Z.

noPrimo(X) :- Y is X-1 , between(2,Y,Z), 0 =:= X mod Z.
esPrimo(X) :- not(noPrimo(X)). 
*/

%IMPORTANTE, si haces el pred esPrimo con la definicion del noPrimo y 0 =\= x mod z,
%vas a tener el problema de que te va a ir diciendo si ninguno de los numeros lo 
%divide, pero imprimiendo, onda, si quiero ver que X es primo, el pred te va

proximoPrimo(N,N2) :- N2 is N + 1, esPrimo(N2).
proximoPrimo(N,P) :- N2 is N + 1, not(esPrimo(N2)), proximoPrimo(N2,P).


%Machete Ernie, como hacer maplist sin usar el metapredicado:

%! p1(?X)
% un predicado que haga lo que vos quieras

/* p2(L) :- maplist(p1, L).
es lo mismo que:
p2([X | Xs]) :- p1(X), p2(Xs).
p2([]). */

% con dos argumentos:

%! p3(?X, ?Y)
% un predicado que haga lo que vos quieras

/* p4(L1, L2) :- maplist(p3, L1, L2).
es lo mismo que:
p4([X | Xs], [Y | Ys]) :- p3(X, Y), p4(Xs, Ys).
p4([], []). */

/* sumar15(X,Y) :- Y is X + 15.

sumar15Todos([L|LS]) :- sumar15(X,L), sumar15Todos(LS). 
 */


%PLP - Segundo Parcial - 2do cuatrimestre de 2024

%!subsecuenciaCreciente(+L,-S) 

subsecuenciaCreciente(L,S) :- subConjunto(L,S), creciente(S).
%Las sublista S es una tq _ ++ S1 = L, S1 es un sufijo de L, [1,2,3,4], puede ser [1,2,3,4],[2,3,4],[3,4],[4] y []
%tonces, una sublista de L es una lista que es prefijo de del sufijo, todos los prefijos son sublista, todos los prefijos
%de los siguientes tambien lo son y asi.

creciente([]).
creciente([_]).
creciente([X|[Y|XS]]) :- X < Y, creciente([Y|XS]).  

%subsecuenciaCrecienteMasLarga(+L,-S)

subsecuenciaCrecienteMasLarga(L,S) :- subsecuenciaCreciente(L,S), not(haySubCrecienteMejor(L,S)).

haySubCrecienteMejor(L,S) :- subsecuenciaCreciente(L,S2), length(S,LONG1), length(S2,LONG2), LONG1 < LONG2.

%! fibonacci(-X)

fibonacci(X) :- desde(0,M), fibonacciM(M,X).

fibonacciM(0,1).
fibonacciM(1,1).
fibonacciM(M,N) :- M \= 0, M \=1,M1 is M - 1, M2 is M - 2, fibonacciM(M1,N1), fibonacciM(M2,N2), N is N1 + N2.
%IMPORTANTE PARA FIBO: Si no pones el limite de M /= 0 y de 1, tonces con fibo de 0, se va a meter tambien en
%fibo M N, y le va a restar 1 a 0, y va a buscar fibo de -1 y de -2 y bla bla bla.a
desde2(X,X).
desde2(X,Y) :- N is X + 1, desde2(N,Y).

ramas(nil,[]).
ramas(bin(nil,R,nil),[R]).
ramas(bin(_,R,D),[R|XS]) :- D \= nil, ramas(D,XS). 
ramas(bin(I,R,_),[R|XS]) :- I \= nil, ramas(I,XS).
%Diria que si no aclaras la parte de que sean distintos de nil, tonces lo cuenta mas veces...(???)

%Es reversible en C, porque se va a fijar uno a uno si los elementos de la lista coinciden con las raices,
%no tiene problemas de instanciacion tampoco.

%!ramaMasLarga(+A,-C)


ramaMasLarga(A,C) :- ramas(A,C), not(hayRamaMasLarga(A,C)).

hayRamaMasLarga(A,R) :- ramas(A,R2), length(R,LONG1), length(R2,LONG2), LONG1 < LONG2.

%!ramaUnicaDeLong(+A,+N,-C) 

ramaUnicaDeLong(A,N,R) :- ramas(A,R),length(R,N), not(hayOtraRamaLong(A,N,R)).

hayOtraRamaLong(A,N,R) :- ramas(A,R2), length(R2,N), R2 \= R.

%Otro parcial:

%!generarCapicuas(-L).

%generarCapicuas([]).
%Tienen que ser listas de naturales, sin el 0.
generarCapicuas(L) :- length(L,LONG), 0 =:= LONG mod 2, append(L1,L2,L), reverse1(L1,L2).
generarCapicuas(L) :- length(L,LONG), 0 =\= LONG mod 2, append(L1,[_|L2],L), reverse1(L1,L2).

reverse1([],[]).
reverse1([X|L],RL) :- reverse1(L,L2), append(L2,[X],RL).


capicua([_]).
capicua(L) :- append([X],XS,L), append(YS,[X],XS),capicua(YS).
%Esta version de capicua es pretty linda, se fija si el primer y ultimo elemento son iguales y 
%componen la lista.

listaDeN(0,[]).
listaDeN(N,L) :- N > 0, between(1,N,N2), N3 is N - N2, listaDeN(N3,L1), append([N2],L1,L).
%Te va creando listas de naturales donde si va avanzando el tema, no es el grafico lineal,
%sino que recorre en diagonal.


%Otro parcial:
%Tenemos el pred estudiante(?E) que nos da los estudiantes, notas(-XS) que nos da la lista de triplas 
%correspondientes a las notas de un estudiante en una materia, (Estudiante, Materia, Nota), 

notas((eze,plp,10)).
notas((juan,tda,1)).

%!tieneMateriaAprobada(+E,+M)
%tieneMateriaAprobada(E,M) :- notas(XS), member((E,M,N)), N => 4.

%!eliminarAplazos(+NS,-L)
%Un estudiante pertenece a L si tiene nota >= 4, o alguna materia aprobada,
%eliminarAplazos(NS,L) :- member((E,M,N),L), tieneMateriaAprobada(E,M).
%Esto diria que ta mal, me confunde un toque la logica del ejercicio.

%!promedio(+A,-P)
%Verdadero cuando A es un estudiante y P el promedio de sus notas luego de eliminar los aplazos.


%promedio(A,P) :- notas(XS), estudiante(E), E \= A,not(member((E,M,N),XS)), eliminarAplazos(XS,ZS),
%				 sumaNotas(ZS,S),length(ZS,LEN), P is S / len.  

%sumaNotas([],0).
%sumaNotas([(_,_,N)|XS],P) :- sumaNotas(XS,P1),P is P1 + N. 


%! mejorEstudiante(-A)
%mayor(XS,X) :- member(X,XS), not(member(Y,XS),Y > X).

%Segundo parcial 1c 2025, el que tenes que recuperar

%!esRotacion(+R,+L). 
%Las rotaciones de [1,2,3,4] son [1,2,3,4], [2,3,4,1], [3,4,1,2], [4,1,2,3]
%basicamente ir desplanzando 1 a la derecha o izquierda es una rotacion 

esRotacion(L,L). %Si no pones esta linea y abajo aclaras L /= R, imprime 2 veces la lista.
esRotacion(L,R) :- append(L1,L2,L), append(L2,L1,R), L \= R.

%!Collatz(+N,-S)
%La secuencia de collatz, que si n es par, el siguiente es n/2, y n impar, 3n + 1, asi hasta que 
%llega a 1.

collatz(N,N) :- N > 0.
collatz(N,S) :- N \= 1, 0 =:= N mod 2, N2 is N/2, collatz(N2,S).
collatz(N,S) :- N \= 1, 0 =\= N mod 2, N2 is ((3*N) + 1), collatz(N2,S).

%Sep, es reversible en S, ya que +N, todas las operaciones aritmeticas funcionan, entonces, cuando llama
%a S con un numero, va a empezar a hacer la serie de collatz de esa, terminar en algun momento y decirte
%si ese numero a partir de ese momento de la serie pertenecia.

%!collatzMayor(+N,-M).
%El mayor elemento de la serie de collatz de algun numero

collatzMayor(N,M) :- collatz(N,M), not(hayMayorCollatz(N,M)).

hayMayorCollatz(N,M) :- collatz(N,X), X > M.


%Final 1 que tenia a mano:

/* 
Un arbol general es o bien el simbolo x o bien una lista de arboles generales.
Definir arbolGenerar(-A), que genere todos los posibles arboles generales.
Ej: x [] [x,x] [[x,x],x,[x,x]] [x,[x,x,[x,[]],x],[x,[]]] 
*/

arbolGeneral(X) :- desde(0,N), generarArbolito(N,X).

generarArbolito(0,x).
generarArbolito(0,[]).
generarArbolito(N,[A|L]) :- N > 0, Nm1 is N - 1, between(0,Nm1,Na), Nl is Nm1 - Na,
	generarArbolito(Na,A), generarArbolito(Nl,L).

/* 
Objetivo, generar los distintos arboles generales que son con 'x', '[]' y lo mismo dentro de mas
listas, podemos pensarlo como distintos niveles de profundidad. Entonces, desde ahi, podemos definir
la profundidad del arbol con el N generado con desde(0,N), y hacemos arboles de distintas profs, ahora
lo siguiente seria pensar como se ven las profundidades de estos arbolitos(?
prof 0 -> x []
prof 1 -> [x] [[]]
porf 2 -> [x,x] [x,[]] , [[],x] [[],[]]
Otra idea: limitar por cantidad de hojas(?)
\
*/

%Punto odioso del recu del segundo:

/* 
Hay que representar un subconjunto de formulas de logica prop, la negacion y la implicacion.
Vamos a utilizar los atomos neg(P) y imp(P,Q), siendo P y Q formulas. Hay que definir el predicado
formula(+VS,-F) que instancia en F todas las fomrulas que usan las variables proposicionales que aparecen
en VS, ej:
formula([p,q],F).
p , q, neg(p), neg(q), neg(neg(p)), neg(neg(q)), imp(p,q), imp(neg(p),q), etc...
*/

formula(VS,F) :- desde(0,N), formulaN(N,VS,F).

formulaN(0,VS,F) :- member(F,VS).
formulaN(N,VS,neg(F)) :- N > 0, Nm1 is N - 1, formulaN(Nm1,VS,F).
formulaN(N,VS,imp(F1,F2)) :- N > 0, Nm1 is N - 1, between(0,Nm1,N1),
	N2 is Nm1 - N1, formulaN(N1,VS,F1), formulaN(N2,VS,F2).

/* 
El secreto era hacer los ultimos puntos de la guia.....ERA RE SIMPLE PTM.
*/

%Final 2 que tenia a mano

/*  
Representamos en prolog las fomrulas de logica proposicional con los simbolos de funcion binarios
and, or, imp, el unario neg, y el unario prop, de tal modo que prop(n) representa la n-esima 
variable proposicional. Se puede asumir que en una formula prop(n) el valor de n siempre es 
positivo, Ej: P1 or neg(P2 and neg(P2)) se representa como: or(prop(1),neg(and(prop(2),neg(prop2))))).
Definir el pred esTautologia(+Form) que dada una formula Form, sea verdadero si todas las valuaciones
satisfacen.
*/


/* esTautologia(prop(N)).
esTautologia(and(X,Y)) :- esTautologia(X), esTautologia(Y).
esTautologia(or(X,Y)) :- not((esTautologia(neg(X))), esTautologia(neg(Y))).
esTautologia(imp(X,Y)) :- not(es

not((esTautologia(neg(X)), not(esTautologia(neg(Y)))))

esTautologia(neg(esTautologia(neg(X))))
 */


/*  
Okey, diria que una formula prop es satisfactible segun las definiciones  
*/






%Punto final cuba wiki
/* 
var(X) donde X es un numero natural. Representa al uso de la variable numero X.
lam(X, M) donde X es un numero natural y M es un λ-termino. Representa la ligadura de la variable numero X con respecto al λ-termino M.
app(M, N) donde M y N son λ-terminos. Representa a la aplicacion.

lam(1, lam(2, lam(3, app(var(1), app(var(2), var(3))))) es equivalente al termino λf. λg. λx. f (g x)
*/ 

/*
1.
Definir variablesLibres(+M, -L) que instancia en una lista L las variables libres del termino M.
Ej: variablesLibres(lam(1, app(var(1), var(2))), L) instancia L = [2].
*/

variablesLibres(M,L) :- 
	variables(M,Variables), 
	ligadas(M,Ligadas), 
	subtract(Variables,Ligadas,Libres),
	list_to_set(Libres,L).

variables(var(N),[N]).
variables(lam(X,M),L) :- variables(M,VM), append([X],VM,L).
variables(app(M,N),L) :- variables(M,VM), variables(N,VN), append(VM,VN,L).

ligadas(var(_),[]).
ligadas(lam(X,M),L) :- ligadas(M,L1), append([X],L1,L).
ligadas(app(M,N),L) :- ligadas(M,LM), ligadas(N,LN), append(LM,LN,L).

%-----------------------------------------------------

libres(var(N),[N]).
libres(lam(X,M),L) :- libres(M,L1), subtract(L1,[X],L).
libres(app(M,N),L) :- libres(M,L1), libres(N,L2), union(L1,L2,L).



/* libres(var(N),NL,[N]) :- not(member(N,NL)).
libres(lam(X,M),NL,L) :- append([X],LM,) libres(M,[X],L).
libres(app(M,N),NL,L) :- libres(M,NLM,LM), libres(N,NLN,LN), append(NLM,NLN,NL), append(LM,LN,L). 
 */
%Idea, hago un pred qeu me da todas las variables, despues otro que me da las libres......
%probablemente se pueda ahorrar mucho codigo con alguna otra alternativa, actualmente, no me sale.


/*
(B) Definir tamano(+M, -T) que calcula el tamano de un termino. 
var(X) suma 1. lam(X, M) suma 1 + tamano(M). app(M, N) suma 1 + tamano(M) + tamano(N).  
*/

tamano(var(_),1).
tamano(lam(_,M),T) :- tamano(M,TM), T is 1 + TM.
tamano(app(M,N),T) :- tamano(M,TM), tamano(N,TN), T is 1 + TM + TN.

/* 
(C) Definir generarLambdaTerminos(+xs, -M) que dada una lista de numeros naturales XS 
instancia en M λ-terminos infinitos. Sugerencia: Crear los λ-terminos en base al tamano.
*/

generarLambdaTerminos(XS,M):- desde(0,N), terminoProfN(N,XS,M).

terminoProfN(0,XS,var(N)) :- member(N,XS).
terminoProfN(N,XS,lam(X,M)) :- N > 0, Nm1 is N - 1, member(X,XS), terminoProfN(Nm1,XS,M).
terminoProfN(N,XS,app(M,N)) :- N > 0, Nm1 is N - 1, between(0,Nm1,N1), N2 is Nm1 - N1,
	terminoProfN(N1,XS,M), terminoProfN(N2,XS,M).
