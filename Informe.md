# $1^{er}$ Proyecto de Programación Declarativa

### Hive

Integrantes:

Richard García De la Osa C412.

Andy A. Castañeda Guerra.



#### Estructura:

El proyecto está estructurado en varios scripts. Los scripts con nombre de bichos contienen el comportamiento de cada bicho respectivamente. Luego están __utils.pl__ y __dfs.pl__ que contienen la mayoría de los metodos auxiliares que se utilizan en los demás asi como los algoritmos desarrollados para moverse por la colmena entre otros. El __board.pl__ es el que se encarga de convertir de un formato de celdas a un formato bidimensional y escribir en consola el tablero finalmente en forma de colmena. Tenemos __ia.pl__ que es el que contiene el algoritmo desarrollado para la IA. __main.pl__ se encarga de manejar la interface del juego y es con la cual deberemos interactuar para utilizar la aplicación.

También tenemos otros archivos como examples.txt donde encontrará ejemplos de juegos que utilizamos para probar el funcionamiento de nuestro proyecto.

#### Funcionamiento:

Las celdas están representadas por un predicado denominado __hex__ el cual tiene 7 parametros, Type, Row, Col, Color, Heigth, OnGame y Blocked, estos representan las propiedades que utilizamos para representar una celda en nuestro tablero. De estos Type es de tipo string y todos los demás son de tipo entero. OnGame alterna entre 0(no está en juego) y 1(está en juego). Blocked representa si la ficha está bloqueada(si acaba de ser movida/puesta o trasladada por el efecto especial del pillbug). Tenemos dos listas que representan a los jugadores, cada una de las listas tienen a 11 bichos los cuales pueden estar o no en juego(existe un predicado para conocer los que están en juego). En cada turno se decide que jugada se va a realizar, si poner una ficha, mover o usar algun movimiento especial. Para conocer si el grafo que comprende la colmena se desconecta realizamos un dfs a partir de uno de los adyacentes a la ficha a mover quitando la ficha que se va a mover, contamos la cantidad de fichas en juego y este valor tiene que corresponderse con la cantidad que ya había menos 1, luego comprobamos que la posición a la que se va a a mover contenga un adyacente de la colmena. Luego se comprueba de los caminos posibles hacia esa posición si hay alguno valido(que se cumplan todas las reglas durante el recorrido de el mismo). Además antes de ver si se puede mover verificamos la regla de que la reina tiene que estar en juego antes de mover cualquier ficha. Luego para colocar una ficha se verifica su validez viendo que ningún adyacente sea del color contrario, que exista algún bicho libre del tipo que se quiere colocar y que la posición en done se va a colocar no este ocupada y que este adyacente a la colmena.

El visual del juego es desarrollado en la consola, se escriben pequeños octágonos para que la información brindada sea mejor, pero el comportamiento de las celdas es de hexagonos como se podrá comprobar durante el desarrollo del juego. Cada celda tiene en su tope dos letras, la primera corresponde a su color(__B__ para las negras y __W__ las blancas) y la segunda corresponde a la primera letra del nombre en ingles del bicho que representan. Luego en el centro de la celda en los extremos derecho e izquierdo se escriben la posición $x$, $y$ de la casilla en el tablero. Esto es para el mejor desenvolvimiento del jugador a la hora de realizar movimientos y además se escriben las celdas vacias alrededor de la colmena para que sea más fácil identificar las celdas adyacentes.

Después de que cada player juegue su turno se revisa los adyacentes a cada reina para verificar el estado del juego. Si la reina contraria fue rodeada gana este jugador, si la suya fue rodeada pierde y si ambas son rodeadas a la ves se considera un empate, luego de esto se ejecuta el predicado __abort().__ y se asume concluido el juego. 

#### Modos de juego:

El proyecto posee 3 modos de juego, player vs player, player vs ia, ia vs ia. En la interface se explica como acceder a cada uno de estos modos.

Player vs Player: como el nombre indica es para 2 jugadores humanos y se esperaran entradas de ambos para cada jugada.

Player vs IA: un jugador en contra del algoritmo desarrollado, deberá escribir la jugada y luego se le pide confirmar la jugada.

IA vs IA: en este modo solo deberá confirmar cada turno para ver como se desarrolla el juego entre dos instancias del algoritmo.

#### Consideraciones:

Tomamos como consideraciones:

-Cada ficha al moverse siempre debe estar en contacto con alguna ficha de la colmena: cada paso del camino recorrido debe estar tocando la colmena.

-El hive nunca debe desconectarse o dividirse en dos: cada paso del camino recorrido debe cumplir esta condición, ningún paso dado puede desconectar la colmena.

-El caso de empate solamente consideramos cuando las dos reinas son rodeadas al mismo tiempo. El segundo caso no lo consideramos un empate, para este caso queda en manos de los jugadores declarar el empate o no, dado que consideramos que esta situación solo se dará si los jugadores no quieren cometer un mal movimiento.

#### IA:

Para el desarrollo de este algoritmo utilizamos Minimax. Tomamos como funciones de peso la cantidad de fichas que rodean a la reina contraria a la propia y la cantidad de fichas bloqueadas. Buscamos todos los posibles movimientos, colocaciones, uso del movimiento especial del pillbug y el bloqueo del beetle y a partir de cada uno de estos cambios en el tablero vemos las posibles jugadas del oponente, desarrollando asi un arbol de profundidad dos en de las jugadas. 

Especificamos que le damos un valor mas alto de peso a colocar una ficha que a mover(lo suficientemente alto como para que no elija siempre poner en ves de moverse) para un mejor funcionamiento de el algoritmo en el desarrollo del juego.

