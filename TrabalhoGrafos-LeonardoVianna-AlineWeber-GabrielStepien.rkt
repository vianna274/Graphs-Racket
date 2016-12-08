;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname TrabalhoGrafos-LeonardoVianna-AlineWeber-GabrielStepien) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; IMPORTANTE: para ter acesso a leitura de arquivos
;; é necessário configurar o DrRacket para a linguagem
;; "Advanced Student" // SÓ CONSEGUI UTILZANDO "THE RACKET LANGUAGE"

;;******************** COMO USAR: UTILIZAR A FUNÇÃO "(final graf)" apenas chamando na execução (final (e o grafo com a estrutura idêntica ao enunciado))

;; Nome do arquivo a ser lido
(define src "grafo.txt")
;; Lê o arquivo de entrada e armazena em r, 
;; separando a informação lida em dois valores
;;   n : número de nodos
;;   g : grafo


(define r (with-input-from-file src read))
(define n (first r))
(define g (rest r))



;; DEFINIÇÃO DE DADOS PARA O GRAFO

; Uma adjacência é uma lista
;   (list s1 s2 ... sn)
; onde
;   s1 : símbolo 
;        representa um nodo no grafo
;   s2 ... sn : símbolo 
;        representam os nodos adjacentes a s1
;
; Um grafo é uma lista de adjacências
;   (list a1 a2 ... an)
; onde todo símbolo referenciado nas adjacências a1 ... an
; ocorre como primeiro elemento em uma única adjacência ak
(define graf(list
   (list 0 1)
  ( list 1 4)
  ( list 4 3 5)
  ( list 3 1)
  ( list 5 2)
  ( list 2 5)))

(define graforiginal(list 6
   (list 0 1)
  ( list 1 4)
  ( list 4 3 5)
  ( list 3 1)
  ( list 5 2)
  ( list 2 5)))

; vizinhos : símbolo, grafo -> lista-de-símbolos
; ( vizinhos n G) retorna todos os nodos no grafo G
; que recebem arestas de n
( define ( vizinhos n G)
   ( cond
      ; erro, não achou nodo
      [( empty? G) empty ]
      ; achou nodo, retorna nodos adjacentes
      [( equal? n ( first ( first G ))) ( rest ( first G ))]
      ; não é o nodo atual, continua busca no resto da lista
      [ else ( vizinhos n ( rest G ))]))

; conectados-larg? : símbolo símbolo grafo -> boolean
( define ( conectados-larg? a b G)
   ( busca-larg ( list a) (list ) b G))

; busca-larg : lista-simb, simb, lista-simb, grafo -> boolean
( define ( busca-larg la p b G)
   ( cond
      ; lista vazia?
      [( empty? la) false ]
      ; destino pertence à lista de nodos atual?
      [( member b la) true ]
      ; repete busca nos vizinhos dos vizinhos
      [ else ( busca-larg ( todos-vizinhos la p G) (append la p) b G )]))

; todos-vizinhos : lista-simb, lista-simb, grafo -> lista-simb
( define ( todos-vizinhos la p G)
   ( cond
      [( empty? la) empty ]
      [ else ( append (filter (lambda (x) (not (member x p)))
                              ( vizinhos ( first la) G))
                      ( todos-vizinhos ( rest la) p G ))]))



; conectados-prof? : símbolo símbolo grafo -> boolean
( define ( conectados-prof? a b G)
   ( busca-prof ( list a) ( list ) b G))
; busca-prof : lista-simb, simb, lista-simb, grafo -> bool
( define ( busca-prof la p b G)
   ( cond
      ; não há nodos na lista
      [( empty? la) false ]
      ; o primeiro nodo é o destino?
      [( equal? ( first la) b) true ]
      ; busca em profundidade a partir do primeiro nodo .
      [ else ( busca-prof
               ( append ( novos-vizinhos ( first la) p G) ( rest la ))
               ( cons ( first la) p) b G )]))
; novos-vizinhos : simb, lista-simb, grafo -> lista-simb
( define ( novos-vizinhos a p G)
   ( filter ( lambda (x) ( not ( member x p )))
            ( vizinhos a G )))


(define (recursive nodo graph visitados graphaux)
  (cond
    [(empty? graph)empty]
    [(tdsvisitados graph visitados)visitados]
    [(not(jatem?2 (first nodo) visitados)) (recursive nodo graph (visitavizinhos nodo graph (append visitados (list(first nodo))))graphaux)]
    [else (recursive (first graphaux) graph visitados (rest graphaux))]))


(define (tdsvisitados graph visitados)
  (cond
    [(empty? graph)#true]
    [(jatem?2 (first(first graph))visitados)(tdsvisitados (rest graph)visitados)]
    [else #false])) 

(define (imag vizinhos graph visitados)
  (cond
    [(empty? vizinhos)visitados]
    [(not(jatem?2 (first vizinhos) visitados))(imag (rest vizinhos) graph (append(visitavizinhos (devolveNodo (first vizinhos) graph) graph visitados)(list (first vizinhos))))]
    [else (imag (rest vizinhos) graph (visitavizinhos (devolveNodo (first vizinhos) graph) graph visitados))]))

(define (visitavizinhos nodo graph visitados)
  (cond
    [(empty? (rest nodo))visitados]
    [(not(jatem?2 (first(rest nodo)) visitados))(visitavizinhos (arrumavizi (first(rest nodo)) (rest nodo) (first nodo))graph (append visitados (list(first(rest nodo)))))]
    [else (visitavizinhos (cons (first nodo)(rest(rest nodo))) graph visitados)]))


(define (arrumavizi num vizinhos principal)
  (cond
    [(not(jatem?2 num vizinhos))(cons principal vizinhos)]
    [(empty? vizinhos)(cons principal vizinhos)]
    [(empty? (rest vizinhos))(cons principal vizinhos)]
    [(= num (first vizinhos))(cons principal (rest vizinhos))]
    [else (arrumavizi num (append (rest vizinhos)(list(first vizinhos)))principal)]))
; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;###############################################################;###############################################################

; final: Grafo -> Lista-de-Lista-de-Numeros
; Recebe um Grafo e devolve seus componentes
;(check-expect (final graforiginal)(list (list 0) (list 1 4 3) (list 5 2)))
(define (final graf)
  (cond
  [(tam2 (separa-final (separando-componentes graf graf)))(haciclo graf)] ;; Se tiver um ciclo devolve os componentes separados
  [else (naohaciclo graf)]))

   
; Dado dois grafos os junta: Grafo Grafo -> Grafo
(define (juntagraph graf)
  (cond
    [(empty? graf)empty]
    [else (append (organizagraf (conexosgraf graf graf)(conexosgraf graf graf))
                  (organizagraf (desconexos graf graf)(desconexos graf graf)))]))

;###############################################################;###############################################################
; separa-final: Lista-de-Lista-de-numeros -> lista-de-numeros
; recebe uma "Lista de Lista-de-Numeros" e retorna ela retirando os repetidos
(define(separa-final g)
  (cond
    [(empty? g)empty]
    [(jatem? (first(first g))(rest g))(separa-final (rest g))]
    [else (cons (first g)(separa-final (rest g)))]))

; separando-componentes: graf -> lista de lista-de-numeros
; recebe um grafo e separa todos os componentes dele (inclusive repetidos)
(define (separando-componentes graf grafori)
  (cond
    [(empty? graf)empty]
    [else (cons (separa(first(first graf))grafori grafori)(separando-componentes (rest graf) grafori))]))

; separa: Numero Grafo Grafo -> Lista-de-Numeros
; Recebe um numero e 2 Grafos, um vai ser modificado durante o programa, o outro vai se manter original para testar a conexão.
(define (separa num1 graf grafori)
  (cond
    [(empty? graf)empty] 
    [(and(conectados-larg? num1 (first(first graf))grafori)
         (conectados-larg? (first(first graf))num1 grafori))
     (cons (first(first graf))(separa num1 (rest graf) grafori))]
    [else (separa num1 (rest graf) grafori)]))

; jatem?: Numero Lista-de-Lista-de-Numeros -> Boolean
; Recebe um Número e uma "Lista de Lista-de-Numeros" e retorna se esse valor está nessa lista
(define (jatem? num lista)
  (cond
    [(empty? lista)#f]
    [(= num(first(first lista)))#t]
    [else (jatem? num (rest lista))]))

(define (any num graf grafori)
  (cond 
    [(empty? graf)#f]
    [(not(conectados-larg? num (first(first graf))(grafate grafori(first(first graf))grafori)))(first graf)]
    [else (any num (rest graf)grafori)]))

; Dado um numero de inicio e 2 grafos, retorna se existe algum elemento nesse grafo que não está ligado a esse numero
(define (any2 num graf grafori)
  (cond 
    [(empty? graf)#f]
    [(not(conectados-larg? num (first(first graf))(grafate grafori(first(first graf))grafori)))#t]
    [else (any2 num (rest graf)grafori)]))

; Dado um grafo o devolve organizado
(define (organizagraf grafori graf2)
  (cond 
    [(empty? graf2)grafori]
    [(any2 (first(first grafori))grafori grafori)(organizagraf (botaultimo (any (first(first grafori))grafori grafori)grafori grafori)(botaultimo (any (first(first grafori))grafori grafori)grafori grafori))]
    [else grafori]))
 
; Dado uma lista de números e um grafo, retorna somente um grafo com  esses números como nodo 
(define (montagraf listanum graf)
  (cond
    [(empty? listanum)empty]
    [else (cons (devolveNodo (first listanum) graf)(montagraf (rest listanum) graf))]))


; Dado um número e um grafo, retorna o nodo desse número
(define (devolveNodo number graf)
  (cond
    [(empty? graf)0]
    [(= number(first(first graf)))(first graf)] 
    [else (devolveNodo number (rest graf))])) 

; Dado 2 grafos e 1 numero, retorna o grafico até esse nodo
(define (grafate graf number graf2)
  (cond
    [(empty? graf2)empty]
    [(=(first(first graf2))number)(cons(devolveNodo(first(first graf2))graf)(grafate graf number empty))]
    [else (cons(devolveNodo(first(first graf2))graf)(grafate graf number (rest graf2)))]))

; Dado um Nodo e um Grafo coloca esse Nodo em ultimo lugar
(define (botaultimo nodo graf graf2)
  (cond
    [(empty? graf2)(cons nodo empty)]
    [(= (first nodo)(first(first graf2)))(botaultimo nodo graf (rest graf2))]
    [(conexo? graf (first graf2))(cons (first graf2)(botaultimo nodo graf (rest graf2)))]
    [else (cons (first graf2)(rest graf2))]))

; Dado um grafo retorna uma lista de numero
(define (ldn? graf)
  (cond
    [(empty? graf)empty] 
    [else (cons(first(first graf))(ldn? (rest graf)))]))

; Dado um nodo, um grafo e uma ldn retorna se ele é alcançável
(define (conexo? graf nodo)
    (conectados-larg? (first(first graf))(first nodo)graf))

; Recebe um numero e uma lista de numeros e retorna se esta na lista
(define (jatem?2 num lista)
  (cond
    [(empty? lista)#f]
    [(= num(first lista))#t]
    [else (jatem?2 num (rest lista))]))

; Recebe uma lista e remove os repetidos
(define (remrep ldn)
  (cond
    [(empty? ldn)empty]
    [(jatem?2 (first ldn) (rest ldn))(remrep (rest ldn))]
    [else (cons (first ldn)(remrep (rest ldn)))]))

; Dado dois grafos iguais, devolve o grafo desconexo
(define (desconexos graf graf2)
  (cond
    [(empty? graf2)empty]
    [(not(conexo? graf (first graf2)))(cons (first graf2)(desconexos graf (rest graf2)))]
    [else (desconexos graf (rest graf2))]))

; Dado 2 grafos devolve o grafo CONEXO
(define (conexosgraf graf graf2)
  (cond
    [(empty? graf2)empty]
    [(conexo? graf (first graf2))(cons (first graf2)(conexosgraf graf(rest graf2)))]
    [else (conexosgraf graf(rest graf2))]))

; Dado uma lista verifica se tem tamanho maior que 2
(define (tam2 lista)
  (cond
    [(empty? lista)#f]
    [(empty? (rest(first lista)))(tam2 (rest lista))]
    [else #t]))

(define (haciclo graf)
  (begin
    (printf "Não há ordenamento topológico\n")
    (printf "Componentes: " )
    (display (separa-final(separando-componentes graf graf)))
    (printf "\n")))

(define (naohaciclo graf)
  (begin
    (printf "Ordenamento topológico: ")
    (display (recursive (first graf)graf (list ) graf))
    (printf "\n")
    (printf "Componentes: ")
    (display (separa-final(separando-componentes graf graf)))
    (printf "\n")))

(final g)