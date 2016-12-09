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

; recursive: Lista-Numeros Lista-Lista-num Lista-Num Lista-Lista-num
; Dado o nodo inicial, começará a visitar todos os outros nodos que estão conectados a ele, o "graphaux" é utilizada caso
; o grafo seja desconexo, assim quando ele terminar de verificar todos os possíveis do conexo, vai utilizar o auxliar para visitar o resto
(define (recursive nodo graph visitados graphaux)
  (cond
    [(empty? graph)empty] ; se o graph que recebeu é empty, devolve empty
    [(tdsvisitados graph visitados)visitados] ; se já visitou todos, devolve visitados
    ; verifica se o nodo já foi visitado, caso não visita ele E seus vizinhos
    [(not(jatem?2 (first nodo) visitados)) (recursive nodo graph (visitavizinhos nodo graph (append visitados (list(first nodo))))graphaux)]
    ; Se o nodo já foi visitado, vai diminuir o "graphaux" em um, e utilizar o primeiro dele como nodo
    [else (recursive (first graphaux) graph visitados (rest graphaux))]))

; tdsvisitados: Lista-Lista-num Lista-num
; Vai receber um Grafo e uma Lista-num, verifica se todos os nodos do grafo já foram visitados
(define (tdsvisitados graph visitados)
  (cond
    [(empty? graph)#true]
    [(jatem?2 (first(first graph))visitados)(tdsvisitados (rest graph)visitados)]
    [else #false])) 

; visitazinhos: Lista-Num Lista-Lista-Num Lista-Num
; Recebe um Nodo, Grafo e os Visitados, se o primeiro vizinho não estiver
; vou visitar os vizinhos dele(excluindo ele desses vizinhos) e atualizando visitados
(define (visitavizinhos nodo graph visitados)
  (cond
    [(empty? (rest nodo))visitados]
    [(not(jatem?2 (first(rest nodo)) visitados))(visitavizinhos (arrumavizi (first(rest nodo)) (rest nodo) (first nodo))graph (append visitados (list(first(rest nodo)))))]
    [else (visitavizinhos (cons (first nodo)(rest(rest nodo))) graph visitados)]))

; arrumavizi: Num Lista-Num Num
; Dado o Número a ser retirado, os Vizinhos e o Principal (1 2 3) 1 é o principal
; Remove o Número dos vizinhos e devolve o Nodo sem ele (colocando o principal como primeiro)
(define (arrumavizi num vizinhos principal)
  (cond
    [(not(jatem?2 num vizinhos))(cons principal vizinhos)]
    [(empty? vizinhos)(cons principal vizinhos)]
    [(empty? (rest vizinhos))(cons principal vizinhos)]
    [(= num (first vizinhos))(cons principal (rest vizinhos))]
    [else (arrumavizi num (append (rest vizinhos)(list(first vizinhos)))principal)]))
; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;###############################################################;###############################################################

; final: Grafo -> Trabalho
; Recebe um Grafo e se houver componentes com tamanho maior que 2 retorna somente eles
; Se não, retorna os componentes e o ordenamento
(define (final graf)
  (cond
  [(tam2 (separa-final (separando-componentes graf graf)))(haciclo graf)] ;; Se tiver um ciclo devolve os componentes separados
  [else (naohaciclo graf)]))


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

; Dado um número e um grafo, retorna o nodo desse número
(define (devolveNodo number graf)
  (cond
    [(empty? graf)0]
    [(= number(first(first graf)))(first graf)] 
    [else (devolveNodo number (rest graf))])) 

; Recebe um numero e uma lista de numeros e retorna se esta na lista
(define (jatem?2 num lista)
  (cond
    [(empty? lista)#f]
    [(= num(first lista))#t]
    [else (jatem?2 num (rest lista))]))

; Dado uma lista verifica se tem tamanho maior que 2
(define (tam2 lista)
  (cond
    [(empty? lista)#f]
    [(empty? (rest(first lista)))(tam2 (rest lista))]
    [else #t]))

; Função para printar caso haja Ciclos nos componentes
(define (haciclo graf)
  (begin
    (printf "Não há ordenamento topológico\n")
    (printf "Componentes: " )
    (display (separa-final(separando-componentes graf graf)))
    (printf "\n")))

; Função para printar caso não haja Ciclos nos componentes
(define (naohaciclo graf)
  (begin
    (printf "Ordenamento topológico: ")
    (display (recursive (first graf)graf (list ) graf))
    (printf "\n")
    (printf "Componentes: ")
    (display (separa-final(separando-componentes graf graf)))
    (printf "\n")))

(final g)