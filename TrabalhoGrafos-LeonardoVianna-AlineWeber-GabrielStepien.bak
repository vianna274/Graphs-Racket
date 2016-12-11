;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname TrabalhoGrafos-LeonardoVianna-AlineWeber-GabrielStepien-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; IMPORTANTE: para ter acesso a leitura de arquivos
;; é necessário configurar o DrRacket para a linguagem
;; "Advanced Student" 

;--------------------------------------------------------------------------------------------------------------
;                                  LEITURA DO ARQUIVO "grafo.txt"                                             ;
;--------------------------------------------------------------------------------------------------------------

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



;--------------------------------------------------------------------------------------------------------------
;                                          FUNÇÕES DO MOODLE                                                  ;
;--------------------------------------------------------------------------------------------------------------

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

;--------------------------------------------------------------------------------------------------------------
;                               FUNÇÃO QUE DECIDE QUAL PRINT CHAMAR                                           ;
;--------------------------------------------------------------------------------------------------------------

; final: Grafo -> Trabalho
; Recebe um grafo e se houver componentes com tamanho maior igual que 2 retorna somente eles
; Se não, retorna os componentes e o ordenamento
(define (final grafo)
  (cond
    [( tamanho-maior-igual-que-2 (separa-final (separando-componentes grafo grafo))) (ha-ciclo grafo)] ;; Se tiver um ciclo devolve os componentes separados
    [else (nao-ha-ciclo grafo)]))

;--------------------------------------------------------------------------------------------------------------
;                                   FUNÇÕES PARA SEPARAR COMPONENTE                                           ;
;--------------------------------------------------------------------------------------------------------------

; separa-final: Lista-de-Lista-de-numeros -> Lista-de-numeros
; Recebe a lista de componentes, com componentes repetidos, e retorna ela retirando os repetidos
(define (separa-final componentes)
  (cond
    [(empty? componentes) empty]
    [(ja-tem? (first (first componentes)) (rest componentes)) (separa-final (rest componentes))] ;Se um nodo está em outro componente, ignora o atual componente
    [else (cons (first componentes) (separa-final (rest componentes)))])) ;Caso contrário, inclui este componente e segue o filtro

; separando-componentes: Grafo Grafo -> Lista-de-lista-de-numeros
; Recebe um grafo e separa todos os componentes dele (inclusive repetidos)
(define (separando-componentes grafo grafo-aux)
  (cond
    [(empty? grafo) empty]
    [else (cons
           (separa (first (first grafo)) grafo-aux grafo-aux) ; Componente do primeiro nodo
           (separando-componentes (rest grafo) grafo-aux))])) ; Resto dos componentes

; separa: Numero Grafo Grafo -> Lista-de-Numeros
; Recebe o mesmo grafo duas vezes pois um vai ser modificado durante o programa, para a recursão, e o outro vai se manter original para testar a conexão.
; Busca todos os nodos no grafo que fazem parte do mesmo componenete que o número passado por parâmetro
(define (separa n grafo grafo-aux)
  (cond
    [(empty? grafo) empty] 
    [(and (conectados-larg? n (first (first grafo)) grafo-aux)
          (conectados-larg? (first (first grafo)) n grafo-aux)) 
     (cons (first (first grafo))(separa n (rest grafo) grafo-aux))] ; Caso os dois nodos estejam conectados, inclui ele na lista e continua a separação do componente 
    [else (separa n (rest grafo) grafo-aux)])) ;Senão apenas continua a separação do componente

;--------------------------------------------------------------------------------------------------------------
;                                   FUNÇÕES AUXILIARES PARA AMBAS PARTES                                       ;
;--------------------------------------------------------------------------------------------------------------

; ja-tem?: Numero Lista-de-Lista-de-Numeros -> Boolean
; Verifica se o número recebido está na lista de lista de números (lista de componentes)
(define (ja-tem? n lista)
  (cond
    [(empty? lista)#f]
    [(= n (first (first lista))) #t]
    [else (ja-tem? n (rest lista))]))

; ja-tem?2: Numero Lista-de-numeros -> Boolean
; Verifica se o número está na lista de números (lista do ordenamento topológico)
(define (ja-tem?2 n lista)
  (cond
    [(empty? lista )#f]
    [(= n (first lista)) #t]
    [else (ja-tem?2 n (rest lista))]))

; tamanho-maior-igual-que-2: Lista -> Boolean
; Dado uma lista verifica se tem tamanho maior que 2
(define ( tamanho-maior-igual-que-2 lista)
  (cond
    [(empty? lista) #f]
    [(empty? (rest (first lista))) ( tamanho-maior-igual-que-2 (rest lista))]
    [else #t]))

;--------------------------------------------------------------------------------------------------------------
;                                  FUNÇÕES PARA ORDEM TOPOLÓGICA                                              ;
;--------------------------------------------------------------------------------------------------------------

; todos-visitados?: Grafo Lista-de-numeros -> Boolean
; Verifica se todos os nodos do grafo já foram visitados
(define (todos-visitados? grafo visitados)
  (cond
    [(empty? grafo)#true]
    [(ja-tem?2 (first(first grafo)) visitados) (todos-visitados? (rest grafo) visitados)] ;Caso o primeiro nodo esteja na lista dos visitados, verifica os próximos
    [else #false])) ;Se o primeiro nodo não estava, já sabe que não foram todos visitados

; aponta?: Grafo Lista-de-numeros Numero -> Boolean
; Verifica se o nodo recebido por parâmetro aponta para outro nodo que ainda não está na lista de visitados
(define (aponta? grafo visitados n)
  (cond
    [(empty? grafo) #false]
    [(and
      (ja-tem?2 n (rest(first grafo)))
      (not (ja-tem?2 (first(first grafo)) visitados))) #true] ; Se o nodo está nas adjacencias do primeiro do grafo e o primeiro do grafo não está nos visitados
    [else (aponta? (rest grafo) visitados n)])) ;Senão, continua a recursão com o resto do grafo

; acha-proximo: Grafo Grafo Lista-de-numeros -> Lista-de-numeros
; Acha o próximo nodo a ser incluído no ordenamento e retorna a lista de visitados com esse nodo no fim
(define (acha-proximo grafo grafo-aux visitados)
  (cond
    [(empty? grafo) empty]
    [(and
      (not (aponta? grafo-aux visitados (first(first grafo))))
      (not (ja-tem?2 (first(first grafo)) visitados)))
     (append visitados (list (first(first grafo))))] ; Se nenhum nodo aponta para ele, exceto os que já estão na lista de visitados (o ordenamento), inclui o nodo no final da lista
    [else (acha-proximo (rest grafo) grafo-aux visitados)]))

; acha-ordenamento: Grafo Lista-de-numeros -> Lista-de-numeros
; Percorre o grafo e organiza os nodos em um ordenamento topológico
(define (acha-ordenamento grafo visitados)
  (cond
    [(empty? grafo) empty]
    [(todos-visitados? grafo visitados) visitados] ; Se já visitou todos os nodos, o ordenamento é a ordem em que eles foram visitados
    [else (acha-ordenamento grafo (acha-proximo grafo grafo visitados))])) ; Se não, continua formando o ordenamento

;--------------------------------------------------------------------------------------------------------------
;                                   PRINTS DO FINAL DO TRABALHO                                               ;
;--------------------------------------------------------------------------------------------------------------
; ha-ciclo: Grafo
; Função para printar caso existam ciclos nos componentes
(define (ha-ciclo grafo)
  (begin
    (printf "Componentes: " )
    (display (separa-final(separando-componentes grafo grafo)))
    (printf "\n")
    (printf "Não há ordenamento topológico\n")
    (printf "\n")))

; nao-ha-ciclo: Grafo
; Função para printar caso não existam nos componentes
(define (nao-ha-ciclo grafo)
  (begin
    (printf "Componentes: ")
    (display (separa-final(separando-componentes grafo grafo)))
    (printf "\n")
    (printf "Ordenamento topológico: ")
    (display (acha-ordenamento grafo (list)))
    (printf "\n")))

(final g)