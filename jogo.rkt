; Este arquivo deve conter as definições das funções do jogo (com exceção da main).
; As definições devem incluir assinatura, propósito, protótipo e templates utilizados.


#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "constantes.rkt")
(require "dados.rkt")

(provide (all-defined-out))

;; =================================================================================================================================
;;FUNCOES

;; Inicio da parte logica 

;; proximo-jogo : Jogo -> Jogo 
;; interp. atualiza o jogo 

; (define (proximo-jogo j)  j)

(define (proximo-jogo j) 
  (local 
    [ 
     (define spawn-tat1? (= (jogo-spawn-tat1 j) 0)) 
     (define spawn2? (= (jogo-spawn2 j) 0))
     (define spawnMoeda? (= (jogo-spawn-moeda j) 0))
     (define pulou-encima-tat? (pulou-encima-alguma-tartaruga? (jogo-mario j) (jogo-tartarugas j)))
     (define colisao-com-a-vida? (colisao-algum-mario-vida? (jogo-mario j) (jogo-vidas j)))
     (define colisao-com-a-moeda? (colisao-algum-mario-moeda? (jogo-mario j) (jogo-moedas j)))
     (define colisao-com-a-estrela? (colisao-algum-mario-estrela? (jogo-mario j) (jogo-stars j)))
     (define chegou-no-final-tat? (alguma-tat-chegou-no-final? (jogo-tartarugas j)))
     (define chegou-no-final-vida? (alguma-vida-chegou-no-final? (jogo-vidas j)))
     (define chegou-no-final-estrela? (alguma-estrela-chegou-no-final? (jogo-stars j)))
     ]
    (cond [(not (false? pulou-encima-tat?))             
           (make-jogo (proximo-mario  (quica (jogo-mario j))) 
                      (jogo-plataformas j) 
                      (jogo-game-over? j)
                      (jogo-spawn-tat1 j)
                      (jogo-spawn2 j) 
                      (remove pulou-encima-tat? (jogo-tartarugas j))
                      (jogo-vidas j)
                      (jogo-stars j)
                      (jogo-qtdVidas j)
                      (jogo-contStar j)
                      (jogo-moedas j)
                      (jogo-spawn-moeda j)
                      (jogo-qtdStars j)
                      (jogo-qtdMoedas j))]
          [(>= (jogo-contStar j) 5)                
           (make-jogo (proximo-mario (jogo-mario j))
                      (jogo-plataformas j) 
                      #false  
                      1
                      1 
                      empty 
                      empty
                      (jogo-stars j)
                      (jogo-qtdVidas j)
                      (if (pegou-todas-moedas? (jogo-qtdMoedas j))
                          0
                          (jogo-contStar j))
                      (cond [(not (false? (colisao-algum-mario-moeda? (jogo-mario j) (jogo-moedas j))))
                             (proximas-moedas (remove colisao-com-a-moeda? (jogo-moedas j)))]
                            [ spawnMoeda? 
                              (proximas-moedas (jogo-moedas j))]
                            
                            [else (jogo-moedas j)])
                      
                      (remainder (+ (jogo-spawn-moeda j) 1) T-SPAWN-MOEDA)
                      (jogo-qtdStars j)
                      (if (not (false? (colisao-algum-mario-moeda? (jogo-mario j) (jogo-moedas j))))
                          (proxima-qtd-moedas (jogo-qtdMoedas j))
                          (jogo-qtdMoedas j))
                      )]
          
          [(not (false? chegou-no-final-tat?))  
           (make-jogo (jogo-mario j)
                      (jogo-plataformas j)   
                      (jogo-game-over? j)
                      (jogo-spawn-tat1 j)
                      (jogo-spawn2 j)
                      (proximas-tartarugas (remove chegou-no-final-tat? (jogo-tartarugas j))) 
                      (jogo-vidas j)
                      (jogo-stars j)
                      (jogo-qtdVidas j)
                      (jogo-contStar j)
                      (jogo-moedas j)
                      (jogo-spawn-moeda j)
                      (jogo-qtdStars j)
                      (jogo-qtdMoedas j))]
          
          [(not (false? chegou-no-final-estrela?))  
           (make-jogo (jogo-mario j)
                      (jogo-plataformas j)  
                      (jogo-game-over? j)
                      (jogo-spawn-tat1 j)
                      (jogo-spawn2 j)
                      (jogo-tartarugas j) 
                      (jogo-vidas j)
                      (proximas-estrelas (remove chegou-no-final-estrela? (jogo-stars j)))
                      (jogo-qtdVidas j)
                      (jogo-contStar j)
                      (jogo-moedas j)
                      (jogo-spawn-moeda j)
                      (jogo-qtdStars j)
                      (jogo-qtdMoedas j))]
          
          [(not (false? chegou-no-final-vida?)) 
           (make-jogo (jogo-mario j)
                      (jogo-plataformas j) 
                      (jogo-game-over? j)
                      (jogo-spawn-tat1 j)
                      (jogo-spawn2 j)
                      (jogo-tartarugas j) 
                      (proximas-vidas (remove chegou-no-final-vida? (jogo-vidas j)))
                      (jogo-stars j)
                      (jogo-qtdVidas j)                      
                      (jogo-contStar j)
                      (jogo-moedas j)
                      (jogo-spawn-moeda j)
                      (jogo-qtdStars j)
                      (jogo-qtdMoedas j))] 
          
          [(and (colisao-algum-mario-tartaruga? (jogo-mario j) (jogo-tartarugas j))
                (acabou-vida-mario? (jogo-qtdVidas j)))
           (make-jogo (jogo-mario j) 
                      (jogo-plataformas j)   
                      #true 
                      (jogo-spawn-tat1 j)
                      (jogo-spawn2 j)
                      (jogo-tartarugas j)
                      (jogo-vidas j)
                      (jogo-stars j)
                      (jogo-qtdVidas j)
                      (jogo-contStar j)
                      (jogo-moedas j)
                      (jogo-spawn-moeda j)
                      (jogo-qtdStars j)
                      (jogo-qtdMoedas j)
                      )]
          
          [(and (colisao-algum-mario-tartaruga? (jogo-mario j) (jogo-tartarugas j))
                (not (acabou-vida-mario? (jogo-qtdVidas j))))
           (make-jogo (proximo-mario (repelir-mario (jogo-mario j)))
                      (jogo-plataformas j)  
                      (jogo-game-over? j)
                      (jogo-spawn-tat1 j)
                      (jogo-spawn2 j)
                      (jogo-tartarugas j)
                      (jogo-vidas j)
                      (jogo-stars j)
                      (qtd-anterior-vida (jogo-qtdVidas j))
                      (jogo-contStar j)
                      (jogo-moedas j)
                      (jogo-spawn-moeda j)
                      (jogo-qtdStars j)
                      (jogo-qtdMoedas j)
                      )]
          
          [(not (false? (colisao-algum-mario-vida? (jogo-mario j) (jogo-vidas j)))) 
           (make-jogo (jogo-mario j)
                      (jogo-plataformas j)   
                      (jogo-game-over? j) 
                      (jogo-spawn-tat1 j)
                      (jogo-spawn2 j)
                      (jogo-tartarugas j)
                      (proximas-vidas (remove colisao-com-a-vida? (jogo-vidas j)))
                      (proximas-estrelas (jogo-stars j))
                      (proxima-qtd-vida (jogo-qtdVidas j))
                      (jogo-contStar j)
                      (jogo-moedas j)
                      (jogo-spawn-moeda j)
                      (jogo-qtdStars j)
                      (jogo-qtdMoedas j)
                      )]
          
          [(not (false? (colisao-algum-mario-estrela? (jogo-mario j) (jogo-stars j)))) 
           (make-jogo (jogo-mario j) 
                      (jogo-plataformas j)   
                      (jogo-game-over? j) 
                      (jogo-spawn-tat1 j)
                      (jogo-spawn2 j)
                      (jogo-tartarugas j)
                      (jogo-vidas j)
                      (proximas-estrelas (remove colisao-com-a-estrela? (jogo-stars j)))
                      (jogo-qtdVidas j)
                      (+ (jogo-contStar j) 1)
                      (jogo-moedas j)
                      (jogo-spawn-moeda j)
                      (proxima-qtd-star (jogo-qtdStars j))
                      (jogo-qtdMoedas j)
                      )]
          
          [else (make-jogo (proximo-mario (jogo-mario j))    
                           (jogo-plataformas j)                       
                           (jogo-game-over? j)
                           (remainder (+ (jogo-spawn-tat1 j) 1) T-SPAWN-TAT1) 
                           (remainder (+ (jogo-spawn2 j) 1) T-SPAWN-VIDA) 
                           (if spawn-tat1? 
                               (spawn-tartaruga (proximas-tartarugas (jogo-tartarugas j)))
                               (proximas-tartarugas (jogo-tartarugas j)))
                           (if spawn2? 
                               (spawn-vida (proximas-vidas (jogo-vidas j)))
                               (proximas-vidas (jogo-vidas j)))
                           (if spawn2? 
                               (spawn-estrela (proximas-estrelas (jogo-stars j)))
                               (proximas-estrelas (jogo-stars j)))
                           (jogo-qtdVidas j)
                           (jogo-contStar j)
                           (jogo-moedas j)
                           (jogo-spawn-moeda j)
                           (if (pegou-todas-moedas? (jogo-qtdMoedas j))
                               (zerar-cont-estrela (jogo-qtdStars j))
                               (jogo-qtdStars j))
                           (jogo-qtdMoedas j))]
          ))
  )

;; Template
#;
(define (fn-para-proximo-jogo j)
  (... (jogo-mario j)
       (jogo-plataformas j)
       (jogo-game-over? j)
       (jogo-spawn-tat1 j)
       (jogo-spawn2 j)
       (jogo-tartarugas j)
       (jogo-vidas j)
       (jogo-stars j)
       (jogo-qtdVidas j)
       (jogo-contStar j)
       (jogo-moedas j)
       (jogo-spawn-moeda j)
       (jogo-qtdStars j)
       (jogo-qtdMoedas j)
       ))

;; zerar-cont-estrela : qtdEstrela -> qtdEstrela
;; zera o contado de quantidade de estrela
; (define (zerar-cont-estrela qtdE) qtdE)

(define (zerar-cont-estrela qtdE)
  (make-qtdStar (qtdStar-x qtdE) 
                (qtdStar-y qtdE)
                 0))

;; Template
#;
(define (fn-para-zerar-cont-estrela e)
  (... (qtdStar-x e)
       (qtdStar-y e)
       (qtdStar-cont e))
  )


;; pegou-todas-moedas? : qtdMoedas -> Bolean
;; interp. retorna true se pegou todas as moedas
;; ou false se nao pegou todas ainda
; (define (pegou-todas-moedas? qtdM) #true)

(define (pegou-todas-moedas? qtdM)
  (if (>= (qtdMoeda-cont qtdM) 10)
      #true
      #false))

;; Template
#;
(define (fn-para-pegou-todas-moedas? v)
  (... (qtdMoeda-x v)
       (qtdMoeda-y v)
       (qtdMoeda-cont v))
  )

;; acabou-vida-mario? : qtdVida -> Boolean
;; interp. retorna se a vida do mario acabou ou nao
; (define (acabou-vida-mario? qtdV) #false)

(define (acabou-vida-mario? qtdV)
  (if (<= (qtdVida-cont qtdV) 0)
      #true
      #false))

;; Template
#;
(define (fn-para-acabou-vida-mario? v)
  (... (qtdVida-x v)
       (qtdVida-y v)
       (qtdVida-cont v))
  )

;; qtd-anterior-vida : qtdVida -> qtdVida
;; interp. recebe uma quantidade de vida e retorna quantidade de vida
; (define (qtd-anterior-vida v) v)


(define (qtd-anterior-vida v)
  (make-qtdVida (qtdVida-x v)
                (qtdVida-y v)
                (- (qtdVida-cont v) 1)))

;; Template
#;
(define (fn-para-qtd-anterior-vida v)
  (... (qtdVida-x v)
       (qtdVida-y v)
       (qtdVida-cont v))
  )

;; proxima-qtd-vida : qtdVida -> qtdVida
;; interp. recebe uma quantidade de vida e retorna quantidade de vida
; (define (proxima-qtd-vida v) v)

(define (proxima-qtd-vida v)
         (if (< (qtdVida-cont v) 5)
             (make-qtdVida (qtdVida-x v) 
                           (qtdVida-y v)
                           (+ (qtdVida-cont v) 1))
             v))

;; Template
#;
(define (fn-para-proxima-qtd-vida v)
  (... (qtdVida-x v)
       (qtdVida-y v)
       (qtdVida-cont v))
  )

;; proxima-qtd-moedas : qtdMoeda -> qtdMoeda
;; interp. recebe uma quantidade de moedas e retorna quantidade de moedas
; (define (proxima-qtd-moedas m) m)

(define (proxima-qtd-moedas m) 
  (make-qtdMoeda (qtdMoeda-x m) 
                (qtdMoeda-y m)
                (+ (qtdMoeda-cont m) 1)))

;; Template
#;
(define (fn-para-proxima-qtd-moedas m)
  (... (qtdMoeda-x m)
       (qtdMoeda-y m)
       (qtdMoeda-cont m))
  )


;; proxima-qtd-star : qtdStar -> qtdStar
;; interp. recebe uma quantidade de estrela e retorna quantidade de estrela
; (define (proxima-qtd-star s) s)

(define (proxima-qtd-star s)
  (make-qtdStar (qtdStar-x s) 
                (qtdStar-y s)
                (+ (qtdStar-cont s) 1)))

;; Template
#;
(define (fn-para-proxima-qtd-star e)
  (... (qtdStar-x e)
       (qtdStar-y e)
       (qtdStar-cont e))
  )


;; spawn-vida : ListaDeVida -> ListaDeVida
;; interp. cria nova vida no local especificado
;(define (spawn-vida ldv) ldv)

(define (spawn-vida ldv)
   (cons VIDA1  ldv))

;; Template
#;
(define (fn-for-ListaDeVida ldv)
  (cond [(empty? ldv) (...)]                         ;CASO BASE (CONDIÇÃO DE PARADA)
        [else (... (first ldv)                       ;plataforma
                   (fn-for-ldv (rest ldv)))]))       ;RECURSÃO EM CAUDA

;; spawn-estrela : ListaDeEstrela -> ListaDeEstrela
;; interp. cria nova estrela no local especificado
; (define (spawn-estrela lde) lde)

(define (spawn-estrela lde)
   (cons STAR1  lde))

;; Template
#;
(define (fn-for-ListaDeEstrela lde)
  (cond [(empty? lde) (...)]                         ;CASO BASE (CONDIÇÃO DE PARADA)
        [else (... (first lde)                       ;plataforma
                   (fn-for-lde (rest lde)))]))       ;RECURSÃO EM CAUDA

;; spawn-tartaruga : ListaDeTartaruga -> ListaDeTartaruga
;; interp. cria nova tartaruga no local especificado
; (define (spawn-tartaruga ldtat) ldtat)

(define (spawn-tartaruga ldtat)
  (cons TARTARUGA-INICIAL1 (cons TARTARUGA-INICIAL2  ldtat)))

;; Template
#;
(define (fn-for-ListaDeTartaruga ldtat)
  (cond [(empty? ldtat) (...)]                         ;CASO BASE (CONDIÇÃO DE PARADA)
        [else (... (first ldtat)                       ;plataforma
                   (fn-for-ldtat (rest ldtat)))]))     ;RECURSÃO EM CAUDA

;; morreu-tat : Tartaruga -> Tartaruga
;; interp. faz a tartaruga cair quando o mario pular em cima
; (define (morreu-tat tat) tat)

(define (morreu-tat tat) 
  (make-tartaruga (tartaruga-x tat)
                      (+ (tartaruga-y tat) DY-TAT-MORTA)
                      (tartaruga-dx tat)
                      (tartaruga-dy tat)))

;; Template 
#; 
(define (fn-para-morreu-tat tat)
  (...  (tartaruga-x tat)
        (tartaruga-y tat)
        (tartaruga-dx tat)
        (tartaruga-dy tat)))

;; repelir-mario : mario -> mario
;; interp. repele o mario quando toca na tartaruga
; (define (repelir-mario m) m)

(define (repelir-mario m)
  (cond [(< (mario-dx m) 0)
          (make-mario (+ (mario-x m) 200)
                      (mario-dx m)
                      (mario-y m)
                      (mario-dy m))]
        [(> (mario-dx m) 0)
          (make-mario (- (mario-x m) 200) 
                      (mario-dx m)
                      (mario-y m)
                      (mario-dy m))]))


;; Template
#;
(define (fn-para-repelir-mario m)
  (... (mario-x m)
       (mario-dx m)
       (mario-y m)
       (mario-dy m))
  )

;; quica : Mario -> Mario
;; interp. faz o mario quicar se ele pular em cima da tartaruga
; (define (quica m) m)
(define (quica m)
  (make-mario (mario-x m)
              (mario-dx m)
              (mario-y m)
             DY-QUICA))

;; Template
#;
(define (fn-para-quica m)
  (... (mario-x m)
       (mario-dx m)
       (mario-y m)
       (mario-dy m))
  )

;; alguma-tat-chegou-no-final? : ListaTartaruga -> tartaruga | false
;; interp. faz a checagem se a tartaruga chegou no final
; (define (alguma-tat-chegou-no-final? ldtat) #false)

(define (alguma-tat-chegou-no-final? ldtat) 
  (local [
          (define busca (memf (lambda (tat) (tat-chegou-no-final? tat))
                              ldtat))]
    (if (false? busca)
        #false
        (first busca))))

;; Template
#;
(define (fn-for-ListaDeTartaruga ldtat)
  (cond [(empty? ldtat) (...)]                         ;CASO BASE (CONDIÇÃO DE PARADA)
        [else (... (first ldtat)                       ;plataforma
                   (fn-for-ldtat (rest ldtat)))]))     ;RECURSÃO EM CAUDA

;; tat-chegou-no-final? : tartaruga -> tartaruga | false 
;;interp. faz a checagem se alguma tartaruga chegou no final
; (define (tat-chegou-no-final? tat) tat)

(define (tat-chegou-no-final? tat)
  (if (and (tartaruga-encima-chao? tat) 
           (or (> (tartaruga-x tat) (- LIMITE-DIREITO-TARTARUGA (image-width TUBO-DIREITO)))
               (< (tartaruga-x tat) (+ LIMITE-ESQUERDO-TARTARUGA (image-width TUBO-DIREITO))))                    
           )
      tat 
      #false))

;; Template
#; 
(define (fn-para-tat-chegou-no-final? tat)
  (...  (tartaruga-x tat)
        (tartaruga-y tat)
        (tartaruga-dx tat)
        (tartaruga-dy tat)))

;; alguma-estrela-chegou-no-final? : ListaDeEstrela -> estrela | false
;; interp. faz a checagem se a estrela chegou no final
; (define (alguma-estrela-chegou-no-final? lde)  #false)

(define (alguma-estrela-chegou-no-final? lde) 
  (local [
          (define busca (memf (lambda (estrela) (estrela-chegou-no-final? estrela))
                              lde))]
    (if (false? busca)
        #false
        (first busca))))

;; Template
#;
(define (fn-for-alguma-estrela-chegou-no-final? lde)
  (cond [(empty? lde) (...)]                         ;CASO BASE (CONDIÇÃO DE PARADA)
        [else (... (first lde)                       ;plataforma
                   (fn-for-lde (rest lde)))]))       ;RECURSÃO EM CAUDA

;; estrela-chegou-no-final? : estrela -> estrela | false
;;interp. faz a checagem se alguma estrela chegou no final
; (define (estrela-chegou-no-final? e) e)

(define (estrela-chegou-no-final? e)
  (if (and (estrela-encima-chao? e) 
           (>= (estrela-x e) LIMITE-DIREITO-CENARIO)                   
           )
      e 
      #false))

;; Template
#;
(define (fn-para-estrela-chegou-no-final? e)
  (... (qtdStar-x e)
       (qtdStar-y e)
       (qtdStar-cont e))
  )

;; alguma-vida-chegou-no-final? : ListaVida -> vida | false 
;; interp. faz a checagem se a vida chegou no final
; (define (alguma-vida-chegou-no-final? ldv) #false)

(define (alguma-vida-chegou-no-final? ldv) 
  (local [
          (define busca (memf (lambda (vida) (vida-chegou-no-final? vida))
                              ldv))]
    (if (false? busca)
        #false
        (first busca))))

;; Template
#;
(define (fn-for-alguma-vida-chegou-no-final? ldv)
  (cond [(empty? ldv) (...)]                         ;CASO BASE (CONDIÇÃO DE PARADA)
        [else (... (first ldv)                       ;plataforma
                   (fn-for-ldv (rest ldv)))]))       ;RECURSÃO EM CAUDA

;; vida-chegou-no-final? : vida -> vida | false
;;interp. faz a checagem se alguma vida chegou no final
; (define (vida-chegou-no-final? v) v)

(define (vida-chegou-no-final? v)
  (if (and (vida-encima-chao? v) 
           (>= (vida-x v) LIMITE-DIREITO-CENARIO)                   
           )
      v 
      #false))

;; Template
#;
(define (fn-para-proxima-qtd-vida v)
  (... (qtdVida-x v)
       (qtdVida-y v)
       (qtdVida-cont v))
  )


;; pulou-encima-alguma-tartaruga? : mario ListaTartaruga -> tartaruga | false
;; interp. verifica se o mario pulou em cima de alguma tartaruga
; (define (pulou-encima-alguma-tartaruga? m ldtat) #false)

(define (pulou-encima-alguma-tartaruga? m ldtat) 
  (local [
          (define busca (memf (lambda (tat) (pulou-encima? m tat))
                              ldtat))]
    (if (false? busca)
        #false
        (first busca)))) 

;; Template
#;
(define (fn-for-pulou-encima-alguma-tartaruga? m ldtat)
  (cond [(empty? ldtat) (...)]                         ;CASO BASE (CONDIÇÃO DE PARADA)
        [else (... (first ldtat)                       ;plataforma
                   (fn-for-ldtat (rest ldtat)))]))       ;RECURSÃO EM CAUDA


;; pulou-encima? : mario tartaruga -> tartaruga | false
;;interp. faz a checagem se ouve colisao entre o mario e uma tartaruga
; (define (pulou-encima? m tat) #false)

(define (pulou-encima? m tat)
  (if (and
       (colisao-mario-tartaruga? m tat)
       (<= (+ (mario-y m) MEIO-H-MARIO) (- (tartaruga-y tat) (/ MEIO-H-TARTARUGA 8))))
       (morreu-tat tat) 
       #false)) 

;; Template
#; 
(define (fn-para-colisao-mario-tartaruga? m tat)
  (...  (tartaruga-x tat)
        (tartaruga-y tat)
        (tartaruga-dx tat)
        (tartaruga-dy tat))
  (...  (mario-x m)
        (mario-dx m)
        (mario-y m)
        (mario-dy m)))

;; proximas-estrelas : ListaDeEstrelas -> ListaDeEstrelas
;; chama lista com as proximas estrelas
; (define (proximas-estrelas lde) lde)

(define (proximas-estrelas lde)
  (map proxima-estrela lde))

;; Template
#;
(define (fn-for-proximas-estrelas lde)
  (cond [(empty? lde) (...)]                         ;CASO BASE (CONDIÇÃO DE PARADA)
        [else (... (first lde)                       ;plataforma
                   (fn-for-lde (rest lde)))]))       ;RECURSÃO EM CAUDA

;; proxima-estrela : estrela -> estrela
;; interp. recebe uma estrela e retorna outra estrela
; (define (proxima-estrela s) s)

(define (proxima-estrela s)   
  (cond  [(and (estrela-encima-plataforma? s)                 
               (<= (estrela-x s) LIMITE-ESQUERDO-STAR))                 
          (make-estrela (+ LIMITE-DIREITO-STAR (estrela-dx s))     
                              (estrela-dx s)
                              (estrela-y s)
                              (estrela-dy s) 
                              )]
         
         [(and (estrela-encima-plataforma? s)                     
               (>= (estrela-x s) LIMITE-DIREITO-STAR)  
               )
          (make-estrela (+ LIMITE-ESQUERDO-STAR (estrela-dx s))   
                              (estrela-dx s)
                              (estrela-y s)
                              (estrela-dy s)  
                              )]
         
         [(and (estrela-encima-chao? s)
               (<= (estrela-x s) LIMITE-DIREITO-CENARIO)                                                                     
               )
          (make-estrela (+ (estrela-x s) (estrela-dx s))      
                     (estrela-dx s)
                     (estrela-y s)
                     (estrela-dy s)
                     )]  
         
         [(and (estrela-encima-plataforma? s)
               (>= (estrela-x s) LIMITE-ESQUERDO-CENARIO)
               (<= (estrela-x s) LIMITE-DIREITO-CENARIO)) 
          (make-estrela (+ (estrela-x s) (estrela-dx s))     
                              (estrela-dx s)
                              (- (estrela-y s) 10)
                              (estrela-dy s)
                              )]
         
         [else (make-estrela (+ (estrela-x s) (estrela-dx s)) 
                                   (estrela-dx s) 
                                   (+ (estrela-y s) 3)
                                   (estrela-dy s)
                                   )]
         ))

;; Template
#;
(define (fn-para-proxima-estrela e)
  (... (qtdStar-x e)
       (qtdStar-y e)
       (qtdStar-cont e))
  )

;; estrela-encima-chao? : Estrela -> Boolean
;; interp. verifica se a estrela colidiu com o chao
; (define (estrela-encima-chao? s) #true)

(define (estrela-encima-chao? s)  
  (if (>= (estrela-y s) Y-CHAO-VIDA) 
      #true
      #false  
      ))

;; Template
#;
(define (fn-para-estrela-encima-chao? e)
  (... (qtdStar-x e)
       (qtdStar-y e)
       (qtdStar-cont e))
  )

;; estrela-encima-plataforma? : Estrela -> Boolean
;; interp. verifica se ouve colisao entre a estrela e as plataformas
; (define (estrela-encima-plataforma? s) #false)

(define (estrela-encima-plataforma? s) 
  (if (or (and (>= (estrela-x s) X1-PLATAFORMA1)
               (<= (estrela-x s) X2-PLATAFORMA1)
               (<= (distancia (estrela-y s) (plataforma-y PLATAFORMA-ONE))
                   (+ (/ (image-height VIDA-MARIO) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               (<= (estrela-y s) (plataforma-y PLATAFORMA-ONE))
               )
          
          (and (>= (estrela-x s) X1-PLATAFORMA2) 
               (<= (estrela-x s) X2-PLATAFORMA2) 
               (<= (distancia (estrela-y s) (plataforma-y PLATAFORMA-TWO))
                   (+ (/ (image-height VIDA-MARIO) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               (<= (estrela-y s) (plataforma-y PLATAFORMA-TWO))
               )
          
          (and (>= (estrela-x s) X1-PLATAFORMA3)
               (<= (estrela-x s) X2-PLATAFORMA3)
               (<= (distancia (estrela-y s) (plataforma-y PLATAFORMA-THREE))
                   (+ (/ (image-height VIDA-MARIO) 2) (/ (image-height IMG-PLATAFORMA) 2))) 
               (<= (estrela-y s) (plataforma-y PLATAFORMA-THREE))
               
               )
          
          (and (>= (estrela-x s) X1-PLATAFORMA4)
               (<= (estrela-x s) X2-PLATAFORMA4) 
               (<= (distancia (estrela-y s) (plataforma-y PLATAFORMA-FOUR))
                   (+ (/ (image-height VIDA-MARIO) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               
               (<= (estrela-y s) (plataforma-y PLATAFORMA-FOUR))
               )
          
          (and (>= (estrela-x s) X1-PLATAFORMA5)
               (<= (estrela-x s) X2-PLATAFORMA5)
               (<= (distancia (estrela-y s) (plataforma-y PLATAFORMA-FIVE))
                   (+ (/ (image-height VIDA-MARIO) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               (<= (estrela-y s) (plataforma-y PLATAFORMA-FIVE))
               )
          
          (and (>= (estrela-x s) X1-PLATAFORMA6)
               (<= (estrela-x s) X2-PLATAFORMA6)
               (<= (distancia (estrela-y s) (plataforma-y PLATAFORMA-SIX))
                   (+ (/ (image-height VIDA-MARIO) 2) (/ (image-height IMG-PLATAFORMA) 2)))                    
               (<= (estrela-y s) (plataforma-y PLATAFORMA-SIX))
               )
          
          (and (>= (estrela-x s) X1-PLATAFORMA7) 
               (<= (estrela-x s) X2-PLATAFORMA7)
               (<= (distancia (estrela-y s) (plataforma-y PLATAFORMA-SEVEN))
                   (+ (/ (image-height VIDA-MARIO) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               (<= (estrela-y s) (plataforma-y PLATAFORMA-SEVEN))
               )
          
          (and (>= (estrela-x s) X1-PLATAFORMA8)
               (<= (estrela-x s) X2-PLATAFORMA8)
               (<= (distancia (estrela-y s) (plataforma-y PLATAFORMA-EIGHT))
                   (+ (/ (image-height VIDA-MARIO) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               (<= (estrela-y s) (plataforma-y PLATAFORMA-EIGHT))
               ))                           
      #true 
      #false))

;; Template
#;
(define (fn-para-estrela-encima-plataforma? e)
  (... (qtdStar-x e)
       (qtdStar-y e)
       (qtdStar-cont e))
  )

;; proximas-vidas : ListaDeVida -> ListaDeVidas
;; interp. chama lista com as proximas vidas
; (define (proximas-vidas ldv) ldv)

(define (proximas-vidas ldv)
  (map proxima-vida ldv))

;; Template
#;
(define (fn-for-proximas-vidas ldv)
  (cond [(empty? ldv) (...)]                         ;CASO BASE (CONDIÇÃO DE PARADA)
        [else (... (first ldv)                       ;plataforma
                   (fn-for-ldv (rest ldv)))]))       ;RECURSÃO EM CAUDA

;; proxima-vida : vida -> vida
;; interp. recebe uma vida e retorna outra vida
; (define (proxima-vida v) v)

(define (proxima-vida v)   
  (cond  [(and (vida-encima-plataforma? v)                 
               (<= (vida-x v) LIMITE-ESQUERDO-VIDA))                 
          (make-vida (+ LIMITE-DIREITO-VIDA (vida-dx v))     
                     (vida-dx v)
                     (vida-y v)
                     (vida-dy v) 
                     )]
         
         [(and (vida-encima-plataforma? v)                     
               (>= (vida-x v) LIMITE-DIREITO-VIDA)  
               )
          (make-vida (+ LIMITE-ESQUERDO-VIDA (vida-dx v))   
                     (vida-dx v)
                     (- (vida-y v) 5)
                     (vida-dy v)  
                     )]
         
         [(and (vida-encima-chao? v)
               (<= (vida-x v) LIMITE-DIREITO-CENARIO)                                                                     
               )
          (make-vida (+ (vida-x v) (vida-dx v))      
                     (vida-dx v)
                     (vida-y v)
                     (vida-dy v)
                     )]  
         
         [(and (vida-encima-plataforma? v)
               (>= (vida-x v) LIMITE-ESQUERDO-CENARIO)
               (<= (vida-x v) LIMITE-DIREITO-CENARIO))
          (make-vida (+ (vida-x v) (vida-dx v))     
                     (vida-dx v)
                     (vida-y v)
                     (vida-dy v)
                     )]
         
         [else (make-vida (+ (vida-x v) (vida-dx v)) 
                          (vida-dx v) 
                          (+ (vida-y v) 3)
                          (vida-dy v)
                          )]
         ))  

;; Template
#; 
(define (fn-para-proxima-vida v)
  (...  (vida-x v)
        (vida-dx v)
        (vida-y v)
        (vida-dy v)))

;; vida-encima-chao? : Vida -> Boolean
;; interp. verifica se a vida esta em cima do chao
; (define (vida-encima-chao? v) #true)

(define (vida-encima-chao? v)  
  (if (>= (vida-y v) Y-CHAO-VIDA) 
      #true
      #false  
      ))

;; Template
#; 
(define (fn-para-proxima-vida v)
  (...  (vida-x v)
        (vida-dx v)
        (vida-y v)
        (vida-dy v)))

;; proximas-moedas : ListaDeMoedas -> ListaDeMoedas
;; chama lista com as proximas Moedas
; (define (proximas-moedas ldm) empty)

(define (proximas-moedas ldm)
  (map proxima-moeda ldm))

;; Template
#;
(define (fn-para-moeda m)
  (... (moeda-x m)
       (moeda-y m)
       (moeda-cont m)) 
  )

;; proxima-moeda : Moeda -> Moeda
;; interp. recebe uma Moeda e retorna outra Moeda
; (define (proxima-moeda m) m)

(define (proxima-moeda m)
  (cond [(< (moeda-cont m) 3)
         (make-moeda (moeda-x m)
                     (moeda-y m)
                     (+ (moeda-cont m) 1))]
        [else (make-moeda (moeda-x m)
                     (moeda-y m)
                     (- (moeda-cont m) 3))]))

;; Template
#;
(define (fn-para-moeda m)
  (... (moeda-x m)
       (moeda-y m)
       (moeda-cont m)) 
  )

;; proximas-tartarugas : ListaDeTartarugas -> ListaDeTartarugas
;; chama lista com as proximas tartarugas
; (define (proximas-tartarugas ldtat) ldtat)

(define (proximas-tartarugas ldtat)
  (map proxima-tartaruga ldtat)) 

;; Template
#;
(define (fn-for-proximas-tartarugas ldtat)
  (cond [(empty? ldtat) (...)]                         ;CASO BASE (CONDIÇÃO DE PARADA)
        [else (... (first ldtat)                       ;plataforma
                   (fn-for-ldtat (rest ldtat)))]))     ;RECURSÃO EM CAUDA

;; proxima-tartaruga : tartaruga -> tartaruga
;; interp. recebe uma tartaruga e retorna outra tartaruga
; (define (proxima-tartaruga tat) tat)

(define (proxima-tartaruga tat) 
  (cond  [(and (tartaruga-encima-plataforma? tat)                 
               (<= (tartaruga-x tat) LIMITE-ESQUERDO-TARTARUGA))                 
          (make-tartaruga (+ LIMITE-DIREITO-TARTARUGA (tartaruga-dx tat))     
                              (tartaruga-y tat)
                              (tartaruga-dx tat)
                              (tartaruga-dy tat)
                              )]
         [(and (tartaruga-encima-plataforma? tat)                     
               (>= (tartaruga-x tat) LIMITE-DIREITO-TARTARUGA)  
               )
          (make-tartaruga (+ 20 (tartaruga-dx tat))     
                              (tartaruga-y tat)
                              (tartaruga-dx tat)
                              (tartaruga-dy tat)  
                              )] 
         [(and (tartaruga-encima-chao? tat)
               (or (and (< (tartaruga-x tat) (- LIMITE-DIREITO-TARTARUGA (image-width TUBO-DIREITO)))
                        (>= (tartaruga-x tat) (/ LARGURA 2)))
                   (and (> (tartaruga-x tat) (+ LIMITE-ESQUERDO-TARTARUGA (image-width TUBO-DIREITO)))
                        (<= (tartaruga-x tat) (/ LARGURA 2))))                     
               )
          (make-tartaruga (+ (tartaruga-x tat) (tartaruga-dx tat))     
                              (tartaruga-y tat)
                              (tartaruga-dx tat)
                              (tartaruga-dy tat)
                              )]
         [(and (tartaruga-encima-plataforma? tat)
               (>= (tartaruga-x tat) LIMITE-ESQUERDO) 
               (<= (tartaruga-x tat) LIMITE-DIREITO))
          (make-tartaruga (+ (tartaruga-x tat) (tartaruga-dx tat))     
                              (tartaruga-y tat)
                              (tartaruga-dx tat)
                              (tartaruga-dy tat)
                              )]
         [else (make-tartaruga (+ (tartaruga-x tat) (tartaruga-dx tat))
                                   (+ (tartaruga-y tat) 3) 
                                   (tartaruga-dx tat)
                                   (tartaruga-dy tat)
                                   )]
         ))

;; Template
#; 
(define (fn-para-tartaruga p)
  (...  (tartaruga-x p)
        (tartaruga-y p)
        (tartaruga-dx p)
        (tartaruga-dy p)))

;; tartaruga-encima-chao? : Tartaruga -> Boolean
;; interp. verifica se a trataruga esta em cima do chao
; (define (tartaruga-encima-chao? t) #false)

(define (tartaruga-encima-chao? t)   
  (if (>= (tartaruga-y t) Y-CHAO-TARTARUGA) 
      #true
      #false  
      ))

;; Template
#; 
(define (fn-para-tartaruga p)
  (...  (tartaruga-x p)
        (tartaruga-y p)
        (tartaruga-dx p)
        (tartaruga-dy p)))

;; vida-encima-plataforma? : Vida -> Boolean
;; interp. verifica se a vida esta em cima da plataforma
; (define (vida-encima-plataforma? v) #true)

(define (vida-encima-plataforma? v) 
  (if (or (and (>= (vida-x v) X1-PLATAFORMA1)
               (<= (vida-x v) X2-PLATAFORMA1)
               (<= (distancia (vida-y v) (plataforma-y PLATAFORMA-ONE))
                   (+ (/ (image-height VIDA-MARIO) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               (<= (vida-y v) (plataforma-y PLATAFORMA-ONE))
               )
          
          (and (>= (vida-x v) X1-PLATAFORMA2) 
               (<= (vida-x v) X2-PLATAFORMA2) 
               (<= (distancia (vida-y v) (plataforma-y PLATAFORMA-TWO))
                   (+ (/ (image-height VIDA-MARIO) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               (<= (vida-y v) (plataforma-y PLATAFORMA-TWO))
               )
          
          (and (>= (vida-x v) X1-PLATAFORMA3)
               (<= (vida-x v) X2-PLATAFORMA3)
               (<= (distancia (vida-y v) (plataforma-y PLATAFORMA-THREE))
                   (+ (/ (image-height VIDA-MARIO) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               (<= (vida-y v) (plataforma-y PLATAFORMA-THREE))
               
               )
          
          (and (>= (vida-x v) X1-PLATAFORMA4)
               (<= (vida-x v) X2-PLATAFORMA4) 
               (<= (distancia (vida-y v) (plataforma-y PLATAFORMA-FOUR))
                   (+ (/ (image-height VIDA-MARIO) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               
               (<= (vida-y v) (plataforma-y PLATAFORMA-FOUR))
               )
          
          (and (>= (vida-x v) X1-PLATAFORMA5)
               (<= (vida-x v) X2-PLATAFORMA5)
               (<= (distancia (vida-y v) (plataforma-y PLATAFORMA-FIVE))
                   (+ (/ (image-height VIDA-MARIO) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               (<= (vida-y v) (plataforma-y PLATAFORMA-FIVE))
               )
          
          (and (>= (vida-x v) X1-PLATAFORMA6)
               (<= (vida-x v) X2-PLATAFORMA6)
               (<= (distancia (vida-y v) (plataforma-y PLATAFORMA-SIX))
                   (+ (/ (image-height VIDA-MARIO) 2) (/ (image-height IMG-PLATAFORMA) 2)))                    
               (<= (vida-y v) (plataforma-y PLATAFORMA-SIX))
               )
          
          (and (>= (vida-x v) X1-PLATAFORMA7) 
               (<= (vida-x v) X2-PLATAFORMA7)
               (<= (distancia (vida-y v) (plataforma-y PLATAFORMA-SEVEN))
                   (+ (/ (image-height VIDA-MARIO) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               (<= (vida-y v) (plataforma-y PLATAFORMA-SEVEN))
               )
          
          (and (>= (vida-x v) X1-PLATAFORMA8)
               (<= (vida-x v) X2-PLATAFORMA8)
               (<= (distancia (vida-y v) (plataforma-y PLATAFORMA-EIGHT))
                   (+ (/ (image-height VIDA-MARIO) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               (<= (vida-y v) (plataforma-y PLATAFORMA-EIGHT))
               ))                           
      #true 
      #false))

;; Template
#; 
(define (fn-para-proxima-vida v)
  (...  (vida-x v)
        (vida-dx v)
        (vida-y v)
        (vida-dy v)))

;; tartaruga-encima-plataforma : Tartaruga -> Boolean
;; interp. verifica se a tartaruga esta em cima da plataforma
; (define (tartaruga-encima-plataforma? t) #true)

(define (tartaruga-encima-plataforma? t) 
  (if (or (and (>= (tartaruga-x t) X1-PLATAFORMA1)
               (<= (tartaruga-x t) X2-PLATAFORMA1)
               (<= (distancia (tartaruga-y t) (plataforma-y PLATAFORMA-ONE))
                   (+ (/ (image-height IMG-TARTARUGA-INDO0) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               (<= (tartaruga-y t) (plataforma-y PLATAFORMA-ONE))
               )
          
          (and (>= (tartaruga-x t) X1-PLATAFORMA2) 
               (<= (tartaruga-x t) X2-PLATAFORMA2) 
               (<= (distancia (tartaruga-y t) (plataforma-y PLATAFORMA-TWO))
                   (+ (/ (image-height IMG-TARTARUGA-INDO0) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               (<= (tartaruga-y t) (plataforma-y PLATAFORMA-TWO))
               )
          
          (and (>= (tartaruga-x t) X1-PLATAFORMA3)
               (<= (tartaruga-x t) X2-PLATAFORMA3)
               (<= (distancia (tartaruga-y t) (plataforma-y PLATAFORMA-THREE))
                   (+ (/ (image-height IMG-TARTARUGA-INDO0) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               (<= (tartaruga-y t) (plataforma-y PLATAFORMA-THREE))
               
               )
          
          (and (>= (tartaruga-x t) X1-PLATAFORMA4)
               (<= (tartaruga-x t) X2-PLATAFORMA4) 
               (<= (distancia (tartaruga-y t) (plataforma-y PLATAFORMA-FOUR)) 
                   (+ (/ (image-height IMG-TARTARUGA-INDO0) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               
               (<= (tartaruga-y t) (plataforma-y PLATAFORMA-FOUR))
               )
          
          (and (>= (tartaruga-x t) X1-PLATAFORMA5) 
               (<= (tartaruga-x t) X2-PLATAFORMA5)
               (<= (distancia (tartaruga-y t) (plataforma-y PLATAFORMA-FIVE))
                   (+ (/ (image-height IMG-TARTARUGA-INDO0) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               (<= (tartaruga-y t) (plataforma-y PLATAFORMA-FIVE))
               )
          
          (and (>= (tartaruga-x t) X1-PLATAFORMA6)
               (<= (tartaruga-x t) X2-PLATAFORMA6)
               (<= (distancia (tartaruga-y t) (plataforma-y PLATAFORMA-SIX))
                   (+ (/ (image-height IMG-TARTARUGA-INDO0) 2) (/ (image-height IMG-PLATAFORMA) 2)))                    
               (<= (tartaruga-y t) (plataforma-y PLATAFORMA-SIX))
               )
          
          (and (>= (tartaruga-x t) X1-PLATAFORMA7) 
               (<= (tartaruga-x t) X2-PLATAFORMA7)
               (<= (distancia (tartaruga-y t) (plataforma-y PLATAFORMA-SEVEN))
                   (+ (/ (image-height IMG-TARTARUGA-INDO0) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               (<= (tartaruga-y t) (plataforma-y PLATAFORMA-SEVEN))
               )
          
          (and (>= (tartaruga-x t) X1-PLATAFORMA8)
               (<= (tartaruga-x t) X2-PLATAFORMA8)
               (<= (distancia (tartaruga-y t) (plataforma-y PLATAFORMA-EIGHT))
                   (+ (/ (image-height IMG-TARTARUGA-INDO0) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               (<= (tartaruga-y t) (plataforma-y PLATAFORMA-EIGHT))
               ))                           
      #true 
      #false))


;; Template
#; 
(define (fn-para-tartaruga-encima-plataforma? t)
  (...  (tartaruga-x t)
        (tartaruga-dx t)
        (tartaruga-y t)
        (tartaruga-dy t)))

;; colisao-algum-mario-moeda? : mario ListaDeMoeda -> Moeda | false
;; interp. verifica se o mario colidiu com alguma moeda
; (define (colisao-algum-mario-moeda? m ldm) m)

(define (colisao-algum-mario-moeda? m ldm) 
  (local [
          (define busca (memf (lambda (moeda) (colisao-mario-moeda? m moeda))
                              ldm))]
    (if (false? busca)
        #false
        (first busca))))

;; Template
#;
(define (fn-for-colisao-algum-mario-moeda? m ldtat)
  (cond [(empty? ldtat) (...)]                         ;CASO BASE (CONDIÇÃO DE PARADA)
        [else (... (first ldtat)                       ;plataforma
                   (fn-for-ldtat (rest ldtat)))]))     ;RECURSÃO EM CAUDA

;; colisao-mario-moeda? : mario moeda -> moeda | false 
;; verifica se o mario e a moeda colidiu
; (define (colisao-mario-moeda? m moeda) moeda)

(define (colisao-mario-moeda? m moeda)
  (if (<= (distancia-euclidiana (mario-x m) 
                                (mario-y m)
                                (moeda-x moeda)  
                                (moeda-y moeda))
             MEIO-H-MARIO)
      moeda
      #false))

;; Template
#;
(define (fn-para-colisao-mario-moeda? m moeda)
  (...  (mario-x m)
        (mario-y m)
        (moeda-x moeda)
        (moeda-y moeda)))

;; colisao-algum-mario-estrela? : mario ListaDeEstrela -> estrela | false
;; interp. verifica se o mario colidiu com alguma estrela
; (define (colisao-algum-mario-estrela? m lde) #false)

(define (colisao-algum-mario-estrela? m lde) 
  (local [
          (define busca (memf (lambda (e) (colisao-mario-estrela? m e))
                              lde))]
    (if (false? busca)
        #false
        (first busca))))


;; Template
#;
(define (fn-for-colisao-algum-mario-estrela? m lde)
  (cond [(empty? ldtat) (...)]                         ;CASO BASE (CONDIÇÃO DE PARADA)
        [else (... (first lde)                       ;plataforma
                   (fn-for-lde (rest lde)))]))     ;RECURSÃO EM CAUDA

;; colisao-mario-estrela? : mario estrela -> estrela | false 
;; interp. verifica se o mario e a estrela colidiram
; (define (colisao-mario-estrela? m e) #false)

(define (colisao-mario-estrela? m e)
  (if (<= (distancia-euclidiana (mario-x m) 
                                (mario-y m)
                                (estrela-x e)  
                                (estrela-y e))
          (+ MEIO-H-MARIO MEIO-H-STAR))
      e
      #false))

;; Template
#;
(define (fn-para-colisao-mario-estrela? m e)
  (...  (mario-x m)
        (mario-y m)
        (estrela-x e)
        (estrela-y e)))

;; colisao-algum-mario-vida? : mario ListaVida -> vida | false
;; interp. verifica se ouve colisao do mario com alguma vida
; (define (colisao-algum-mario-vida? m ldv) #false)

(define (colisao-algum-mario-vida? m ldv) 
  (local [
          (define busca (memf (lambda (v) (colisao-mario-vida? m v))
                              ldv))]
    (if (false? busca)
        #false
        (first busca))))

;; Template
#;
(define (fn-for-colisao-algum-mario-vida? m ldv)
  (cond [(empty? ldv) (...)]                         ;CASO BASE (CONDIÇÃO DE PARADA)
        [else (... (first ldv)                       ;plataforma
                   (fn-for-ldv (rest ldv)))]))     ;RECURSÃO EM CAUDA

;; colisao-mario-vida? : mario vida -> vida | false 
;; interp. verifica se o mario e a vida colidiram
; (define (colisao-mario-vida? m v) #false)

(define (colisao-mario-vida? m v)
  (if (< (distancia-euclidiana (mario-x m) 
                               (mario-y m)
                               (vida-x v)  
                               (vida-y v))
          (+ MEIO-W-MARIO MEIO-H-VIDA))
      v
      #false))

;; Template
#;
(define (fn-para-colisao-mario-vida? m v)
  (...  (mario-x m)
        (mario-y m)
        (vida-x v)
        (vida-y v)))

;; colisao-algum-mario-tartaruga? : mario tartarugas -> Boolean
;; interp. verifica se o mario colidiu com alguma tartaruga
; (define (colisao-algum-mario-tartaruga? m ldtat) #true)
(define (colisao-algum-mario-tartaruga? m ldtat)
  (ormap (lambda (tat) (colisao-mario-tartaruga? m tat)) ldtat))


;; Template
#;
(define (fn-for-colisao-algum-mario-tartaruga? m ldt)
  (cond [(empty? ldt) (...)]                         ;CASO BASE (CONDIÇÃO DE PARADA)
        [else (... (first ldt)                       ;plataforma
                   (fn-for-ldt (rest ldt)))]))     ;RECURSÃO EM CAUDA

;; colisao-mario-tartaruga? : mario tartaruga -> Boolean  
;; verifica se o mario e a tartaruga colidiram
;(define (colisao-mario-tartaruga? m tat) #false)

(define (colisao-mario-tartaruga? m tat)
  (if (<= (distancia-euclidiana (mario-x m)  
                                (mario-y m)
                                (tartaruga-x tat)  
                                (tartaruga-y tat))
          (+ MEIO-H-MARIO MEIO-H-TARTARUGA))
      #true
      #false))

;; Template 
#;
(define (fn-para-colisao-mario-tartaruga? m t)
  (...  (mario-x m)
        (mario-y m)
        (tartaruga-x t)
        (tartaruga-y t)))

;; distancia-euclidiana : Numero+ Numero+ Numero+ Numero+ -> Numero+
;; interp. calcula a distancia euclidiana
; (define (distancia-euclidiana x1 y1 x2 y2) x1)

(define (distancia-euclidiana x1 y1 x2 y2)
  (sqrt (+ (sqr (- x2 x1)) (sqr (- y2 y1)))))

;; Template
#;
(define (fn-para-distancia-euclidiana x1 y1 x2 y2)
  (...  x1 y1 x2 y2))


;; distancia : Numero+ Numero+ Numero+ Numero+ -> Numero
;; interp. calcula distancia entre os y dos personagens do jogo
; (define (distancia y1 y2) 10)

(define (distancia y1 y2)
  (- y2 y1) )

;; Template
#;
(define (fn-para-distancia y1 y2)
  (...  y1 y2))

;; proximo-mario : mario -> mario
;; interp. recebe um mario na posicao x e retorna um mario com posição
;; x atualizada com o dx
;(define (proximo-mario m) m) 

(define (proximo-mario m)
  (cond
    [(> (mario-x m) LIMITE-DIREITO)
     (make-mario LIMITE-DIREITO (- (mario-dx m))
                 (+ (mario-y m) (mario-dy m)) (mario-dy m) )]
    [(< (mario-x m) LIMITE-ESQUERDO)
     (make-mario LIMITE-ESQUERDO (- (mario-dx m))
                 (+ (mario-y m) (mario-dy m)) (mario-dy m) )] 
    [else 
     (cai m)])) 


;; Template
#;
(define (fn-para-proximo-mario m)
  (...  (mario-x m)
        (mario-dx m)
        (mario-y m)
        (mario-dy m)))

;; cai: mario -> mario
;; interp. recebe um mario e retorna o mario ate chegar ao chao
; (define (cai m) m)

(define (cai m) 
  (make-mario (+ (mario-x m) (round (mario-dx m)))
              (mario-dx m)
              (cond [ (encima-chao? m)
                      Y-CHAO-MARIO]             
                    
                    [(encima-plataforma1? m)
                     (- (- (plataforma-y PLATAFORMA-ONE) MEIO-H-PLATAFORMA) MEIO-H-MARIO)]
                    
                    [(encima-plataforma2? m) 
                     (- (- (plataforma-y PLATAFORMA-TWO) MEIO-H-PLATAFORMA) MEIO-H-MARIO)]
                    
                    [(encima-plataforma3? m)
                     (- (- (plataforma-y PLATAFORMA-THREE) MEIO-H-PLATAFORMA) MEIO-H-MARIO)]
                    
                    
                    [(encima-plataforma4? m) 
                     (- (- (plataforma-y PLATAFORMA-FOUR) MEIO-H-PLATAFORMA) MEIO-H-MARIO)]
                    
                    
                    [(encima-plataforma5? m)
                     (- (- (plataforma-y PLATAFORMA-FIVE) MEIO-H-PLATAFORMA) MEIO-H-MARIO)]
                    
                    
                    [(encima-plataforma6? m)
                     (- (- (plataforma-y PLATAFORMA-SIX)  MEIO-H-PLATAFORMA) MEIO-H-MARIO)]
                    
                    [(encima-plataforma7? m) 
                     (- (- (plataforma-y PLATAFORMA-SEVEN) MEIO-H-PLATAFORMA) MEIO-H-MARIO)]
                    
                    
                    [(encima-plataforma8? m)
                     (- (- (plataforma-y PLATAFORMA-EIGHT) MEIO-H-PLATAFORMA) MEIO-H-MARIO)]
                    
                    [else 
                     (+ (mario-y m)(mario-dy m))])  
              (cond [(encima-plataforma? m) 
                     0]
                    [(encima-chao? m) 
                     0]
                    [(enbaixo-plataforma? m)
                     5]
                    
                    [else (+ (mario-dy m) (/ 107 100))]))) 
 
;; Template
#;
(define (fn-para-proximo-mario m)
  (...  (mario-x m)
        (mario-dx m)
        (mario-y m)
        (mario-dy m)))
 
;; encima-chao? Numero -> Boolean
;; interp. faz a checagem se o mario esta em cima do chao
; (define (encima-chao? m) #true)
(define (encima-chao? m) 
  (if (>= (mario-y m) Y-CHAO-MARIO) 
      #true
      #false  
      ))


;; Template
#;
(define (fn-para-proximo-mario m)
  (...  (mario-x m)
        (mario-dx m)
        (mario-y m)
        (mario-dy m)))

;; encima-plataforma1? : Mario -> Boolean
;; interp. verifica se o mario colidiu com essa plataforma
(define (encima-plataforma1? m)
  (and (>= (mario-x m) X1-PLATAFORMA1)
       (<= (mario-x m) X2-PLATAFORMA1)
       (<= (distancia (mario-y m) (plataforma-y PLATAFORMA-ONE))
           (+ (/ (image-height IMG-MARIO-INDO0) 2) (/ (image-height IMG-PLATAFORMA) 2)))                    
       (<= (mario-y m) (plataforma-y PLATAFORMA-ONE))
       ))

;; Template
#;
(define (fn-para-proximo-mario m)
  (...  (mario-x m)
        (mario-dx m)
        (mario-y m)
        (mario-dy m)))

;; encima-plataforma2? : Mario -> Boolean
;; interp. verifica se o mario colidiu com essa plataforma
(define (encima-plataforma2? m)
  (and (>= (mario-x m) X1-PLATAFORMA2)
       (<= (mario-x m) X2-PLATAFORMA2)
       (<= (distancia (mario-y m) (plataforma-y PLATAFORMA-TWO))
           (+ (/ (image-height IMG-MARIO-INDO0) 2) (/ (image-height IMG-PLATAFORMA) 2)))                    
       (<= (mario-y m) (plataforma-y PLATAFORMA-TWO))
       ))

;; Template
#;
(define (fn-para-proximo-mario m)
  (...  (mario-x m)
        (mario-dx m)
        (mario-y m)
        (mario-dy m)))

;; encima-plataforma3? : Mario -> Boolean
;; interp. verifica se o mario colidiu com essa plataforma
(define (encima-plataforma3? m)
  (and (>= (mario-x m) X1-PLATAFORMA3)
       (<= (mario-x m) X2-PLATAFORMA3)
       (<= (distancia (mario-y m) (plataforma-y PLATAFORMA-THREE))
           (+ (/ (image-height IMG-MARIO-INDO0) 2) (/ (image-height IMG-PLATAFORMA) 2)))                    
       (<= (mario-y m) (plataforma-y PLATAFORMA-THREE))
       ))

;; Template
#;
(define (fn-para-proximo-mario m)
  (...  (mario-x m)
        (mario-dx m)
        (mario-y m)
        (mario-dy m)))

;; encima-plataforma4? : Mario -> Boolean
;; interp. verifica se o mario colidiu com essa plataforma
(define (encima-plataforma4? m)
  (and (>= (mario-x m) X1-PLATAFORMA4)
       (<= (mario-x m) X2-PLATAFORMA4)
       (<= (distancia (mario-y m) (plataforma-y PLATAFORMA-FOUR))
           (+ (/ (image-height IMG-MARIO-INDO0) 2) (/ (image-height IMG-PLATAFORMA) 2)))                    
       (<= (mario-y m) (plataforma-y PLATAFORMA-FOUR))
       ))

;; Template
#;
(define (fn-para-proximo-mario m)
  (...  (mario-x m)
        (mario-dx m)
        (mario-y m)
        (mario-dy m)))

;; encima-plataforma5? : Mario -> Boolean
;; interp. verifica se o mario colidiu com essa plataforma
(define (encima-plataforma5? m)
  (and (>= (mario-x m) X1-PLATAFORMA5)
       (<= (mario-x m) X2-PLATAFORMA5)
       (<= (distancia (mario-y m) (plataforma-y PLATAFORMA-FIVE))
           (+ (/ (image-height IMG-MARIO-INDO0) 2) (/ (image-height IMG-PLATAFORMA) 2)))                    
       (<= (mario-y m) (plataforma-y PLATAFORMA-FIVE))
       ))

;; Template
#;
(define (fn-para-proximo-mario m)
  (...  (mario-x m)
        (mario-dx m)
        (mario-y m)
        (mario-dy m)))

;; encima-plataforma6? : Mario -> Boolean
;; interp. verifica se o mario colidiu com essa plataforma
(define (encima-plataforma6? m)
  (and (>= (mario-x m) X1-PLATAFORMA6)
       (<= (mario-x m) X2-PLATAFORMA6)
       (<= (distancia (mario-y m) (plataforma-y PLATAFORMA-SIX))
           (+ (/ (image-height IMG-MARIO-INDO0) 2) (/ (image-height IMG-PLATAFORMA) 2)))                    
       (<= (mario-y m) (plataforma-y PLATAFORMA-SIX))
       ))

;; Template
#;
(define (fn-para-proximo-mario m)
  (...  (mario-x m)
        (mario-dx m)
        (mario-y m)
        (mario-dy m)))

;; encima-plataforma7? : Mario -> Boolean
;; interp. verifica se o mario colidiu com essa plataforma
(define (encima-plataforma7? m)
  (and (>= (mario-x m) X1-PLATAFORMA7)
       (<= (mario-x m) X2-PLATAFORMA7)
       (<= (distancia (mario-y m) (plataforma-y PLATAFORMA-SEVEN))
           (+ (/ (image-height IMG-MARIO-INDO0) 2) (/ (image-height IMG-PLATAFORMA) 2)))
       (<= (mario-y m) (plataforma-y PLATAFORMA-SEVEN))
       ))

;; Template
#;
(define (fn-para-proximo-mario m)
  (...  (mario-x m)
        (mario-dx m)
        (mario-y m)
        (mario-dy m)))

;; encima-plataforma8? : Mario -> Boolean
;; interp. verifica se o mario colidiu com essa plataforma
(define (encima-plataforma8? m)
  (and (>= (mario-x m) X1-PLATAFORMA8)
       (<= (mario-x m) X2-PLATAFORMA8)
       (<= (distancia (mario-y m) (plataforma-y PLATAFORMA-EIGHT))
           (+ (/ (image-height IMG-MARIO-INDO0) 2) (/ (image-height IMG-PLATAFORMA) 2)))                    
       (<= (mario-y m) (plataforma-y PLATAFORMA-EIGHT))
       ))

;; Template
#;
(define (fn-para-proximo-mario m)
  (...  (mario-x m)
        (mario-dx m)
        (mario-y m)
        (mario-dy m)))

;; encima-plataforma? : Mario -> Boolean
;; interp. verifica se o mario colidiu com alguma plataforma
; (define (encima-plataforma? m) #false)

(define (encima-plataforma? m) 
  (if (or (and (>= (mario-x m) X1-PLATAFORMA1)
               (<= (mario-x m) X2-PLATAFORMA1)
               (<= (distancia (mario-y m) (plataforma-y PLATAFORMA-ONE))
                   (+ (/ (image-height IMG-MARIO-INDO0) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               (<= (mario-y m) (plataforma-y PLATAFORMA-ONE))
               )
          
          (and (>= (mario-x m) X1-PLATAFORMA2)
               (<= (mario-x m) X2-PLATAFORMA2) 
               (<= (distancia (mario-y m) (plataforma-y PLATAFORMA-TWO))
                   (+ (/ (image-height IMG-MARIO-INDO0) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               (<= (mario-y m) (plataforma-y PLATAFORMA-TWO))
               )
          
          (and (>= (mario-x m) X1-PLATAFORMA3)
               (<= (mario-x m) X2-PLATAFORMA3)
               (<= (distancia (mario-y m) (plataforma-y PLATAFORMA-THREE))
                   (+ (/ (image-height IMG-MARIO-INDO0) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               (<= (mario-y m) (plataforma-y PLATAFORMA-THREE))
               
               )
          
          (and (>= (mario-x m) X1-PLATAFORMA4)
               (<= (mario-x m) X2-PLATAFORMA4) 
               (<= (distancia (mario-y m) (plataforma-y PLATAFORMA-FOUR))
                   (+ (/ (image-height IMG-MARIO-INDO0) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               
               (<= (mario-y m) (plataforma-y PLATAFORMA-FOUR))
               )
          
          (and (>= (mario-x m) X1-PLATAFORMA5)
               (<= (mario-x m) X2-PLATAFORMA5)
               (<= (distancia (mario-y m) (plataforma-y PLATAFORMA-FIVE))
                   (+ (/ (image-height IMG-MARIO-INDO0) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               (<= (mario-y m) (plataforma-y PLATAFORMA-FIVE))
               )
          
          (and (>= (mario-x m) X1-PLATAFORMA6)
               (<= (mario-x m) X2-PLATAFORMA6)
               (<= (distancia (mario-y m) (plataforma-y PLATAFORMA-SIX))
                   (+ (/ (image-height IMG-MARIO-INDO0) 2) (/ (image-height IMG-PLATAFORMA) 2)))                    
               (<= (mario-y m) (plataforma-y PLATAFORMA-SIX))
               )
          
          (and (>= (mario-x m) X1-PLATAFORMA7)
               (<= (mario-x m) X2-PLATAFORMA7)
               (<= (distancia (mario-y m) (plataforma-y PLATAFORMA-SEVEN))
                   (+ (/ (image-height IMG-MARIO-INDO0) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               (<= (mario-y m) (plataforma-y PLATAFORMA-SEVEN))
               )
          
          (and (>= (mario-x m) X1-PLATAFORMA8)
               (<= (mario-x m) X2-PLATAFORMA8)
               (<= (distancia (mario-y m) (plataforma-y PLATAFORMA-EIGHT))
                   (+ (/ (image-height IMG-MARIO-INDO0) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               (<= (mario-y m) (plataforma-y PLATAFORMA-EIGHT))
               ))                           
      #true 
      #false))

;; Template
#;
(define (fn-para-proximo-mario m)
  (...  (mario-x m)
        (mario-dx m)
        (mario-y m)
        (mario-dy m)))

;; enbaixo-plataforma? : Mario -> Boolean
;; interp. verifica se o mario colidiu em baixo da plataforma
; (define (enbaixo-plataforma? m) #true)

(define (enbaixo-plataforma? m) 
  (if (or (and (>= (mario-x m) X1-PLATAFORMA1) 
               (<= (mario-x m) X2-PLATAFORMA1)
               (<= (distancia (mario-y m) (plataforma-y PLATAFORMA-ONE))
                   (+ (/ (image-height IMG-MARIO-INDO0) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               (>= (mario-y m) (plataforma-y PLATAFORMA-ONE))
               (<= (mario-y m) (+ (plataforma-y PLATAFORMA-ONE) 70)))
          
          (and (>= (mario-x m) X1-PLATAFORMA2)
               (<= (mario-x m) X2-PLATAFORMA2) 
               (<= (distancia (mario-y m) (plataforma-y PLATAFORMA-TWO))
                   (+ (/ (image-height IMG-MARIO-INDO0) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               (>= (mario-y m) (plataforma-y PLATAFORMA-TWO))
               (<= (mario-y m) (+ (plataforma-y PLATAFORMA-TWO) 70)))
          
          (and (>= (mario-x m) X1-PLATAFORMA3)
               (<= (mario-x m) X2-PLATAFORMA3)
               (<= (distancia (mario-y m) (plataforma-y PLATAFORMA-THREE))
                   (+ (/ (image-height IMG-MARIO-INDO0) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               (>= (mario-y m) (plataforma-y PLATAFORMA-THREE))
               (<= (mario-y m) (+ (plataforma-y PLATAFORMA-THREE) 70))                    
               )
          
          (and (>= (mario-x m) X1-PLATAFORMA4)
               (<= (mario-x m) X2-PLATAFORMA4) 
               (<= (distancia (mario-y m) (plataforma-y PLATAFORMA-FOUR))
                   (+ (/ (image-height IMG-MARIO-INDO0) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               (>= (mario-y m) (plataforma-y PLATAFORMA-FOUR))
               (<= (mario-y m) (+ (plataforma-y PLATAFORMA-FOUR) 70))
               )
          
          (and (>= (mario-x m) X1-PLATAFORMA5) 
               (<= (mario-x m) X2-PLATAFORMA5) 
               (<= (distancia (mario-y m) (plataforma-y PLATAFORMA-FIVE))
                   (+ (/ (image-height IMG-MARIO-INDO0) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               (>= (mario-y m) (plataforma-y PLATAFORMA-FIVE))
               (<= (mario-y m) (+ (plataforma-y PLATAFORMA-FIVE) 70))
               )
          
          (and (>= (mario-x m) X1-PLATAFORMA6)
               (<= (mario-x m) X2-PLATAFORMA6)
               (<= (distancia (mario-y m) (plataforma-y PLATAFORMA-SIX))
                   (+ (/ (image-height IMG-MARIO-INDO0) 2) (/ (image-height IMG-PLATAFORMA) 2)))                    
               (>= (mario-y m) (plataforma-y PLATAFORMA-SIX))
               (<= (mario-y m) (+ (plataforma-y PLATAFORMA-SIX) 70))
               )
          
          (and (>= (mario-x m) X1-PLATAFORMA7)
               (<= (mario-x m) X2-PLATAFORMA7)
               (<= (distancia (mario-y m) (plataforma-y PLATAFORMA-SEVEN))
                   (+ (/ (image-height IMG-MARIO-INDO0) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               (>= (mario-y m) (plataforma-y PLATAFORMA-SEVEN))
               (<= (mario-y m) (+ (plataforma-y PLATAFORMA-SEVEN) 70))
               )
          
          (and (>= (mario-x m) X1-PLATAFORMA8)
               (<= (mario-x m) X2-PLATAFORMA8)
               (<= (distancia (mario-y m) (plataforma-y PLATAFORMA-EIGHT))
                   (+ (/ (image-height IMG-MARIO-INDO0) 2) (/ (image-height IMG-PLATAFORMA) 2)))
               (>= (mario-y m) (plataforma-y PLATAFORMA-EIGHT))
               (<= (mario-y m) (+ (plataforma-y PLATAFORMA-EIGHT) 70))
               )
          (and (>= (mario-x m) 0)
               (<= (mario-x m) LARGURA)
               (<= (distancia (mario-y m) 0)
                   (image-height IMG-MARIO-INDO0))
               (>= (mario-y m) 0)
               (<= (mario-y m)  50)
               )
          )                           
      #true 
      #false))

;; Template
#;
(define (fn-para-proximo-mario m)
  (...  (mario-x m)
        (mario-dx m)
        (mario-y m)
        (mario-dy m)))

;;Fim da parte logica

;;**********************************************************************************************************************************
;;Inicio da parte visual 

;; desenha-jogo : Jogo -> Image
;; interp. desenha o jogo
(define (desenha-jogo j)
  (cond[ (jogo-game-over? j) GAME-OVER]
       
       [(>= (jogo-contStar j) 5)
        (overlay
         (overlay
          (overlay
           (overlay
            (desenha-qtd-moedas (jogo-qtdMoedas j))
            (desenha-moedas (jogo-moedas j)))
           (desenha-qtd-vida (jogo-qtdVidas j)))
          (desenha-plataformas (jogo-plataformas j)))
         (desenha-mario2 (jogo-mario j)))]
       
       [else (overlay
              (overlay
               (overlay
                (overlay
                 (overlay
                  (overlay
                   (desenha-qtd-estrela (jogo-qtdStars j))
                   (desenha-stars (jogo-stars j)))
                  (desenha-qtd-vida (jogo-qtdVidas j)))
                 (desenha-vidas (jogo-vidas j)))
                (desenha-tartarugas (jogo-tartarugas j)))
               (desenha-plataformas (jogo-plataformas j)))
              (desenha-mario (jogo-mario j))) ]       
       ))  

;; desenha-plataformas : ListaDeplataforma -> Image
;; interp. desenha varias plataformas 
(define (desenha-plataformas ldplat)
  (foldl overlay CENARIO1 
         (map desenha-plataforma ldplat)))

;; desenha-plataforma : plataforma -> Image
;; interp. desenha uma plataforma 
(define (desenha-plataforma plat)
  (place-image IMG-PLATAFORMA (plataforma-x plat) (plataforma-y plat) CENARIO1))

;; desenha-stars : ListaDeStars -> Image
;; interp. desenha varias estrelas 
(define (desenha-stars lds)
  (foldl overlay CENARIO1 
         (map desenha-star lds)))

;; desenha-vidas : ListaDeVidas -> Image
;; interp. desenha varias Vidas 
(define (desenha-vidas ldv)
  (foldl overlay CENARIO1 
         (map desenha-vida ldv)))


;; desenha-star : star -> Image
;; interp. desenha uma estrela 
(define (desenha-star s) 
  (cond [(even? (estrela-x s))
         (place-image STAR (estrela-x s) (estrela-y s) CENARIO1)]
        [else (place-image STAR2 (estrela-x s) (estrela-y s) CENARIO1)]))

;; desenha-qtd-estrela : Star -> Image
;; interp. desenha uma estrela 
(define (desenha-qtd-estrela s) 
  (cond [(= (qtdStar-cont s) 1) 
         (place-image STAR 
                      (- (qtdStar-x s) 40)
                      (qtdStar-y s) 
                      (place-image
                       IMG-QTD1 (qtdStar-x s)
                       (qtdStar-y s) CENARIO1))]
        [(= (qtdStar-cont s) 2)
         (place-image STAR 
                      (- (qtdStar-x s) 40)
                      (qtdStar-y s) 
                      (place-image
                       IMG-QTD2 (qtdStar-x s)
                       (qtdStar-y s) CENARIO1))]
        [(= (qtdStar-cont s) 3)
         (place-image STAR 
                      (- (qtdStar-x s) 40)
                      (qtdStar-y s) 
                      (place-image
                       IMG-QTD3 (qtdStar-x s)
                       (qtdStar-y s) CENARIO1))]
        [(= (qtdStar-cont s) 4)
         (place-image STAR 
                      (- (qtdStar-x s) 40)
                      (qtdStar-y s) 
                      (place-image
                       IMG-QTD4 (qtdStar-x s)
                       (qtdStar-y s) CENARIO1))]
        [(= (qtdStar-cont s) 5)
         (place-image STAR 
                      (- (qtdStar-x s) 40)
                      (qtdStar-y s) 
                      (place-image
                       IMG-QTD5 (qtdStar-x s)
                       (qtdStar-y s) CENARIO1))]
        [(= (qtdStar-cont s) 0)
         (place-image STAR 
                      (- (qtdStar-x s) 40) 
                      (qtdStar-y s) 
                      (place-image
                       IMG-QTD0 (qtdStar-x s)
                       (qtdStar-y s) CENARIO1))]
        [else (place-image STAR 
                           (- (qtdStar-x s) 40)
                           (qtdStar-y s) 
                           (place-image
                            IMG-QTD5 (qtdStar-x s)
                            (qtdStar-y s) CENARIO1))]))


;; desenha-qtd-vida : Vida -> Image
;; interp. desenha uma Vida 
(define (desenha-qtd-vida v)   
  (cond [(= (qtdVida-cont v) 1)
         (place-image VIDA-MARIO 
                      (- (qtdVida-x v) 7)
                      (qtdVida-y v) 
                      (place-image
                       IMG-QTD1 (qtdVida-x v)
                       (qtdVida-y v) CENARIO1))]
        [(= (qtdVida-cont v) 2)
         (place-image VIDA-MARIO 
                      (- (qtdVida-x v) 7)
                      (qtdVida-y v) 
                      (place-image
                       IMG-QTD2 (qtdVida-x v)
                       (qtdVida-y v) CENARIO1))]
        [(= (qtdVida-cont v) 3)
         (place-image VIDA-MARIO 
                      (- (qtdVida-x v) 7)
                      (qtdVida-y v) 
                      (place-image
                       IMG-QTD3 (qtdVida-x v) 
                       (qtdVida-y v) CENARIO1))]
        [(= (qtdVida-cont v) 4)
         (place-image VIDA-MARIO 
                      (- (qtdVida-x v) 7)
                      (qtdVida-y v) 
                      (place-image
                       IMG-QTD4 (qtdVida-x v)
                       (qtdVida-y v) CENARIO1))]
        [(= (qtdVida-cont v) 5)
         (place-image VIDA-MARIO 
                      (- (qtdVida-x v) 7)
                      (qtdVida-y v) 
                      (place-image
                       IMG-QTD5 (qtdVida-x v)
                       (qtdVida-y v) CENARIO1))]
        [(= (qtdVida-cont v) 0)
         (place-image VIDA-MARIO 
                      (- (qtdVida-x v) 7) 
                      (qtdVida-y v) 
                      (place-image
                       IMG-QTD0 (qtdVida-x v)
                       (qtdVida-y v) CENARIO1))]
        [else (place-image VIDA-MARIO 
                           (- (qtdVida-x v) 7)
                           (qtdVida-y v) 
                           (place-image
                            IMG-QTD5 (qtdVida-x v)
                            (qtdVida-y v) CENARIO1))]))

;; desenha-qtd-moedas : Moeda -> Image
;; interp. desenha uma Moeda 
(define (desenha-qtd-moedas v)  
  (cond [(= (qtdMoeda-cont v) 1)
         (place-image IMG-MOEDA4 
                      (- (qtdMoeda-x v) 30)
                      (qtdMoeda-y v) 
                      (place-image
                       IMG-QTD1 (qtdMoeda-x v)
                       (qtdMoeda-y v) CENARIO1))]
        [(= (qtdMoeda-cont v) 2)
         (place-image IMG-MOEDA4 
                      (- (qtdMoeda-x v) 30)
                      (qtdMoeda-y v) 
                      (place-image
                       IMG-QTD2 (qtdMoeda-x v)
                       (qtdMoeda-y v) CENARIO1))]
        [(= (qtdMoeda-cont v) 3)
         (place-image IMG-MOEDA4  
                      (- (qtdMoeda-x v) 30)
                      (qtdMoeda-y v) 
                      (place-image
                       IMG-QTD3 (qtdMoeda-x v) 
                       (qtdMoeda-y v) CENARIO1))]
        [(= (qtdMoeda-cont v) 4)
         (place-image IMG-MOEDA4 
                      (- (qtdMoeda-x v) 30)
                      (qtdMoeda-y v) 
                      (place-image
                       IMG-QTD4 (qtdMoeda-x v)
                       (qtdMoeda-y v) CENARIO1))]
        [(= (qtdMoeda-cont v) 5)
         (place-image IMG-MOEDA4 
                      (- (qtdMoeda-x v) 30)
                      (qtdMoeda-y v) 
                      (place-image
                       IMG-QTD5 (qtdMoeda-x v)
                       (qtdMoeda-y v) CENARIO1))]
        [(= (qtdMoeda-cont v) 6)
         (place-image IMG-MOEDA4 
                      (- (qtdMoeda-x v) 30) 
                      (qtdMoeda-y v) 
                      (place-image
                       IMG-QTD6 (qtdMoeda-x v)
                       (qtdMoeda-y v) CENARIO1))]
        [(= (qtdMoeda-cont v) 7)
         (place-image IMG-MOEDA4 
                      (- (qtdMoeda-x v) 30) 
                      (qtdMoeda-y v) 
                      (place-image
                       IMG-QTD7 (qtdMoeda-x v)
                       (qtdMoeda-y v) CENARIO1))]
        [(= (qtdMoeda-cont v) 8)
         (place-image IMG-MOEDA4 
                      (- (qtdMoeda-x v) 30) 
                      (qtdMoeda-y v) 
                      (place-image
                       IMG-QTD8 (qtdMoeda-x v)
                       (qtdMoeda-y v) CENARIO1))]
        [(= (qtdMoeda-cont v) 9)
         (place-image IMG-MOEDA4 
                      (- (qtdMoeda-x v) 30) 
                      (qtdMoeda-y v) 
                      (place-image
                       IMG-QTD9 (qtdMoeda-x v)
                       (qtdMoeda-y v) CENARIO1))]
        [(= (qtdMoeda-cont v) 0)
         (place-image IMG-MOEDA4 
                      (- (qtdMoeda-x v) 30) 
                      (qtdMoeda-y v) 
                      (place-image
                       IMG-QTD0 (qtdMoeda-x v)
                       (qtdMoeda-y v) CENARIO1))]
        [else (place-image IMG-MOEDA4 
                           (- (qtdMoeda-x v) 30)
                           (qtdMoeda-y v) 
                           (place-image
                            IMG-QTD9 (qtdMoeda-x v)
                            (qtdMoeda-y v) CENARIO1))]))

;; desenha-vida : Vida -> Image
;; interp. desenha uma Vida 
(define (desenha-vida v) 
  (place-image VIDA-MARIO (vida-x v) (vida-y v) CENARIO1))


;; desenha-moedas : ListaDeMoeda -> Image
;; interp. desenha varias Moedas 
(define (desenha-moedas ldm)
  (foldl overlay CENARIO1 
         (map desenha-moeda ldm)))

;; desenha-moeda : Moeda -> Image
;; interp. retorna a imagem de uma moeda
(define (desenha-moeda m)
  (cond [(= (moeda-cont m) 0)
         (place-image IMG-MOEDA4 (moeda-x m) (moeda-y m) CENARIO1)] 
        
        [(= (moeda-cont m) 1)
         (place-image IMG-MOEDA3 (moeda-x m) (moeda-y m) CENARIO1)]
        
        [(= (moeda-cont m) 2)
         (place-image IMG-MOEDA1 (moeda-x m) (moeda-y m) CENARIO1)]
        
        [(= (moeda-cont m) 3)
         (place-image IMG-MOEDA2 (moeda-x m) (moeda-y m) CENARIO1)]
        
        [else
         (place-image VIDA-MARIO (moeda-x m) (moeda-y m) CENARIO1)]
        )) 

;; desenha-tartarugas : ListaDeTartaruga -> Image
;; interp. desenha varias Tartarugas 
(define (desenha-tartarugas ldtat)
  (foldl overlay CENARIO1 
         (map desenha-tartaruga ldtat)))

;; desenha-tartaruga : tartaruga -> Image
;; interp. desenha uma tartaruga 
(define (desenha-tartaruga tat)  
  (cond [(and (even? (tartaruga-x tat))
              (> (tartaruga-dx tat) 0))
         (place-image IMG-TARTARUGA-INDO1 (tartaruga-x tat) (tartaruga-y tat) CENARIO1)]
        [(and (not (even? (tartaruga-x tat)))
              (> (tartaruga-dx tat) 0))
         (place-image IMG-TARTARUGA-INDO0 (tartaruga-x tat) (tartaruga-y tat) CENARIO1)]
        
        [(and (even? (tartaruga-x tat)) 
              (<= (tartaruga-dx tat) 0))
         (place-image IMG-TARTARUGA-VOLTANDO1 (tartaruga-x tat) (tartaruga-y tat) CENARIO1)]
        [(and (not (even? (tartaruga-x tat)))
              (<= (tartaruga-dx tat) 0))
         (place-image IMG-TARTARUGA-VOLTANDO0 (tartaruga-x tat) (tartaruga-y tat) CENARIO1)]))
        
      

;; desenha-mario: mario -> Image 
;; interp. retorna a representação do cenário com o mario
(define (desenha-mario m) 
  (place-image
   (cond [(and (< (mario-dx m) 0)
               (even? (mario-x m)))
          IMG-MARIO-VOLTANDO0
          ]
         [(and (< (mario-dx m) 0)
               (not (even? (mario-x m))))
          IMG-MARIO-VOLTANDO1 
          ]
         [(and (> (mario-dx m) 0)
               (even? (mario-x m))
               ) 
          IMG-MARIO-INDO1]
         
         [(and (> (mario-dx m) 0) 
               (not (even? (mario-x m)))
               )
          IMG-MARIO-INDO0]
         [else IMG-MARIO-INDO0]
         ) 
   (mario-x m) 
   (mario-y m) 
   CENARIO)               
  )
 
;; desenha-mario2: mario -> Image
;; interp. retorna a representação do cenário com o mario
(define (desenha-mario2 m)
  (place-image
   (cond [(and (< (mario-dx m) 0)
               (even? (mario-x m)))
          IMG-MARIO-VOLTANDO0
          ]
         [(and (< (mario-dx m) 0)
               (not (even? (mario-x m))))
          IMG-MARIO-VOLTANDO1
          ]
         [(and (> (mario-dx m) 0)
               (even? (mario-x m))
               ) 
          IMG-MARIO-INDO1]
          
         [(and (> (mario-dx m) 0) 
               (not (even? (mario-x m)))
               )
          IMG-MARIO-INDO0]
         [else IMG-MARIO-INDO0]
         ) 
   (mario-x m)
   (mario-y m) 
   FUNDO-2)               
  )

;;Fim da parte visual

;;**********************************************************************************************************************************
;;Inicio da parte de interacao

;; trata-tecla-press : Jogo KeyEvent -> Jogo
;; interp.  trata tecla usando trata-tecla-press-mario

(define (trata-tecla-press j ke)
  (cond
    [(and (jogo-game-over? j) (key=? ke "\r"))   
     JOGO-NO-INICIO]
    [else (make-jogo
           (trata-tecla-press-mario (jogo-mario j) ke)
           (jogo-plataformas j)
           (jogo-game-over? j)
           (jogo-spawn-tat1 j) 
           (jogo-spawn2 j)
           (jogo-tartarugas j) 
           (jogo-vidas j)
           (jogo-stars j)
           (jogo-qtdVidas j)
           (jogo-contStar j)
           (jogo-moedas j)
           (jogo-spawn-moeda j)
           (jogo-qtdStars j)
           (jogo-qtdMoedas j)
           )]))

;; Template
#;
(define (fn-para-jogo j)
  (... (jogo-mario j)
       (jogo-plataformas j)
       (jogo-game-over? j)
       (jogo-spawn-tat1 j)
       (jogo-spawn2 j)
       (jogo-tartarugas j)
       (jogo-vidas j)
       (jogo-stars j)
       (jogo-qtdVidas j)
       (jogo-contStar j)
       (jogo-moedas j)
       (jogo-spawn-moeda j)
       (jogo-qtdStars j)
       (jogo-qtdMoedas j)
       ))

;=====================================================================================================
;; trata-tecla-solta : Jogo KeyEvent -> Jogo
;; interp. trata tecla usando trata-tecla-mario
; (define (trata-tecla-solta j ke) j)
(define (trata-tecla-solta j ke)
  (make-jogo
   (trata-tecla-solta-mario (jogo-mario j) ke)
   (jogo-plataformas j)
   (jogo-game-over? j)
   (jogo-spawn-tat1 j)
   (jogo-spawn2 j)
   (jogo-tartarugas j)
   (jogo-vidas j)
   (jogo-stars j)
   (jogo-qtdVidas j)
   (jogo-contStar j)
   (jogo-moedas j)
   (jogo-spawn-moeda j)
   (jogo-qtdStars j)
   (jogo-qtdMoedas j)
   ))

;; Template
#;
(define (fn-para-jogo j)
  (... (jogo-mario j)
       (jogo-plataformas j)
       (jogo-game-over? j)
       (jogo-spawn-tat1 j)
       (jogo-spawn2 j)
       (jogo-tartarugas j)
       (jogo-vidas j)
       (jogo-stars j)
       (jogo-qtdVidas j)
       (jogo-contStar j)
       (jogo-moedas j)
       (jogo-spawn-moeda j)
       (jogo-qtdStars j)
       (jogo-qtdMoedas j)
       ))

;=====================================================================================================

;; trata-tecla-press-mario: mario KeyEvent -> mario
;; interp. quando tecla up é pressionada, mario deve pular,
;; quando teclar seta para frente ele deve andar para direita,e quando teclar seta para 
;; esquerda ele deve andar para tras
;(define (trata-tecla-mario v ke) v)
(define (trata-tecla-press-mario m ke)  
  (cond [(and (key=? ke "right")
         (or (encima-chao? m)
             (encima-plataforma? m)
             ))
         (make-mario (mario-x m) VEL-PADRAO-MARIO (mario-y m) (mario-dy m))]
        [(and (key=? ke "left") 
              (or (encima-chao? m) 
                  (encima-plataforma? m)
                  ))
         (make-mario (mario-x m) (- VEL-PADRAO-MARIO) (mario-y m) (mario-dy m))]       
        [(and (key=? ke "up") 
              (= (mario-dy m) 0)             
              )                        
         (make-mario (mario-x m) (mario-dx m)  (- (mario-y m) 10) DY-PULO)] 
        
        [else m])) 

;; Template
#;
(define (fn-para-trata-tecla-press-mario m)
  (...  (mario-x m)
        (mario-dx m)
        (mario-y m)
        (mario-dy m)))

;; trata-tecla-solta-mario: mario KeyEvent -> mario
;; quando tecla e solta ele deve parar com o comando da tecla
;(define (trata-tecla-mario m ke) m)

(define (trata-tecla-solta-mario m ke)
  (make-mario (mario-x m)
              (cond [(key=? ke "right")
                     0.000001]
                    [(key=? ke "left") 
                     (- 0.000001)]
                    [else (mario-dx m)])  
              (mario-y m) (mario-dy m)))   
 
;; Template
#;
(define (fn-para-trata-tecla-press-mario m)
  (...  (mario-x m)
        (mario-dx m)
        (mario-y m)
        (mario-dy m))) 

;;Fim da parte de interaçao

;;********************************************************************************************************************************** 