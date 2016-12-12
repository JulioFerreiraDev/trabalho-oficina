#lang racket

(require 2htdp/image)
(require "constantes.rkt")


(provide (all-defined-out))

;; =================================================================================================================================
;; DEFINICOES DE DADOS  
;; Definições de dados da Plataforma

(define-struct plataforma (x y) #:transparent)
;; plataforma é (make-plataforma Numero+ Numero+)
;; interp. representa a plataforma que está numa posição (x,y) no cenario

;exemplos:
(define PLATAFORMA-ONE (make-plataforma (- LARGURA MEIO-W-PLATAFORMA) (+ ALTURA-TUBO MEIO-H-PLATAFORMA)))
(define PLATAFORMA-TWO (make-plataforma MEIO-W-PLATAFORMA (+ ALTURA-TUBO MEIO-H-PLATAFORMA)))
(define PLATAFORMA-THREE (make-plataforma (/ LARGURA 2) (- ALTURA (+ ALTURA-TUBO (* 6 MEIO-H-PLATAFORMA)))))
(define PLATAFORMA-FOUR (make-plataforma (- LARGURA (* (/ 1 3) MEIO-W-PLATAFORMA)) (/ ALTURA 2)))
(define PLATAFORMA-FIVE (make-plataforma (* (/ 1 3) MEIO-W-PLATAFORMA) (/ ALTURA 2)))
(define PLATAFORMA-SIX (make-plataforma  (- LARGURA MEIO-W-PLATAFORMA) (- (- ALTURA ALTURA-TUBO) MEIO-H-PLATAFORMA)))
(define PLATAFORMA-SEVEN (make-plataforma MEIO-W-PLATAFORMA (- (- ALTURA ALTURA-TUBO) MEIO-H-PLATAFORMA)))
(define PLATAFORMA-EIGHT (make-plataforma (/ LARGURA 2) (+ ALTURA-TUBO (* 6.5 MEIO-H-PLATAFORMA))))

(define X1-PLATAFORMA1 (- LARGURA (* MEIO-W-PLATAFORMA 2)))
(define X1-PLATAFORMA2 0)
(define X1-PLATAFORMA3 (- (/ LARGURA 2) MEIO-W-PLATAFORMA))
(define X1-PLATAFORMA4 (- (- LARGURA (* (/ 1 3) MEIO-W-PLATAFORMA)) MEIO-W-PLATAFORMA))
(define X1-PLATAFORMA5 0) 
(define X1-PLATAFORMA6 (- LARGURA (* MEIO-W-PLATAFORMA 2)))
(define X1-PLATAFORMA7 0)
(define X1-PLATAFORMA8 (- (/ LARGURA 2) MEIO-W-PLATAFORMA))

(define X2-PLATAFORMA1 LARGURA)
(define X2-PLATAFORMA2 (* MEIO-W-PLATAFORMA 2)) 
(define X2-PLATAFORMA3 (+ (/ LARGURA 2) MEIO-W-PLATAFORMA))
(define X2-PLATAFORMA4 LARGURA)
(define X2-PLATAFORMA5 (+ MEIO-W-PLATAFORMA (* (/ 1 3) MEIO-W-PLATAFORMA)))
(define X2-PLATAFORMA6 LARGURA)
(define X2-PLATAFORMA7 (* MEIO-W-PLATAFORMA 2))
(define X2-PLATAFORMA8 (+ (/ LARGURA 2) MEIO-W-PLATAFORMA))

#;
(define (fn-para-plataforma plat)
  (... (plataforma-x plat)
       (plataforma-y plat)) 
  )


;;**********************************************************************************************************************************
;;Definiçoes de dados da Lista de plataforma

; ListaDeplataforma é um desses:
;; - empty
;; - (cons plataforma ListaDeplataforma)
;; interp. uma lista de plataformas
(define LDPLAT1 empty)
(define LDPLAT2 (cons PLATAFORMA-ONE  empty)) 
(define LDPLAT3 (cons PLATAFORMA-ONE (cons PLATAFORMA-TWO empty)))

(define LDPLAT-J1 (list PLATAFORMA-ONE
                        (make-plataforma (/ LARGURA 4) (/ ALTURA 2))
                        (make-plataforma (/ LARGURA 4/3) (/ ALTURA 3/2))))
                      
(define LDPLAT-PRINCIPAL (list PLATAFORMA-ONE
                               PLATAFORMA-TWO
                               PLATAFORMA-THREE 
                               PLATAFORMA-FOUR
                               PLATAFORMA-FIVE
                               PLATAFORMA-SIX
                               PLATAFORMA-SEVEN
                               PLATAFORMA-EIGHT
                               ))

#;
(define (fn-for-ldplat ldplat)
  (cond [(empty? ldplat) (...)]                         ;CASO BASE (CONDIÇÃO DE PARADA)
        [else (... (first ldplat)                       ;plataforma
                   (fn-for-ldplat (rest ldplat)))]))    ;RECURSÃO EM CAUDA


;;**********************************************************************************************************************************
;;Definiçoes de dados da moeda

(define-struct moeda (x y cont) #:transparent)
;; moeda é (make-moeda Numero+ Numero+ Numero+)
;; interp. representa a moeda que esta num posicao (x,y) do cenario e um contador

;; exemplo
(define MOEDA1 (make-moeda 25 (- ALTURA-TUBO (+ MEIO-H-MOEDA 10)) 0)) 
(define MOEDA2 (make-moeda 75 (- ALTURA-TUBO (+ MEIO-H-MOEDA 10)) 0)) 
(define MOEDA3 (make-moeda 125 (- ALTURA-TUBO (+ MEIO-H-MOEDA 10)) 0))
(define MOEDA4 (make-moeda 175 (- ALTURA-TUBO (+ MEIO-H-MOEDA 10)) 0))
(define MOEDA5 (make-moeda 225 (- ALTURA-TUBO (+ MEIO-H-MOEDA 10)) 0))
(define MOEDA6 (make-moeda 275 (- ALTURA-TUBO (+ MEIO-H-MOEDA 10)) 0))

(define MOEDA7 (make-moeda 25 (- (- (- ALTURA ALTURA-TUBO) (* MEIO-H-PLATAFORMA 2)) 30) 0))
(define MOEDA8 (make-moeda 75 (- (- (- ALTURA ALTURA-TUBO) (* MEIO-H-PLATAFORMA 2)) 30) 0)) 
(define MOEDA9 (make-moeda 125 (- (- (- ALTURA ALTURA-TUBO) (* MEIO-H-PLATAFORMA 2)) 30) 0))
(define MOEDA10 (make-moeda 175 (- (- (- ALTURA ALTURA-TUBO) (* MEIO-H-PLATAFORMA 2)) 30) 0))
(define MOEDA11 (make-moeda 225 (- (- (- ALTURA ALTURA-TUBO) (* MEIO-H-PLATAFORMA 2)) 30) 0))
(define MOEDA12 (make-moeda 275 (- (- (- ALTURA ALTURA-TUBO) (* MEIO-H-PLATAFORMA 2)) 30) 0))

(define MOEDA13 (make-moeda 25 (- (/ ALTURA 2) (+ MEIO-H-PLATAFORMA 30)) 0)) 
(define MOEDA14 (make-moeda 75 (- (/ ALTURA 2) (+ MEIO-H-PLATAFORMA 30)) 0)) 
(define MOEDA15 (make-moeda 125 (- (/ ALTURA 2) (+ MEIO-H-PLATAFORMA 30)) 0)) 
(define MOEDA16 (make-moeda 175 (- (/ ALTURA 2) (+ MEIO-H-PLATAFORMA 30)) 0))

(define MOEDA17 (make-moeda (- LARGURA 25) (- ALTURA-TUBO (+ MEIO-H-MOEDA 10)) 0)) 
(define MOEDA18 (make-moeda (- LARGURA 75) (- ALTURA-TUBO (+ MEIO-H-MOEDA 10)) 0)) 
(define MOEDA19 (make-moeda (- LARGURA 125) (- ALTURA-TUBO (+ MEIO-H-MOEDA 10)) 0))
(define MOEDA20 (make-moeda (- LARGURA 175) (- ALTURA-TUBO (+ MEIO-H-MOEDA 10)) 0))
(define MOEDA21 (make-moeda (- LARGURA 225) (- ALTURA-TUBO (+ MEIO-H-MOEDA 10)) 0))
(define MOEDA22 (make-moeda (- LARGURA 275) (- ALTURA-TUBO (+ MEIO-H-MOEDA 10)) 0)) 

(define MOEDA23 (make-moeda (- LARGURA 25) (- (- (- ALTURA ALTURA-TUBO) (* MEIO-H-PLATAFORMA 2)) 30) 0))
(define MOEDA24 (make-moeda (- LARGURA 75) (- (- (- ALTURA ALTURA-TUBO) (* MEIO-H-PLATAFORMA 2)) 30) 0)) 
(define MOEDA25 (make-moeda (- LARGURA 125) (- (- (- ALTURA ALTURA-TUBO) (* MEIO-H-PLATAFORMA 2)) 30) 0))
(define MOEDA26 (make-moeda (- LARGURA 175) (- (- (- ALTURA ALTURA-TUBO) (* MEIO-H-PLATAFORMA 2)) 30) 0))
(define MOEDA27 (make-moeda (- LARGURA 225) (- (- (- ALTURA ALTURA-TUBO) (* MEIO-H-PLATAFORMA 2)) 30) 0))
(define MOEDA28 (make-moeda (- LARGURA 275) (- (- (- ALTURA ALTURA-TUBO) (* MEIO-H-PLATAFORMA 2)) 30) 0))

(define MOEDA29 (make-moeda (- LARGURA 25) (- (/ ALTURA 2) (+ MEIO-H-PLATAFORMA 30)) 0)) 
(define MOEDA30 (make-moeda (- LARGURA 75) (- (/ ALTURA 2) (+ MEIO-H-PLATAFORMA 30)) 0)) 
(define MOEDA31 (make-moeda (- LARGURA 125) (- (/ ALTURA 2) (+ MEIO-H-PLATAFORMA 30)) 0)) 
(define MOEDA32 (make-moeda (- LARGURA 175) (- (/ ALTURA 2) (+ MEIO-H-PLATAFORMA 30)) 0))

(define MOEDA33 (make-moeda (+ (- (/ LARGURA 2) MEIO-W-PLATAFORMA) 25) 
                            (- (- ALTURA (+ ALTURA-TUBO (* 6 MEIO-H-PLATAFORMA))) 50) 0)) 
(define MOEDA34 (make-moeda (+ (- (/ LARGURA 2) MEIO-W-PLATAFORMA) 75) 
                            (- (- ALTURA (+ ALTURA-TUBO (* 6 MEIO-H-PLATAFORMA))) 50) 0))
(define MOEDA35 (make-moeda (+ (- (/ LARGURA 2) MEIO-W-PLATAFORMA) 125) 
                            (- (- ALTURA (+ ALTURA-TUBO (* 6 MEIO-H-PLATAFORMA))) 50) 0))
(define MOEDA36 (make-moeda (+ (- (/ LARGURA 2) MEIO-W-PLATAFORMA) 175) 
                            (- (- ALTURA (+ ALTURA-TUBO (* 6 MEIO-H-PLATAFORMA))) 50) 0))
(define MOEDA37 (make-moeda (+ (- (/ LARGURA 2) MEIO-W-PLATAFORMA) 225) 
                            (- (- ALTURA (+ ALTURA-TUBO (* 6 MEIO-H-PLATAFORMA))) 50) 0))
(define MOEDA38 (make-moeda (+ (- (/ LARGURA 2) MEIO-W-PLATAFORMA) 275) 
                            (- (- ALTURA (+ ALTURA-TUBO (* 6 MEIO-H-PLATAFORMA))) 50) 0))

(define MOEDA39 (make-moeda (+ (- (/ LARGURA 2) MEIO-W-PLATAFORMA) 25)
                            (- (+ ALTURA-TUBO (* 6.5 MEIO-H-PLATAFORMA)) 50) 0))
(define MOEDA40 (make-moeda (+ (- (/ LARGURA 2) MEIO-W-PLATAFORMA) 75)
                            (- (+ ALTURA-TUBO (* 6.5 MEIO-H-PLATAFORMA)) 50) 0))
(define MOEDA41 (make-moeda (+ (- (/ LARGURA 2) MEIO-W-PLATAFORMA) 125)
                            (- (+ ALTURA-TUBO (* 6.5 MEIO-H-PLATAFORMA)) 50) 0))
(define MOEDA42 (make-moeda (+ (- (/ LARGURA 2) MEIO-W-PLATAFORMA) 175)
                            (- (+ ALTURA-TUBO (* 6.5 MEIO-H-PLATAFORMA)) 50) 0))
(define MOEDA43 (make-moeda (+ (- (/ LARGURA 2) MEIO-W-PLATAFORMA) 225)
                            (- (+ ALTURA-TUBO (* 6.5 MEIO-H-PLATAFORMA)) 50) 0))
(define MOEDA44 (make-moeda (+ (- (/ LARGURA 2) MEIO-W-PLATAFORMA) 275)
                            (- (+ ALTURA-TUBO (* 6.5 MEIO-H-PLATAFORMA)) 50) 0))

;; Template
#;
(define (fn-para-moeda m)
  (... (moeda-x m)
       (moeda-y m)
       (moeda-cont m)) 
  )

;;**********************************************************************************************************************************
;;Definiçoes de dados da Lista de moeda

; ListaMoeda é um desses:
;; - empty
;; - (cons moeda ListaMoeda)
;; interp. uma lista de moedas

;; exemplos
(define LDM1 (cons MOEDA1 
                   (cons MOEDA2 
                         (cons MOEDA3
                               (cons MOEDA4
                                     (cons MOEDA5
                                           (cons MOEDA6 
                                                 (cons MOEDA7
                                                       (cons MOEDA8
                                                             (cons MOEDA9
                                                                   (cons MOEDA10
                                                                         (cons MOEDA11
                                                                               (cons MOEDA12 
                                                                                      (cons MOEDA13 
                                                                                             (cons MOEDA14
                                                                                                    (cons MOEDA15
                                                                                                           (cons MOEDA16
 (cons MOEDA17 
       (cons MOEDA18
             (cons MOEDA19
                   (cons MOEDA20
                         (cons MOEDA21
                               (cons MOEDA22
                                     (cons MOEDA23
                                           (cons MOEDA24
                                                 (cons MOEDA25
                                                       (cons MOEDA26
                                                             (cons MOEDA27
                                                                   (cons MOEDA28
                                                                         (cons MOEDA29
                                                                               (cons MOEDA30
                                                                                     (cons MOEDA31
                                                                                           (cons MOEDA32
 (cons MOEDA33
       (cons MOEDA34 
             (cons MOEDA35
                   (cons MOEDA36
                         (cons MOEDA37
                               (cons MOEDA38
                                     (cons MOEDA39
                                           (cons MOEDA40 
                                                 (cons MOEDA41
                                                       (cons MOEDA42
                                                             (cons MOEDA43
                                                                   (cons MOEDA44 empty)))))))))))))))))))))))))))))))))))))))))))))

#;
(define (fn-for-ldplat ldm)
  (cond [(empty? ldm) (...)]                         ;CASO BASE (CONDIÇÃO DE PARADA)
        [else (... (first ldm)                       ;plataforma
                   (fn-for-ldm (rest ldm)))]))       ;RECURSÃO EM CAUDA


;;**********************************************************************************************************************************
;;Definiçoes de dados d quantidade de vidas

(define-struct qtdVida (x y cont) #:transparent)
;; qtdVida e (make-vida Numero+ Numero+ Numero+)
;; interp. representa o placar onde mostra a quantidade de vidas que o mario tem

;; exemplos
(define QTD-VIDA0 (make-qtdVida X-QTD Y-QTD 0))
(define QTD-VIDA1 (make-qtdVida X-QTD Y-QTD 1))

#;
(define (fn-para-qtdVida v)
  (... (qtdVida-x v)
       (qtdVida-y v)
       (qtdVida-cont v))
  )


;;**********************************************************************************************************************************
;;Definiçoes de dados do Mario

(define-struct mario (x dx y dy) #:transparent)
;;mario é (make-mario Numero+ Numero Numero+ Numero)
;;interp. representa o mario que está numa posição (x,y)
;;da tela e anda a uma velocidade dx ou dy

;exemplos:
(define MARIO-INICIAL (make-mario LIMITE-ESQUERDO 10 (/ ALTURA 2) 0))
(define MARIO-MEIO (make-mario (/ LARGURA 2) 10 (/ ALTURA 2) 0))
(define MARIO-ANTES-VIRAR (make-mario (+ LIMITE-DIREITO 5) 10 (/ ALTURA 2) 0))
(define MARIO-VIRADA (make-mario LIMITE-DIREITO -10 (/ ALTURA 2) 0))
(define MARIO-MEIO-VOlTANDO (make-mario (/ LARGURA 2) -10 (/ ALTURA 2) 0))
(define MARIO-CHEGANDO (make-mario 50 -10 (/ ALTURA 2) 0)) 
(define MARIO-ULTRAPASSOU (make-mario (+ LIMITE-DIREITO 20) 50 (/ ALTURA 2) 0))
(define MARIO-NO-LIMITE (make-mario LIMITE-DIREITO -50 (/ ALTURA 2) 0))
(define MARIO-CHAO (make-mario LIMITE-ESQUERDO 0 Y-CHAO-MARIO 0))

#;
(define (fn-para-mario m)
  (... (mario-x m)
       (mario-dx m)
       (mario-y m)
       (mario-dy m))
  )


;;**********************************************************************************************************************************
;; Definiçoes de dados da vida do mario

(define-struct vida (x dx y dy) #:transparent)
;; Vida e (make-vida Numero+ Numero Numero+ Numero
;; interp. sao as informacoes da vida do mario,as posicoes (x,y) e as velocidades dx ou dy

;; exemplos : 
(define VIDA1 (make-vida (+ (image-width TUBO-ESQUERDO) MEIO-W-VIDA)
                         7
                         (- (image-height TUBO-ESQUERDO) MEIO-H-VIDA)
                         0))

;template 
#; 
(define (fn-para-vida v)
  (...  (vida-x v)
        (vida-dx v)
        (vida-y v)
        (vida-dy v)))


;;**********************************************************************************************************************************
;;Definiçoes de dados da Lista de Vidas

; ListaDeVidas é uma dessas:
;; - empty
;; - (cons vida ListaDeVida)
;; interp. uma lista de vidas 

;; exemplos
(define LDV1 empty)
(define LDV2 (cons VIDA1 LDV1))

;; Template
#;
(define (fn-for-ListaDeVidas ldv)
  (cond [(empty? ldv) (...)]                         ;CASO BASE (CONDIÇÃO DE PARADA)
        [else (... (first ldv)                       ;plataforma
                   (fn-for-ldv (rest ldv)))]))       ;RECURSÃO EM CAUDA


;;**********************************************************************************************************************************
;;Definicoes de dados da Tartaruga
 
(define-struct tartaruga (x y dx dy) #:transparent)    

;;Tartaruga é (make-tartaruga Numero+ Numero+ Numero Numero)
;;interp. sao as informaçoes da posiçao (x,y) da tartaruga e da velocidade dx e dy da tartaruga

;exemplos:
(define TARTARUGA-INICIAL1 (make-tartaruga (+ (image-width TUBO-ESQUERDO) MEIO-W-TARTARUGA)
                                               (- (image-height TUBO-ESQUERDO) MEIO-H-TARTARUGA)
                                               5 0))
  
(define TARTARUGA-INICIAL2 (make-tartaruga (- LARGURA (+ (image-width TUBO-DIREITO) MEIO-W-TARTARUGA))
                                               (- (image-height TUBO-ESQUERDO) MEIO-H-TARTARUGA)
                                               -5 0))

(define TARTARUGA-INICIAL3 (make-tartaruga 10
                                               (- (/ ALTURA 2) 50)
                                               5 0))

(define TARTARUGA-INICIAL4 (make-tartaruga (- LARGURA 10)
                                               (- (/ ALTURA 2) 50)
                                               -7 0))
  
(define TARTARUGA-FINAL1 (make-tartaruga (- LARGURA (image-width TUBO-ESQUERDO))
                                             (- ALTURA (/ (image-height IMG-TARTARUGA-INDO1) 2))
                                             0 0)) 
  
(define TARTARUGA-FINAL2 (make-tartaruga (image-width TUBO-ESQUERDO)
                                             (- ALTURA (/ (image-height IMG-TARTARUGA-VOLTANDO1) 2))
                                             0 0))

;template 
#; 
(define (fn-para-tartaruga p)
  (...  (tartaruga-x p)
        (tartaruga-y p)
        (tartaruga-dx p)
        (tartaruga-dy p)))


;;**********************************************************************************************************************************
;;Definiçoes de dados da Lista de Tartarugas

; ListaDeTartarugas é um desses:
;; - empty
;; - (cons tartaruga ListaDeTartaruga)
;; interp. uma lista de tartarugas 
(define LDTAT1 empty)
(define LDTAT2 (cons TARTARUGA-INICIAL1 (cons TARTARUGA-INICIAL2 empty))) 
(define LDTAT3 (cons TARTARUGA-INICIAL2 (cons TARTARUGA-INICIAL1 empty)))

;; Template
#;
(define (fn-for-ldplat ldplat)
  (cond [(empty? ldplat) (...)]                         ;CASO BASE (CONDIÇÃO DE PARADA)
        [else (... (first ldplat)                       ;plataforma
                   (fn-for-ldplat (rest ldplat)))]))    ;RECURSÃO EM CAUDA


;;**********************************************************************************************************************************
;;Definiçoes de dados de stars

(define-struct qtdMoeda (x y cont) #:transparent)

;; qtdMoeda e (make-qtdMoeda Numero+ Numero+ Numero+)
;; interp. representa o placar onde mostra a quantidade de moedas que o mario tem

;; exemplos
(define QTD-MOEDA-0 (make-qtdMoeda X-QTD-MOEDA Y-QTD-MOEDA 0))

;; Template
#;
(define (fn-para-qtdMoeda m)
  (... (qtdMoeda-x m)
       (qtdMoeda-y m)
       (qtdMoeda-cont m))
  )


;;**********************************************************************************************************************************
;;Definiçoes de dados de stars

(define-struct qtdStar (x y cont) #:transparent)

;; qtdVida e (make-qtdStar Numero+ Numero+ Numero+)
;; interp. representa o placar onde mostra a quantidade de estrela que o mario tem

;; exemplos
(define QTD-STAR-0 (make-qtdStar X-QTD-STAR Y-QTD-STAR 0))
(define QTD-STAR-4 (make-qtdStar X-QTD-STAR Y-QTD-STAR 4))

;; Template
#;
(define (fn-para-qtdMoeda e)
  (... (qtdStar-x e)
       (qtdStar-y e)
       (qtdStar-cont e))
  )

;;**********************************************************************************************************************************
;; Definiçoes de dados da estrela do mario

(define-struct estrela (x dx y dy) #:transparent)

;; Star e (make-estrela Numero+ Numero Numero+ Numero+
;; interp. sao as informacoes da estrela do mario,as posicoes (x,y) e as velocidades dx e dy

;; exemplos : 
(define STAR1 (make-estrela (+ (- LARGURA (* 2 (image-width TUBO-ESQUERDO))) MEIO-W-STAR)
                            7
                            (- (image-height TUBO-ESQUERDO) MEIO-H-STAR)
                            0))

;template 
#; 
(define (fn-para-etrela s)
  (...  (estrela-x s)
        (estrela-dx s)
        (estrela-y s)
        (estrela-dy s)))


;;**********************************************************************************************************************************
;;Definiçoes de dados da Lista de Stars

; ListaDeStars é uma dessas:
;; - empty
;; - (cons star ListaDeStars)
;; interp. uma lista de estrelas 
(define LDE1 empty)
(define LDE2 (cons STAR1 LDE1))

;; Template
#;
(define (fn-for-ListaDeStars ldv)
  (cond [(empty? lds) (...)]                         ;CASO BASE (CONDIÇÃO DE PARADA)
        [else (... (first lds)                       ;plataforma
                   (fn-for-lds (rest lds)))]))       ;RECURSÃO EM CAUDA


;;**********************************************************************************************************************************
;;Definicoes de dados do Jogo

(define-struct jogo (mario plataformas game-over? spawn-tat1 spawn2 tartarugas vidas stars qtdVidas contStar moedas spawn-moeda qtdStars qtdMoedas)#:transparent)
;; Jogo é (make-jogo mario ListaDeplataforma Boolean Numero)
;; interp. representa um jogo que tem um mario
;; ,algumas plataformas e varias tartarugas.

(define JOGO-INICIAL (make-jogo MARIO-INICIAL
                                (list PLATAFORMA-ONE)
                                #false
                                1
                                1
                                LDTAT1
                                LDV2
                                LDE2
                                QTD-VIDA0
                                0
                                LDM1
                                1
                                QTD-STAR-0
                                QTD-MOEDA-0))
(define JOGO-MEIO (make-jogo MARIO-ANTES-VIRAR
                                (list PLATAFORMA-TWO)
                                #false
                                1
                                1
                                LDTAT1
                                LDV2
                                LDE2
                                QTD-VIDA0
                                0
                                LDM1
                                1
                                QTD-STAR-0
                                QTD-MOEDA-0))
(define JOGO-DANDO-RUIM (make-jogo
                   (make-mario (- (/ LARGURA 2) MEIO-W-PLATAFORMA -5) 10 (/ ALTURA 2) 0)
                   (list PLATAFORMA-TWO)
                   #false
                   1
                   1
                   LDTAT1
                   LDV2
                   LDE2
                   QTD-VIDA0
                   0
                   LDM1
                   1
                   QTD-STAR-0
                   QTD-MOEDA-0))
(define JOGO-DEU-RUIM (make-jogo
                   (make-mario (- (/ LARGURA 2) MEIO-W-PLATAFORMA -5) 10 (/ ALTURA 2) 0)
                   (list PLATAFORMA-TWO)
                   #true
                   0
                   0
                   LDTAT1
                   LDV2
                   LDE2
                   QTD-VIDA0
                   0
                   LDM1
                   1
                   QTD-STAR-0
                   QTD-MOEDA-0))
(define JOGO-ACABOU (make-jogo MARIO-MEIO
                               (list PLATAFORMA-TWO)
                               #true
                               0
                               0
                               LDTAT1
                               LDV2
                               LDE2
                               QTD-VIDA0
                               0
                               LDM1
                               1
                               QTD-STAR-0
                               QTD-MOEDA-0))

(define JOGO-INICIAL-N-PLATAFORMAS (make-jogo MARIO-INICIAL
                                LDPLAT-J1
                                #false
                                1
                                1
                                LDTAT1
                                LDV2
                                LDE2
                                QTD-VIDA0
                                0
                                LDM1
                                1
                                QTD-STAR-0
                                QTD-MOEDA-0))

(define JOGO-INICIAL-EXEMPLE (make-jogo MARIO-INICIAL
                                empty
                                #false
                                1
                                1
                                LDTAT1
                                LDV2
                                LDE2
                                QTD-VIDA0
                                0
                                LDM1
                                1
                                QTD-STAR-0
                                QTD-MOEDA-0))

(define JOGO-NO-INICIO (make-jogo MARIO-CHAO
                                     LDPLAT-PRINCIPAL
                                     #false
                                     1
                                     1
                                     LDTAT2
                                     LDV2
                                     LDE2
                                     QTD-VIDA0
                                     4 
                                     LDM1
                                     1
                                     QTD-STAR-4
                                     QTD-MOEDA-0))

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


 