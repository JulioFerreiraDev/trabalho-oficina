#lang racket

(require rackunit)
(require "constantes.rkt")
(require "dados.rkt")
(require "jogo.rkt")

;; Exemplos

(check-equal? (qtd-anterior-vida QTD-VIDA0) (make-qtdVida 450 40 -1))

;--------------------------------------------------------------------------------------------------------------

(check-equal? (proxima-qtd-vida QTD-VIDA0) (make-qtdVida 450 40 1))
(check-equal? (proxima-qtd-vida (make-qtdVida 450 40 6)) (make-qtdVida 450 40 6))

;--------------------------------------------------------------------------------------------------------------

(check-equal? (proxima-qtd-moedas QTD-MOEDA-0) (make-qtdMoeda 600 40 1))

;--------------------------------------------------------------------------------------------------------------

(check-equal? (proxima-qtd-star QTD-STAR-0) (make-qtdStar 600 40 1))

;--------------------------------------------------------------------------------------------------------------

(check-equal? (spawn-vida LDV1) (list (make-vida (/ 349 2) 7 89 0)))

;--------------------------------------------------------------------------------------------------------------

(check-equal? (spawn-estrela LDE1) (list (make-estrela 756 7 91 0)))

;--------------------------------------------------------------------------------------------------------------
  
(check-equal? (spawn-tartaruga LDTAT1) (list (make-tartaruga 140 (/ 159 2) 5 0) (make-tartaruga 860 (/ 159 2) -5 0)))

;--------------------------------------------------------------------------------------------------------------

(check-equal? (morreu-tat TARTARUGA-INICIAL1) (make-tartaruga 140 (/ 175 2) 5 0))

;--------------------------------------------------------------------------------------------------------------

(check-equal? (repelir-mario MARIO-INICIAL) (make-mario -187 10 (/ 667 2) 0))
(check-equal? (repelir-mario (make-mario -187 -10 (/ 667 2) 0)) (make-mario 13 -10 (/ 667 2) 0))

;--------------------------------------------------------------------------------------------------------------

(check-equal? (quica MARIO-INICIAL) (make-mario 13 10 (/ 667 2) -10))

;--------------------------------------------------------------------------------------------------------------

(check-equal? (alguma-tat-chegou-no-final? LDTAT1) #false)

;--------------------------------------------------------------------------------------------------------------

(check-equal? (tat-chegou-no-final? TARTARUGA-INICIAL1) #false)
(check-equal? (tat-chegou-no-final? TARTARUGA-FINAL1) TARTARUGA-FINAL1)
(check-equal? (tat-chegou-no-final? TARTARUGA-FINAL2) TARTARUGA-FINAL2)

;--------------------------------------------------------------------------------------------------------------

(check-equal? (alguma-estrela-chegou-no-final? LDE1) #false)

;--------------------------------------------------------------------------------------------------------------

(check-equal? (estrela-chegou-no-final? STAR1) #false)
(check-equal? (estrela-chegou-no-final? (make-estrela (+ LIMITE-DIREITO-CENARIO 50) 7 Y-CHAO 0))
                                        (make-estrela (+ LIMITE-DIREITO-CENARIO 50) 7 Y-CHAO 0))

;--------------------------------------------------------------------------------------------------------------

(check-equal? (alguma-vida-chegou-no-final? LDV1) #false)

;--------------------------------------------------------------------------------------------------------------

(check-equal? (vida-chegou-no-final? VIDA1) #false)
(check-equal? (vida-chegou-no-final? (make-vida (+ LIMITE-DIREITO-CENARIO 50) 7 Y-CHAO 0))
                                        (make-vida (+ LIMITE-DIREITO-CENARIO 50) 7 Y-CHAO 0))

;--------------------------------------------------------------------------------------------------------------

(check-equal? (pulou-encima-alguma-tartaruga? MARIO-INICIAL LDTAT1) #false)

;--------------------------------------------------------------------------------------------------------------

(check-equal? (colisao-mario-tartaruga? MARIO-INICIAL TARTARUGA-INICIAL1) #false)

;--------------------------------------------------------------------------------------------------------------

(check-equal? (proximas-estrelas LDE1) empty)

;--------------------------------------------------------------------------------------------------------------

(check-equal? (proxima-estrela STAR1) (make-estrela 763 7 81 0))

;--------------------------------------------------------------------------------------------------------------

(check-equal? (estrela-encima-chao? STAR1) #false)

;--------------------------------------------------------------------------------------------------------------

(check-equal? (estrela-encima-plataforma? STAR1) #true)

;--------------------------------------------------------------------------------------------------------------

(check-equal? (proximas-vidas LDV1) empty)

;--------------------------------------------------------------------------------------------------------------

(check-equal? (proxima-vida VIDA1) (make-vida (/ 363 2) 7 89 0))

;--------------------------------------------------------------------------------------------------------------

(check-equal? (vida-encima-chao? VIDA1) #false)

;--------------------------------------------------------------------------------------------------------------

(check-equal? (proxima-moeda MOEDA1) (make-moeda 25 76 1))
(check-equal? (proxima-moeda (make-moeda 35 75 1)) (make-moeda 35 75 2))

;--------------------------------------------------------------------------------------------------------------

(check-equal? (proximas-tartarugas LDTAT1) empty)

;--------------------------------------------------------------------------------------------------------------

(check-equal? (proxima-tartaruga TARTARUGA-INICIAL1) (make-tartaruga 145 (/ 159 2) 5 0))
(check-equal? (proxima-tartaruga TARTARUGA-FINAL1) (make-tartaruga 872 (/ 1295 2) 0 0))

;--------------------------------------------------------------------------------------------------------------

(check-equal? (tartaruga-encima-chao? TARTARUGA-INICIAL1) #false)
(check-equal?  (tartaruga-encima-chao? TARTARUGA-FINAL1) #true)

;--------------------------------------------------------------------------------------------------------------

(check-equal? (vida-encima-plataforma? VIDA1) #true)

;--------------------------------------------------------------------------------------------------------------

(check-equal? (tartaruga-encima-plataforma? TARTARUGA-INICIAL1) #true)

;--------------------------------------------------------------------------------------------------------------

(check-equal? (colisao-algum-mario-moeda? MARIO-INICIAL LDM1) #false)

;--------------------------------------------------------------------------------------------------------------

(check-equal? (colisao-mario-moeda? MARIO-INICIAL MOEDA1) #false)

;--------------------------------------------------------------------------------------------------------------

(check-equal? (colisao-algum-mario-estrela? MARIO-INICIAL LDE1) #false)

;--------------------------------------------------------------------------------------------------------------

(check-equal? (colisao-mario-estrela? MARIO-INICIAL STAR1) #false)

;--------------------------------------------------------------------------------------------------------------

(check-equal? (colisao-algum-mario-vida? MARIO-MEIO LDV1) #false)

;--------------------------------------------------------------------------------------------------------------

(check-equal? (colisao-mario-vida? MARIO-MEIO VIDA1) #false)

;--------------------------------------------------------------------------------------------------------------

(check-equal? (colisao-algum-mario-tartaruga? MARIO-MEIO LDTAT1) #false)

;--------------------------------------------------------------------------------------------------------------

(check-equal? (colisao-mario-tartaruga? MARIO-INICIAL TARTARUGA-FINAL2) #false)

;--------------------------------------------------------------------------------------------------------------

(check-equal? (distancia-euclidiana 2 2 2 2) 0)
(check-equal? (distancia-euclidiana 5 6 7 6) 2)

;--------------------------------------------------------------------------------------------------------------

(check-equal? (distancia 10 20) 10)
(check-equal? (distancia 50 60) 10)
(check-equal? (distancia 60 50) -10)

;--------------------------------------------------------------------------------------------------------------

(check-equal? (proximo-mario MARIO-MEIO) (make-mario 510 10 (/ 667 2) (/ 107 100)))
(check-equal? (proximo-mario MARIO-NO-LIMITE) (make-mario 937 -50 283 0))

;--------------------------------------------------------------------------------------------------------------

(check-equal? (cai MARIO-MEIO) (make-mario 510 10 (/ 667 2) (/ 107 100))) 
(check-equal? (cai MARIO-INICIAL) (make-mario 23 10 283 0))
(check-equal? (cai MARIO-CHAO) (make-mario 13 0 638 0))


;--------------------------------------------------------------------------------------------------------------

(check-equal? (encima-chao? MARIO-INICIAL) #false)
(check-equal? (encima-chao? (make-mario 50 10 640 0)) #true)

;--------------------------------------------------------------------------------------------------------------

(check-equal? (encima-plataforma? MARIO-INICIAL) #true)
(check-equal? (encima-plataforma? MARIO-MEIO) #false)

;--------------------------------------------------------------------------------------------------------------

(check-equal? (encima-plataforma? MARIO-MEIO) #false)

;--------------------------------------------------------------------------------------------------------------

(check-equal? (trata-tecla-press JOGO-DEU-RUIM "\r")
              JOGO-NO-INICIO) 
(check-equal? (trata-tecla-press JOGO-DEU-RUIM "a") JOGO-DEU-RUIM)

;--------------------------------------------------------------------------------------------------------------

(check-equal? (trata-tecla-press-mario MARIO-INICIAL "right") (make-mario 13 13 (/ 667 2) 0))
(check-equal? (trata-tecla-press-mario MARIO-INICIAL "left") (make-mario 13 -13 (/ 667 2) 0)) 
(check-equal? (trata-tecla-press-mario MARIO-INICIAL "up") (make-mario 13 10 (/ 647 2) -18)) 
