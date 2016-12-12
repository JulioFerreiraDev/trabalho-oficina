#lang racket

(require 2htdp/image)

(provide (all-defined-out))

;; =================================================================================================================================
;;CONTANTES 
;; Constantes do cenario:

(define LARGURA 1000)  
(define ALTURA 667) 
(define TUBO-DIREITO-AMARELO  (scale/xy 1.275 2.66 (bitmap "img-tubo-amarelo.png")))
(define TUBO-ESQUERDO-AMARELO (rotate 180 TUBO-DIREITO-AMARELO))
(define TUBO-DIREITO  (bitmap "img-tubo-verde-direito.png")) 
(define TUBO-ESQUERDO  (rotate 180 TUBO-DIREITO))
(define PRE-CENARIO1 (bitmap "img-fundo.jpg"))

(define FUNDO-F2 (scale/xy 2.085 1.853 (bitmap "img-fundo2.png")))
(define PRE-CENARIO-3 (place-image TUBO-ESQUERDO-AMARELO 51 52 FUNDO-F2))
(define PRE-CENARIO-4 (place-image TUBO-DIREITO-AMARELO (- LARGURA 51) 52 PRE-CENARIO-3))
(define PRE-CENARIO-5 (place-image TUBO-ESQUERDO-AMARELO 51 (- ALTURA 52) PRE-CENARIO-4)) 
(define PRE-CENARIO-6 (place-image TUBO-DIREITO-AMARELO (- LARGURA 51) (- ALTURA 52) PRE-CENARIO-5))
(define FUNDO-2 PRE-CENARIO-6)

(define PRE-CENARIO2 (scale 5 PRE-CENARIO1))
(define FUNDO-CENARIO (scale (/ LARGURA (image-width PRE-CENARIO1)) PRE-CENARIO1))
(define PRE-CENARIO3 (place-image TUBO-ESQUERDO 62 52 FUNDO-CENARIO))
(define PRE-CENARIO4 (place-image TUBO-DIREITO (- LARGURA 62) 52 PRE-CENARIO3))
(define PRE-CENARIO5 (place-image TUBO-ESQUERDO 62 (- ALTURA 52) PRE-CENARIO4)) 
(define PRE-CENARIO6 (place-image TUBO-DIREITO (- LARGURA 62) (- ALTURA 52) PRE-CENARIO5))
(define CENARIO PRE-CENARIO6) 

(define CENARIO1 (rectangle LARGURA ALTURA "outline" "green"))
(define IMG-GAME-OVER (scale/xy 2.1 1.86 (bitmap "gameOver.jpg"))) 
(define GAME-OVER (place-image (text "Start again click ENTER" 30 "white") 
                               700 620 
                               (overlay IMG-GAME-OVER 
                                        CENARIO1)))  
(define Y-CHAO ALTURA)
(define ALTURA-TUBO (image-height TUBO-DIREITO))
(define LIMITE-DIREITO-CENARIO LARGURA)
(define LIMITE-ESQUERDO-CENARIO 0)

 
;;***********************************************************************************************************************************
;;Constante da plataforma

(define TIJOLO (scale (/ 50 57) (bitmap "IMG-1TIJOLO.png")))  
(define PLATAFORMA (beside TIJOLO TIJOLO TIJOLO TIJOLO TIJOLO TIJOLO))
(define IMG-PLATAFORMA (overlay PLATAFORMA (rectangle 300 43 "outline" "black")))
(define X-PLATAFORMA (/ LARGURA 2)) 
(define MEIO-W-PLATAFORMA (/ (image-width IMG-PLATAFORMA) 2 ))
(define MEIO-H-PLATAFORMA (/ (image-height IMG-PLATAFORMA) 2 )) 

;;**********************************************************************************************************************************
;;Constante da moeda

(define IMG-MOEDA1 (scale 2 (bitmap "MOEDA1.png")))
(define IMG-MOEDA2 (scale 2 (bitmap "MOEDA2.png")))
(define IMG-MOEDA3 (scale 2 (bitmap "MOEDA3.png")))
(define IMG-MOEDA4 (scale 2 (bitmap "MOEDA4.png")))

(define MEIO-H-MOEDA (/ (image-height IMG-MOEDA1) 2))
(define MEIO-W-MOEDA (/ (image-width IMG-MOEDA1) 2))
(define T-SPAWN-MOEDA 1)
(define X-QTD-MOEDA (+ (/ LARGURA 2) 100))
(define Y-QTD-MOEDA 40)


;;**********************************************************************************************************************************
;;Constante da tartaruga

(define IMG-TARTARUGA-VOLTANDO0 (scale 1.5 (bitmap "TAT-VOLTANDO0.png"))) 
(define IMG-TARTARUGA-VOLTANDO1 (scale 1.5 (bitmap "TAT-VOLTANDO1.png")))
(define IMG-TARTARUGA-INDO0 (flip-horizontal IMG-TARTARUGA-VOLTANDO0))
(define IMG-TARTARUGA-INDO1 (flip-horizontal IMG-TARTARUGA-VOLTANDO1))
(define IMG-TARTARUGA-MORTA-CAINDO (bitmap "tat-morta-caindo.png"))
(define IMG-TARTARUGA-MORTA-PARADA (rotate 180 IMG-TARTARUGA-MORTA-CAINDO))
(define VELOCIDADE-TARTARUGA-VIVA 5)
(define DY-TAT-MORTA 8) 
(define MEIO-W-TARTARUGA (/ (image-width IMG-TARTARUGA-INDO0) 2 ))
(define MEIO-H-TARTARUGA (/ (image-height IMG-TARTARUGA-INDO0) 2 ))
(define Y-CHAO-TARTARUGA (- Y-CHAO MEIO-H-TARTARUGA)) 
(define LIMITE-DIREITO-TARTARUGA (- LARGURA MEIO-W-TARTARUGA))
(define LIMITE-ESQUERDO-TARTARUGA MEIO-W-TARTARUGA)
(define LIMITE-BAIXO-TARTARUGA (- ALTURA MEIO-H-TARTARUGA))
(define T-SPAWN-TAT1 170)


;;**********************************************************************************************************************************
;;Constantes do mario :

(define IMG-MARIO-INDO0 (scale 1.7 (bitmap "mario-indo0.png")))
(define IMG-MARIO-INDO1 (scale 1.7 (bitmap "mario-indo1.png"))) 
(define IMG-MARIO-VOLTANDO0 (flip-horizontal IMG-MARIO-INDO0))
(define IMG-MARIO-VOLTANDO1 (flip-horizontal IMG-MARIO-INDO1)) 
(define MEIO-W-MARIO (/ (image-width IMG-MARIO-INDO0) 2 ))
(define MEIO-H-MARIO (/ (image-height IMG-MARIO-INDO0) 2 ))
(define Y-CHAO-MARIO (- Y-CHAO MEIO-H-MARIO))
(define LIMITE-DIREITO (- LARGURA MEIO-W-MARIO))
(define LIMITE-ESQUERDO MEIO-W-MARIO)
(define LIMITE-BAIXO (- ALTURA MEIO-H-MARIO))  
(define LIMITE-CIMA MEIO-H-MARIO)
(define VEL-PADRAO-MARIO 13) 
(define DY-PULO -18) 
(define DY-QUICA -10)


;;**********************************************************************************************************************************
;;Constantes da estrela :
(define STAR (bitmap"star1.png"))
(define STAR2 (bitmap "star2.png"))
(define MEIO-W-STAR (/ (image-width STAR) 2 ))
(define MEIO-H-STAR (/ (image-height STAR) 2 ))
(define Y-CHAO-STAR (- Y-CHAO MEIO-H-STAR))
(define LIMITE-DIREITO-STAR (- LARGURA MEIO-W-STAR))
(define LIMITE-ESQUERDO-STAR MEIO-W-STAR)
(define LIMITE-BAIXO-STAR (- ALTURA MEIO-H-STAR))
(define T-SPAWN-STAR 500)
(define X-QTD-STAR (+ (/ LARGURA 2) 100)) 
(define Y-QTD-STAR 40)


;;**********************************************************************************************************************************
;;Constantes da vida :
(define VIDA-MARIO (scale 0.3 (bitmap "vida-mario.png")))
(define MEIO-W-VIDA (/ (image-width VIDA-MARIO) 2 ))
(define MEIO-H-VIDA (/ (image-height VIDA-MARIO) 2 ))
(define Y-CHAO-VIDA (- Y-CHAO MEIO-H-VIDA))
(define LIMITE-DIREITO-VIDA (- LARGURA MEIO-W-VIDA))
(define LIMITE-ESQUERDO-VIDA MEIO-W-VIDA)
(define LIMITE-BAIXO-VIDA (- ALTURA MEIO-H-VIDA))
(define T-SPAWN-VIDA 500)


;;**********************************************************************************************************************************
;;Constantes da quantidade de vidas estrelas ou moedas :
(define IMG-QTD0 (scale 1.5 (bitmap "placar0.png")))
(define IMG-QTD1 (scale 1.5 (bitmap "placar1.png")))
(define IMG-QTD2 (scale 1.5 (bitmap "placar2.png")))
(define IMG-QTD3 (scale 1.5 (bitmap "placar3.png")))
(define IMG-QTD4 (scale 1.5 (bitmap "placar4.png")))
(define IMG-QTD5 (scale 1.5 (bitmap "placar5.png")))
(define IMG-QTD6 (scale 1.5 (bitmap "placar6.png")))
(define IMG-QTD7 (scale 1.5 (bitmap "placar7.png")))
(define IMG-QTD8 (scale 1.5 (bitmap "placar8.png")))
(define IMG-QTD9 (scale 1.5 (bitmap "placar9.png")))
(define X-QTD (- (/ LARGURA 2) 50))
(define Y-QTD 40)   
