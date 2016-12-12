#lang racket

(require 2htdp/universe)
(require "jogo.rkt") 
(require "dados.rkt")

(provide (all-defined-out))
;**********************************************************************************************************************************         
;;Main
 
;; Jogo -> Jogo
;; inicie o Jogo com (main JOGO-NO-INICIO)        
 
(define (main j)
  (big-bang j                                    ; Jogo
            (on-tick proximo-jogo)               ; Jogo -> Jogo
                                                   ;(retorna um novo estado do jogo dado atual a cada tick do clock)
            (to-draw desenha-jogo)               ; Jogo -> Image
                                                   ;(retorna uma imagem que representa o estado atual do Jogo)
            (on-key trata-tecla-press)           ; Jogo KeyEvent -> Jogo
                                                   ;(retorna um novo estado do jogo dado o estado atual e uma interação com o teclado)
            (on-release trata-tecla-solta)))     ; Jogo keyEvent -> Jogo
                                                   ;(retorna um novo estado do jogo dado o estado atual e uma interação com o teclado)