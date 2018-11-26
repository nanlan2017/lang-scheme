#lang racket

;; 1 x 2 x ... x n
(define POWER
  (λ (n)
    (cond
      [(= n 0) 1]
      [#t (* n (POWER (- n 1)))])))

(POWER 5)
;---------------------------------------------------------------------------
;; 这个定义是错误的,无法使用
(define meta-power
  (λ (self n)    
    (cond
      [(= n 0) 1]
      [#t (* n (self (- n 1)))])))  

(meta-power POWER 3)    ;;;;    meta-power POWER === POWER   (柯里化)
;---------------------------------------------------------------------------
;; 将fact自身作为参数
(define self-power
  (λ (self n)    ;; self 又是P自己 （反正只是个纯函数而已）
    (cond
      [(= n 0) 1]
      [#t (* n (self self (- n 1)))]))) 

(self-power self-power 4)  ;;;   self-power self-power === POWER   <---  power_gen power_gen ==== POWER   
                                                                          ##########
; λ calculas [OK]
; ((λ (f x) (cond [(= x 0) 1] [#t (* x (f f (- x 1)))])) (λ (f x) (cond [(= x 0) 1] [#t (* x (f f (- x 1)))])) 6)

;==============================================================================================================================================
;;  所谓的不动点。
;;  即对于函数P来说power是这样一个“点”:当把P用到power身上的时候,得到的结果仍然还是power,
;;   也就是说,power这个“点”在P的作用下是“不动”的。
;; (meta-power power) ;;  === power

;; ----->   如何根据 meta-power, 算出 POWER

Y meta-power === POWER === meta-power POWER === meta-power (Y meta-power)
Y metaF      === f     === metaF f          === metaF (Y metaF)
Y F          === f     === F f              === F (Y F)              ;;  F f === f  <----------  f 就是不动点，我们就是要算出任意F 的不动点 f  (充要？)

###### Y F ===  F (Y F)                <---- 解出一个与F无关的Y算子, 使其恒成立
           ===  F (F (Y F))
           ===  ...                     
# 我们很容易写出F， 如果Y是固定的， 则 我们想要的真正递归的 f = Y F





###### 我们已知 power_gen power_gen === POWER
# 即  genF genF === f

############### genF f === metaF (f f)     ;;;<------------- 代入便知相同  self-power f  ===  meta-power (f f)
即   genF = \f -> metaF (f f)
得   genF genfF  === metaF (genF genF)

#####  gen  gen === F (gen gen)       # 
---->   从而  gen  gen  就是 F 的不动点，  ###  Y F = gen gen 


Y = \F -> gen gen
    where  gen = \f -> F (f f)
即 Y =  \F -> (\f -> F (f f)) (\f -> F (f f))


即 Y =  \y -> (\x -> y (x x)) (\x -> y (x x))

;==============================================================================================================================================
(define Y
  (λ (y)
    ((λ (x) (y (x x)))
     (λ (x) (y (x x))))))

(define (Y-power n)
  ((Y meta-power) n))  ;; Y 

;(Y-power 6)


证明： 对任意f , 有 f (Y f) == Y f
Y f = ((\x -> f (x x)) (\x -> f (x x)))    ;; belta[f/y]
    = ((\x -> f (x x)) (\a -> f (a a)))    ;; alpha[a/x]
    = f ((\a -> f (a a)) (\a -> f (a a)))  ;; belta[.../x]
    = f ((\x -> f (x x)) (\x -> f (x x)))  ;; alpha[x/a]
    = f (Y f)
证明毕。


已知：
;; meta-power %POWER = %POWER
%POWER = self-power self-power
self-power f = meta-power (f f)
求:用meta-power 表示 %POWER   ( %POWER = ...meta-power...)


--> %POWER = meta-power (self-power self-power)
           = m ((\f -> m (f f)) (\f -> m (f f)))
           = Y m




Y meta-power = meta-power (Y meta-power) 


递归函数

#meta-fact
(λ (f x) (cond [(= x 0) 1] [#t (* x (f (- x 1)))])) 

#Y meta-fact
(((λ (F) ((λ (x) (F (x x))) (λ (x) (F (x x))))) (λ (f x) (cond [(= x 0) 1] [#t (* x (f (- x 1)))]))) 5)





































