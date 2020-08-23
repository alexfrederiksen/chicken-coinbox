;; author: Alexander Frederiksen
;; date: August 23, 2020

(import
  scheme
  (chicken base)
  (chicken random)
  (chicken condition)
  (chicken bitwise)
  (chicken repl)
  (chicken format)
  (chicken memory representation)
  (prefix glfw3 #:glfw)
  (prefix epoxy #:gl)
  (prefix soil #:soil)
  srfi-69 ;; hash tables
  gl-math
  gl-utils)

(include "render.scm")
(include "reactive.scm")

;; TODO: networking
;; tbh i don't think networking will be a thing until physics is properly reworked
;; (include "network.scm")

;; ### Utilities ################################################################

;; define some textures
(define t-polka)
(define t-coin)
(define t-coinbox)

;; TODO: streamline this, perhaps have a request system?
(define (load-textures)
  (set! t-polka (load-texure "polka.png"))
  (set! t-coin (load-texure "coin.png"))
  (set! t-coinbox (load-texure "coinbox.png")))

;; ### Logic ####################################################################

;; TODO: move components to different namespace?
(define-component (c-frame x y w h))

(define-component (c-velocity x y) depends-on
  c-frame)

(define-component (c-key-controller speed can-jump? left down up right) depends-on
  c-velocity)

(define-component (c-renderer texture scale-x scale-y) depends-on
  c-frame)

(define-component (c-bouncer) depends-on
  c-renderer)

(define-component (c-spinner) depends-on
  c-renderer)

(define-component (c-elastic relapse) depends-on
  c-velocity)

(define-component (c-collidable solid?) depends-on
  c-frame)

(define-component (c-pickup) depends-on
  c-frame)

;; can jerk an anchored point
(define-component (c-anchored-jerker anchored-x anchored-y jerk-x jerk-y res-ratio) depends-on
  c-frame)

(define-component (c-player))
(define-component (c-coin))
(define-component (c-coinbox))

;; TODO: and move these somewhere else?
(define (make-player x y left down up right)
  (let [(obj (create-object '()))]
    (add-component obj 'c-frame (make-c-frame x y 1 1))
    (add-component obj 'c-velocity (make-c-velocity 0 0))
    (add-component obj 'c-renderer (make-c-renderer t-polka 1 1))
    (add-component obj 'c-bouncer (make-c-bouncer))
    (add-component obj 'c-collidable (make-c-collidable #f))
    ;;(add-component obj 'c-elastic (make-c-elastic 0.9))
    (add-component obj 'c-key-controller (make-c-key-controller 0.2 #f left down up right))
    ;;(add-component obj 'c-spinner (make-c-spinner))
    (add-component obj 'c-player (make-c-player))
    obj))

(define (make-coinbox x y)
  (let [(obj (create-object '()))]
    (add-component obj 'c-frame (make-c-frame x y 1 1))
    (add-component obj 'c-renderer (make-c-renderer t-coinbox 1 1))
    (add-component obj 'c-collidable (make-c-collidable #t))
    (add-component obj 'c-anchored-jerker (make-c-anchored-jerker x y 0 0 0.8))
    (add-component obj 'c-coinbox (make-c-coinbox))
    obj))

(define (make-coin x y)
  (let [(obj (create-object '()))]
    (add-component obj 'c-frame (make-c-frame x y 0.3 0.3))
    (add-component obj 'c-velocity (make-c-velocity 0 0))
    (add-component obj 'c-renderer (make-c-renderer t-coin 1 1))
    (add-component obj 'c-elastic (make-c-elastic 0.9))
    (add-component obj 'c-spinner (make-c-spinner))
    (add-component obj 'c-coin (make-c-coin))
    ;;(add-component obj 'c-key-controller (make-c-key-controller 0.2 left down up right))
    obj))


(define (make-wall x y w h)
  (let [(obj (create-object '()))]
    (add-component obj 'c-frame (make-c-frame x y w h))
    (add-component obj 'c-collidable (make-c-collidable #t))))

(define (spawn-objects)
  (make-player -3 7 glfw:+key-a+ glfw:+key-s+ glfw:+key-w+ glfw:+key-d+)
  (make-player 3 7 glfw:+key-left+ glfw:+key-down+ glfw:+key-up+ glfw:+key-right+)
  (make-coin 0 5)
  (make-coinbox 0 6)
  ;; ground
  (make-wall 0 -1 30 1)
  ;; left
  (make-wall (- left-bound 1) 10 1 30)
  ;; right
  (make-wall (+ right-bound 1) 10 1 30)
  )

(define (find-object name)
  (car (hash-table-keys (hash-table-ref (current-components) name))))

(define (reset-objects)
  (current-components (make-component-table))
  (spawn-objects))

(define (key-down? key)
  (eq? (glfw:get-key (glfw:window) key) glfw:+press+))

(define jump-speed 0.8)

(define-event* e-input
  read-each-only: 'c-key-controller
  (lambda (emit id controller)
    (let [(vx (+ (if (key-down? (c-key-controller-left controller))
                     (- (c-key-controller-speed controller)) 0)
                 (if (key-down? (c-key-controller-right controller))
                     (c-key-controller-speed controller) 0)))
          (velocity (find-component id 'c-velocity))]
      ;; apply velocity
      (c-velocity-x-set! velocity vx)
      ;; only make neccessary changes to vy for gravity reasons
      (when (and (c-key-controller-can-jump? controller)
                 (key-down? (c-key-controller-up controller)))
        (c-key-controller-can-jump?-set! controller #f)
        (c-velocity-y-set! velocity jump-speed))

      ;; this will override y-velocity, just like x
      ;;(c-velocity-y-set! velocity vy)

      (unless (zero? vx)
        (emit id))
      )))

(define-sink s-flipper (e-input)
  filter: (cut has-component? <> 'c-renderer)
  (lambda (id)
    (let [(velocity (find-component id 'c-velocity))
          (renderer (find-component id 'c-renderer))]
      (cond
       [(positive? (c-velocity-x velocity))
        (c-renderer-scale-x-set! renderer 1)]
       [(negative? (c-velocity-x velocity))
        (c-renderer-scale-x-set! renderer -1)]))))

(define-map-signal printer ()
  (lambda (id)
    (let [(frame (find-component id 'c-frame))]
      (print "moving " (c-frame-x frame) " " (c-frame-y frame)))))

;; ### Collision System #########################################################

(define (c-frame-left frame)
  (- (c-frame-x frame) (c-frame-w frame)))

(define (c-frame-bottom frame)
  (- (c-frame-y frame) (c-frame-h frame)))

(define (c-frame-top frame)
  (+ (c-frame-y frame) (c-frame-h frame)))

(define (c-frame-right frame)
  (+ (c-frame-x frame) (c-frame-w frame)))

(define bottom-bound 0)
(define top-bound 10)
(define left-bound (camera-left-bound))
(define right-bound (camera-right-bound))

(define (clamp x low high)
  (cond [(< x low) low]
        [(> x high) high]
        [else x]))


(define (c-frame-intersect? frame1 frame2)
  (and (< (c-frame-left frame1) (c-frame-right frame2))
       (> (c-frame-right frame1) (c-frame-left frame2))
       (< (c-frame-bottom frame1) (c-frame-top frame2))
       (> (c-frame-top frame1) (c-frame-bottom frame2))))


(define-record collision id other-id)

(define epsilon 0.00001)
(define (axis-frame-resolution frame1 frame2 coord radius delta)
  (if (c-frame-intersect? frame1 frame2)
      ;; resolve collision
      (- (coord frame2)
         (coord frame1)
         (* (signum delta)
            (+ (radius frame1)
               (radius frame2)
               epsilon)))
      ;; otherwise there is no need
      0))


(define extra-fix 0.01)
(define (axis-resolution! id frame coord radius delta emit)
  (let [(max-resolution 0)]
    (for-each-component 'c-collidable
     (lambda (other-id collidable)
       (unless (eq? id other-id)
         (let* [(other-frame (find-component other-id 'c-frame))
                (res (axis-frame-resolution frame other-frame
                                            coord radius delta))]
           (unless (zero? res)
             ;; only account for its resolution if its solid
             (when (and (c-collidable-solid? collidable)
                        (> (abs res) (abs max-resolution)))
               (set! max-resolution res))
             ;; however emit solid or not
             (emit (make-collision id other-id)))))))
    (if (> (abs max-resolution) (abs delta))
        ;; we must have already been collided, cap the resolution
        (- (- delta) (* (signum delta) extra-fix))
        max-resolution)))

;; TODO apologize for writing C code and calling it scheme lmao
;; TODO actually just rewrite this as C code?
(define (move-and-collide! id frame dx dy x-collider y-collider emit)
  (let* [
         ;; move along x-axis
         (_ (c-frame-x-set! frame (+ (c-frame-x frame) dx)))
         ;; and perform axis resolution
         (res-x (axis-resolution! id frame c-frame-x c-frame-w dx emit))
         (_ (c-frame-x-set! frame (+ (c-frame-x frame) res-x)))
         (_ (unless (zero? res-x) (x-collider)))

         ;; move along y-axis
         (_ (c-frame-y-set! frame (+ (c-frame-y frame) dy)))
         ;; and again perform resolution
         (res-y (axis-resolution! id frame c-frame-y c-frame-h dy emit))
         (_ (c-frame-y-set! frame (+ (c-frame-y frame) res-y)))
         (_ (unless (zero? res-y) (y-collider)))]
    (void)))

(define (vec2-zero? v)
  (and (zero? (car v)) (zero? (cdr v))))

;; ID
;; NX: normal x-coordinate
;; NY: normal y-coordinate
(define-record axis-collision id nx ny)

;; used for client-side-prediction
(define (basic-physics-update! id deltatime)
  (let [(frame (find-component id 'c-frame))
        (vel (find-component id 'c-velocity))]
    ;; apply gravity
    (c-velocity-y-set! velocity (- (c-velocity-y velocity) 0.01))
    ;; move x
    (c-frame-x-set! frame (+ (c-frame-x frame)
                             (* (c-velocity-x vel)
                                deltatime)))
    ;; move y
    (c-frame-y-set! frame (+ (c-frame-y frame)
                             (* (c-velocity-y vel)
                                deltatime)))))

(define gravity 0.04)
(define-event* e-physics
  read-each-only: 'c-velocity
  (lambda (emit id velocity)
    (let [(frame (find-component id 'c-frame))]
      ;; apply gravity
      (c-velocity-y-set! velocity (- (c-velocity-y velocity) gravity))
      ;; and move and collide
      (move-and-collide!
       id frame
       (c-velocity-x velocity)
       (c-velocity-y velocity)
       ;; pass x collider
       (lambda ()
         (let [(vel-x (c-velocity-x velocity))]
           ;; default to resetting velocity to zero
           (c-velocity-x-set! velocity 0)
           (invoke e-axis-collided
                   (make-axis-collision id (- vel-x) 0))))
       ;; pass y collider
       (lambda ()
         (let [(vel-y (c-velocity-y velocity))]
           ;; default to resetting velocity to zero
           (c-velocity-y-set! velocity 0)
           (invoke e-axis-collided
                   (make-axis-collision id 0 (- vel-y)))))
       emit))))

;; ### End of Collision System ##################################################

(define-event e-axis-collided
  ;; AXIS-COLLISION: one coordinate is non-zero velocity at collision
  (lambda (collision)
    collision))

(define-sink elastic-reflect (e-axis-collided)
  filter: (o (cut has-component? <> 'c-elastic) axis-collision-id)
  (lambda (collision)
    (let* [(id (axis-collision-id collision))
           (elastic (find-component id 'c-elastic))
           (velocity (find-component id 'c-velocity))]

      (unless (zero? (axis-collision-nx collision))
        (c-velocity-x-set! velocity (* (c-elastic-relapse elastic)
                                       (axis-collision-nx collision))))

      (unless (zero? (axis-collision-ny collision))
        (c-velocity-y-set! velocity (* (c-elastic-relapse elastic)
                                       (axis-collision-ny collision)))))))

(define-sink ground-controllers (e-axis-collided)
  filter: (o (cut has-component? <> 'c-key-controller) axis-collision-id)
  (lambda (collision)
    (let* [(id (axis-collision-id collision))
           (controller (find-component id 'c-key-controller))]
      (when (positive? (axis-collision-ny collision))
        (c-key-controller-can-jump?-set! controller #t))
      )))

;; useful function for filtering different collision types
(define ((collision-filter c1 c2) emit collision)
  (cond
   ;; natural order
   ((and (has-component? (collision-id collision) c1)
         (has-component? (collision-other-id collision) c2))
    (emit (collision-id collision)
          (collision-other-id collision)))
   ;; reverse order
   ((and (has-component? (collision-id collision) c2)
         (has-component? (collision-other-id collision) c1))
    ;; fix the order
    (emit (collision-other-id collision)
          (collision-id collision)))))

(define-map-signal s-coin-pickup (e-physics)
  from: (collision-filter 'c-player 'c-coin)
  (lambda (player-id coin-id)
    (print "Player collided with coin.")
    (destroy-object coin-id)
    ;; make player longer
    #;(let [(frame (find-component player-id 'c-frame))]
      (c-frame-w-set! frame (+ 0.1 (c-frame-w frame)))
      (c-frame-h-set! frame (+ 0.8 (c-frame-h frame)))
      (c-frame-y-set! frame (+ 1.60 (c-frame-y frame)))
    )
    ))

(define pi 3.141592)
(define coin-spawn-speed 0.9)

(define (spawn-coin x y angle)
  (let* [(id (make-coin x y))
         (vel (find-component id 'c-velocity))]
    (c-velocity-x-set! vel (* (cos angle) coin-spawn-speed))
    (c-velocity-y-set! vel (* (sin angle) coin-spawn-speed))
    ))

(define coin-spawn-spread 1.0)
(define (spawn-coins x y angle amount)
  (when (positive? amount)
    (spawn-coin x y (+ angle (rand (- coin-spawn-spread) coin-spawn-spread)))
    (spawn-coins x y angle (sub1 amount))))

(define (debounce secs f)
  (let [(last-time 0)]
    (lambda args
      (let [(now (current-elapsed))]
        (when (> (- now last-time) secs)
          (set! last-time now)
          (apply f args))))))

(define-map-signal s-player-hit-box (e-physics)
  from: (collision-filter 'c-player 'c-coinbox)
  (debounce
   0.2
   (lambda (player-id coinbox-id)
     (print "Player collided with coinbox.")
     (let [(jerker (find-component coinbox-id 'c-anchored-jerker))]
       ;; jerk box a little
       (c-anchored-jerker-jerk-y-set! jerker 0.6)
       ;; spawn some coins
       (spawn-coins (c-anchored-jerker-anchored-x jerker)
                    (+ (c-anchored-jerker-anchored-y jerker) 2.0)
                    (/ pi 2.0) 3))
     )))

#;(define-map-signal s-pancake (e-physics)
  from: (collision-filter 'c-player 'c-player)
  (lambda (player1-id player2-id)
    (let [(frame1 (find-component player1-id 'c-frame))
          (frame2 (find-component player2-id 'c-frame))]
      (when (< (abs (- (c-frame-x frame1) (c-frame-x frame2))) 1)
        (cond
         [(>= (c-frame-y frame1) (c-frame-y frame2))
          ;; squash frame2
          (c-frame-h-set! frame2 (- (c-frame-h frame2) 0.1))]

         [(>= (c-frame-y frame2) (c-frame-y frame1))
          ;; squash frame2
          (c-frame-h-set! frame1 (- (c-frame-h frame1) 0.1))]
         )))))

;; currently, used for animations
(define (current-elapsed)
  (glfw:get-time))

(define-event e-render
  read-each-only: 'c-renderer
  (lambda (id renderer)
    (let [(frame (find-component id 'c-frame))]
      (render (c-renderer-texture renderer)
              (c-frame-x frame)
              ;; correct y coordinates
              (- (c-frame-y frame)
                 (* (c-frame-h frame)
                    (- 1 (c-renderer-scale-y renderer))))
              (* (c-frame-w frame)
                 (c-renderer-scale-x renderer))
              (* (c-frame-h frame)
                 (c-renderer-scale-y renderer)))
      )))

;; event for triggering "animation" stuff i.e. stuff after
;; physics update but before render
(define-event e-animation
  (lambda (x)
    (current-elapsed)))

;; TODO: utility, move it
(define (rand min max)
  (+ (* (- max min) (pseudo-random-real)) min))

(define coin-interval 1)
(define last-coin-time 0)

#;(define-sink rain-coins (e-animation)
  (lambda (elapsed)
    (when (and (> (floor elapsed) last-coin-time)
               (zero? (modulo (floor elapsed) coin-interval)))
      (let [(x (rand left-bound right-bound))
            (y top-bound)]
        (make-coin x y)
        (set! last-coin-time elapsed)))))

(define-sink bounce (e-animation)
  read-each: 'c-bouncer
  (lambda (elapsed id bouncer)
    (let [(renderer (find-component id 'c-renderer))]
      (c-renderer-scale-y-set! renderer
                               (+ 1 (* 0.05 (sin (* elapsed 10))))))))

(define-sink spin (e-animation)
  read-each: 'c-spinner
  (lambda (elapsed id spinner)
    (let [(renderer (find-component id 'c-renderer))]
      (c-renderer-scale-x-set! renderer
                               (sin (* elapsed 5))))))


;; keeps an object spring anchored to a position, allowing it
;; to be jerked around
(define-sink jerk (e-animation)
  read-each: 'c-anchored-jerker
  (lambda (elapsed id jerker)
    (let [(frame (find-component id 'c-frame))]
      ;; jerk x
      (c-frame-x-set! frame (+ (c-anchored-jerker-anchored-x jerker)
                               (c-anchored-jerker-jerk-x jerker)))
      ;; jerk y
      (c-frame-y-set! frame (+ (c-anchored-jerker-anchored-y jerker)
                               (c-anchored-jerker-jerk-y jerker)))

      ;; resolve jerk
      (c-anchored-jerker-jerk-x-set! jerker
                                     (* (c-anchored-jerker-res-ratio jerker)
                                        (c-anchored-jerker-jerk-x jerker)))
      (c-anchored-jerker-jerk-y-set! jerker
                                     (* (c-anchored-jerker-res-ratio jerker)
                                        (c-anchored-jerker-jerk-y jerker)))
      )))

;; ### Scenes ###################################################################

(define current-scene)

(define (invoke-game)
  (current-scene))

(define (scene-main)
  (invoke e-input #f)
  (invoke e-physics #f)
  (invoke e-animation #f)
  (invoke e-render #f))

(define (scene-pause)
  (with-objects main
                (invoke e-animation #f)
                (invoke e-render #f))
  (with-objects menu
                (invoke e-input #f)
                (invoke e-render #f)))

(set! current-scene scene-main)


;; (define say-stuff
;;   (coroutine (lambda (yield)
;;                (yield "hi")
;;                (yield "there")
;;                (yield "friend")
;;                )))
;;
;; (say-stuff)

;; ### REPL #####################################################################

;; breaks to a repl and returns using a continuation, stole this from
;; https://demonastery.org/2011/04/chicken-scheme-and-opengl/

(define return #f)
(define (break-to-repl)
  (print "Type (return) to resume rendering")
  (call/cc
   (lambda (k)
     (set! return (lambda () (k #f)))
     (repl))))


;; ### Keybindings ##############################################################

;; define keybindings
(define (keydown? key window event-key scancode action mods)
  (and (eq? key event-key) (eq? action glfw:+press+)))

(define (event->window window event-key scancode action mods)
  window)

(glfw:key-callback
 (lambda event
   (cond
    ((apply keydown? glfw:+key-q+ event)
     (glfw:set-window-should-close (apply event->window event) #t))

    ((apply keydown? glfw:+key-escape+ event)
     (break-to-repl)) )))

(glfw:window-focus-callback
 (lambda (window focused?)
   (print "window focus state changed: " focused?)
   #;(unless focused?
     (break-to-repl))))

;; ### Window Management ########################################################

(define (run-game)
  ;; create window
  (glfw:with-window
   (640 480 "Coinbox?" resizable: #f)
   ;; setup shaders
   (compile-shaders)

   ;; load textures
   (load-textures)
   (spawn-objects)

   (print "GL: " (gl:gl-version))
   (print "GLSL: " (gl:glsl-version))

   ;; build rect vao
   (mesh-make-vao! rect '((position . 0)
                          (color    . 1)
                          (coords   . 2)))
   (let loop ()
     (glfw:swap-buffers (glfw:window))
     (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
     (glfw:poll-events)

     ;; safely call invoke-game
     (call/cc (lambda (k)
                (with-exception-handler
                    (lambda (exn)
                      (print-call-chain)
                      (print "(" ((condition-property-accessor 'exn 'location) exn)
                             "): " ((condition-property-accessor 'exn 'message) exn))
                      (print "Saved ya!")
                      (break-to-repl)
                      (k #f))
                  invoke-game)))

     (unless (glfw:window-should-close (glfw:window))
       (loop)))))
