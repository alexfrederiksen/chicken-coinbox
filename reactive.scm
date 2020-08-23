;; author: Alexander Frederiksen
;; date: August 23, 2020

;; ------------------------------------------------------------------------------
;; --- reactive library ---------------------------------------------------------
;; ------------------------------------------------------------------------------

;; INPUTS :: (subscription code, signal) pair
;; OUTPUTS :: a -> ()
(define-record signal state f inputs outputs)

(define registered-signals (make-parameter '()))

(define ((invoke-outputs outputs) . args)
  (for-each (lambda (output)
              ;; invoke second slot of output
              (apply (cdr output) args))
            outputs))

(define (invoke signal . args)
  (let [(emit (invoke-outputs (signal-outputs signal)))]
    (apply (signal-f signal)
           (signal-state signal) emit args)))



(define (subscribe signal callback)
  (let [(id (gensym))]
    ;; add id, signal pair to outputs
    (signal-outputs-set! signal
                         (cons (cons id callback)
                               (signal-outputs signal)))
    ;; return id
    id))

(define (subscribe-as-signal signal-name subscriber . args)
  (let* [(signal (eval signal-name))
         (id (subscribe signal
                       (lambda (x)
                         (apply invoke subscriber x args))))]
    ;; add input to subscriber
    (signal-inputs-set! subscriber
                        (cons (cons signal-name id)
                              (signal-inputs subscriber)))))

(import srfi-1)

(define (unsubscribe signal id)
  (signal-outputs-set! signal
                       (alist-delete! id (signal-outputs signal))))

(define (registered-signal? signal-sym)
  (member signal-sym (registered-signals)))

(define (reconnect-signal old-signal new-signal)
  ;; pass old signal's outputs to the new signal
  (signal-outputs-set! new-signal (signal-outputs old-signal))
  ;; remove old signal's inputs i.e. unsubscribe
  (for-each (lambda (sub)
              (unsubscribe (eval (car sub)) (cdr sub)))
            (signal-inputs old-signal)))

(define (register-signal signal-sym signal)
  (if (registered-signal? signal-sym)
      ;; reconnect
      (begin
        (reconnect-signal (eval signal-sym) signal)
        (print "Reconnected " signal-sym " after redefinition."))
      ;; otherwise add signal
      (begin
        (registered-signals (cons signal-sym (registered-signals)))
        (print "Defined new signal " signal-sym "."))))

(define (make-signal* state f . signals-to-subscribe)
  (let [(signal (make-signal state f '() '()))]
    ;; and subscribe to all the signals
    (for-each (cut subscribe-as-signal <> signal) signals-to-subscribe)
    ;; and return the signal
    signal))

;; --- signal constructors ------------------------------------------------------

(define (signal-source f)
  (make-signal* #f (lambda (state emit x)
                     (emit (f x)))))

(define (signal-source* f)
  (make-signal* #f (lambda (state emit x)
                     (f emit x))))

(define (signal-map f . inputs)
  (apply make-signal* #f
         (lambda (state emit x)
           (emit (f x)))
         inputs))

(define (signal-map* f . inputs)
  (apply make-signal* #f
         (lambda (state emit x)
           (f emit x))
         inputs))

;; strictly for signal-map/signal-map*
(define (input-option-emittable #!key
                                (filter #f)
                                (read-each #f)
                                (read-each-only #f)
                                (from #f))
  (cond
   [read-each (if filter
                  (lambda (emit x)
                    (for-each-component read-each
                                        (lambda (id comp)
                                          (when (filter comp) (emit x id comp)))))
                  (lambda (emit x)
                    (for-each-component read-each (cut emit x <> <>))))]
   [read-each-only (if filter
                       (lambda (emit x)
                         (for-each-component read-each-only
                                             (lambda (id comp)
                                               (when (filter comp) (emit id comp)))))
                       (lambda (emit x)
                         (for-each-component read-each-only emit)))]
   [from from]
   [filter (lambda (emit x)
             (when (filter x) (emit x)))]
   [else (lambda (emit x) (emit x))]))


(define (attach-input-options f . options)
  (let [(attachment (apply input-option-emittable options))]
    (lambda (x) (attachment f x))))

(define (attach-input-options* f . options)
  (let [(attachment (apply input-option-emittable options))]
    (lambda (emit x)
      (attachment (cut f emit <...>) x))))

;; TODO
(define (signal-multimap f . inputs)
  (let [(signal (lambda (x) x))] signal))

;; --- utility functions --------------------------------------------------------

;; "composes" FS procedure, short-circuiting if #f appears at any point
(define (try-step x . fs)
  (if (or (null? fs) (not x))
      x
      (apply try-step ((car fs) x) (cdr fs))))

;; ------------------------------------------------------------------------------
;; --- component library --------------------------------------------------------
;; ------------------------------------------------------------------------------

;; component types with AUTO-ADD? enabled will not be initialized
;; when automatically added to an object, instead #f shall be used as a
;; placeholder
(define-record component-type name dependencies auto-add?)

(import srfi-69)

;; a place for all registered component types
(define component-type-table
  (make-parameter (make-hash-table)))

(define (component-types)
  (hash-table-values (component-type-table)))

(define (find-component-type name)
  (hash-table-ref/default (component-type-table) name #f))

(define (add-component-type component-type)
  (hash-table-set! (component-type-table)
                   (component-type-name component-type)
                   component-type))

(define (make-component-table)
  (make-hash-table))

(define current-components
  (make-parameter (make-component-table)))

(define (for-each-component component-name f)
  ;; grab table for component, then walk
  (try-step (hash-table-ref/default (current-components) component-name #f)
            (cut hash-table-for-each <> f) ))

(define (has-component? obj-id component-name)
  (try-step (hash-table-ref/default (current-components) component-name #f)
            (cut hash-table-exists? <> obj-id)))

(define (find-component obj-id component-name)
  (let [(component
         ;; perform a careful step through hash-table referencing
         (try-step (hash-table-ref/default (current-components)
                                           component-name #f)
                   (cut hash-table-ref/default <> obj-id #f)))]
    (if component
        component
        (print obj-id " does not have the requested " component-name " component."))))

(define (append-component-table component-name obj-id component)
  (hash-table-update! (current-components) component-name
                      ;; append component to component's table
                      (lambda (component-table)
                        (hash-table-set! component-table obj-id component)
                        component-table)
                      ;; otherwise make a new table if missing
                      make-hash-table))

(define (satisfies-dependencies? obj-id component-type)
  ((list-of? (cut has-component? obj-id <>))
   (component-type-dependencies component-type)))

(define (add-auto-components obj-id)
  (for-each (lambda (component-type)
              ;; add component to obj-id with placeholder, #f value
              (add-component obj-id
                             (component-type-name component-type)
                             #f))
            ;; filter all auto-add component types that
            ;; are dependency satisfied
            (filter (conjoin component-type-auto-add?
                             (cut satisfies-dependencies? obj-id <>))
                    (component-types))))

(define show-duplicate-component-debug? (make-parameter #t))
(define show-component-debug? (make-parameter #f))

(define (add-component obj-id name component)
  (cond [(not (satisfies-dependencies? obj-id (find-component-type name)))
         (print obj-id " does not have all dependencies for " name " component.")
         #f]

        [(has-component? obj-id name)
         (when (show-duplicate-component-debug?)
           (print obj-id " already has a " name " component."))
         #f]
        [else
         ;; add the component
         (append-component-table name obj-id component)
         ;; add auto components
         (parameterize [(show-duplicate-component-debug? #f)]
           (add-auto-components obj-id))
         (when (show-component-debug?)
           (print "Added " name " component to " obj-id "."))
         #t]))

(define (remove-component obj-id name)
  (try-step (hash-table-ref/default (current-components) name #f)
            (cut hash-table-delete! <> obj-id)
            (lambda _
              (when (show-component-debug?)
                (print "Removed " name " component from " obj-id ".")))))

(define (add-object obj-id name-component-list)
  (for-each (lambda (name-component)
              ;; add and evaluate component
              (add-component obj-id
                             (car name-component)
                             (eval (cdr name-component))))
            name-component-list))

(define (create-object name-component-list)
  (let [(obj-id (gensym 'obj))]
    (add-object obj-id name-component-list)
    (print "Create new blank object " obj-id ".")
    obj-id))

(define (destroy-object obj-id)
  (for-each (o (lambda (name)
                 (remove-component obj-id name))
               component-type-name)
            (component-types))
  (print "Destroyed object " obj-id "."))


;; --- syntax definitions -------------------------------------------------------

(define-syntax define-signal
  (syntax-rules ()
    [(define-signal name value)
     (define name (let [(signal value)]
                    (register-signal (quote name) signal)
                    signal))]
    ))

(define-syntax define-component
  (syntax-rules (depends-on)
    [(define-component (name fields ...) depends-on dependencies ...)
     (begin
       (define-record name fields ...)
       (add-component-type (make-component-type (quote name) '(dependencies ...) #f)))
     ]
    [(define-component (name fields ...))
     (begin
       (define-record name fields ...)
       (add-component-type (make-component-type (quote name) '() #f)))
     ]))

(define-syntax define-auto-component
  (syntax-rules (depends-on)
    [(define-component (name fields ...) depends-on dependencies ...)
     (begin
       (define-record name fields ...)
       (add-component-type (make-component-type (quote name) '(dependencies ...) #t)))
     ]
    [(define-component (name fields ...))
     (begin
       (define-record name fields ...)
       (add-component-type (make-component-type (quote name) '() #t)))
     ]))

(define-syntax define-map-signal
  (syntax-rules ()
    [(define-map-signal name (inputs ...) options ... f)
     (define-signal name (apply signal-map (attach-input-options f options ...)
                                     '(inputs ...)))
     ]))

(define-syntax define-map-signal*
  (syntax-rules ()
    [(define-map-signal* name (inputs ...) options ... f)
     (define-signal name (apply signal-map* (attach-input-options* f options ...)
                                      '(inputs ...)))
     ]))

;; has no inputs
(define-syntax define-event
  (syntax-rules ()
    [(define-event name options ... f)
     (define-map-signal name () options ... f)
     ]))

;; has no inputs (right now is a synonym for DEFINE-SOURCE)
(define-syntax define-event*
  (syntax-rules ()
    [(define-event name options ... f)
     (define-map-signal* name () options ... f)]
    ))

;; has no outputs
(define-syntax define-sink
  (syntax-rules ()
    [(define-sink name (inputs ...) options ... f)
     (define-map-signal name (inputs ...) options ... f)]))

;; defines an order for invoking various sources
(define-syntax define-order
  (syntax-rules ()
    [(define-order name value commands ...)
     (name)]
    ))


;; --- debugging tools ----------------------------------------------------------

(import (chicken format))
(import srfi-13)

(define (print-components)
  (hash-table-for-each (current-components)
                       (lambda (name id-table)
                         (printf "~A: " (string-pad-right (symbol->string name) 20))
                         (apply print (intersperse (hash-table-keys id-table) ", ")))))


;; --- testing suite ------------------------------------------------------------

#|
(define-event trigger
  (lambda (x) x))

(define-event* trigger*
  (lambda (emit x)
    (emit x)))

(define-sink printer (trigger*)
  (lambda (x)
    (print x)))

(signal-inputs printer)

(invoke trigger* "hi")

(define-auto-component (c-framable) depends-on
  c-frame)

(define-auto-component (c-obj))

(define-component (c-frame x y w h))

(define-component (c-renderable) depends-on
  c-frame)

(define-component (c-money amount))


(define-map-signal frame-logger ()
  read-each: 'c-frame
  filter: (lambda (frame) (> (c-frame-x frame) 2))
  (lambda (id frame)
    (print "Frame x of " id " is " (c-frame-x frame))))

(define player (create-object '((c-frame . (make-c-frame 1 2 3 4))
                                (c-renderable . (make-c-renderable)))))

(define enemy (create-object '((c-frame . (make-c-frame 6 6 6 6))
                               (c-renderable . (make-c-renderable)))))

(define coin (create-object '((c-money . (make-c-money 20)))))


(destroy-object coin)

(invoke frame-logger 1)

(print-components)
|#


