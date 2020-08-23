;; author: Alexander Frederiksen
;; date: August 23, 2020

(define (make-ring-buffer))


;; SYNC: old-a new-a old-b -> new-b
(define (number-sync old-a new-a old-b)
  (+ old-b (- new-a old-a)))

(define (net-smart-sync x accessor modifier sync)
  ;; create ring buffer
  (let [buffer (make-ring-buffer)]
    (lambda (i new-x)
      ;; find what x was, a packet send ago
      (let [old-x (find-recent buffer latency)]
        ;; sync each x to now in the buffer
        (map-recent buffer latency
                    (lambda (old-b)
                      (sync old-x new-x old-b)))
        ;; and sync the actual value
        (modifier x (old-x new-x sync (accessor x)))))
    ))


(import srfi-1)
(import srfi-18)
(import udp)
(import s11n)
(import (chicken port))
(import (chicken memory representation))

(include "jobs.scm")

(define-record network-remote ip port)
(define-record addressed-packet from data)

(define-record urgent-packet id data)
(define-record confirm-packet id)

(define local-socket (make-parameter (udp-open-socket)))

(define packet-listeners (make-job-system (make-mutex)
                                          (make-mutex)
                                          '()))
(define (send-packet remote packet)
  (udp-connect! (local-socket) (network-remote-ip remote) (network-remote-port remote))
  (udp-send (local-socket) (with-output-to-string (cut serialize obj))))


(define ((valid-confirmation? id) packet)
  (and (equal? (record-instance-type packet) 'confirm-packet)
       (equal? (confirm-packet-id packet) id)))

(define packet-id-count 0)
(define (gen-packet-id)
  (set! packet-id-count (add1 packet-id-count))
  packet-id-count)

(define confirmation-timeout 1.0) ;; seconds
(define max-send-tries 3) ;; seconds

;; returns success status
(define (send-and-confirm-packet remote packet try)
  (let* [(id (gen-packet-id))
         (job (make-filter-job (valid-confirmation? id)
                               (job-system-mutex packet-listeners)))
         (packet (make-urgent-packet id packet))]
    (add-job packet-listeners job)
    (send-packet remote packet)
    (let [(result (wait-for-job job confirmation-timeout))]
      (remove-job packet-listeners job)
      ;; make decision to retry
      (if result
          #t
          (if (< try max-send-tries)
              (send-and-confirm-packet remote packet (add1 try))
              #f)))
    ))

(define (confirm from urg-packet)
  (send-packet from (make-confirm-packet (urgent-packet-id urg-packet))))

;; TODO: make this better?
(define (process-packet from packet)
  (print "woah i just gotta " packet)
  (case (record-instance-type packet)
    ((urgent-packet)
     (confirm from urg-packet)
     (process-packet from (urgent-packet-data packet)))
    (else
     (invoke-jobs packet-listener (make-addressed-packet from packet)))))

(define (on-raw-packet host port raw)
  (on-net-obj (make-network-remote host port)
              (deserialize (open-input-string raw))))

(import (chicken gc))

(define (print-heap)
  (let* [(sizes (memory-statistics))
         (total-heap (vector-ref sizes 0))
         (used-heap (vector-ref sizes 1))]
    (print (exact->inexact (/ (* 100 used-heap) total-heap)) " percent of heap is used")))

(define server-port 5685)
(define max-msg-len 1024)

(define listener)
(define active-socket)


(define (make-listener socket port callback)
  (make-thread
   (lambda ()
     ;; bind my socket to the port
     (udp-bind! socket #f port)
     (let loop []
       ;; block just this thread until something happens on input,
       ;; otherwise recvfrom will block all the threads (real bad)
       (print "Waiting for packet...")
       (thread-wait-for-i/o! (udp-socket-fd socket) #:input)
       ;; receive some stuff
       (receive (len msg host port) (udp-recvfrom socket max-msg-len)
         (print "(" host ":" port ") sent " len " bytes.")
         (callback host port msg)
         (print-heap)
         ;; destroy packet!!!
         )
       (loop))
     ;; and close socket
     (udp-close-socket socket)
     (print "Closed server.")
     )))

;; TODO: maybe also terminate server?
(define (kill-listener socket)
  (udp-close-socket socket)
  (print "Killed listener."))

(define (start-server port)
  (set! active-socket (udp-open-socket))
  (set! listener (make-listener active-socket port on-raw-packet))
  (thread-start! listener))

(define (start-client)
  (set! active-socket (udp-open-socket))
  (set! listener (make-listener active-socket 0 on-raw-packet))
  (thread-start! listener))


;; define network components

;; server should send updates to clients
(define-component (c-server syncables))

;; client should send updates to server
(define-component (c-client syncables))

;; send object upon creation
(define-component (c-network))

(define-record component-packet obj-id component-name component)

(define (send-component remote obj-id component-name component)
  (send-packet ))

(define (connect-to-server host port)
  (let [(remote (make-network-remote host port))
        (confirmed (send-and-confirm-packet))]))

(define-event sync-server
  read-each: 'c-server
  (lambda (clients id syncer)
    (for-each (lambda (comp-name)
                ;; sync component, i.e. send it across the network
                (net-send-component id (find-component id comp-name)))
              (c-server-syncables syncer))))

(define-event sync-client
  read-each-only: 'c-client
  (lambda (syncer)
    (for-each (lambda (comp)
                ;; sync component, i.e. send it accross the network
                )
              (c-network-sync-components syncer))))
