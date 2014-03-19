;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Facts

(deffacts trucks
 ; Number | Current Location | Destination | Space Avail | current time | action | package | eta: the expected time to arrival
 (truck 1 orlando none 10 idle none 0)
 (truck 2 orlando none 10 idle none 0)
 (truck 3 orlando none 8 idle none 0)
 (truck 4 orlando none 7 idle none 0))

(deffacts package-data
  ; Number | Depart City | Delivery City | Size | Order Arrival Time | Expected Delivery Time
  (package 1 key-west jacksonville 1 2 15 )
  (package 2 west-palm st-augustine 3 4 10 )
  (package 3 gainesville tallahassee 4 5 10 )
  (package 4 jacksonville orlando 2 8 18 )
  (package 5 ft-myers key-west 6 9 20 )
  (package 6 orlando lake-city 4 9 16 )
  (package 7 west-palm miami 5 9 16 )
  (package 8 miami ocala 4 10 20 )
  (package 9 gainesville orlando 7 11 17 )
  (package 10 tampa tallahassee 6 12 25 ))

(deffacts city-travel-time
  (travel-time orlando ocala 1)
  (travel-time orlando tampa 1)
  (travel-time orlando west-palm 3)
  (travel-time orlando st-augustine 2)
  (travel-time west-palm miami 2)
  (travel-time miami key-west 2)
  (travel-time west-palm ft-myers 3)
  (travel-time tampa ft-myers 2)
  (travel-time ocala gainesville 1)
  (travel-time st-augustine jacksonville 1)
  (travel-time gainesville lake-city 1)
  (travel-time lake-city tallahassee 2))

(deffacts time
  (old-time 0)
  (current-time 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
(deffunction inc (?n) (+ ?n 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rules

(defrule build-travel-time-graph
  (declare (salience 100))
  (travel-time ?middle-city $?interpaths2 ?other-city ?time2)
  (travel-time ?start-city $?interpaths1 ?middle-city ?time1)

  (neq ?start-city ?other-city)
  (neq ?middle-city ?other-city)
  (neq ?middle-city ?start-city)

  (not (test (member$ ?start-city $?interpaths1)))
  (not (test (member$ ?start-city $?interpaths2)))

  (not (test (member$ ?middle-city $?interpaths1)))
  (not (test (member$ ?middle-city $?interpaths2)))

  (not (test (member$ ?other-city $?interpaths1)))
  (not (test (member$ ?other-city $?interpaths2)))
  =>
  (assert
   (travel-time ?start-city $?interpaths1 ?middle-city $?interpaths2 ?other-city (+ ?time1 ?time2))))

(defrule commutatitve-property-of-travel
  (declare (salience 110))
  (travel-time ?start-city ?end-city ?time)
  (not (travel-time ?end-city ?start-city ?))
  =>
  (assert (travel-time ?end-city ?start-city ?time)))

(defrule set-initial-fast-paths
  (travel-time ?start-city $?interpaths ?final-city ?time)
  ;; And there isn't already a path that goes there
  (not (fastest-path ?start-city $? ?final-city ?))
  =>
  ;; save the fastest path
  (assert (fastest-path ?start-city $?interpaths ?final-city ?time)))

(defrule find-fastest-path
  ?path <- (travel-time ?start-city $?interpaths1 ?final-city ?time)
  ?fastest <- (fastest-path  ?start-city $?interpaths2&~$?interpaths1 ?final-city ?fastest-time)

  (test (< ?time ?fastest-time))

  =>
  (retract ?fastest)
  (assert (fastest-path ?start-city $?interpaths1 ?final-city ?time)))


(defrule start-truck-delivery
  (current-time ?current-time)
  ?available-truck <- (truck ?number ?current-location none ?space-avail idle none ?)
  (package ?package-number ?package-location ?package-dest ?package-size $? ?order-arrival-time ?)
  ;; make sure no other truck has this package
  (not (truck $? ?package-number ?))

  ;; Make sure the order arrival is <= to the current time
  (test (<= ?order-arrival-time ?current-time))

  ;; make sure we have space
  (test (>= ?space-avail ?package-size))

  ;; Find the fastest path, and set that as the eta
  (fastest-path ?current-location $? ?package-location ?eta)

  =>
  (retract ?available-truck)
  (assert (truck ?number ?current-location ?package-location
                 (- ?space-avail ?package-size)
                 dispatched ?package-number (+ ?current-time ?eta)))
  )

(defrule pick-up-package
  (current-time ?current-time)
  ?pickup-truck <- (truck ?number ? ?pack-loc ?space-avail dispatched ?package-number ?eta)
  (package ?package-number ?pack-loc ?final-dest $?)

  ;; Make sure we've made it
  (test (= ?current-time ?eta))

  ;; find the fastest-path
  (fastest-path ?pack-loc $? ?final-dest ?delivery-time)

  =>
  (retract ?pickup-truck)
  (assert
   (truck ?number ?pack-loc ?final-dest ?space-avail
          delivering ?package-number (+ ?delivery-time ?current-time)))
  )

(defrule update-time
  (declare (salience -10))
  ?current-time <- (current-time ?time)
  ?old-time <- (old-time ?)
  =>
  (retract ?current-time)
  (retract ?old-time)

  (assert (current-time (inc ?time)))
  (assert (old-time ?time)))

(defrule tick-trucks
  (current-time ?current-time)
  ?truck <- (truck ?number ?current-location ?dest ?space-avail ~?current-time ?action ?package ?eta)
  =>

  )


(run 1)
(run 20)
(facts)

(watch rules)
(reset)

(agenda)

(matches find-fastest-path)

(matches build-travel-time-graph)


;; (assert
;;  (foo 3)
;;  (foo 4)
;;  (foo 6)
;;  (foo 9)
;;  (foo 2)
;;  (foo 5)
;;  )

;; (defrule lowest
;;   (foo ?a)
;;   (foo ?b)
;;   (test (< ?a ?b))
;;   (not (foo ?c&:(< ?c ?a)))
;;   (not (lowest-foo ?))
;;   =>
;;   (bind ?asdf (lowest-foo ?))
;;   (assert (lowest-foo ?a)))


;; (defrule update-lowest
;;   ?foo <- (foo ?a)
;;   (not (foo ?c&:(< ?c ?a)))

;;   ?lowest <- (lowest-foo ?b)
;;   (test (< ?a ?b))
;;   =>
;;   (retract ?lowest)
;;   (assert (lowest-foo ?a)))

;; (run 1)
;; (facts)
