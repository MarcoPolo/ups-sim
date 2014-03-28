;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Templates

(deftemplate result
  "Log the results"
  (slot time)
  (slot package-number)
  (slot truck-number)
  (slot action)
  (slot arrival-location)
  (slot arrival-time))

(deftemplate delivered-package
  (slot number)
  (slot depart-city)
  (slot delivery-city)
  (slot size)
  (slot order-arrival-time)
  (slot expected-delivery-time)
  (slot actual-delivery-time))

(deftemplate package-avg-report
  (slot avg-wait)
  (slot num-packages-on-time)
  (slot num-packages-late)
  (slot avg-lateness-for-late-packages)
  (slot avg-lateness-for-packages))

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
  (package 1 key-west jacksonville 1 2 15)
  (package 2 west-palm st-augustine 3 4 10)
  (package 3 gainesville tallahassee 4 5 10)
  (package 4 jacksonville orlando 2 8 18)
  (package 5 ft-myers key-west 6 9 20)
  (package 6 orlando lake-city 4 9 16)
  (package 7 west-palm miami 5 9 16)
  (package 8 miami ocala 4 10 20)
  (package 9 gainesville orlando 7 11 17)
  (package 10 tampa tallahassee 6 12 25))

(deffacts city-travel-time
  (travel-time orlando ocala 1)
  (travel-time orlando tampa 1)
  (travel-time orlando west-palm 3)
  (travel-time orlando st-augustine 2)
  (travel-time west-palm miami 2)
  (travel-time west-palm st-augustine 3)
  (travel-time miami key-west 2)
  (travel-time west-palm ft-myers 3)
  (travel-time tampa ft-myers 2)
  (travel-time ocala gainesville 1)
  (travel-time st-augustine jacksonville 1)
  (travel-time gainesville lake-city 1)
  (travel-time lake-city tallahassee 2))

(deffacts time
  (current-time 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
(deffunction inc (?n) (+ ?n 1))

(deffunction pad-str (?str ?len)
  (bind ?str-len (length ?str))
  (if (< ?str-len ?len)
    then
    (pad-str (str-cat ?str " ") ?len)
    else
    ?str))

(deffunction print-results ()

  (printout t crlf crlf
            "--------------------------------------------------------------------------------" crlf
            "                                  Simulation results                            " crlf
            "--------------------------------------------------------------------------------" crlf)
  (printout t crlf
            "Time" tab "Package #" tab "Truck #" tab "Action" tab tab "Arrival Location" tab "Arrival time" crlf )
  (do-for-all-facts ((?r result)) TRUE
                    (printout t
                              ?r:time tab ?r:package-number tab tab
                              ?r:truck-number tab ?r:action tab
                              (pad-str ?r:arrival-location 20) tab
                              ?r:arrival-time crlf)))

(deffunction time-diff-btwn-2-events (?truck-number ?event1 ?event2)
  (bind ?total-time 0)
  (bind ?last-first-event-time 0)
  (do-for-all-facts ((?r result)) (and (= ?truck-number ?r:truck-number)
                                       (or (eq ?r:action ?event1)
                                           (eq ?r:action ?event2)))
                    (if (eq ?r:action ?event1)
                      then
                      (bind ?last-first-event-time ?r:time)
                      else
                      (bind ?total-time (+ ?total-time (- ?r:time ?last-first-event-time)))))
  (create$ ?total-time ?last-first-event-time))

(deffunction first (?lst)
  (nth$ 1 ?lst))


(deffunction get-slot-of-with-number (?fact-name ?number ?slot)
  (fact-slot-value
   (first (find-fact ((?p ?fact-name)) (= ?number ?p:number)))
   ?slot))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rules

(defrule update-time
  (declare (salience -10))
  ?current-time <- (current-time ?time)
  =>
  (retract ?current-time)
  (assert (current-time (inc ?time))))

(defrule log-initial-trucks
  (current-time 0)
  (truck ?number $?)
  =>
  ;; Log result
  (assert
   (result
    (time 0)
    (package-number none)
    (truck-number ?number)
    (action home-idle)
    (arrival-location home-idle)
    (arrival-time 0))))

(defrule to-route-trucks
  (truck ? ?location $? idle none ?)
  (package ? ?package-location $?)
  (not (travel-time ?location $? ?package-location ?))
  =>
  (assert (to-route ?location ?package-location)))

(defrule to-route-packages
  (package ? ?start-city ?end-city $?)
  (not (travel-time ?start-city $? ?end-city ?))
  =>
  (assert (to-route ?start-city ?end-city)))

(defrule to-route-home
  (package ? ? ?end-city $?)
  (not (travel-time ?end-city $? orlando ?))
  =>
  (assert (to-route ?end-city orlando)))

(defrule build-travel-time-graph
  (declare (salience 100))
  ;(to-route ?start-city ?other-city)
  (travel-time ?start-city $?interpaths1 ?middle-city ?time1)
  (travel-time ?middle-city ?other-city ?time2)

  (not (test (member$ ?start-city $?interpaths1)))
  (not (test (member$ ?other-city $?interpaths1)))
  (not (test (member$ ?middle-city $?interpaths1)))


  (not (eq ?start-city ?other-city))
  (not (eq ?start-city ?middle-city))
  (not (eq ?middle-city ?other-city))

  (not (travel-time ?start-city $? ?other-city ?time3&:(<= ?time3 (+ ?time1 ?time2))))

  =>

  (printout t "Start city: " ?start-city crlf
              "Interpaths1 City: " $?interpaths1 crlf
              "Middle City: " ?middle-city crlf
              "End City: " ?other-city crlf)
  (assert
   (travel-time ?start-city $?interpaths1 ?middle-city ?other-city (+ ?time1 ?time2))))

(defrule commutatitve-property-of-travel
  (declare (salience 110))
  (travel-time ?start-city ?end-city ?time)
  (not (travel-time ?end-city ?start-city ?))
  =>
  (assert (travel-time ?end-city ?start-city ?time)))

(defrule identity-property-of-travel
  (declare (salience 110))
  (travel-time ?city $?)
  (not (travel-time ?city ?city 0))
  =>
  (assert (travel-time ?city ?city 0)))

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
  (bind ?new-eta (+ ?eta ?current-time))

  ;; Log result
  (assert
   (result
    (time ?current-time)
    (package-number (str-cat "for " ?package-number))
    (truck-number ?number)
    (action dispatched)
    (arrival-location ?package-location)
    (arrival-time ?new-eta)))

  (retract ?available-truck)
  (assert (truck ?number ?current-location ?package-location
                 (- ?space-avail ?package-size)
                 dispatched ?package-number ?new-eta)))

(defrule pick-up-package
  (current-time ?current-time)
  ?pickup-truck <- (truck ?number ? ?pack-loc ?space-avail dispatched ?package-number ?eta)
  (package ?package-number ?pack-loc ?final-dest $?)

  ;; Make sure we've made it
  (test (= ?current-time ?eta))

  ;; find the fastest-path
  (fastest-path ?pack-loc $? ?final-dest ?delivery-time)

  =>

  (bind ?new-eta (+ ?delivery-time ?current-time))

  ;; Log result
  (assert
   (result
    (time ?current-time)
    (package-number ?package-number)
    (truck-number ?number)
    (action delivering)
    (arrival-location ?final-dest)
    (arrival-time ?new-eta)))


  (retract ?pickup-truck)
  (printout t "Delivering " ?package-number " with " ?number " to " ?final-dest crlf)
  (assert
   (truck ?number ?pack-loc ?final-dest ?space-avail
          delivering ?package-number ?new-eta))
  )


(defrule deliver-package
  (current-time ?current-time)
  ?truck <- (truck ?number ? ?dest ?space-avail delivering ?package-number ?eta)
  ?package <- (package ?package-number
                       ?pack-depart ?pack-delivery ?package-size
                       ?pack-order-arrival ?pack-expected-delivery)

  (test (= ?current-time ?eta))

  ;; Go back home
  (fastest-path ?dest $? orlando ?time)

  =>

  (bind ?new-eta (+ ?time ?current-time))

  ;; Log result
  (assert
   (result
    (time ?current-time)
    (package-number none)
    (truck-number ?number)
    (action returning)
    (arrival-location orlando)
    (arrival-time ?new-eta)))

  (retract ?truck)
  (retract ?package)

  ;; Lets save the fact we've delivered the package
  (assert (delivered-package (number ?package-number)
                             (depart-city ?pack-depart)
                             (delivery-city ?pack-delivery)
                             (size ?package-size)
                             (order-arrival-time ?pack-order-arrival)
                             (expected-delivery-time ?pack-expected-delivery)
                             (actual-delivery-time ?current-time)))


  (printout t "Delivered " ?package-number " with " ?number ", going home now" crlf)
  (assert
   (truck ?number ?dest orlando (+ ?space-avail ?package-size) returning none ?new-eta))

  )

(defrule arrive-home
  (current-time ?current-time)
  ?truck <- (truck ?number ? ?dest ?space-avail returning none ?eta)

  (test (= ?current-time ?eta))

  =>
  (retract ?truck)
  (printout t "Truck " ?number " has returned home " crlf)

  ;; Log result
  (assert
   (result
    (time ?current-time)
    (package-number none)
    (truck-number ?number)
    (action home-idle)
    (arrival-location none)
    (arrival-time ?current-time)))

  (assert
   (truck ?number ?dest none ?space-avail idle none ?current-time)))

(defrule print-finished
  ?time <- (current-time ?)

  ;; Make sure there are no packages left
  (test
   (=
    0
    (length (find-all-facts ((?p package)) TRUE))))

  ;; Make sure all trucks are idle
  (test
   (=
    (length (find-all-facts ((?t truck)) (eq (nth$ 5 ?t:implied) idle)))
    (length (find-all-facts ((?t truck)) TRUE))))

  =>
  ;; Stop the simulation
  (retract ?time)
  (assert (simulation complete))

  (print-results))

;; util rules

(defrule print-packages-descending
  (declare (salience 20))
  (facts-like package)
  ?p <- (package $?attrs)
  (not (facts-like-printed package $?attrs))
  =>
  (printout t crlf "Package: " $?attrs crlf crlf)
  (assert (facts-like-printed package $?attrs)))


(defrule make-truck-reports
  (simulation complete)
  =>
  (do-for-all-facts ((?t truck)) TRUE
                    (assert (truck-report idle (first$ ?t:implied)))
                    (assert (truck-report packages-delivered (first$ ?t:implied)))
                    (assert (truck-report occupied-space (first$ ?t:implied)))
                    (assert (truck-report non-delivery-time (first$ ?t:implied)))
                    (assert (truck-report busy (first$ ?t:implied)))))

(defrule idle-report
  (simulation complete)
  ?report <- (truck-report idle ?truck-number)
  =>
  (retract ?report)

  ;; first we need to calculate what the last truck was to idle
  (bind ?last-truck-to-arrive-time 0)
  (do-for-all-facts ((?r result)) (eq ?r:action home-idle)
                    (if (> ?r:time ?last-truck-to-arrive-time)
                      then
                      (bind ?last-truck-to-arrive-time ?r:time)))


  (bind ?time-diff-info (time-diff-btwn-2-events ?truck-number home-idle dispatched))

  ;; Break up the time diff info
  (bind ?idle-time (nth$ 1 ?time-diff-info))
  (bind ?last-idle-time (nth$ 2 ?time-diff-info))

  (printout t "normal idle " ?idle-time crlf)
  (printout t "last idle " ?last-idle-time crlf)

  ;; Take into account waiting for the last truck to arrive
  (bind ?idle-time (+ ?idle-time (- ?last-truck-to-arrive-time ?last-idle-time)))

  (printout t "last truck: " ?last-truck-to-arrive-time crlf)

  ;; Save the data
  (assert (truck-report-results idle ?truck-number ?idle-time)))

(defrule time-busy-report
  (simulation complete)
  ?report <- (truck-report busy ?truck-number)
  =>
  (retract ?report)

  (bind ?time-diff-info (time-diff-btwn-2-events ?truck-number dispatched home-idle))
  (bind ?total-time-busy (first ?time-diff-info))

  (assert (truck-report-results busy ?truck-number ?total-time-busy)))


(defrule busy-percent-report
  (simulation complete)
  (truck-report-results busy ?truck-number ?total-time-busy)
  (truck-report-results idle ?truck-number ?total-time-idle)
  =>
  (assert
   (truck-report-results busy-percent ?truck-number (/ ?total-time-busy
                                                       (+ ?total-time-busy ?total-time-idle)))))

(defrule packages-transported-report
  (simulation complete)
  ?report <- (truck-report packages-delivered ?truck-number)
  =>
  (retract ?report)

  (bind ?package-count
        (length
         (find-all-facts ((?r result)) (and (= ?truck-number ?r:truck-number) (eq ?r:action returning)))))

  (assert
   (truck-report-results packages-delivered ?truck-number ?package-count)))

(defrule occupied-space-report
  ?report <- (truck-report occupied-space ?truck-number)

  (truck ?truck-number ? ? ?truck-size $?)

  =>
  (retract ?report)

  (bind ?space-used-in-trips 0)
  (bind ?trips-count 0)

  (do-for-all-facts ((?r result)) (and
                                   (= ?r:truck-number ?truck-number)
                                   (eq ?r:action delivering))


                    (bind ?trips-count (inc ?trips-count))
                    (bind ?space-used-in-trips
                          (+ ?space-used-in-trips
                             (get-slot-of-with-number
                              delivered-package
                              ?r:package-number
                              size))))

  (assert
   (truck-report-results average-used-space ?truck-number (/ ?space-used-in-trips
                                                             (* ?trips-count ?truck-size)))))

(defrule non-delivery-time-report
  ?report <- (truck-report non-delivery-time ?truck-number)
  =>
  (retract ?report)

  (bind ?time-diff-info1 (time-diff-btwn-2-events ?truck-number dispatched delivering))
  (bind ?time-diff-info2 (time-diff-btwn-2-events ?truck-number returning home-idle))
  (bind ?total-time (+ (first ?time-diff-info1) (first ?time-diff-info2)))

  (assert
   (truck-report-results non-delivery-time ?truck-number ?total-time)))

(defrule delivering-percent-of-busy-time
  (truck-report-results non-delivery-time ?truck-number ?non-delivery-time)
  (truck-report-results busy ?truck-number ?busy-time)
  =>
  (assert
   (truck-report-results delivering-percent-of-busy-time ?truck-number
                         (/ (- ?busy-time ?non-delivery-time)
                            ?busy-time))))

(defrule make-package-reports
  (simulation complete)
  =>
  (do-for-all-facts ((?p delivered-package)) TRUE
                    (assert (package-report wait-time ?p:number))
                    (assert (package-report pickup-time ?p:number))
                    (assert (package-report delivery-time ?p:number))
                    ))

(defrule package-wait-report
  ?report <- (package-report wait-time ?number)
  (delivered-package (number ?number) (order-arrival-time ?order-arrival-time))
  (result (package-number ?number) (time ?pickup-time))
  =>
  (retract ?report)

  (assert (package-report-result wait-time ?number (- ?pickup-time ?order-arrival-time))))

(defrule pickup-time-report
  ?report <- (package-report pickup-time ?number)
  (result (package-number ?number) (time ?pickup-time))
  =>
  (retract ?report)
  (assert (package-report-result pickup-time ?number ?pickup-time )))

(defrule delivery-time-report
  ?report <- (package-report delivery-time ?number)
  (result (package-number ?number) (action delivering) (arrival-time ?delivery-time))
  =>
  (retract ?report)
  (assert (package-report-result delivery-time ?number ?delivery-time)))

(defrule lateness-time
  (package-report-result delivery-time ?number ?delivery-time)
  (delivered-package (expected-delivery-time ?expected-delivery-time) (number ?number))
  =>

  (bind ?lateness-time (- ?delivery-time ?expected-delivery-time))

  (assert (package-report-result lateness-time ?number ?lateness-time)))


(defrule make-package-avg-reports
  (simulation complete)

  ;; make sure all packages have their report
  (forall
   (delivered-package (number ?number))
   (package-report-result wait-time ?number ?)
   (package-report-result delivery-time ?number ?)
   (package-report-result pickup-time ?number ?)
   (package-report-result lateness-time ?number ?))
  =>

  (printout t crlf crlf "Packages have made their reports" crlf crlf)

  (bind ?total-wait 0)
  (bind ?total-lateness 0)

  (bind ?num-packages-on-time 0)
  (bind ?num-packages-late 0)
  (bind ?package-count 0)


  (bind ?avg-wait 0)
  (bind ?avg-lateness-for-late-packages 0)
  (bind ?avg-lateness-for-all-packages 0)

  (do-for-all-facts ((?p delivered-package)) TRUE
                    (bind ?package-count (inc ?package-count))

                    ;; Get lateness
                    (do-for-fact ((?r package-report-result))
                                 (and (eq lateness-time (first ?r:implied))
                                      (= ?p:number (nth$ 2 ?r:implied)))
                                 (bind ?lateness (nth$ 3 ?r:implied))
                                 ;(printout t (nth$ 3 ?r:implied) crlf)
                                 )
                    ;; get wait
                    (do-for-fact ((?s package-report-result))
                                 (and (eq wait-time (first ?s:implied))
                                      (= ?p:number (nth$ 2 ?s:implied)))
                                 (bind ?wait-time (nth$ 3 ?s:implied)))

                    (bind ?total-wait (+ ?total-wait ?wait-time))

                    (if (> ?lateness 0)
                      then
                      (bind ?total-lateness (+ ?total-lateness ?lateness))
                      (bind ?num-packages-late (inc ?num-packages-late))
                      else
                      (bind ?num-packages-on-time (inc ?num-packages-on-time))))

  (assert (package-avg-report
           (avg-wait (/ ?total-wait ?package-count))
           (avg-lateness-for-packages (/ ?total-lateness ?package-count))
           (avg-lateness-for-late-packages (/ ?total-lateness ?num-packages-late))
           (num-packages-on-time ?num-packages-on-time)
           (num-packages-late ?num-packages-late))))



;; (foreach ?f (find-all-facts ((?t truck)) TRUE)
;;           (ppfact ?f))



;  (slot time)
;  (slot package-number)
;  (slot truck-number)
;  (slot action)
;  (slot arrival-location)
;  (slot arrival-time)


(do-for-all-facts ((?t truck-report-results)) TRUE (retract ?t))
(do-for-all-facts ((?t truck-report)) TRUE (retract ?t))
(print-results)

;; (find-all-facts ((?r result)) (and (= 2 ?r:truck-number) (eq ?r:action returning)))

;(bind ?k (nth$ 1 (find-fact ((?r result)) TRUE)))

;; (do-for-all-facts ((?t simulation)) TRUE (retract ?t))
;; (assert (simulation complete))

(do-for-all-facts ((?p package-report-result)) TRUE (ppfact ?p))

; 3 -> 2 -> 10 -> 5
;      3    6     6

;(get-slot-of-with-number delivered-package 5 size)
(do-for-all-facts ((?t truck-report-results)) (ppfact ?t))

(do-for-fact ((?p package-report-result))
             (and (eq lateness-time (first ?p:implied))
                  (= 4 (nth$ 2 ?p:implied)))
             (printout t (nth$ 3 ?p:implied) crlf))

;(find-fact (result $?))

;(printout t "foo " crlf)

;(assert (facts-like package))
;(retract (facts-like package))

;(clear)
;(load "part1.clp")
(run 1)
(run 5)
(run 20)
(run 40)
(run 100)
(run 300)
(run 390)
(run 500)
(reset)
(run)
;(facts)

(matches make-package-avg-reports)

;; (agenda)
;; (matches print-finished)

;(watch facts)
;(unwatch facts)
;; (watch rules)
;; (unwatch rules)
;(reset)

;; (agenda)

;; (matches find-fastest-path)

;; (matches build-travel-time-graph)


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

;; (facts)
