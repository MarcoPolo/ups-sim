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

(deftemplate aggr-truck-report
  (slot number)
  (slot delivering-percent-of-busy-time)
  (slot non-delivery-time)
  (slot average-used-space)
  (slot packages-delivered)
  (slot busy-percent)
  (slot busy)
  (slot idle))

(deftemplate aggr-package-report
  (slot number)
  (slot wait-time)
  (slot pickup-time)
  (slot delivery-time)
  (slot lateness-time))

(deftemplate package-avg-report
  (slot avg-wait)
  (slot num-packages-on-time)
  (slot num-packages-late)
  (slot avg-lateness-for-late-packages)
  (slot avg-lateness-for-packages))

(deftemplate global-state
  (slot current-stage)
  (slot stage)) 

(deftemplate truck-state
  (slot number)
  (slot current-location)
  (multislot path)
  (slot space-avail)
  (multislot package-queue))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Facts

(deffacts trucks
 ; Number | Current Location | Destination | Space Avail | current time | action | package | eta: the expected time to arrival
  (truck 1 miami none 10 idle none 0)
  (truck 2 miami none 8 idle none 0)
  (truck 3 miami none 6 idle none 0)
  (truck 4 orlando none 10 idle none 0)
  (truck 5 orlando none 12 idle none 0)
  (truck 6 orlando none 6 idle none 0))

(deffacts package-data
  ; Number | Depart City | Delivery City | Size | Order Arrival Time | Expected Delivery Time
  (package 1 orlando jacksonville 4 1 15)
  (package 2 tampa st-augustine 4 4 10)
  (package 3 key-west miami 3 8 25)
  (package 4 miami orlando 5 20 30)
  (package 5 ocala orlando 7 30 40)
  (package 6 orlando lake-city 6 40 45)
  (package 7 jacksonville tallahassee 8 65 80)
  (package 8 tallahassee gainesville 4 80 100)
  (package 9 st-augustine tallahassee 5 90 110)
  (package 10 west-palm ft-myers 1 110 120)
  (package 11 ocala ft-myers 1 110 120)
  (package 12 jacksonville key-west 2 120 150)
  (package 13 miami ocala 2 150 155)
  (package 14 miami gainesville 5 150 160)
  (package 15 miami tallahassee 2 150 170)
  (package 16 tallahassee lake-city 2 200 210)
  (package 17 lake-city tallahassee 7 220 240)
  (package 18 tallahassee key-west 9 240 300)
  (package 19 st-augustine gainesville 8 250 260)
  (package 20 tampa jacksonville 1 250 270))


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


