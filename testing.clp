
(assert (foo 1 4))

(defrule test1 
  (foo ?a ?b)
  (forall 
   (foo ?c ?d)
   (test (<= ?b ?d)))
  =>
  (printout t "Foo is: " ?a " " ?b crlf))

(run 1)

(reset)

(facts)

(clear)
