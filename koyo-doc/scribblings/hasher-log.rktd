;; This file was created by make-log-based-eval
((require component koyo) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((define the-hasher (component-start ((make-argon2id-hasher-factory))))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((define p "supersecret") ((3) 0 () 0 () () (c values c (void))) #"" #"")
((define h (hasher-make-hash the-hasher p))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
(h
 ((3)
  0
  ()
  0
  ()
  ()
  (q
   values
   "$argon2id$v=19$m=2048,t=256,p=16$e0gUGCU9E3MH3ik94QEX8A$4AASykMvNYkxpvZg2gCwWpHsP6brXszduYHbeu1bqwE"))
 #""
 #"")
((hasher-hash-matches? the-hasher h "nope")
 ((3) 0 () 0 () () (q values #f))
 #""
 #"")
((hasher-hash-matches? the-hasher h p)
 ((3) 0 () 0 () () (q values #t))
 #""
 #"")
