(ns ^{:doc "XOR Shift PRNG Implementation"
     :author "Jeremy Bondeson"}
  data.prng.xorshift
  )

(comment
  ;; from the Java Random class...
  (def ^:static ^:private 
    multiplier 0x5DEECE66D)

  (def ^:static ^:private 
    uniquifier 8682522807148012))


(defn
  genseed-k1
  "XOR-Shift PRNG with one seed.
Algorithm from 'Numerical Recipes 3rd Ed' ch 7.1, with a1=14, a2=29, a3=11
x ← x ^ (x << a1)
x ← x ^ (x >> a2)
x ← x ^ (x << a3)
Algorithm has a period of 2^64 - 1.
"
  [s]
  (let [s'   (bit-xor s   (unchecked-long (bit-shift-left  s   14)))
        s''  (bit-xor s'  (unchecked-long (bit-shift-right s'  29)))
        s''' (bit-xor s'' (unchecked-long (bit-shift-left  s'' 11)))]
    s'''))


(defn 
  genseed-k5
  "XOR-Shift PRNG with 5 seeds.
XOR Shift PRNG algorithm comes from:
      George Marsaglia
      http://groups.google.com/group/comp.lang.c/msg/e3c4ea1169e463ae
  Seed construction comes from:
     George Marsaglia
     'Seeds for random number generators', Communications of the ACM, v.46 n.5, May 2003
     [doi>10.1145/769800.769827]

 Reference C Implementation:
 ------------------------------
 static unsigned long x=123456789,y=362436069,z=521288629,w=88675123,v=886756453;

 unsigned long xorshift(void) { 
    unsigned long t;
    t=(x^(x>>7));
    x=y
    y=z;
    z=w;
    w=v; 
    v=(v^(v<<6))^(t^(t<<13));
    return (y+y+1)*v;
 }"
  [[x y z w v]]
  (let [t  (bit-xor x (unchecked-long (bit-shift-right x 7)))
        v' (bit-xor (bit-xor v (unchecked-long (bit-shift-left v 6)))
                    (bit-xor t (unchecked-long (bit-shift-left t 13))))
        r  (unchecked-long (* v (+ (bigint y) y 1)))]
    [r [y z w v v']]))

(defn
  extend-seed
  "Extends a single seed into an array of multiple seeds."
  [s n]
  (loop [coll [s]
         i    1
         s'   s]
    (if (< i n)
      (let [s'' (genseed-k1 s')]
        (recur (conj coll s'') (inc i) s''))
      coll)))