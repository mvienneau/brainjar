(ns brainjar.core
  (:gen-class))

(defn matching-paren [text bracket opbracket inp dir]
  (loop [i (dir inp) found 0]
    (condp = (nth text i)
      bracket (recur (dir i) (inc found))
      opbracket (if (= found 0)
                  i
                  (recur (dir i) (dec found)))
      (recur (dir i) found))))

(defn eval [text]
  (loop [cells [0] cell-pointer 0 inp 0]
    (condp = (get text inp)
      \> (recur (conj cells 0) (inc cell-pointer) (inc inp))
      \< (recur cells (dec cell-pointer) (inc inp))
      \+ (recur (update-in cells [cell-pointer] inc) cell-pointer (inc inp))
      \- (recur (update-in cells [cell-pointer] dec) cell-pointer (inc inp))
      \[ (if (= (get cells cell-pointer) 0)
           (recur cells cell-pointer (inc (matching-paren text \[ \] inp inc)))
           (recur cells cell-pointer (inc inp)))
      \] (if (= (get cells cell-pointer) 0)
           (recur cells cell-pointer (inc inp))
           (recur cells cell-pointer (inc (matching-paren text \] \[ inp dec))))
      \. (do
           (print (char (nth cells cell-pointer)))
           (recur cells cell-pointer (inc inp)))
      nil cells
      (recur cells cell-pointer (inc inp)))))


(defn -main
  [& args]
  (def program "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>")
  (eval program))
