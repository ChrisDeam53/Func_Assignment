(ns assignment.core
  (:gen-class) 
  (:require [clojure.string :as str]
            [clojure.set :as cljSet]))

;; RUN CODE:
;; Alt+Enter
;; SEE FULL RESULT:
;; Ctrl+K
;; ESCAPE - To clear debug stuff
;; Ctrl+Alt+Enter will evaluate the current enclosing form up to the cursor.
;; To see the result at each step in the thread
;; You can also Ctrl+Alt+Enter after each form
;; Shift+Alt+Enter will evaluate all code from
;; the start of the current top level form, up until
;; the cursor, with all open brackets closed.

;; Evauluate Current Form: ctrl+enter
;; Evaluate Current Top Level Form (defun), alt+enter.
;; https://calva.io/try-first/

;; https://calva.io/debugger/
;; https://calva.io 

;; def is for global constants, let is for local variables.

;; Morse Code:
;; https://morsedecoder.com/
;; https://www.electronics-notes.com/articles/ham_radio/morse_code/characters-table-chart.php
;; https://encyclopedia2.thefreedictionary.com/List+of+Morse+Code
;; How to document your code: https://practical.li/clojure/reference/clojure-syntax/code-documentation.html#reveal-answer
;; Seemingly useful resource: https://kimh.github.io/clojure-by-example/#about-this-page

(require '[clojure.string :as str])
(require '[clojure.set :as cljSet])
(require '[clojure.java.io :as io])

;; /****************************************************************************************/
;; ASCII->Morse & Morse->ASCII solution.
;; /****************************************************************************************/

(def letter-hash-map
  "Define strings to their respective Morse counterparts."
  {"A" ".-", "B" "-...", "C" "-.-.", "D" "-..", "E" ".", "F" "..-.", "G" "--.", "H" "....",
   "I" "..", "J" ".---", "K" "-.-", "L" ".-..", "M" "--", "N" "-.", "O" "---", "P" ".--.",
   "Q" "--.-", "R" ".-.", "S" "...", "T" "-", "U" "..-", "V" "...-", "W" ".--", "X" "-..-",
   "Y" "-.--", "Z" "--..", "0" "-----", "1" ".----", "2" "..---", "3" "...--", "4" "....-",
   "5" ".....", "6" "-....", "7" "--...", "8" "---..", "9" "----.", " " "       "})

(def morse-hash-map 
"Define morse to their respective ASCII counterparts by inverting the letter-hash-map."
  (cljSet/map-invert letter-hash-map))

(defn translate-character-to-morse 
  "Performs the get method on the character found inside the letter-hash-map.
   Parameters: currentCharacter - Individual Character in the string to perform the get method."
  [currentCharacter]
  (let [returnString (get letter-hash-map currentCharacter)]
    (if (str/includes? returnString " ")
      ;; Append 5x spaces to the end, totals to 7x spaces between words.
      (str (str/trim (get letter-hash-map currentCharacter)) "    ")
      ;; Append 2x spaces to the end of each morse character returned if not a space.
      (str returnString "   "))))

(defn translate-character-and-space-to-ascii 
  "Performs string manipulation to split two characters separated by a space.
   Parameters: currentString - Individual String in the string to perform the get method."
  [currentString]
  (let [firstCharacter (first currentString)]
    (let [lastCharacter (last currentString)]
      ;; Get the characters *before* and *after* the space & return with a singular space.
      (str (get morse-hash-map firstCharacter) " " (get morse-hash-map lastCharacter)))))

(defn translate-character-to-ascii 
  "Performs the get method on the character found inside the letter-hash-map.
   Parameters: currentCharacter - Individual Character in the string to perform the get method."
  [currentCharacter]
  (if (str/includes? currentCharacter " ")
    ;; String contains morse spaces (7x spaces).
    (let [returnString (str/split currentCharacter #"       ")]
      ;; As Regex includes the 7x spaces, string must be split to return properly.
      (translate-character-and-space-to-ascii returnString))
    ;; No spaces. return as normal.
    (get morse-hash-map currentCharacter)))

(defn get-character 
  "Gets the respective alternate character for the string inputted & returns these mapped values.
   Parameters: enteredString - The string to be converted to its appropriate counterpart."
  [enteredString]
    ;; Covert all characters to uppercase for uniformty.
  (let [characterVectorUpper (str/upper-case enteredString)]
    ;; Use Regex to get each individual character, including spaces. Returns a vector.
    (if (or (= (get characterVectorUpper 0) \.) (= (get characterVectorUpper 0) \-))
      (let [characterVector (str/split characterVectorUpper #"\s+(?!\s{3})(?<!\s{4})")]
        ;; String entered is Morse.
        ;; Join all empty spaces between. Example: "A B" -> "AB".
        (str/join "" (map translate-character-to-ascii characterVector)))
      (let [characterVector (str/split characterVectorUpper #"")]
        ;; String entered is ASCII.
        (apply str(map translate-character-to-morse characterVector))))))

;; EXAMPLE STRINGS:
;; Hello World
;; ....   .   .-..   .-..   ---       .--   ---   .-.   .-..   -..

(defn request-string-to-convert 
  "Accepts user input & calls conversion method. Invokes [[string-to-morse]]."
  []
  (println "Please enter a valid ASCII string or Morse Code.")
  (flush)
  (let [enteredString (str (read-line))]
    (println(get-character enteredString))))

;; /****************************************************************************************/
;; CET Solution.
;; NOTE: Uses the "Legacy" data for: 1772toDate, 2019, 2020, 2021 & 2022 text files.
;; /****************************************************************************************/

(def months-map
  "Define each month to a value."
  { 3 "January", 4 "February", 5 "March", 6 "April", 7 "May", 8 "June", 9 "July",
   10 "August", 11 "September", 12 "October", 13 "November", 14 "December"})

;; Use an "Atom" as a counter for the current day.
(def current-day
  "Stores the current day."
  (atom 0))

;; Use an "Atom" as a counter for the current year.
(def current-year
  "Stores the current year."
  (atom 1772))

(def warmest-day-each-month
  "Hash-Map containing a {Month: {Day Temperature} }"
  { 1 {0 0}, 2 {0 0}, 3 {0 0}, 4 {0 0}, 5 {0 0}, 6 {0 0}, 7 {0 0}, 8 {0 0},
   9 {0 0}, 10 {0 0}, 11 {0 0}, 12 {0 0}})


(defn update-current-map
  "Updates the current daily map key and then the value of the KVP."
  [mappedMonth mappedMonthDay currentDay currentTemperature]
  (let [updatedMap (cljSet/rename-keys mappedMonth {mappedMonthDay, currentDay})]
    (assoc updatedMap currentDay currentTemperature)))

;; 1.	Find the warmest day for each calendar month 
;; NOTE: External sources (non-Clojure documentation) have been referenced.
;; REFERENCE: https://stackoverflow.com/questions/28408743/how-do-you-destructure-a-map-into-key-value-pairs-without-knowing-the-keys-in-cl
;; ^ Reference used: Post by Thumbnail - Answered Feb 9, 2015 at 11:53. (Used from map to mappedMonth)
;; For JAN -> 45, Day 10
(defn find-warmest-day-in-month
  "TODO:"
  [currentDay currentMonth currentTemperature]
  (let [mappedMonth (get warmest-day-each-month (- currentMonth 2))]
    (if (not= mappedMonth nil)
      (map (fn [[mappedMonthDay]]
             (update-current-map mappedMonth mappedMonthDay currentDay currentTemperature))
           mappedMonth))))

;; 2.	Find the warmest and coldest years.
(defn process-year-data
  "TODO"
  [yearlyData]
  (->> yearlyData
       (drop 2)
       (map (comp second first))))

(defn addIndex
 "TODO"
  [data]
  (apply (partial map (fn [& nums] (apply + nums))) data))

(defn divideNumber
  "TODO"
  [number]
  (float (/ number 12)))

;; 3.	Find the mean temperature for each calendar month
;; (the average for all Mays, for example) and the instance
;; of each month that has the greatest and smallest variation
;; from that mean.
(defn find-mean-temperature-per-month
"TOOD"
  [lineData]
  (println "TODO"))

(defn set-data
  "DOING"
  [currentPos currentValue]
  ;; (if (= currentPos 1)
  ;;   ;; Year Column
  ;;   (println "Current Year:" (Integer/parseInt (apply str (take 6 currentValue)))))
  (if (= currentPos 2)
    ;; Day column
    (cond
      (not= current-day 31)
      ;; Day Column is <= 31
      (swap! current-day inc)
      (= current-day 31)
      ;; Else day is 31 - Sets the value of atom to newval without regard for the current value. Returns newval.
      (reset! current-day 1)))
  (if (> currentPos 2)
    ;; Month Column
    ;; (println "Current Month:" (get months-map currentPos) "Data: " currentValue))
    (get months-map currentPos))
  
  ;; (println "Current Day: " @current-day)
  
  ;; Return a new collection consisting of the currentMonth applied to the start. Which has been transformed into a sequence.
  (into (find-warmest-day-in-month @current-day currentPos (Integer/parseInt (apply str currentValue))) (seq [(- currentPos 2)])))

(defn check-line-data-1772
  "Obtains data from the line.
   Parameters: currentLine - The current line that the reader has parsed."
  [currentLine]
  (loop [currentPos 1 numberLine currentLine dailyValuesVector []]
        (if-not (= currentPos 15)
          (let [currentValue (take 1 numberLine)]
            (let [currentDayData (set-data currentPos currentValue)]
              ;; Data format: (MONTH {Day Temperature})
              ;; (1 {1 23})
              ;; Use recur in tail position | Increments position & drops first item.
              (recur (+ currentPos 1) (drop 1 numberLine) (conj dailyValuesVector (last currentDayData)))))
          dailyValuesVector)))

;; Example data:
;; 1772    1   32  -15   18   25   87  128  187  177  105  111   78  112

;; Reference: https://stackoverflow.com/questions/40370240/easy-way-to-change-specific-list-item-in-list
;; Inspired by: Mark Fisher's post - Answered Nov 3, 2016 at 9:45.
(defn replace-value
  "Replaces the old Key:Value pair (hashmap) with the newer version, populates the rest of the structure with original values."
  [dayTemperatureList currentVectorIndex currentItem]
  (loop [newVec [] oldList dayTemperatureList]
    (if (seq oldList)
      ;; Return a seq on the collection.
      (if (= (count newVec) currentVectorIndex)
        (recur (conj newVec currentItem) (rest oldList))
        (recur (conj newVec (first oldList)) (rest oldList))) 
    (apply list newVec))))

(defn check-daily-values
  "TODO"
  [warmestTempSequence currentSequence currentIndex]
  (if (> (get currentSequence (+ currentIndex 1)) (get warmestTempSequence 1))
    ;; If the current value in the vector is > old value THEN replace
    (replace-value (warmestTempSequence (- currentIndex 1)) (currentSequence (- currentIndex 1)) (+ currentIndex 1))
    warmestTempSequence))

(defn check-daily-values
  "TODO"
  [warmestTempSequence currentSequence currentIndex]
  (if (> (get currentSequence (+ currentIndex 1)) (get warmestTempSequence 1))
    ;; If the current value in the vector is > old value THEN replace
    currentSequence
    warmestTempSequence))

(defn read-by-line
  "Reads the file line by line - As opposed to the entire file at once.
   Parameters: fileName - The name of the entered file to be read."
  [fileName]
  (let [lineData
        (with-open [reader (io/reader fileName)]
          (reduce (fn [lineData line]
                    ;; Use reduce with function of two arguments - Take first value for accumulated result.
                    ;; Take sequence of elements & apply function to that result & then to each element etc.
                    ;; Returns the accumulated result.
                    (let [currentLine (str/trimr (str/triml line))
                          splitLine (str/split currentLine #"\s+")
                          numberLine (map parse-long splitLine)]
                      (conj lineData (check-line-data-1772 numberLine))))
                  ;; Define structure as a vector.
                  []
                  (line-seq reader)))]
    ;; Note: The position of the values in the vector represent the month.
    ;; 1st position = Jan, 2nd Position = Feb etc.

    ;; (get-year-data lineData)

    ;; OG Working:
    ;; (println "ZZZZZZ: " (map process-year-data lineData))
    ;; Use the return result from this in part 3
    ;;(println "Combined: " (map vector (map process-year-data lineData)))
    (println "1.5 Combined: " (partition 31 (map process-year-data lineData)))
    
    ;; Works - But it does it for all 62 values
    (println "1.6 Combined: " (apply (partial map (fn [& nums] (apply + nums))) (map process-year-data lineData)))
    ;; (println "1.65 Combined: " (partition 31 (apply (partial map (fn [& nums] (apply + nums))) (map process-year-data lineData))))
    
    
    (println "1.66 Combined: " (map addIndex (partition 31 (map process-year-data lineData))))
    (println "1.67 Combined: " (map #(apply + %) (map addIndex (partition 31 (map process-year-data lineData)))))
    ;;(println "1.68 Combined:" (apply (partial map (fn [& nums] (divideNumber nums))) (map process-year-data lineData)))
    ;;(println "1.69 Combined:"  (map inc (map process-year-data lineData)))
    ;; (println "1.69 Combined:"  (map inc [1 2 3 4 5]))
    
    (println "1.70 Combined: " (map (fn [& num] (apply divideNumber num)) (map #(apply + %) (map addIndex (partition 31 (map process-year-data lineData))))))
    (println "Coldest Year: " (.indexOf (map (fn [& num] (apply divideNumber num)) (map #(apply + %) (map addIndex (partition 31 (map process-year-data lineData))))) (apply min (map (fn [& num] (apply divideNumber num)) (map #(apply + %) (map addIndex (partition 31 (map process-year-data lineData))))))))
    (println "Warmest Year:" (.indexOf (map (fn [& num] (apply divideNumber num)) (map #(apply + %) (map addIndex (partition 31 (map process-year-data lineData))))) (apply max (map (fn [& num] (apply divideNumber num)) (map #(apply + %) (map addIndex (partition 31 (map process-year-data lineData))))))))
    
    
    ;; (fn [coll] (filter even? coll))
    
    ;; (println "1.70 Combined: " (apply map (fn [& nums] (apply divideNumber nums)) (map #(apply + %) (map addIndex (partition 31 (map process-year-data lineData))))))
    ;; (println "1.689 Combined: "(apply map (fn [number] (float (/ number 12))) (map process-year-data lineData)))
    ;;(apply map (fn ...) (map process-year-data lineData))
    
    ;; Works - Answer = 2298
    ;; (println "DIVIDE: " (divideNumber 27576))
    

    ;;(println "1.7 Combined: " (map #(apply + %) (partition 31 (map first (map process-year-data lineData)))))
    
    ;; USE
    ;; (apply (partial map (fn [& nums] (apply + nums))) (map process-year-data lineData))
    
    ;; (map (fn [& nums] (apply + nums)) (flatten v1) (flatten v2) (flatten v3) (flatten v4))
    
    
    ;;(map (fn [& nums] (apply + nums)) (flatten (map process-year-data lineData)))
    ;;(println "Second Combined: " (vec (first (first (map vector (map process-year-data lineData))))))


    ;; (println "Results: " (map (partial apply +) (map vector (map process-year-data lineData))))

    (mapv + [1 2 3] [4 5 6])
    ;; (mapv + (map vector (map process-year-data lineData)))

    ;; If you would like to find the largest item **within** the vector, you would need
    ;; to use `apply`
    ;; (apply max [1 2 3])
    ;;=> 3
    ;; If elements are already in a sequence, use apply
    ;;user=> (apply min [1 2 3 4 3])
    ;;1


    ;; (def first-items
    ;;   '(32 -15 18 25 87 128 187 177 105 111 78 112))
    ;; (def second-items
    ;;   '(21 18 85 76 90 87 567 456 674 283 17 113))

    ;; (def combined
    ;;   (map vector first-items second-items))

    ;; (println "Combined data:")
    ;; (println combined)

    ;; (println "Results:")
    ;; (map (partial apply +) combined)


    (find-mean-temperature-per-month lineData)


    (loop [currentIndex 1 warmestTemp (drop 2 (nth lineData (- currentIndex 1)))]
      (if-not (= currentIndex (count lineData))
        (let [currentVector (drop 2 (nth lineData currentIndex))]
            ;; As vectors are associative, use get-in
            ;; Gets the current nested Vector.
            ;; Result of the inner loop used in the outer.
          (recur (+ currentIndex 1)
                 (loop [warmestTemp warmestTemp currentVectorIndex 1]
                   (if-not (= currentVectorIndex 12)
                      ;; Whilst month is December or any month beforehand.  
                     (if (> (get (nth currentVector (- currentVectorIndex 1)) (+ currentIndex 1)) (get (nth warmestTemp (- currentVectorIndex 1)) (first (keys (nth warmestTemp (- currentVectorIndex 1))))))
                        ;; If the current value in the vector is greater than the old value then replace.
                       (recur (replace-value warmestTemp (- currentVectorIndex 1) (nth currentVector (- currentVectorIndex 1))) (+ currentVectorIndex 1))
                        ;; Else do not replace & increase counter.
                       (recur warmestTemp (+ currentVectorIndex 1)))
                     warmestTemp))))
        warmestTemp))))

(defn slurp-1772-file
  "Slurps the 1772toDate.txt file line by line."
  []
  (println "Warmest Day of Each month: " (read-by-line "src/assignment/test.txt")))

(defn slurp-2019-file
  "Slurps the 2019.txt file line by line."
  []
  (println (slurp "src/assignment/2019.txt")))

(defn slurp-2020-file
  "Slurps the 2020.txt file line by line."
  []
  (println (slurp "src/assignment/2020.txt")))

(defn slurp-2021-file
  "Slurps the 2021.txt file line by line."
  []
  (println (slurp "src/assignment/2021.txt")))

(defn slurp-2022-file
  "Slurps the 2022.txt file line by line."
  []
  (println (slurp "src/assignment/2022.txt")))

(defn initialise-cet-solution 
  "Initialise the solution. Slurp the respective text files."
  []
  (slurp-1772-file))

(defn initialise-project 
  "Initialise the project. Run solution depending on user input."
  []
  (println "Which solution would you like to see?\n
            Enter '1' for ASCII/Morse Conversion\n
            Enter '2' for the CET solution.")
  (flush)
  (let [userInput (Integer/parseInt (read-line))]
      (cond (= 1 userInput)
            (request-string-to-convert)
            (= 2 userInput)
            (initialise-cet-solution)
            :else
            (println "Unnacepted Request.")
            )))
        

(defn -main 
  "Entry point for the program."
  [& args]
  (println "Executing project:\n Author: Chris Deam.")
  (initialise-project)
)