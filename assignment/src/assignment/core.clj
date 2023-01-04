(ns assignment.core
  (:gen-class) 
  (:require [clojure.string :as str]
            [clojure.set :as cljSet]
            [clojure.spec.alpha :as s]))

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
(require '[clojure.spec.alpha :as s])

;; /****************************************************************************************/
;; ASCII->Morse & Morse->ASCII solution.
;; /****************************************************************************************/

(def regex-to-check-morse #"^[\.\-\ ]+$")
(def regex-to-check-ascii #"^[a-zA-Z0-9\ ]+$")

(s/def ::morse-code-input-validation (s/and string? #(re-matches regex-to-check-morse %)))

(s/def ::string-input-validation (s/and string? #(re-matches regex-to-check-ascii %)))

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
   Parameters: current-character - Individual Character in the string to perform the get method."
  [current-character]
  (let [return-string (get letter-hash-map current-character)]
    (if (str/includes? return-string " ")
      ;; Append 5x spaces to the end, totals to 7x spaces between words.
      (str (str/trim (get letter-hash-map current-character)) "    ")
      ;; Append 2x spaces to the end of each morse character returned if not a space.
      (str return-string "   "))))

(defn translate-character-and-space-to-ascii 
  "Performs string manipulation to split two characters separated by a space.
   Parameters: current-string - Individual String in the string to perform the get method."
  [current-string]
  (let [first-character (first current-string)]
    (let [last-character (last current-string)]
      ;; Get the characters *before* and *after* the space & return with a singular space.
      (str (get morse-hash-map first-character) " " (get morse-hash-map last-character)))))

(defn translate-character-to-ascii 
  "Performs the get method on the character found inside the letter-hash-map.
   Parameters: current-character - Individual Character in the string to perform the get method."
  [current-character]
  (if (str/includes? current-character " ")
    ;; String contains morse spaces (7x spaces).
    (let [return-string (str/split current-character #"       ")]
      ;; As Regex includes the 7x spaces, string must be split to return properly.
      (translate-character-and-space-to-ascii return-string))
    ;; No spaces. return as normal.
    (get morse-hash-map current-character)))

(defn get-character 
  "Gets the respective alternate character for the string inputted & returns these mapped values.
   Parameters: entered-string - The string to be converted to its appropriate counterpart."
  [entered-string]
  (if (or (s/valid? ::string-input-validation entered-string) (s/valid? ::morse-code-input-validation entered-string))
    ;; Covert all characters to uppercase for uniformty.
    (let [character-vector-upper (str/upper-case entered-string)]
      ;; Use Regex to get each individual character, including spaces. Returns a vector.
      (if (or (= (get character-vector-upper 0) \.) (= (get character-vector-upper 0) \-))
        (let [character-vector (str/split character-vector-upper #"\s+(?!\s{3})(?<!\s{4})")]
          ;; String entered is Morse.
          ;; Join all empty spaces between. Example: "A B" -> "AB".
          (str/join "" (map translate-character-to-ascii character-vector)))
        (let [character-vector (str/split character-vector-upper #"")]
          ;; String entered is ASCII.
          (apply str(map translate-character-to-morse character-vector)))))
  (str "Invalid Input. Only valid A-Z,a-z,0-9 characters OR . - characters valid.")))

(defn request-string-to-convert 
  "Accepts user input & calls conversion method. Invokes [[string-to-morse]]."
  []
  (println "Please enter a valid ASCII string or Morse Code.")
  (flush)
  (let [entered-string (str (read-line))]
    (println(get-character entered-string))))

;; /****************************************************************************************/
;; CET Solution.
;; NOTE: Uses the "Legacy" data for: the 1772toDate file.
;; /****************************************************************************************/

(s/def ::data-number int?)
(s/def ::data-lazy-seq seq?)
(s/def ::data-float float?)
(s/def ::data-map map?)
(s/def ::data-lazy-seq seq?)
(s/def ::data-float float?)
(s/def ::data-string string?)
(s/def ::does-file-exist #(.exists (io/file %)))
(s/def ::is-file-valid ::does-file-exist)

(def months-map
  "Define each month to a value."
  { 3 "January", 4 "February", 5 "March", 6 "April", 7 "May", 8 "June", 9 "July",
   10 "August", 11 "September", 12 "October", 13 "November", 14 "December"})

;; Use an "Atom" as a counter for the current day.
(def current-day
  "Stores the current day."
  (atom 0))

(def warmest-day-each-month
  "Hash-Map containing a {Month: {Day Temperature} }"
  { 1 {0 0}, 2 {0 0}, 3 {0 0}, 4 {0 0}, 5 {0 0}, 6 {0 0}, 7 {0 0}, 8 {0 0},
   9 {0 0}, 10 {0 0}, 11 {0 0}, 12 {0 0}})

(defn update-current-map
  "Updates the current daily map key and then the value of the KVP."
  [mapped-month mapped-month-day current-day current-temperature]
  {:pre [s/valid? ::data-number mapped-month s/valid? ::data-number mapped-month-day ::data-number current-day ::data-number current-temperature]
   :post [s/valid? ::data-map %]}
  (let [updatedMap (cljSet/rename-keys mapped-month {mapped-month-day, current-day})]
    (assoc updatedMap current-day current-temperature)))

;; 1.	Find the warmest day for each calendar month 
;; NOTE: External sources (non-Clojure documentation) have been referenced.
;; REFERENCE: https://stackoverflow.com/questions/28408743/how-do-you-destructure-a-map-into-key-value-pairs-without-knowing-the-keys-in-cl
;; ^ Reference used: Post by Thumbnail - Answered Feb 9, 2015 at 11:53. (Used from map to mappedMonth)
;; For JAN -> 45, Day 10
(defn apply-current-month-to-data
  "Applies the current month into the data structure:"
  [current-day current-month current-temperature]
  {:pre [s/valid? ::data-number current-day s/valid? ::data-number current-month ::data-number current-temperature]}
  (let [mapped-month (get warmest-day-each-month (- current-month 2))]
    (if (not= mapped-month nil)
      (map (fn [[mapped-month-day]]
             (update-current-map mapped-month mapped-month-day current-day current-temperature))
           mapped-month))))

;; First the map to obtain the first K:V pair as a vector of [K:V] 
;; Get the second element of that, the Value.
;; Takes first item from the collection, then the second item from the result of that.
;; Thread-last macro takes a value & passes it as the last argument to the next function in the chain.
(defn process-year-data
  "Given a vector of Hashmaps, only retrieve the mapped values."
  [yearly-data]
  (->> yearly-data
       (drop 2)
       (map (comp second first))))

(defn add-index
 "Adds the current index values together and maps these together.
  Parameters: data - Sequence containing indexed value of every month for that year."
  [data]
  {:pre [s/valid? ::data-number data]
   :post [s/valid? ::data-lazy-seq %]}
  (apply (partial map (fn [& nums] (apply + nums))) data))

(defn conj-index
  "Conjoins all the indexed values into one list. (All first indexes together, all seconds together etc.)
   Parameters: data - Sequence containing the mean values of each month per year."
  [data]
  {:pre [s/valid? ::data-number data]
   :post [s/valid? ::data-lazy-seq %]}
  (apply (partial map (fn [& nums] (conj nums))) data))

(defn divide-number-month
  "Divides the current number by 31 & parse to float.
   Parameters: number - The number to be divided."
  [number]
  {:pre [s/valid? ::data-number number]
   :post [s/valid? ::data-float %]}
  (float (/ number 31)))

(defn divide-number-year
  "Divides the current number by 12 & parse to float.
   Parameters: number - The number to be divided."
  [number]
  {:pre [s/valid? ::data-number number]
   :post [s/valid? ::data-float %]}
  (float (/ number 12)))

(defn divide-number-var
  "Divides the current number by another & parse to float.
   Parameters: number - The number to be divided."
  [number divide-by]
  {:pre [s/valid? ::data-number number s/valid? ::data-number divide-by]
   :post [s/valid? ::data-float %]}
  (float (/ number divide-by)))


;; Reference when finding the "Closest Variation to the Mean:"
;; https://groups.google.com/g/clojure/c/quEzEM_ndCY
;; Post by: Nicolas Oury On Sat, Sep 25, 2010 at 3:40 PM.
(defn get-variations
  "Calculates the closest and furthest value from the mean for that current month.
   Parameters: data - The current month and all the average values for all years.
   mean-data-list - The current mean for ALL of that month."
  [data mean-data-list]
  {:pre [s/valid? ::data-lazy-seq data s/valid? ::data-float mean-data-list]
   :post [s/valid? ::data-string %]}
  (let [max-month-average (apply min data)]
    (let [max-month-index (.indexOf data (apply min data))]
      (let [closest-variation-to-mean (apply min-key #(Math/abs (- % mean-data-list)) data)]
        (let [closest-variation-to-meanIndex (.indexOf data (apply min-key #(Math/abs (- % mean-data-list)) data))]
          (str "Furthest Month Instance(year) to the mean: " (+ '1772 max-month-index) " |  Max Average Temp: " max-month-average
               "    Closest Month Instance(year) to the mean: " (+ '1772 closest-variation-to-meanIndex) " |  Min Average Temp:" closest-variation-to-mean "\n"))))))

(defn mod-month-day
  "Mods the current Hashmap value by 31 so that a correct day is returned.
   Parameters: data - The hashmap containing the day & temperature."
  [data]
  {:pre [s/valid? ::data-map data]
   :post [s/valid? ::data-map %]}
  {(mod (Integer/parseInt (subs (str (keys data)) 1 (- (count (str (keys data))) 1))) 31) (get data (Integer/parseInt (subs (str (keys data)) 1 (- (count (str (keys data))) 1))))})

(defn set-data
  "Sets the current day & increments values."
  [current-pos current-value]
  ;; (if (= current-pos 1)
  ;;   ;; Year Column
  ;;   (println "Current Year:" (Integer/parseInt (apply str (take 6 current-value)))))
  (if (= current-pos 2)
    ;; Day column
    (cond
      (not= current-day 31)
      ;; Day Column is <= 31
      (swap! current-day inc)
      (= current-day 31)
      ;; Else day is 31 - Sets the value of atom to newval without regard for the current value. Returns newval.
      (reset! current-day 1)))
  (if (> current-pos 2)
    ;; Month Column
    (get months-map current-pos))
  
  
  ;; Return a new collection consisting of the currentMonth applied to the start. Which has been transformed into a sequence.
  (into (apply-current-month-to-data @current-day current-pos (Integer/parseInt (apply str current-value))) (seq [(- current-pos 2)])))

(defn check-line-data-1772
  "Obtains data from the line, looping through each value.
   Parameters: current-line - The current line that the reader has parsed."
  [current-line]
  (loop [current-pos 1 number-line current-line daily-values-vector []]
        (if-not (= current-pos 15)
          (let [current-value (take 1 number-line)]
            (let [current-day-data (set-data current-pos current-value)]
              ;; Data format: (MONTH {Day Temperature})
              ;; (1 {1 23})
              ;; Use recur in tail position | Increments position & drops first item.
              (recur (+ current-pos 1) (drop 1 number-line) (conj daily-values-vector (last current-day-data)))))
          daily-values-vector)))

;; 2.	Find the warmest and coldest years.
(defn find-warmest-and-coldest-years
  "Finds the warmest and coldest years using the total averages of each year."
  [line-data]
  (let [total-average (map (fn [& num] (apply divide-number-month num)) (map #(apply + %) (map add-index (partition 31 (map process-year-data line-data)))))]
    (let [year-average (map (fn [& num] (apply divide-number-year num)) total-average)]
        ;; (println "Year Average: " year-average)
      (println "Coldest Year: " (+ '1772 (.indexOf year-average (apply min year-average))) "with a temperature of: " (apply min year-average))
      (println "Warmest Year: " (+ '1772 (.indexOf year-average (apply max year-average))) "with a temperature of: " (apply max year-average)))))

;; 3.	Find the mean temperature for each calendar month
;; (the average for all Mays, for example) and the instance
;; of each month that has the greatest and smallest variation
;; from that mean.
(defn find-mean-for-each-month-and-variations-from-mean
  "Finds the mean value for each month & smallest/greatest variation from that mean value."
  [line-data]
  (let [divisor-days (* 31 (count (map add-index (partition 31 (map process-year-data line-data)))))]
    (let [days-data (map (fn [num] (divide-number-var num divisor-days)) (add-index (map add-index (partition 31 (map process-year-data line-data)))))]
      (let [divisor-years (count (map add-index (partition 31 (map process-year-data line-data))))]
        (let [years-data (map (fn [num] (divide-number-var num divisor-years)) days-data)]
          (println "Mean Temperature for Each Calender Month (All of that month):" years-data)
          (let [variation-data (map conj-index (partition (count (map add-index (partition 31 (map process-year-data line-data)))) (partition 12 (map (fn [num] (divide-number-month num)) (flatten (map add-index (partition 31 (map process-year-data line-data))))))))]
            (println "Variations: \n" (map (fn [month-average years-data] (get-variations month-average years-data)) (partition (count (map add-index (partition 31 (map process-year-data line-data)))) (flatten variation-data)) years-data))))))))


;; Reference: https://stackoverflow.com/questions/40370240/easy-way-to-change-specific-list-item-in-list
;; Inspired by: Mark Fisher's post - Answered Nov 3, 2016 at 9:45.
(defn replace-value
  "Replaces the old Key:Value pair (hashmap) with the newer version, populates the rest of the structure with original values."
  [day-temperature-list current-vector-index current-item]
  (loop [new-vec [] old-list day-temperature-list]
    (if (seq old-list)
      ;; Return a seq on the collection.
      (if (= (count new-vec) current-vector-index)
        (recur (conj new-vec current-item) (rest old-list))
        (recur (conj new-vec (first old-list)) (rest old-list))) 
    (apply list new-vec))))

(defn read-by-line
  "Reads the file line by line - As opposed to the entire file at once.
   Parameters: file-name - The name of the entered file to be read."
  [file-name]
  (if (not (s/valid? ::is-file-valid file-name))
    (println "File is invalid.")
    (let [line-data
          (with-open [reader (io/reader file-name)]
            (reduce (fn [line-data line]
                      ;; Use reduce with function of two arguments - Take first value for accumulated result.
                      ;; Take sequence of elements & apply function to that result & then to each element etc.
                      ;; Returns the accumulated result.
                      (let [current-line (str/trimr (str/triml line))
                            split-line (str/split current-line #"\s+")
                            number-line (map parse-long split-line)]
                        (conj line-data (check-line-data-1772 number-line))))
                    ;; Define structure as a vector.
                    []
                    (line-seq reader)))]
      line-data)))

;; 1.	Find the warmest day for each calendar month (e.g. the warmest January day, warmest February day and so on) .
(defn find-warmest-and-coldest-day-for-each-month
  "Recurs through each line of data - Replaces the old value with the newer is newTemperature > oldTemperature."
  [line-data]
  (loop [current-index 1 warmest-temp (drop 2 (nth line-data (- current-index 1)))]
    (if-not (= current-index (count line-data))
      (let [current-vector (drop 2 (nth line-data current-index))]
          ;; As vectors are associative, use get-in
          ;; Gets the current nested Vector.
          ;; Result of the inner loop used in the outer.
        (recur (+ current-index 1)
              (loop [warmest-temp warmest-temp current-vector-index 1]
                (if-not (= current-vector-index 12)
                    ;; Whilst month is December or any month beforehand.  
                  (if (> (get (nth current-vector (- current-vector-index 1)) (+ current-index 1)) (get (nth warmest-temp (- current-vector-index 1)) (first (keys (nth warmest-temp (- current-vector-index 1))))))
                      ;; If the current value in the vector is greater than the old value then replace.
                    (recur (replace-value warmest-temp (- current-vector-index 1) (nth current-vector (- current-vector-index 1))) (+ current-vector-index 1))
                      ;; Else do not replace & increase counter.
                    (recur warmest-temp (+ current-vector-index 1)))
                  warmest-temp))))
      (map (fn [month-day] (mod-month-day month-day)) warmest-temp))))

(defn slurp-1772-file
  "Slurps the 1772toDate.txt file line by line."
  []
  (let [line-data (read-by-line "src/assignment/1772toDate.txt")]
    (println "Warmest Day for Each Calender Month: " (find-warmest-and-coldest-day-for-each-month line-data))
    (find-warmest-and-coldest-years line-data)
    (find-mean-for-each-month-and-variations-from-mean line-data)
  ))

(defn initialise-cet-solution 
  "Initialise the solution. Slurp the respective text file."
  []
  (slurp-1772-file))

(defn initialise-project 
  "Initialise the project. Run solution depending on user input."
  []
  (println "Which solution would you like to see?\n
            Enter '1' for ASCII/Morse Conversion.\n
            Enter '2' for the CET solution.")
  (flush)
  (let [userInput (Integer/parseInt (read-line))]
    (if (not (s/valid? ::data-number userInput))
      (println "Only numbers are permitted as input.")
      (cond (= 1 userInput)
            (request-string-to-convert)
            (= 2 userInput)
            (initialise-cet-solution)
            :else
            (println "Unnacepted Request.")
            ))))
        

(defn -main 
  "Entry point for the program."
  [& args]
  (println "Executing project:\n Author: Chris Deam.")
  (initialise-project)
)