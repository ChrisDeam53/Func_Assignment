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

(def warmest-day-each-month [])

(def warmest-year "")

(def coldest-year "")

(def mean-temperature-each-month [])

(def instance-of-month-with-greatest-variation-from-mean "")

(def instance-of-month-with-smallest-variation-from-mean "")

(defn check-warmest-day-for-each-month
  "TODO"
  [enteredMonth temperature]
  ;; Perhaps use pattern matching
  )

(defn set-data
  "DOING"
  [currentPos currentValue]
  (println currentPos)
  (println "CURRENT VALUE INT:")
  (println (Integer/parseInt (apply str (take 6 currentValue))))
  
  (if (> currentPos 2)
    (check-warmest-day-for-each-month currentPos currentValue))
  )

(defn check-line-data-1772
  "Obtains data from the line.
   Parameters: currentLine - The current line that the reader has parsed."
  [currentLine]
  ;; (println currentLine)
  (let [splitLine (str/split currentLine #"\s+")]
    (let [numberLine (map parse-long splitLine)]
      (println numberLine)
      (loop [currentPos 1 currentData numberLine]
        ;; Loop through each character in the string.
        ;; Default is 15
        (when (< currentPos 15)
          (let [currentValue (take 1 currentData)]
            (set-data currentPos currentValue)
            ;; Use recur in tail position | Increments position & drops first item.
            (recur (+ currentPos 1) (drop currentPos numberLine))))))))

;; Example data:
;; 1772    1   32  -15   18   25   87  128  187  177  105  111   78  112

;; NOTE: External sources (non-Clojure documentation) has been referenced to read line-by-line.
;; REFERENCES:
;; https://www.tutorialspoint.com/clojure/clojure_file_io.htm
;;  ^ Reference used to compare "slurp" to the "clojure.java.io" approach.
;; https://stackoverflow.com/questions/25948813/read-line-by-line-for-big-files 
;;  ^ Snippet inspired by the post by "schaueho" - Answered Sep 20, 2014 at 16:17
(defn read-by-line
  "Reads the file line by line - As opposed to the entire file at once.
   Parameters: fileName - The name of the entered file to be read."
  [fileName]
  (with-open [reader (io/reader fileName)]
    (doseq [line (line-seq reader)]
      ;; Repeatedly executes the body- Does not retain the head of the sequence (doseq).
      ;; Returns the lines of text from reader as a lazy sequence of strings.
      (check-line-data-1772 (str/trimr (str/triml line))))))

(defn slurp-1772-file
  "Slurps the 1772toDate.txt file line by line."
  []
  (println (read-by-line "src/assignment/test.txt")))

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





;; (initialise-project)

;; Example code provided by Calva.
;; (comment
;;   (+ (* 2 2)
;;      2)

;;   (Math/abs -1)

;;   (hello "Calva REPL")

;;   (defn hello [s]
;;     (str "Hello " s))

;;   (range 10)

;;   "I ♥️ Clojure"
;;   )