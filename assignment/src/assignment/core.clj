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

(def letter-hash-map
  "Define strings to their respective Morse counterparts."
  {"A" ".-", "B" "-...", "C" "-.-.", "D" "-..", "E" ".", "F" "..-.", "G" "--.", "H" "....",
   "I" "..", "J" ".---", "K" "-.-", "L" ".-..", "M" "--", "N" "-.", "O" "---", "P" ".--.",
   "Q" "--.-", "R" ".-.", "S" "...", "T" "-", "U" "..-", "V" "...-", "W" ".--", "X" "-..-",
   "Y" "-.--", "Z" "--..", "0" "-----", "1" ".----", "2" "..---", "3" "...--", "4" "....-",
   "5" ".....", "6" "-....", "7" "--...", "8" "---..", "9" "----.", " " "       "})

(def morse-hash-map (cljSet/map-invert letter-hash-map))

(defn translate-character-to-morse [currentCharacter]
  "Performs the get method on the character found inside the letter-hash-map.
   Arguments: currentCharacter - Individual Character in the string to perform the get method."
  (let [returnString (get letter-hash-map currentCharacter)]
    (if (str/includes? returnString " ")
      ;; Append 2x spaces to the end, totals to 7 spaces between words.
      (str (str/trim (get letter-hash-map currentCharacter)) "   ")
      ;; Append 3x spaces to the end of each morse character returned if not a space.
      (str returnString "  "))))

(defn translate-character-to-ascii [currentCharacter]
  "Performs the get method on the character found inside the letter-hash-map.
   Arguments: currentCharacter - Individual Character in the string to perform the get method."
  (println currentCharacter)
  (get morse-hash-map currentCharacter))

(defn get-character [enteredString]
  "TODO: This method.
   Arguments: enteredString - The string to be converted to its appropriate counterpart."
    ;; Covert all characters to uppercase for uniformty.
  (let [characterVectorUpper (str/upper-case enteredString)]
    ;; Use Regex to get each individual character, including spaces. Returns a vector.
    (if (or (= (get characterVectorUpper 0) \.) (= (get characterVectorUpper 0) \-))
      (let [characterVector (str/split characterVectorUpper #"\s+")]
        ;; String entered is Morse.
        ;; Join all empty spaces between. Example: "A B" -> "AB".
        (str/join "" (map translate-character-to-ascii characterVector)))
      (let [characterVector (str/split characterVectorUpper #"")]
        ;; String entered is ASCII.
        (map translate-character-to-morse characterVector)))))


(defn request-string-to-convert []
  "Accepts user input & calls conversion method. Invokes [[string-to-morse]]."
  (println "Please enter a valid ASCII string or Morse Code.")
  (flush)
  (let [enteredString (str (read-line))]
    (println(get-character enteredString))))

(defn func2 []
  (println "BBBBBBBBBBBB"))

(defn initialise-project []
  "Initialise the project. Run solution depending on user input."
  
  (println "Which solution would you like to see?\n
            Enter '1' for ASCII/Morse Conversion\n
            Enter '2' for the CET solution.")
  (flush)
  (let [userInput (Integer/parseInt (read-line))]
      (cond (= 1 userInput)
            (request-string-to-convert)
            (= 2 userInput)
            (func2)
            :else
            (println "Unnacepted Request.")
            )))
        

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (println "Executing project:\n Author: Chris Deam")
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