(ns assignment.core-test
  (:require [clojure.test :refer :all]
            [assignment.core :refer :all]))


;; /****************************************************************************************/
;; ASCII->Morse & Morse->ASCII Unit Tests.
;; /****************************************************************************************/

;; Test that a-z & 0-9 works.
(deftest get-character-test
  (testing "Test that correct Morse Code is supplied when provided ASCII values.."
    (is (= (get-character "abcdefghijklmnopqrstuvwxyz 1234567890") ".-   -...   -.-.   -..   .   ..-.   --.   ....   ..   .---   -.-   .-..   --   -.   ---   .--.   --.-   .-.   ...   -   ..-   ...-   .--   -..-   -.--   --..       .----   ..---   ...--   ....-   .....   -....   --...   ---..   ----.   -----   "))))

;; Test that a-z & 0-9 works.
(deftest get-character-test
  (testing "Test that correct Morse Code is supplied when provided Morse values.."
    (is (= (get-character ".-   -...   -.-.   -..   .   ..-.   --.   ....   ..   .---   -.-   .-..   --   -.   ---   .--.   --.-   .-.   ...   -   ..-   ...-   .--   -..-   -.--   --..       .----   ..---   ...--   ....-   .....   -....   --...   ---..   ----.   -----") "ABCDEFGHIJKLMNOPQRSTUVWXYZ 1234567890"))))

(deftest translate-character-to-ascii-test
  (testing "Ensure correct ASCII->Map value is retrieved.")
  (is (= (translate-character-to-ascii ".-") "A")))

(deftest translate-character-and-space-to-ascii-test
  (testing "Ensure that a space is retrieved from the values.")
  (is (= (translate-character-and-space-to-ascii ".- -...") " ")))

(deftest translate-character-to-morse-test
  (testing "Ensure that a space is retrieved from the values.")
  (is (= (translate-character-to-morse "A") ".-   "))
  (is (= (translate-character-to-morse "B") "-...   ")))



;; /****************************************************************************************/
;; CET Unit Tests.
;; NOTE: Some Anonymous functions are not tested (instead the caller of these functions are tested
;; and the results are checked).
;; /****************************************************************************************/

(deftest add-index-test)

(deftest conj-index-test)

(deftest replace-value-test)

(deftest process-year-data-test
  (testing "Ensure that a correct sequence containing the correct Values from the sequence of Hashmaps is returned.")
  (is (= (process-year-data [-1 0 {59 32} {59 70} {59 29} {59 121} {59 117} {59 138} {59 130} {59 163} {59 105} {59 113} {59 66} {59 60}]) '(32 70 29 121 117 138 130 163 105 113 66 60)))
  (is (= (process-year-data [-1 0 {49 52} {49 42} {49 66} {49 52} {49 161} {49 150} {49 173} {49 134} {49 107} {49 113} {49 71} {49 68}]) '(52 42 66 52 161 150 173 134 107 113 71 68))))

(deftest update-current-map-test
  (testing "Ensure that the current daily-map key and value are updated correctly.")
  (is (= (update-current-map {0 0} 0 21 151) {21 151}))
  (is (= (update-current-map {0 0} 0 23 -28) {23 -28})))

(deftest apply-current-month-to-data-test
  (testing "Ensure that the hashmap correctly returns the {DAY TEMPERATURE} in that format.")
  (is (= (apply-current-month-to-data 24 4 -10) '({24 -10})))
  (is (= (apply-current-month-to-data 27 6 74) '({27 74}))))

(deftest check-line-data-1772-test
  (testing "Ensure correct vector is returned when passed in a line of data from the .txt file.")
  (is (= (check-line-data-1772 '(1772 1 32 -15 18 25 87 128 187 177 105 111 78 112))) [-1 0 {1 32} {1 -15} {1 18} {1 25} {1 87} {1 128} {1 187} {1 177} {1 105} {1 111} {1 78} {1 112}])
  (is (= (check-line-data-1772 '(1773 31 -5 -999 54 -999 163 -999 152 160 -999 44 -999 -17))) [-1 0 {62 -5} {62 -999} {62 54} {62 -999} {62 163} {62 -999} {62 152} {62 160} {62 -999} {62 44} {62 -999} {62 -17}]))

(deftest mod-month-day-test
  (testing "Ensure correct modulo operation is taking place.")
  (is (= (mod-month-day {45 92}) {14 92}))
  (is (= (mod-month-day {54 92}) {23 92}))
  (is (= (mod-month-day {56 119}) {25 119})))

(deftest get-variations-test
  (testing "Ensure that the furthest and closest numbers from the means return correct values.")
  (is (= (get-variations '(12.16129 40.064518) 13.056452) "Furthest Month Instance(year) to the mean: 1772 |  Max Average Temp: 12.16129    Closest Month Instance(year) to the mean: 1772 |  Min Average Temp:12.16129\n")))

(deftest divide-number-month-test
  (testing "Ensure month division is correct.")
  (is (= (divide-number-month -1454) (float -46.903225)))
  (is (= (divide-number-month 1481) (float 47.774193)))
  (is (= (divide-number-month 377) (float 12.16129))))

(deftest divide-number-year-test
  (testing "Ensure yearly division is correct.")
  (is (= (divide-number-year 27576) 2298.0)))

(deftest divide-number-var-test
  (testing "Ensure variable division is correct.")
  (is (= (divide-number-var 50 2) (float 25.0)))
  (is (= (divide-number-var 100 2) (float 50.0))))