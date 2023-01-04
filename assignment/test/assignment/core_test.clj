(ns assignment.core-test
  (:require [clojure.test :refer :all]
            [assignment.core :refer :all]
            [clojure.spec.alpha :as s]))

(require '[clojure.spec.alpha :as s])
(require '[clojure.java.io :as io])

;; /***********************************************************************************************************************/
;; ASCII->Morse & Morse->ASCII Unit Tests.
;; /***********************************************************************************************************************/
(def regex-to-check-morse-test #"^[\.\-\ ]+$")
(def regex-to-check-ascii-test #"^[a-zA-Z0-9\ ]+$")

(s/def ::morse-code-input-validation (s/and string? #(re-matches regex-to-check-morse-test %)))

(s/def ::string-input-validation (s/and string? #(re-matches regex-to-check-ascii-test %)))

(deftest user-input-validation-for-morse-test
  (testing "Test that spec works with morse.")
  (is (s/valid? ::morse-code-input-validation ".-   -...   -.-."))
  (is (s/valid? ::morse-code-input-validation ".-   -...   -.-.   -..   .   ..-.   --.   ....   ..   .---   -.-   .-..")))

(deftest user-input-validation-for-ascii-test
  (testing "Test that spec works with ascii.")
  (is (s/valid? ::string-input-validation "Hello World"))
  (is (s/valid? ::string-input-validation "abcdefghijklmnopqrstuvwxyz 1234567890"))
  (is (not (s/valid? ::string-input-validation "[]@#;<>]")))
  (is (not (s/valid? ::string-input-validation "Hello World []"))))

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

;; /***********************************************************************************************************************/
;; CET Unit Tests.
;; NOTE: Some methods are *not* tested as they contain pure Clojure functions. Therefore it would be pointless to test.
;; NOTE: Some methods not tested are anonymous functions, which are therefore tested when testing the caller of this method.
;; /***********************************************************************************************************************/
(s/def ::data-number int?)
(s/def ::does-file-exist #(.exists (io/file %)))
(s/def ::is-file-valid ::does-file-exist)

(deftest check-file-is-valid-test
  (testing "Ensures that the test passes if the file exists and is correct")
  (is (s/valid? ::is-file-valid "src/assignment/1772toDate.txt"))
  (is (not (s/valid? ::is-file-valid "src/assignment/non-existent-file.txt"))))

(deftest is-number-test
  (testing "Ensures that the integer check in spec works.")
  (is (s/valid? ::data-number 45))
  (is (not (s/valid? ::data-number "abd"))))

(deftest read-by-line-test
  (testing "Ensure that read-by-line returns the correct  vector value.")
  (is (= (read-by-line "test/assignment/testFile.txt") [[-1 0 {3 0} {3 56} {3 21} {3 87} {3 67} {3 155} {3 179} {3 155} {3 143} {3 106} {3 70} {3 62}] [-1 0 {4 45} {4 28} {4 13} {4 67} {4 59} {4 148} {4 177} {4 153} {4 120} {4 119} {4 122} {4 100}] [-1 0 {5 62} {5 20} {5 16} {5 58} {5 87} {5 176} {5 173} {5 136} {5 143} {5 109} {5 95} {5 45}] [-1 0 {6 52} {6 40} {6 8} {6 109} {6 111} {6 152} {6 160} {6 139} {6 160} {6 138} {6 92} {6 67}] [-1 0 {7 25} {7 57} {7 -2} {7 99} {7 116} {7 143} {7 168} {7 160} {7 133} {7 101} {7 50} {7 82}] [-1 0 {8 17} {8 38} {8 -17} {8 48} {8 74} {8 146} {8 203} {8 153} {8 143} {8 91} {8 43} {8 77}] [-1 0 {9 30} {9 2} {9 -14} {9 58} {9 101} {9 171} {9 184} {9 163} {9 143} {9 124} {9 73} {9 70}] [-1 0 {10 20} {10 5} {10 -2} {10 33} {10 137} {10 171} {10 192} {10 155} {10 128} {10 138} {10 48} {10 87}] [-1 0 {11 -18} {11 25} {11 8} {11 45} {11 121} {11 193} {11 165} {11 174} {11 133} {11 134} {11 55} {11 90}] [-1 0 {12 -13} {12 -3} {12 41} {12 48} {12 79} {12 171} {12 170} {12 192} {12 123} {12 119} {12 60} {12 60}] [-1 0 {13 -18} {13 0} {13 73} {13 -2} {13 92} {13 184} {13 151} {13 194} {13 138} {13 114} {13 36} {13 112}] [-1 0 {14 -10} {14 7} {14 53} {14 53} {14 74} {14 195} {14 151} {14 136} {14 108} {14 129} {14 78} {14 97}] [-1 0 {15 -6} {15 7} {15 51} {15 53} {15 96} {15 162} {15 151} {15 136} {15 143} {15 119} {15 85} {15 67}] [-1 0 {16 15} {16 -3} {16 98} {16 74} {16 111} {16 171} {16 158} {16 155} {16 135} {16 84} {16 26} {16 10}] [-1 0 {17 12} {17 -10} {17 76} {17 84} {17 136} {17 179} {17 197} {17 155} {17 110} {17 91} {17 92} {17 -28}] [-1 0 {18 5} {18 -10} {18 73} {18 104} {18 118} {18 195} {18 192} {18 165} {18 99} {18 119} {18 53} {18 7}] [-1 0 {19 12} {19 30} {19 81} {19 74} {19 131} {19 171} {19 192} {19 134} {19 113} {19 121} {19 45} {19 -40}] [-1 0 {20 15} {20 85} {20 71} {20 74} {20 108} {20 208} {20 160} {20 158} {20 105} {20 104} {20 63} {20 -8}] [-1 0 {21 0} {21 46} {21 66} {21 74} {21 77} {21 198} {21 156} {21 144} {21 76} {21 104} {21 45} {21 5}] [-1 0 {22 15} {22 77} {22 86} {22 64} {22 116} {22 167} {22 151} {22 155} {22 66} {22 84} {22 60} {22 10}] [-1 0 {23 -33} {23 56} {23 83} {23 50} {23 113} {23 131} {23 170} {23 182} {23 135} {23 140} {23 63} {23 12}] [-1 0 {24 -10} {24 -999} {24 66} {24 77} {24 121} {24 122} {24 179} {24 163} {24 143} {24 143} {24 55} {24 15}] [-1 0 {25 -8} {25 -999} {25 46} {25 -999} {25 108} {25 -999} {25 168} {25 144} {25 -999} {25 145} {25 -999} {25 22}]])))

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
  (is (= (check-line-data-1772 '(1772 1 32 -15 18 25 87 128 187 177 105 111 78 112)) [-1 0 {1 32} {1 -15} {1 18} {1 25} {1 87} {1 128} {1 187} {1 177} {1 105} {1 111} {1 78} {1 112}]))
  (is (= (check-line-data-1772 '(1773 31 -5 -999 54 -999 163 -999 152 160 -999 44 -999 -17)) [-1 0 {2 -5} {2 -999} {2 54} {2 -999} {2 163} {2 -999} {2 152} {2 160} {2 -999} {2 44} {2 -999} {2 -17}])))

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