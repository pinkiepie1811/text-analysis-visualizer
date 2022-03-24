#lang racket
(require 2htdp/image)
(require csc151)
(require csc151/rex)
(require rackunit)
(require rackunit/text-ui)

; CSC-151-01 Spring 2021 Term 1
; Mini Project 4
; Author: Hong Doan
; Date: 2021-02-28
; Acknowledgements:
;  Zaen Chou and Laura Kiely for helping debugging the code
;; Reference:
;;   Minqing Hu and Bing Liu. "Mining and Summarizing Customer Reviews." 
;       Proceedings of the ACM SIGKDD International Conference on Knowledge 
;       Discovery and Data Mining (KDD-2004), Aug 22-25, 2004, Seattle, 
;       Washington, USA
; <https://www.gutenberg.org/ebooks/1342>
; Hong, Doan - Mini Project 3



(define uppercase
  (rex-char-range #\A #\Z))

(define lowercase
  (rex-char-range #\a #\z))

(define fanboys
  (rex-any-of (rex-string "for")
              (rex-string "and")
              (rex-string "nor")
              (rex-string "but")
              (rex-string "or")
              (rex-string "yet")
              (rex-string "so")))

  
(define compound-sentence
  (rex-concat  uppercase
               (rex-repeat (rex-any-char))
               (rex-any-of (rex-concat (rex-string ", ") fanboys)
                           (rex-string ";"))
               (rex-repeat (rex-any-char))
               (rex-any-of (rex-string ".")
                           (rex-string "!")
                           (rex-string "?"))))

(test-true "a simple case" (rex-matches? compound-sentence "I am a student, and I am studying."))
(test-false "wrong case" (rex-matches? compound-sentence "I am a student, I am studying."))
(test-false "no period" (rex-matches? compound-sentence "I am a student, and I am studying"))
(test-false "empty" (rex-matches? compound-sentence ""))
                          
;;; (compound-sentence-count str) -> integer?
;;; str: string?
;;; Count the number of compound sentences in a string.
(define compound-sentence-count
  (lambda (str)
    (length (rex-find-matches compound-sentence str))))

;;; (num-sentences str)->integer?
;;; str: string?
;;; count the number of sentences in a string
(define num-sentences
  (lambda (str)
    (length (rex-split-string (rex-any-of (rex-string ".")
                                          (rex-string "!")
                                          (rex-string "?")) str))))

;;; (compound-sentence-rate-visual str)-> image?
;;; str: string?
;;; produce a rectangle with the height relative to the percentage of compound sentences in a string
;;; (bigger rectangle indicates higher frequency of compound sentences in a string)
(define compound-sentence-rate-visual
  (lambda (str)
    (let ([percent (/ (compound-sentence-count str) (num-sentences str))])
      (rectangle 150 (* percent 250) 'solid "spring green"))))


  
;;; (extract-words str)
;;;   str : string?
;;; Extract all of the words from str.  A word is a sequence of
;;; letters and apostrophes.
;;; This procedure is copied from the assignment webpage
;;; <https://rebelsky.cs.grinnell.edu/Courses/CSC151/2021SpT1/assignments/mp03.html>
(define extract-words
  (let ([rex-word
         (rex-repeat (rex-any-of (rex-char-range #\a #\z)
                                 (rex-char-range #\A #\Z)
                                 (rex-string "'")
                                 (rex-string "-")))])
    (lambda (str)
      (rex-find-matches rex-word str))))

;;; (count-one-word word lst) -> int?
;;; word: string?
;;; lst: list?
;;; count how many times a word appear in a list
(define count-one-word
  (lambda (word lst)
    (tally  (section string-ci=? word <>) lst)))

;;; (num-positive-words str)->int?
;;; str: string?
;;; Count the number of positive words appear in a string
(define num-positive-words
  (lambda (str)
    (apply + (map (section count-one-word <> (extract-words str)) (extract-words (file->string "positive-words.txt"))))))

;;; (num-negative-words str)->int?
;;; str: string?
;;; Count the number of negative words appear in a string
(define num-negative-words
  (lambda (str)
    (apply + (map (section count-one-word <> (extract-words str)) (extract-words (file->string "negative-words.txt"))))))

;;; (total-words str) -> integers?
;;;    str: string?
;;; Count the total number of words in a string
(define total-words
  (lambda (str)
    (length (extract-words str))))

;;; (square-block color) -> image?
;;; color: string?
;;; Draw a square with the color indicated
(define square-block
  (lambda (color)
    (overlay (square 10 'outline "black")
             (square 10 'solid color))))

;;; (block-column color) -> image?
;;; color: string?
;;; Draw a column of 10 square blocks with the color indicated
(define block-column
  (lambda (color)
    (apply above (make-list 10 (square-block color)))))


;;; (column-set number color) -> image?
;;; number: integer?, natural number
;;; color: string?
;;; return a set of number of columns with indicated color
(define column-set
  (lambda (number color)
    (if (< number 1)
        empty-image
        (beside (block-column color) (column-set (- number 1) color)))))

;;; (set-of-blocks number color) -> image?
;;; number: integer?, natural number
;;; color: string?
;;; return a column with number blocks in indicated color
(define set-of-blocks
  (lambda (number color)
    (if (< number 1)
        empty-image
        (above (square-block color) (set-of-blocks (- number 1) color)))))


;;;(posneg-visual str)-> image?
;;; str: string?
;;; return a 10x10 grid that visualize the use of positive/negative words in a string.
;;; one block represent 1% of words in a string
;;; pink is for positve, teal is for negative, and white is for neutral
(define posneg-visual
  (lambda (str)
    (let* ([total (total-words str)]
           [pos (round (* (/ (num-positive-words str) (total-words str)) 100))]
           [neg (round (* (/ (num-negative-words str) (total-words str)) 100))]
           [neutral (- 100 pos neg)]
           [pos-quotient (quotient pos 10)]
           [neg-quotient (quotient neg 10)]
           [pos-remainder (remainder pos 10)]
           [neg-remainder (remainder neg 10)]
           [neutral-pos (- 10 pos-remainder)]
           [neutral-neg (- 10 neg-remainder)]
           [neutral-quotient (quotient neutral 10)])
      (beside (column-set pos-quotient "pink")
              (above (set-of-blocks pos-remainder "pink")
                     (set-of-blocks neutral-pos "white"))
              (column-set neutral-quotient "white")
              (above (set-of-blocks neutral-neg "white")
                     (set-of-blocks neg-remainder "teal"))
              (column-set neg-quotient "teal") ))))
      

;;; (visualization filename)-> image
;;; filename: string?
;;; return an image with a 10x10 grid representing the use of positive/negative words
;;; and a rectangle representing the percentage of compound sentences.
(define visualization
  (lambda (filename)
    (let ([text (file->string filename)])
      (overlay (posneg-visual text)
               (compound-sentence-rate-visual text)))))



#| (save-image
   (beside (visualization "sample.txt")
           (visualization "pride-and-prejudices-chap-1.txt"))
   "analysis.png")
#t
.

- The file "sample.txt" used compound sentences more frequently
(which means its ratio (compound sentences/total sentences) is higher,
and can not be inferred to the file "sample.txt" has more compound sentences.)

- The percentages of both positive and negative words in "sample.txt" is less than 1%,
which indicated by all blocks in the grid are white (neutral).
- The file "pride-and-prejudice-chap-1.txt" contains about 6% positve words and 3% negative words
(6 pink blocks and 3 teal blocks).

|#
