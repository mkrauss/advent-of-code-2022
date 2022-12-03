;; -*- lexical-binding: t -*-

(defun aoc/translate-their-move (move)
  (cl-case (intern move)
    (A 'rock)
    (B 'paper)
    (C 'scissors)))

(defun aoc/translate-response-type (response)
  (cl-case (intern response)
    (X 'lose)
    (Y 'draw)
    (Z 'win)))

(defun aoc/translate-my-move (their-move my-move)
  (cl-case (aoc/translate-their-move their-move)
    (rock (cl-case (aoc/translate-response-type my-move)
            (lose 'scissors)
            (draw 'rock)
            (win 'paper)))
    (paper (cl-case (aoc/translate-response-type my-move)
             (lose 'rock)
             (draw 'paper)
             (win 'scissors)))
    (scissors (cl-case (aoc/translate-response-type my-move)
                (lose 'paper)
                (draw 'scissors)
                (win 'rock)))))

(defun aoc/translate-guide (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (mapcar (lambda (round)
              (cl-destructuring-bind (them me) (split-string round " " t)
                (cons (aoc/translate-their-move them)
                      (aoc/translate-my-move them me))))
            (split-string (buffer-string) "\n" t))))

(defun aoc/score-for-shape (shape)
  (cl-case shape
    ('rock 1)
    ('paper 2)
    ('scissors 3)))

(defun aoc/x-beats-y (x y)
  (or (and (eql x 'rock) (eql y 'scissors) "rock breaks scissors")
      (and (eql x 'scissors) (eql y 'paper) "scissors cut paper")
      (and (eql x 'paper) (eql y 'rock) "paper covers rock")))

(defun aoc/score-for-outcome (round)
  (cl-destructuring-bind (them . me) round
    (cond ((eql them me) 3)
          ((aoc/x-beats-y me them) 6)
          ((aoc/x-beats-y them me) 0))))

(defun aoc/score-for-round (round)
  (+ (aoc/score-for-shape (cdr round))
     (aoc/score-for-outcome round)))

(defun aoc/compute-score-from-guide (filename)
  (let ((guide (aoc/translate-guide filename)))
    (reduce '+ (mapcar 'aoc/score-for-round guide))))

(aoc/compute-score-from-guide "input")
