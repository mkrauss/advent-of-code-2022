;; -*- lexical-binding: t -*-

(defun aoc/read-all-elf-calories (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (mapcar (lambda (elf)
              (apply '+
                     (mapcar 'string-to-number (split-string elf "\n" t))))
            (split-string (buffer-string) "\n\n" t))))

(defun aoc/top-three-elves (filename)
  (cl-subseq (sort (aoc/read-all-elf-calories filename) '>) 0 3))

;; Part 1
(apply 'max (aoc/read-all-elf-calories "input"))

;; Part 2
(apply '+ (aoc/top-three-elves "input"))
