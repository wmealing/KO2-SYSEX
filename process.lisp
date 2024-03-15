
(ql:quickload "str")
(ql:quickload "cl-ascii-table")
(ql:quickload "trivia")
(ql:quickload "split-sequence")

(defparameter SOX-START "F0")
(defvar TEENAGE-ID "00 20 76")
(defvar THIRTY-THREE "33")
(defvar FORTY "40")
(defvar TO-KO2 "77")
(defvar FROM-KO2 "37" )
(defvar SOX-END "F7")


(defparameter header-start 0)
(defparameter header-len 4)
(defparameter header-end (+ header-start header-len))

(defparameter header-padding-start header-end)
(defparameter header-padding-len 2)
(defparameter header-padding-end (+ header-padding-start header-padding-len))

(defparameter direction-start header-padding-end)
(defparameter direction-len 1 )
(defparameter direction-end (+ direction-start direction-len))

(defparameter seq-num-start direction-end)
(defparameter seq-num-len 1 )
(defparameter seq-num-end (+ seq-num-start seq-num-len))


;; FIX
(defparameter rest seq-num-end)

(defvar message-count 0)

(defun pack (source-array)
  (let ((r 1)
        (i 0)
        (target-array  (make-array (length source-array))))
    (loop for s below (length source-array)
          do (let ((o (mod s 7))
                   (l (ash (aref source-array s) -7)))
               (setf (aref target-array i) (logior (aref target-array i) (ash l o)))
               (setf (aref target-array r) (logand (aref source-array s) 127))
               (when (= o 6)
                 (when (< s (- (length source-array) 1))
                   (incf i 8)
                   (incf r)))))
    target-array))

(defun process-line (line)
  (check-type line string)

  (format t "Midi Message #~s ~%" message-count)

  (setq message-count (+ message-count 1))
  
  ;;  (format t "LINE LENGTH: ~s~%" (length line))
  
  (if (>= 18 (length line))
      (progn
        (format t "LINE TOO SHORT ~%")
        (force-output)
        (return-from process-line nil)))

  (let* (;;(table (ascii-table:make-table '("Data" "Description") :header "Packet #x"))
         (line-trimmed   (string-left-trim '(#\Space #\Tab #\Newline)  line))
         (final-seq      (split-sequence:split-sequence  #\Space line-trimmed))
         (header         (subseq final-seq header-start header-end))
         (header-padding (subseq final-seq header-padding-start header-padding-end))
         (direction (subseq final-seq direction-start direction-end))
         (seq-number (subseq final-seq seq-num-start seq-num-end))
         ;;         (mmmm (subseq final-seq header-padding-end))
         )

    (format t "FINAL-SEQ LENGTH: ~s~%" (length final-seq))
    ;;    (format t "SEQ: ~s~%" final-seq)
    (format t "HEADER: ~s~%" header)
    (format t "PADDING: ~s~%" header-padding)
    ;;    (format t "DIRECTION-START: ~s~%" direction-start)
    ;;    (format t "DIRECTION-END: ~s~%" direction-end)
    (format t "DIRECTION: ~s~%"   direction)
    (format t "SEQ-NUM: ~s~%"   seq-number)
    (format t "----------~%")


    ;;    (ascii-table:add-row table '(1 "Bob" 150))
    ;;    (ascii-table:add-separator table)
    ;;    (ascii-table:add-row table '("" "Total" 350))
    ;; (ascii-table:display table)
    ))


(defun start ()
  (let ((in (open "sample-tool-dump.dat" :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
            while line do (progn
                            (let* ((s1 (str:split "bytes"  line)))
                              (process-line (second s1))
                              )))
      (close in))))


