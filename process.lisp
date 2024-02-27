
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


(defun process-line (line)

  (format t "Packet #N - ")

  (let ((table (ascii-table:make-table '("Data" "Description") :header "Packet #x"))
        (sp  (split-sequence:split-sequence  #\Space line)))

    (trivia:match sp
      ((cons x y)
       (format t "X: ~s~%" x)
       (format t "Y: ~s~%" y)))

    ;;    (ascii-table:add-row table '(1 "Bob" 150))
    ;;    (ascii-table:add-separator table)
    ;;    (ascii-table:add-row table '("" "Total" 350))
    (ascii-table:display table)))


(defun start ()
  (let ((in (open "sample-tool-dump.dat" :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
            while line do (progn
                            (let* ((s1 (str:split "bytes"  line))
                                   (s2 (str:substring 4 t s1)))
                              ;;                            (format t "LINE: ~a~%" (first s2))
                              (process-line (second s1))
                              )))
      (close in))))


