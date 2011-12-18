
;; Big endian, byte adressable, two byte word size memory

(set '*BYTE-LENGTH* 2)
(set '*WORD-LENGTH* 2)
(set '*BYTE-MAX* #xFF)
(set '*WORD-MAX* #xFFFF)
(set '*ROW-LENGTH* 8)

(defun snd (word)
  (if (> word *WORD-MAX*)
      (error (format "Number %X is too large." word))
    (logand #x00FF word)))

(defun fst (word)
  (if (> word *WORD-MAX*)
      (error (format "Number %X is too large." word))
    (lsh word (- 0 8))))

(set 'MEM 
     (make-hash-table :test 'eql 
		      :size *WORD-MAX*))

(set 'CACHE1
     (make-hash-table :test 'eql
		      :size *BYTE-MAX*))

(defun fetch (addr)
    (if (>= addr *WORD-MAX*)
	(error (format "Maximum address size: %X" (- *WORD-MAX* 1)))
      (logior (lsh (gethash addr MEM #x0000) 8) 
	      (gethash (+ addr 1) MEM #x0000))))

(defun write (addr word)
    (if (>= addr *WORD-MAX*)
	(error (format "Maximum address size: %X" (- *WORD-MAX* 1)))
      (if (>= word *WORD-MAX*)
	  	(error (format "Maximum data size: %X" (- *WORD-MAX* 1)))
	(progn (puthash addr (fst word) MEM)
	       (puthash (+ addr 1) (snd word) MEM)))))

(defun print-word (word)
  (format " %04X" word))

(defun print-row (addr &optional dec out)
  (if (eq dec nil)
      (print-row addr *ROW-LENGTH* (format "\n%04X:" addr))
    (cond ((and (< addr *WORD-MAX*) (>= dec *WORD-LENGTH*))
	   (print-row (+ addr *WORD-LENGTH*) (- dec *WORD-LENGTH*) 
		      (concat out (print-word (fetch addr)))))
	  ((eq dec 0) out)
	  ((or (eq addr *WORD-MAX*) (eq dec 1))
	   (concat out (format " %02X" (fetch addr)))))))

(defun print-mem (addr rows &optional out)
  (if (eq out nil)
      (print-mem addr rows "")
    (if (and (> rows 0) 
	     (<= addr *WORD-MAX*))
	(print-mem (+ addr *ROW-LENGTH*) (- rows 1)
		   (concat out (print-row addr)))
      out)))

(defun display-data (data)
  (progn 
    (pop-to-buffer "*VMEM*")
    (erase-buffer)
    (insert data)))

(defun clean-display ()
  (progn 
    (delete-windows-on "*VMEM*")
    (kill-buffer "*VMEM*")))


;;
;; Test
;;

(write #x1 #xAAAA)
(format "%X" (fetch #x1))

(display-data (print-mem #x00 10))
(clean-display)
