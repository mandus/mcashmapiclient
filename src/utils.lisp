(defun x-mcash-headers (headers)
  (labels ((inner-x-mcash (headers ret)
             (if (null headers)
               ret
               (if (cl-ppcre:scan "^X-Mcash" (car (car headers)))
                 (inner-x-mcash (cdr headers) (cons 
                                                (cons (string-upcase (car (car headers))) (cdr (car headers)))
                                                ret))
                 (inner-x-mcash (cdr headers) ret)))))
    (inner-x-mcash headers nil)))

(defun sort-headers-on-key (headers)
  (sort headers #'(lambda (x y) (string-lessp (car x) (car y)))))

(defun downcase-prot-in-url (url)
  (cl-ppcre:register-groups-bind 
    (prot adr)
    ("^(.*)://(.*)" url)
    (concatenate 'string (string-downcase prot) "://" adr)))

(defun join-strings-with-pipe (string-list)
  (format nil "~{~A~^|~}" string-list))

(defun join-strings (string-list &key (symb "|"))
  (format nil (concatenate 'string "~{~A~^" symb "~}") string-list))

(defun join-header-pairs (headers)
  (let ((head (car headers))
        (tail (cdr headers)))
    (cons 
      (if (cdr head)
        (concatenate 'string (car head) "=" (cdr head))
        (car head))
      (if tail
        (join-header-pairs tail)
        nil))))

(defun get-time-stamp ()
  (multiple-value-bind
    (second minute hour date month year day-of-week dst-p tz)
    (get-decoded-time)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year
            month
            date
            hour
            minute
            second)))

(defun base64-encode-string (string)
  (let ((strbytes (map 'vector #'char-code string)))
    (with-output-to-string (out)
      (s-base64:encode-base64-bytes strbytes out))))

(defun base64-decode-string (string)
  (map 'string #'code-char 
       (with-input-from-string (in string)
         (s-base64:decode-base64-bytes in))))

(defun sha256-digest (string)
  (let ((strbytes (ironclad:ascii-string-to-byte-array string)))
    (ironclad:byte-array-to-hex-string
      (ironclad:digest-sequence :sha256 strbytes))))

(defun base64-sha256-digest (string)
  (let ((strbytes (ironclad:ascii-string-to-byte-array string)))
    (with-output-to-string (out) 
      (s-base64:encode-base64-bytes 
        (ironclad:digest-sequence :sha256 strbytes) out)))) 
