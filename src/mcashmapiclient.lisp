;
; (c) [expert . analytics] (2014)
; author: asmund@xal.no
; 
; Client for the Merchant API of mCASH.no
;

(in-package #:mcashmapiclient)

(export '(create-pos create-shortlink list-shortlinks get-shortlink-image 
          request-payment outcome-payment capture-payment
          lookup-merchant lookup-merchant-pos-list lookup-pos-by-id update-pos-name))

; load user params. The format of the rc-file is a simple json array.
; TODO document properly
(defvar *rc* (merge-pathnames (user-homedir-pathname) ".mcashapirc"))
(defparameter *userparams* 
  (with-open-file (s *rc*)
    (st-json:read-json s)))

; load configuration variables
(defparameter *merchantid* (st-json:getjso "merchantid" *userparams*))
(defparameter *integratorid* (st-json:getjso "integratorid" *userparams*))
(defparameter *merchantuser* (st-json:getjso "merchantuser" *userparams*))
(defparameter *merchantusersecret* (st-json:getjso "merchantusersecret" *userparams*))

(defparameter *apiurl* (st-json:getjso "apiurl" *userparams*))
(defparameter *qrurl* (st-json:getjso "qrurl" *userparams*))
(defparameter *usetestbed* 
  (st-json:from-json-bool (st-json:getjso "usetestbed" *userparams*)))
(defparameter *testbedtoken* (st-json:getjso "testbedtoken" *userparams*))
(defparameter *debug* 
  (st-json:from-json-bool (st-json:getjso "debug" *userparams*)))

(defparameter *jsonaccept* "application/vnd.mcash.api.merchant.v1+json")

; set this to log drakma headers to stdout.
(if *debug* 
  (setf drakma:*header-stream* *standard-output*))


(defun method-url-headers-string (method url headers)
  (join-strings-with-pipe (list (string-upcase method) (downcase-prot-in-url url) (join-strings (join-header-pairs headers) :symb "&"))))

(defun rsa-sign-method-url-headers (method url headers)
  ; need the pemfile in order to continue
  (if (st-json:getjso "pemfile" *userparams*) 
    (cl-rsasign:sign 
      (method-url-headers-string method url headers)
      (cl-rsasign:rsa-private-key-from-file (st-json:getjso "pemfile" *userparams*)))))

;  
; TODO Better handling of eof on stream (or 204 on status, i.e. no content)
(defun apicall (url &key request (method :post)
                    (auth-method :secret)
                    (integrator-p nil) 
                    (readstream t) 
                    content-type)
  (let* ((current-time-stamp (get-time-stamp))
         (json-content (if request 
                         (json:encode-json-to-string (yason:encode-alist request)) 
                         nil))
         (extra-headers (list (cons "X-Mcash-Merchant" *merchantid*)
                              (if integrator-p
                                (cons "X-Mcash-Integrator" *integratorid*)
                                (cons "X-Mcash-User" *merchantuser*)) 
                              (if (eql auth-method :secret) 
                                (cons "Authorization" 
                                      (concatenate 'string "SECRET " *merchantusersecret*)))
                              (if (eql auth-method :rsa)
                                (cons "X-Mcash-Timestamp" current-time-stamp))
                              (if (eql auth-method :rsa) 
                                (cons "X-Mcash-Content-Digest" 
                                      (concatenate 'string "SHA256=" (base64-sha256-digest json-content))))
                              (if *usetestbed* 
                                (cons "X-Testbed-Token" *testbedtoken*))))
         (rsa-auth-header (if (eql auth-method :rsa)
                            (list (cons "Authorization"
                                        (concatenate 'string "RSA-SHA256 " (rsa-sign-method-url-headers method url (sort-headers-on-key (x-mcash-headers extra-headers))))))
                            nil))
         (stream (drakma:http-request url
                                      :method (if method method :post)
                                      :additional-headers (concatenate 'list extra-headers rsa-auth-header) 
                                      :accept *jsonaccept*
                                      :content-type (if content-type content-type "application/json")
                                      :content json-content
                                      :want-stream t)))
    (if readstream 
      (st-json:read-json stream)
      stream)))

; creating pos
; need to send "id", "name", and "type" as a json-object
(defun create-pos (posid posname postype &key (auth-method :secret))
  (let ((url (concatenate 'string *apiurl* "pos/"))
        (request (list (cons "id" posid)
                       (cons "name" posname)
                       (cons "type" postype))))
    (apicall url :request request :auth-method auth-method)))

; lookup merchant-detail
; (only relevant for integrators with access to multiple merchants?)
(defun lookup-merchant (m-id &key (auth-method :secret))
  (let ((url (concatenate 'string *apiurl* "merchant/" m-id "/")))
    (apicall url :method :get :auth-method auth-method)))

; lookup all pos'es
(defun lookup-merchant-pos-list (&key (auth-method :secret))
  (let ((url (concatenate 'string *apiurl* "pos/")))
    (apicall url :method :get :auth-method auth-method)))

(defun lookup-pos-by-id (pos-id &key (auth-method :secret))
  (let ((url (concatenate 'string *apiurl* "pos/" pos-id "/")))
    (apicall url :method :get :auth-method auth-method)))

(defun update-pos-name (pos-id name &key (auth-method :secret))
   ; despite the mapi-docs it seems we have to provide type also when
  ; updating the name
  (let ((url (concatenate 'string *apiurl* "pos/" pos-id "/"))
        (request (list (cons "name" name)
                       (cons "type" (st-json:getjso "type" (lookup-pos-by-id pos-id))))))
    (apicall url :request request :method :put :readstream nil :auth-method auth-method)))

; create shortlink
; returns json respons with the shortlink id
(defun create-shortlink (callback descr &key (auth-method :secret))
  (let ((url (concatenate 'string *apiurl* "shortlink/"))
        (request (list (cons "callback_uri" callback)
                       (cons "description" descr))))
    (apicall url :request request :auth-method auth-method)))

; list registered shortlinks
(defun list-shortlinks (&key (auth-method :secret))
  (let ((url (concatenate 'string *apiurl* "shortlink/")))
    (apicall url :method :get :auth-method auth-method)))


; obtain shortlink qr-image
; on url shortlink/v1/qr_image/<shortlink-id>/<argstring>/
; in order to write the png-img to a file, the following code
; can be used:
; 
; (with-open-file 
;    (fh "img.png" :direction :output :element-type '(unsigned-byte 8)) 
;   (let ((stream (get-shortlink-image "<id>"))) 
;      (loop for byte = (read-byte stream nil 'eof) 
;       until (eq byte 'eof) 
;       do (write-byte byte fh))))
(defun get-shortlink-image (shortlink-id &key (argstring ""))
  ; Get shortlink-image for shortlink id with argstring (defaults to empty)
  (let ((url (concatenate 'string *qrurl* shortlink-id "/" argstring)))
    (drakma:http-request url :want-stream t)))
   
; request a payment
; usage e.g. (setf *response* (request-payment <params...>))
; and then (setf *tid* (st-json:getjso "id" *response*))
(defun request-payment (customer posid pos-tid amount text &key (auth-method :secret))
  (let ((url (concatenate 'string *apiurl* "payment_request/"))
        (request (list (cons "customer" customer)
                       (cons "pos_id" posid)
                       (cons "pos_tid" (write-to-string pos-tid))
                       (cons "action" "auth")
                       (cons "currency" "NOK")
                       (cons "amount" amount)
                       (cons "allow_credit" "true")
                       (cons "text" text) 
                       (cons "callback_uri" (concatenate 'string "http://devtest.xal.no:9090/payment_request/" posid "/")))))
    (apicall url :request request :auth-method auth-method)))

; check current status of a payment on the outcome endpoint
; do e.g. (st-json:getjso "status" (outcome-payment *tid*))
(defun outcome-payment (tid &key (auth-method :secret))
  (let ((url (concatenate 'string *apiurl* "payment_request/" tid "/outcome/")))
    (apicall url :method :get :auth-method auth-method)))

; capture a payment
; if capture-id is set, amount should also be set
(defun capture-payment (tid &key capture-id amount ledger (auth-method :secret))
  (let ((url (concatenate 'string *apiurl* "payment_request/" tid "/"))
        (request (concatenate 'list (list (cons "action" "CAPTURE"))
                              (if ledger (list (cons "ledger" ledger)))
                              (if capture-id (list (cons "capture_id" capture-id)
                                                   (cons "amount" amount)
                                                   (cons "additional_amount" "0.00")
                                                   (cons "currency" "NOK"))))))
    (apicall url :request request :method :put :readstream nil :auth-method auth-method)))
