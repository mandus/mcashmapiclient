;
; (c) [expert . analytics] (2014)
; author: asmund@xal.no
; 
; Client for the Merchant API of mCASH.no
;

(defpackage #:mcashmapiclient
  (:use #:cl #:cl-rsasign)
  (:export
    #:create-pos
    #:create-shortlink
    #:list-shortlinks
    #:get-shortlink-image
    #:request-payment
    #:outcome-payment
    #:capture-payment
    #:lookup-merchant
    #:lookup-merchant-pos-list
    #:lookup-pos-by-id
    #:update-pos-name))

;(defpackage #:mcashmapiclient-tests
;  (:use #:cl :lisp-unit #:mcashmapiclient))
