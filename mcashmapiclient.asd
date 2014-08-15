;;;; mcashmapiclient.asd

(asdf:defsystem #:mcashmapiclient
  :serial t
  :description "Client for the Merchant API of mCASH"
  :author "Åsmund Ødegård <asmund@xal.no>"
  :license "Apache License v. 2.0"
  :depends-on (#:drakma
               #:ironclad
               #:s-base64
               #:yason
               #:cl-json
               #:st-json
               #:cl-ppcre
               #:cl-rsasign)
  :components 
  ((:module src
    :serial t
    :components
    ((:file "package")
     (:file "mcashmapiclient")
     (:file "utils")))))
