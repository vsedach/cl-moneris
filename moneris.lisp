(in-package :moneris)

(defun trim-description (string)
  (string-trim " " (remove #\" string)))

(defparameter *response-codes*
  (let ((ht (make-hash-table)))
    (with-open-file (csv-file 
		     (merge-pathnames (asdf:component-pathname (asdf:find-system :moneris))
				      (make-pathname :name "Response_Codes" :type "csv")))
      (loop for line = (read-line csv-file nil nil)
	    while line do
	    (cl-ppcre:register-groups-bind ((#'parse-integer code) (#'trim-description description))
		("(\\d+),([\\S\\s]+)$" line)
	      (setf (gethash code ht) description))))
    ht))


(define-condition moneris-error (error)
  ())

(define-condition moneris-post-error (moneris-error)
  ((http-code :initarg :http-code :accessor http-code)
   (http-headers :initarg :http-headers :accessor http-headers)
   (http-body :initarg :http-body :accessor http-body)
   (reason-phrase :initarg :reason-phrase :accessor reason-phrase)))

(define-condition moneris-transaction-error (moneris-error)
  ((code :initarg :code :accessor code)
   (code-description :initarg :code-description :accessor code-description)
   (message :initarg :message :accessor message)
   (raw-xml :initarg :raw-xml :accessor raw-xml)
   (s-xml :initarg :s-xml :accessor s-xml)))

(defun is-error-code (code) (or (null code) (>= code 50)))

(defun response-code-description (code)
  (gethash code *response-codes* "<No Description Available>"))

(defun gen-order-id ()
  (string (gensym (format nil "ORDER-~A-" (get-universal-time)))))

(defun lispify-response (s-xml-response)
  "Transform keyword Moneris strings and s-xml lowercase
   keywords to lisp types and uppercase keywords"
  (mapcar (lambda (token)
	    (typecase token
	      (keyword (intern (string-upcase token) :keyword))
	      (string (cond
			((string-equal token "null") nil)
			((string-equal token "true") t)
			((string-equal token "false") nil)
			(t token)))
	      (cons (lispify-response token))))
	  s-xml-response))

     
(defun process-request (uri username password xml-request)

  "Post a HTTPS request to a Moneris server supporting the mpg protocol.
   Parameters:
     uri - full string based URI of the Moneris Server
     username - HTTP basic authorization Username
     password - HTTP basic authorization Password
     xml-request - XML string"

 (multiple-value-bind (body http-status-code http-headers uri stream must-close reason-phrase)

      (drakma:http-request uri
			   :method :post
			   :force-ssl t
			   :content xml-request
			   :basic-authorization (list username password))

    (declare (ignore uri must-close))
    (unwind-protect
	 (if (/= http-status-code 200)
	     (error 'moneris-post-error :http-code http-status-code
		    :http-headers http-headers
		    :http-body body
		    :reason-phrase reason-phrase)
	     (let* ((raw-xml (s-xml:parse-xml-string body))
		    (response (lispify-response (second raw-xml)))
		    (code (let ((string-code (second (assoc :responsecode (cdr response)))))
			    (when string-code (parse-integer string-code))))
		    (message (second (assoc :message (cdr response)))))
	       (if (is-error-code code)
		   (error 'moneris-transaction-error
			  :code code
			  :code-description (response-code-description code)
			  :message message
			  :raw-xml body
			  :s-xml response)
		   (values response code message))))
      (close stream))))

#| 

The following functions are the main interface to perform transactions
with the Moneris mpg server.

   Function
   --------

   process-purchase            ---  Transact a purchase for a credit card.
   process-purchase-correction ---  Void a same-day purchase.  No purchase
                                    will appear on the customer's credit card
   process-refund              ---  Refund a purchase.

   Parameters   Lisp Type Description
   --------------------------------------------------------------
   uri          string    Full URI of the Moneris Server e.g. "https://moneris.com/mpg"
   username     string    HTTP basic authorization username
   password     string    HTTP basic authorization


   Moneris DTD Types    Corresponding Lisp Argument
   -----------------    ---------------------------

   store_id             store-id (string)

   Your store id created during Moneris store creation.

   api_token            api-token (string)
 
   Your api token created during Moneris store creation.

   order_id 50/an       order-id  (string)

   Merchant defined unique transaction identifier - must be unique for
every Purchase, PreAuth and Independent Refund attempt. For Refunds,
Completions and Voids the order_id must reference the original
transaction.

   pan   20/num         pan  (string)

   Credit Card Number - no spaces or dashes. Most credit card numbers
today are 16 digits in length but some 13 digits are still accepted by
some issuers. This field has been intentionally expanded to 20 digits
in consideration for future expansion and/or potential support of
private label card ranges.


   expdate    4 / num  expdate (string)

   Expiry Date - format YYMM no spaces or slashes.  PLEASE NOTE THAT
THIS IS REVERSED FROM THE DATE DISPLAYED ON THE PHYSICAL CARD WHICH IS
MMYY

   amount     9 / decimal   amount (string)

   Amount of the transaction. This must contain 3 digits with two
penny values. The minimum value passed can be 0.01 and the maximum
9999999.99

   crypt_type 1 / an      crypt-type (string)

   E-Commerce Indicator:
   1 - Mail Order/Telephone Order - Single
   2 - Mail Order/Telephone Order - Recurring
   3 - Mail Order Telephone Order - Instalment
   4 - Mail Order Telephone Order - Unknown Classification
   5 - Authenticated E-commerce Transaction (VBV)
   6 - Non Authenticated Ecommerce Transaction (VBV)
   7 - SSL enabled merchant
   8 - Non Secure Transaction (Web or Email Based)
   9 - SET Non Authenticated transaction

   txn_number 255 / an     txn-number (string)

   Used when performing follow on transactions - this must be filled
with the value that was varchar returned as the Txn_number in the
response of the original transaction. When performing a Capture this
must reference the PreAuth. When performing a Refund or a Void this
must reference the Capture or the Purchase.

   cust_id    50/an        cust-id (string)

   This is an optional field that can be sent as part of a Purchase or
PreAuth request. It is searchable in the Moneris Merchant Resource
Centre. It is commonly used for policy number, membership number,
student ID or invoice number.

|#

(defun process-purchase (uri username password 
			 &key store-id api-token order-id cust-id amount pan expdate
			 &allow-other-keys)
  "Validate a customer credit card purchase.  On success returns
   a receipt list.  Signals an error on failure."
  (process-request uri username password
		   (with-output-to-string (stream)
		     (s-xml:print-xml
		      `(:|request|
			(:|store_id| ,store-id)
			(:|api_token| ,api-token)
			(:|purchase|
			 (:|order_id| ,order-id)
			 (:|cust_id| ,cust-id)
			 (:|amount| ,amount)
			 (:|pan| ,pan)
			 (:|expdate| ,expdate)
			 (:|crypt_type| "7")))
		      :header "<?xml version=\"1.0\"?>"
		      :stream stream))))

(defun process-purchase-correction (uri username password 
				    &key store-id api-token order-id txn-number
				    &allow-other-keys)
  "Correct (Voids) a same day customer purchase.  Must pass in the
   original order-id and txn-number the purchase receipt.  On success
   returns a receipt list.  On error signals an error."
  (process-request uri username password
		   (with-output-to-string (stream)
		     (s-xml:print-xml
		      `(:|request|
			(:|store_id| ,store-id)
			(:|api_token| ,api-token)
			(:|purchasecorrection|
			 (:|order_id| ,order-id)
			 (:|txn_number| ,txn-number)
			 (:|crypt_type| "7")))
		      :header "<?xml version=\"1.0\"?>"
		      :stream stream))))

(defun process-refund (uri username password 
		       &key store-id api-token order-id amount txn-number
		       &allow-other-keys)
  "Refunds a purchase.  Must pass in the original order-id, amount and
   txn-number of the original purchase receipt.  On success returns
   a receipt list.  On error signals an error."
  (process-request uri username password
		   (with-output-to-string (stream)
		     (s-xml:print-xml
		      `(:|request|
			(:|store_id| ,store-id)
			(:|api_token| ,api-token)
			(:|refund|
			 (:|order_id| ,order-id)
			 (:|amount| ,amount)
			 (:|txn_number| ,txn-number)
			 (:|crypt_type| "7")))
		      :header "<?xml version=\"1.0\"?>"
		      :stream stream))))


;; The following make-xxx interfaces are for future functionality.  In particular
;; when Moneris requests contain multiple transactions.

(defun process (uri username password store-id api-token &rest requests)
  (process-request uri username password
		   (with-output-to-string (stream)
		     (s-xml:print-xml 
		      (apply #'make-request store-id api-token requests)
		      :stream stream
		      :header "<?xml version=\"1.0\"?>"))))


(defun make-purchase (&key order-id cust-id amount pan expdate (crypt-type "7"))
  `(:|purchase|
    (:|order_id| ,order-id)
    (:|cust_id| ,cust-id)
    (:|amount| ,amount)
    (:|pan| ,pan)
    (:|expdate| ,expdate)
    (:|crypt_type| ,crypt-type)))
  

(defun make-purchase-correction (&key order-id txn-number (crypt-type "7"))
  `(:|purchasecorrection|
    (:|order_id| ,order-id)
    (:|txn_number| ,txn-number)
    (:|crypt_type| ,crypt-type)))

(defun make-refund (&key order-id amount txn-number (crypt-type "7"))
  `(:|refund|
    (:|order_id| ,order-id)
    (:|amount| ,amount)
    (:|txn_number| ,txn-number)
    (:|crypt_type| ,crypt-type)))

(defun make-request (store-id api-token &rest requests)
  `(:|request|
    (:|store_id| ,store-id)
    (:|api_token| ,api-token)
    ,@requests))



