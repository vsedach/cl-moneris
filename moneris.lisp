(in-package #:cl-moneris)

(define-condition moneris-error (error)
  ()
  (:documentation "Superclass of network errors or
  transaction-declined conditions. Handle this when you don't care why
  your transaction failed to process."))

(define-condition moneris-post-error (moneris-error)
  ((http-code     :initarg :http-code     :accessor http-code)
   (http-headers  :initarg :http-headers  :accessor http-headers)
   (http-body     :initarg :http-body     :accessor http-body)
   (reason-phrase :initarg :reason-phrase :accessor reason-phrase
                  :documentation "Reason phrase from HTTP header status line"))
  (:documentation "Condition raised due to network error"))

(define-condition moneris-transaction-error (moneris-error)
  ((code             :initarg :code             :accessor code
    :documentation "Numeric Moneris response code")
   (code-description :initarg :code-description :accessor code-description
    :documentation "Text description of Moneris response code")
   (message          :initarg :message          :accessor message
    :documentation "Message returned by Moneris")
   (raw-xml          :initarg :raw-xml          :accessor raw-xml
    :documentation "XML string returned by Moneris")
   (parsed-xml       :initarg :parsed-xml       :accessor parsed-xml
    :documentation "Parsed XML returned by Moneris"))
  (:documentation "Condition raised when transaction is declined"))

(defclass merchant-token ()
  ((moneris-uri :reader moneris-uri :initarg :moneris-uri
    :documentation "Full URI of the Moneris Server e.g. https://moneris.com/mpg")
   (api-token   :reader api-token   :initarg :api-token
    :documentation "Your api token issued during Moneris store creation.")
   (store-id    :reader store-id    :initarg :store-id
    :documentation "Your store-id issued during Moneris store creation."))
  (:documentation "Merchant-token objects specify your unique Moneris store information."))

(defun lispify-response (s-xml-response)
  (mapcar (lambda (token)
	    (typecase token
	      (keyword (intern (string-upcase token) :keyword))
	      (string (cond ((string-equal token "null") nil)
                            ((string-equal token "true") t)
                            ((string-equal token "false") nil)
                            (t token)))
	      (cons (lispify-response token))))
	  s-xml-response))

(defun process (merchant-token transaction)
  "Send a 'purchase', 'correction', or 'refund' transaction for processing to Moneris merchant account specified by merchant-token.

If the Moneris mpg server returns an HTTP status code other than 200, raises a moneris-post-error

If transaction is declined, raises a moneris-transaction-error

If transaction is successfuly processed, returns (as multiple values):
txn-id           - transaction ID
code             - numeric response code
code-description - response code description
message          - message returned in response
response-xml     - parsed XML of response
response-string  - raw XML response string"
  (multiple-value-bind (response-string http-status-code http-headers uri stream must-close reason-phrase)
      (drakma:http-request (moneris-uri merchant-token)
                           :method :post
                           :force-ssl t
                           :content (with-output-to-string (stream)
                                      (s-xml:print-xml
                                       `(:|request|
                                          (:|store_id| ,(store-id merchant-token))
                                          (:|api_token| ,(api-token merchant-token))
                                          ,transaction)
                                       :header "<?xml version=\"1.0\"?>"
                                       :stream stream)))
    (declare (ignore uri must-close))
    (unwind-protect
         (if (/= http-status-code 200)
             (error 'moneris-post-error
                    :http-code http-status-code
                    :http-headers http-headers
                    :http-body response-string
                    :reason-phrase reason-phrase)
             (let* ((response-xml (lispify-response (second (s-xml:parse-xml-string response-string))))
                    (code (let ((string-code (second (assoc :responsecode (cdr response-xml)))))
                            (when string-code (parse-integer string-code))))
                    (code-description (response-code-description code))
                    (message (second (assoc :message (cdr response-xml)))))
               (if (is-error-code? code)
                   (error 'moneris-transaction-error
                          :code code
                          :code-description code-description
                          :message message
                          :raw-xml response-string
                          :parsed-xml response-xml)
                   (values (second (assoc :transid (cdr response-xml))) code code-description message response-xml response-string))))
      (close stream))))

(defun purchase (&key order-id cust-id amount pan expdate (crypt-type "7"))
  "Generate a purchase transaction for processing.

Description of input parameters:

order-id (string, 50 chars max, alphanumeric)
  Merchant defined unique transaction identifier - must be uniquely
  generated by caller for every Purchase.

amount (string, 9 chars max, decimal)
  Amount of the transaction. This must contain 3 digits with two
  penny values. The minimum value passed can be 0.01 and the maximum
  9999999.99

pan (string, 20 chars max, numeric)
  Credit Card Number - no spaces or dashes. Most credit card numbers
  today are 16 digits in length but some 13 digits are still accepted
  by some issuers. This field has been intentionally expanded to 20
  digits in consideration for future expansion and/or potential
  support of private label card ranges.

expdate (string, 4 chars, numeric)
  Expiry Date - format YYMM no spaces or slashes.  PLEASE NOTE THAT
  THIS IS REVERSED FROM THE DATE DISPLAYED ON THE PHYSICAL CARD WHICH
  IS MMYY

cust-id (string, 50 chars max, alphanumeric)
  This is an optional field that can be sent as part of a Purchase or
  PreAuth request. It is searchable in the Moneris Merchant Resource
  Centre. It is commonly used for policy number, membership number,
  student ID or invoice number."
  `(:|purchase|
    (:|order_id| ,order-id)
    (:|cust_id| ,cust-id)
    (:|amount| ,amount)
    (:|pan| ,pan)
    (:|expdate| ,expdate)
    (:|crypt_type| ,crypt-type)))

(defun correction (&key order-id txn-id (crypt-type "7"))
  "Generate a transaction to void a same-day purchase. No purchase will appear on the customer's credit card.

Description of input parameters:

order-id (string, 50 chars max, alphanumeric)
  Merchant defined unique transaction identifier - must reference a
  previously processed purchase transaction order-id.

txn-id (string, 255 chars max, alphanumeric)
  This must be the value returned as the Txn_number in the
  response to the original purchase.

crypt-type (string, 1 char, alphanumeric)
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
"
  `(:|purchasecorrection|
    (:|order_id| ,order-id)
    (:|txn_number| ,txn-id)
    (:|crypt_type| ,crypt-type)))

(defun refund (&key order-id txn-id amount (crypt-type "7"))
  "Generate a transaction to refund a purchase.

order-id (string, 50 chars max, alphanumeric)
  Merchant defined unique transaction identifier. For Independent
  Refund attempts, must be uniquely generated by caller. For refunds
  of previous purchases, must reference a previously processed
  purchase transaction order-id.

txn-id (string, 255 chars max, alphanumeric)
  Used when performing follow on transactions - this must be the value
  returned as the Txn_number in the response to the original purchase.

amount (string, 9 chars max, decimal)
  Amount of the transaction. This must contain 3 digits with two
  penny values. The minimum value passed can be 0.01 and the maximum
  9999999.99

crypt-type (string, 1 char, alphanumeric)
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
"
  `(:|refund|
    (:|order_id| ,order-id)
    (:|amount| ,amount)
    (:|txn_number| ,txn-id)
    (:|crypt_type| ,crypt-type)))
