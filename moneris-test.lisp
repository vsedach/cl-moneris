(in-package :moneris-test)

;; Light testing functions

(defun test-purchase (&key (pan "4242424242424242"))
  (process-purchase "https://esqa.moneris.com/gateway2/servlet/MpgRequest"
		    "demouser" "password"
		    :store-id "store1"
		    :api-token "yesguy"
		    :order-id (moneris::gen-order-id)
		    :cust-id "thecustomer"
		    :amount "10.00"
		    :pan pan
		    :expdate "1010"))

(defun test-purchase-catching-error (&key (pan "42424242424241"))
  (handler-case
      (test-purchase :pan pan)
    (moneris:moneris-transaction-error (c)
      (values (moneris:code c)
	      (moneris:s-xml c)
	      (moneris:raw-xml c)))))

(defun test-correction (order-id txn-number)
  (process-purchase-correction "https://esqa.moneris.com/gateway2/servlet/MpgRequest"
			       "demouser" "password"
			       :store-id "store1"
			       :api-token "yesguy"
			       :order-id order-id
			       :txn-number txn-number))

(defun test-purchase-then-correction ()
  (let* ((receipt (test-purchase))
	 (txn-number (second (assoc :transid (cdr receipt))))
	 (order-id (second (assoc :receiptid (cdr receipt))))
	 (correction (test-correction order-id txn-number)))
    (format t "Made Purchase --~%   ~S~%Corrected ---~%  ~S" receipt correction)))

(defun test-refund (order-id amount txn-number)
  (process-refund "https://esqa.moneris.com/gateway2/servlet/MpgRequest"
		  "demouser" "password"
		  :store-id "store1"
		  :api-token "yesguy"
		  :order-id order-id
		  :amount amount
		  :txn-number txn-number))

(defun test-purchase-then-refund ()
  (let* ((receipt (test-purchase))
	 (txn-number (second (assoc :transid (cdr receipt))))
	 (order-id (second (assoc :receiptid (cdr receipt))))
	 (amount (second (assoc :transamount (cdr receipt))))
	 (refund (test-refund order-id amount txn-number)))
    (format t "Made Purchase --~%   ~S~%Refund ---~%  ~S" receipt refund)))
  

(defun test-error-invaliduri ()
  (print "Invalid URI Path Test -------")
  (handler-case
      (process-purchase "https://esqa.moneris.com/gateway3/servlet/MpgRequest"
			"demouser" "password"
			:store-id "store1"
			:api-token "yesguy"
			:order-id (moneris::gen-order-id)
			:cust-id "thecustomer"
			:amount "100.00"
			:pan "1010101010101010"
			:expdate "1010")
    (error (c) (describe c)))

  (print "Invalid URI Hostname Test-------")
  (handler-case
      (process-purchase "https://esqa1.moneris.com/gateway2/servlet/MpgRequest"
			"demouser" "password"
			:store-id "store1"
			:api-token "yesguy"
			:order-id (moneris::gen-order-id)
			:cust-id "thecustomer"
			:amount "100.00"
			:pan "1010101010101010"
			:expdate "1010")
    (error (c) (describe c)))

  (print "Invalid URI Protocol Test-------")
  (handler-case
      (process-purchase "http://esqa1.moneris.com/gateway2/servlet/MpgRequest"
			"demouser" "password"
			:store-id "store1"
			:api-token "yesguy"
			:order-id (moneris::gen-order-id)
			:cust-id "thecustomer"
			:amount "100.00"
			:pan "1010101010101010"
			:expdate "1010")
    (error (c) (describe c))))

(defun test-error-invalidcard ()
  (handler-case
      (process-purchase "https://esqa.moneris.com/gateway2/servlet/MpgRequest"
			"demouser" "password"
			:store-id "store1"
			:api-token "yesguy"
			:order-id (moneris::gen-order-id)
			:cust-id "thecustomer"
			:amount "100.00"
			:pan "1010101010101010"
			:expdate "1010")
    (error (c) (describe c))))

