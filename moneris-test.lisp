(in-package :moneris-test)

(defparameter *test-store*
  (make-instance 'merchant-token
                 :moneris-uri "https://esqa.moneris.com/gateway2/servlet/MpgRequest"
                 :api-token "yesguy"
                 :store-id "store1"))

(defun test-purchase (&key (pan "4242424242424242") (order-id (gen-order-id)))
  (process *test-store* (purchase :order-id order-id
                                  :cust-id "thecustomer"
                                  :amount "10.00"
                                  :pan pan
                                  :expdate "1010")))

(defun test-purchase-catching-error (&key (pan "42424242424241"))
  (handler-case
      (test-purchase :pan pan)
    (moneris:moneris-transaction-error (c)
      (values (moneris:code c)
	      (moneris:s-xml c)
	      (moneris:raw-xml c)))))

(defun test-correction (order-id txn-number)
  (process *test-store* (correction :order-id order-id
                                    :txn-number txn-number)))

(defun test-purchase-then-correction ()
  (let* ((receipt (test-purchase))
	 (txn-number (second (assoc :transid (cdr receipt))))
	 (order-id (second (assoc :receiptid (cdr receipt))))
	 (correction (test-correction order-id txn-number)))
    (format t "Made Purchase --~%   ~S~%Corrected ---~%  ~S" receipt correction)))

(defun test-refund (order-id amount txn-number)
  (process *test-store* (refund :order-id order-id
                                :amount amount
                                :txn-number txn-number)))

(defun test-purchase-then-refund ()
  (let* ((receipt (test-purchase))
	 (txn-number (second (assoc :transid (cdr receipt))))
	 (order-id (second (assoc :receiptid (cdr receipt))))
	 (amount (second (assoc :transamount (cdr receipt))))
	 (refund (test-refund order-id amount txn-number)))
    (format t "Made Purchase --~%   ~S~%Refund ---~%  ~S" receipt refund)))
  
(defun test-error-invalidcard ()
  (handler-case
      (process *test-store* (purchase :order-id (gen-order-id)
                                      :cust-id "thecustomer"
                                      :amount "100.00"
                                      :pan "1010101010101010"
                                      :expdate "1010"))
    (error (c) (describe c))))

(defun gen-order-id ()
  (string (gensym (format nil "ORDER-~A-" (get-universal-time)))))
