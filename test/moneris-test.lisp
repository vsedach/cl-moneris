;;; The contents of this file are released into the public
;;; domain. Absolutely no warranty.

(in-package #:cl-moneris-test)

(def-suite moneris-tests)

(in-suite moneris-tests)

(defparameter *test-store*
  (make-instance 'merchant-token
                 :moneris-uri "https://esqa.moneris.com/gateway2/servlet/MpgRequest"
                 :api-token "yesguy"
                 :store-id "store1"))

(defun gen-order-id ()
  (string (gensym (format nil "ORDER-~A-" (get-universal-time)))))

(defun test-purchase (&key (pan "4242424242424242") (order-id (gen-order-id)))
  (process *test-store* (purchase :order-id order-id
                                  :cust-id "thecustomer"
                                  :amount "10.00"
                                  :pan pan
                                  :expdate "1010")))

(test purchase-ok
  (is (stringp (test-purchase))))

(test purchase-error
  (signals moneris-transaction-error
           (test-purchase :pan "42424242424241")))

(defun test-correction (order-id txn-id)
  (process *test-store* (correction :order-id order-id
                                    :txn-id txn-id)))

(test purchase-then-correction
  (let* ((receipt (nth-value 4 (test-purchase)))
	 (txn-id (second (assoc :transid (cdr receipt))))
	 (order-id (second (assoc :receiptid (cdr receipt)))))
    (is (stringp (test-correction order-id txn-id)))))

(defun test-refund (order-id amount txn-number)
  (process *test-store* (refund :order-id order-id
                                :amount amount
                                :txn-id txn-number)))

(test purchase-then-refund
  (let* ((receipt (nth-value 4 (test-purchase)))
	 (txn-number (second (assoc :transid (cdr receipt))))
	 (order-id (second (assoc :receiptid (cdr receipt))))
	 (amount (second (assoc :transamount (cdr receipt)))))
    (is (stringp (test-refund order-id amount txn-number)))))

(defun run-tests ()
  (run! 'moneris-tests))
