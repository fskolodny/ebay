(in-package :cl-user)
(defpackage #:models
  (:use #:cl
	)
  (:export initialize-models
	   add-investor add-inventory-item add-listing add-purchase add-remittance add-sale add-purchase-item
	   get-investors get-items get-listings get-purchases get-remittances get-sales get-purchase-items
	   get-inventory-item
	   get-investor get-listing get-sale
	   get-purchases-for-investor
	   get-listings-for-purchase
	   get-sales-for-listing
	   update-shipping
	   object-id
	   investor-name investor-percentage investor-purchases
	   item-code item-description
	   purchase-investor purchase-item purchase-date purchase-quantity purchase-on-hand
	   purchase-price purchase-items
	   listing-purchase listing-date listing-quantity listing-price
	   sale-listing sale-date sale-quantity sale-price sale-fees sale-shipping sale-customer
	   remittance-investor remittance-date remittance-amount
	   get-total-remittances-for-investor
	   )
  )

(in-package :models)

(defvar *log*)
(defvar *investors* nil)
(defvar *items* nil)
(defvar *purchases* nil)
(defvar *purchase-items* nil)
(defvar *listings* nil)
(defvar *sales* nil)
(defvar *remittances* nil)

(defclass investor ()
  ((name :type string :accessor investor-name :initarg :name)
   (percentage :type integer :accessor investor-percentage :initarg :percentage)
   (purchases :type list :accessor investor-purchases :initform (list) :initarg :purchases)
   )
  )
(defmethod print-object ((investor investor) stream)
  (print-unreadable-object (investor stream)
    (format stream ":name ~a :percentage ~a" (investor-name investor) (investor-percentage investor))
    )
  )

(defclass item ()
  ((code :type string :accessor item-code :initarg :code)
   (description :type string :accessor item-description :initarg :description)
   (purchases :type list :accessor item-purchases :initform (list) :initarg :purchases)
   (listings :type list :accessor item-listings :initform (list) :initarg :listings)
   )
  )

(defclass purchase ()
  ((investor :type investor :accessor purchase-investor :initarg :investor)
   (date :type date :accessor purchase-date :initarg :date)
   (items :type list :accessor purchase-items :initform (list) :initarg :items)
   )
  )
(defmethod print-object ((purchase purchase) stream)
  (print-unreadable-object (purchase stream)
    (format stream ":date ~a ~(~a~)" (purchase-date purchase) (purchase-items purchase))
    )
  )

(defclass purchase-item ()
  ((item :type item :accessor purchase-item :initarg :item)
   (quantity :type integer :accessor purchase-quantity :initarg :quantity)
   (on-hand :type integer :accessor purchase-on-hand :initarg :on-hand)
   (price :type rational :accessor purchase-price :initarg :price)
   (purchase :type purchase :accessor purchase-item-purchase :initarg :purchase)
   (sales :type list :accessor purchase-item-sales :initarg :sales)
   )
  )
(defmethod print-object ((purchase-item purchase-item) stream)
  (print-unreadable-object (purchase-item stream)
    (format stream ":item ~a :quantity ~a :price ~a" (purchase-item purchase-item) (purchase-quantity purchase-item) (purchase-price purchase-item))
    )
  )

(defclass listing ()
   ((item :type item :accessor listing-item :initarg :item)
    (date :type :date :accessor listing-date :initarg :date)
    (quantity :type integer :accessor listing-quantity :initarg :quantity)
    (price :type rational :accessor listing-price :initarg :price)
    )
   )

(defclass sale ()
  ((listing :type listing :accessor sale-listing :initarg :listing)
   (date :type date :accessor sale-date :initarg :date)
   (quantity :type integer :accessor sale-quantity :initarg :quantity)
   (price :type rational :accessor sale-price :initarg :price)
   (fees :type rational :accessor sale-fees :initarg :fees)
   (shipping :type rational :accessor sale-shipping :initarg :shipping)
   (customer :type string :accessor sale-customer :initarg :customer)
   (purchases :type list :accessor sale-purchases :initform (list) :initarg :purchases)
   )
  )

(defclass remittance ()
  ((investor :type investor :accessor remittance-investor :initarg :investor)
   (date :type date :accessor remittance-date :initarg :date)
   (amount :type rational :accessor remittance-amount :initarg :amount)
   )
  )

(defmethod make-uplink ((purchase purchase) (investor investor))
  (setf (purchase-investor purchase) investor)
  )
(defmethod make-uplink ((pitem purchase-item) (purchase purchase))
  (setf (purchase-item-purchase pitem) purchase)
  )
(defmethod make-uplink ((pitem purchase-item) (item item))
  (setf (purchase-item pitem) item)
  )

(defun initialize-models ()
  (mito:connect-toplevel :sqlite3 :database-name "arele.db")
  )

(defun add-investor (&key name percentage)
  (let ((investor (make-instance 'investor :name name :percentage percentage))
	)
    (setf *investors* (append *investors* (list investor)))
    investor
    )
  )

(defun add-inventory-item (&key code description &allow-other-keys)
  (let ((item (make-instance 'item :code code :description description))
	)
    (setf *items* (append *items* (list item)))
    item
    )
  )

(defun add-purchase (&key date investor &allow-other-keys)
  (let ((purchase (make-instance 'purchase :date date :investor investor))
	)
    (setf *purchases* (append *purchases* (list purchase))
	  (investor-purchases investor) (append (investor-purchases investor) (list purchase))
	  )
    purchase
    )
  )

(defun add-purchase-item (&key purchase item quantity price &allow-other-keys)
  (let ((purchase-item (make-instance 'purchase-item :purchase purchase :item item :quantity quantity :price price))
	)
    (setf *purchase-items* (append *purchase-items* (list purchase-item))
	  (purchase-items purchase) (append (purchase-items purchase) (list purchase-item))
	  (item-purchases item) (append (item-purchases item) (list purchase-item))
	  )
    purchase-item
    )
  )

(defun add-listing (&key item date quantity price)
  (setf *listings* (append *listings* (list (make-instance 'listing :item item :date date :quantity quantity :price price))))
  )

(defun add-sale (&key listing date quantity price fees shipping customer)
  (setf (listing-quantity listing) (- (listing-quantity listing) quantity))
  (setf *sales* (append *sales* (list (make-instance 'sale :listing listing :date date :quantity quantity :price price :fees fees :shipping shipping :customer customer))))
  )

(defun update-shipping (&key sale shipping &allow-other-keys)
  (setf (sale-shipping sale) shipping)
  )

(defun add-remittance (&key investor date amount &allow-other-keys)
  (setf *remittances* (append *remittances* (list (make-instance 'remittance :investor investor :date date :amount amount))))
  )

(defun get-investors ()
  *investors*
  )

(defun get-items ()
  *items*
  )

(defun get-purchase-items ()
  *purchase-items*
  )

(defun get-listings ()
  *listings*
  )

(defun get-purchases ()
  *purchases*
  )

(defun get-sales ()
  *sales*
  )

(defun get-investor (id)
  (or (and (integerp id) (nth (1- id) *investors*)) id)
  )

(defun get-inventory-item (id)
  (nth (1- id) *items*)
  )

(defun get-purchase (id)
  (nth (1- id) *purchases*)
  )

(defun get-listing (id)
  (nth (1- id) *listings*)
  )

(defun get-sale (id)
  (nth (1- id) *sales*)
  )

(defun get-purchases-for-investor (investor)
  (remove-if-not (lambda (i) (equal i investor)) *purchases* :key 'purchase-investor)
  )

(defun get-sales-for-listing (listing)
  (remove-if-not (lambda (l) (equal l listing)) *sales* :key 'sale-listing)
  )

(defun get-total-remittances-for-investor (investor)
  (reduce (lambda (a b) (+ a b))
	  (remove-if-not (lambda (i) (equal investor i)) *remittances* :key 'remittance-investor)
	  :key 'remittance-amount :initial-value 0)
  )
