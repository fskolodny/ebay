(in-package :cl-user)
(defpackage #:models
  (:use #:cl
	)
  (:export initialize-models
	   add-investor add-inventory-item add-listing add-purchase add-remittance add-sale
	   get-investors get-items get-listings get-purchases get-remittances get-sales
	   get-investor get-listing get-sale
	   get-purchases-for-investor
	   get-listings-for-purchase
	   get-sales-for-listing
	   update-shipping
	   object-id
	   investor-name investor-percentage
	   item-code item-description
	   purchase-investor purchase-item purchase-date purchase-quantity purchase-on-hand purchase-price
	   listing-purchase listing-date listing-quantity listing-price
	   sale-listing sale-date sale-quantity sale-price sale-fees sale-shipping sale-customer
	   remittance-investor remittance-date remittance-amount
	   get-total-remittances-for-investor
	   execute
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

(defun execute (function &rest args)
  (eval (cons function args))
  (clobber:log-transaction (cons function args) *log*)
  )
(defclass investor ()
  ((name :type string :accessor investor-name :initarg :name)
   (percentage :type integer :accessor investor-percentage :initarg :percentage)
   (purchases :type list :accessor investor-purchases :initform (list))
   )
  )

(defclass item ()
  ((code :type string :accessor item-code :initarg :code)
   (description :type string :accessor item-description :initarg :description)
   (purchases :type list :accessor item-purchases :initform (list))
   )
  )

(defclass purchase ()
  ((investor :type investor :accessor purchase-investor :initarg :investor)
   (date :type date :accessor purchase-date :initarg :date)
   )
  )
(defclass purchase-item ()
  (
   (item :type item :accessor purchase-item :initarg :item)
   (quantity :type :integer :accessor purchase-quantity :initarg :quantity)
   (on-hand :type :integer :accessor purchase-on-hand :initarg :on-hand)
   (price :type rational :accessor purchase-price :initarg :price)
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
   (purchases :type list :accessor sale-purchases :initform (list))
   )
  )

(defclass remittance ()
  ((investor :type investor :accessor remittance-investor :initarg :investor)
   (date :type date :accessor remittance-date :initarg :date)
   (amount :type rational :accessor remittance-amount :initarg :amount)
   )
  )

(defun initialize-models ()
  (setf *log* (clobber:open-transaction-log "arele.log"
					    (lambda (tran) (eval tran))
					    )
	sb-ext:*exit-hooks* (push
			     (lambda () (clobber:close-transaction-log *log*))
			     sb-ext:*exit-hooks*))
  (mito:connect-toplevel :sqlite3 :database-name "arele.db")
;  (mapc (lambda (model)
;	  (ensure-table-exists model)
;	  )
;	'(investor item purchase listing sale remittance)
;	)
  )

(defun add-investor (&key name percentage)
  (setf *investors* (append *investors* (list (make-instance 'investor :name name :percentage percentage))))
  )

(defun add-inventory-item (&key code description &allow-other-keys)
  (setf *items* (append *items* (list (make-instance 'item :code code :description description))))
  )

(defun add-purchase (&key date investor &allow-other-keys)
  (setf *purchases* (append *purchases* (list (make-instance 'purchase :date date :investor (nth (1- investor) *investors*)))))
  )

(defun add-purchase-item (&key purchase item quantity price &allow-other-keys)
  (setf *purchase-items* (append *purchase-items* (list (make-instance 'purchase-item :purchase purchase :item (nth (1- item) *items*)
										      :quantity quantity :price price))))
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
  (nth (1- id) *investors*)
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
