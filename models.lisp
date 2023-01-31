(in-package :cl-user)
(defpackage #:models
  (:use #:cl
	#:mito
	#:sxql
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
	   item-description
	   purchase-investor purchase-item purchase-date purchase-quantity purchase-on-hand purchase-price
	   listing-purchase listing-date listing-quantity listing-price
	   sale-listing sale-date sale-quantity sale-price sale-fees sale-shipping sale-customer
	   remittance-investor remittance-date remittance-amount
	   get-max-on-hand-for-item
	   get-first-purchase-with-enough
	   get-total-remittances-for-investor
	   )
  )

(in-package :models)

(deftable investor ()
  ((name :col-type (:varchar 128))
   (percentage :col-type :smallint))
  (:table-name "investors")
  )

(deftable item ()
  ((description :col-type (:varchar 128))
   )
  (:table-name "inventory")
  )

(deftable purchase ()
  ((investor :col-type investor)
   (item :col-type item)
   (date :col-type :datetime)
   (quantity :col-type :integer)
   (on-hand :col-type :integer)
   (price :col-type :integer)
   )
  (:table-name "purchases")
  )

(deftable listing ()
  ((purchase :col-type purchase)
   (date :col-type :datetime)
   (quantity :col-type :integer)
   (price :col-type :integer)
   )
  (:table-name "listings")
  )

(deftable sale ()
  ((listing :col-type listing)
   (date :col-type :datetime)
   (quantity :col-type :integer)
   (price :col-type :integer)
   (fees :col-type :integer)
   (shipping :col-type :integer)
   (customer :col-type (:varchar 128))
   )
  (:table-name "sales")
  )

(deftable remittance ()
  ((investor :col-type investor)
   (date :col-type :datetime)
   (amount :col-type :integer)
   )
  (:table-name "remittances")
  )

(defun initialize-models ()
  (connect-toplevel :sqlite3 :database-name "arele.db")
  (mapc (lambda (model)
	  (ensure-table-exists model)
	  )
	'(investor item purchase listing sale remittance)
	)
  )

(defun add-investor (&key name percentage)
  (create-dao 'investor :name name :percentage percentage)
  )

(defun add-inventory-item (&key description &allow-other-keys)
  (create-dao 'item :description description)
  )

(defun add-purchase (&key item date quantity price investor &allow-other-keys)
  (create-dao 'purchase :item (find-dao 'item :id item) :date date :quantity quantity
			:on-hand quantity :price price :investor (find-dao 'investor :id investor))
  )

(defun add-listing (&key purchase date quantity price &allow-other-keys)
  (let ((p (find-dao 'purchase :id purchase))
	)
    (setf (purchase-on-hand p) (- (purchase-on-hand p) quantity))
    (update-dao p)
    (create-dao 'listing :purchase p :date date :quantity quantity :price price)
    )
  )

(defun add-sale (&key listing date quantity price fees shipping customer)
  (let ((l (get-listing listing))
	)
    (setf (listing-quantity l) (- (listing-quantity l) quantity))
    (create-dao 'sale :listing l :date date :quantity quantity :price price :fees fees :shipping shipping :customer customer)
    (update-dao l)
    )
  )

(defun update-shipping (&key sale shipping &allow-other-keys)
  (let ((s (find-dao 'sale :id sale))
	)
    (setf (sale-shipping s) shipping)
    (update-dao s)
    )
  )

(defun add-remittance (&key investor date amount &allow-other-keys)
  (create-dao 'remittance :investor (find-dao 'investor :id investor) :date date :amount amount)
  )

(defun get-investors ()
  (retrieve-dao 'investor)
  )

(defun get-items ()
  (retrieve-dao 'item)
  )

(defun get-listings ()
  (retrieve-dao 'listing)
  )

(defun get-purchases ()
  (retrieve-dao 'purchase)
  )

(defun get-sales ()
  (retrieve-dao 'sale)
  )

(defun get-investor (id)
  (find-dao 'investor :id id)
  )

(defun get-listing (id)
  (if (or (typep id 'string) (typep id 'number))
      (find-dao 'listing :id id)
      id)
  )

(defun get-sale (id)
  (find-dao 'sale :id id)
  )

(defun get-purchases-for-investor (investor)
  (select-dao 'purchase (where (:= :investor_id (if (typep investor 'number) investor (object-id investor)))))
  )

(defun get-listings-for-purchase (purchase)
  (select-dao 'listing (where (:= :purchase_id (if (typep purchase 'number) purchase (object-id purchase)))))
  )

(defun get-sales-for-listing (listing)
  (select-dao 'sale (where (:= :listing_id (if (typep listing 'number) listing (object-id listing)))))
  )

(defun get-max-on-hand-for-item (item)
  (or (getf (first (retrieve-by-sql (select (fields (:as (:max :on_hand) :on-hand))
				      (from :purchases)
				      (where (:= :item_id item)))))
	    :on-hand)
      0)
  )

(defun get-first-purchase-with-enough (item quantity)
  (let ((purchase (first (select-dao 'purchase
			   (where (:and (:= :item_id item) (:>= :on_hand quantity)))
			   (order-by :date))))
	)
    (and purchase (object-id purchase))
    )
  )

(defun get-total-remittances-for-investor (investor)
  (reduce (lambda (a b) (+ a b))
	  (select-dao 'remittance (where (:= :investor_id (if (typep investor 'number) investor (object-id investor)))))
	  :key 'remittance-amount :initial-value 0)
  )
