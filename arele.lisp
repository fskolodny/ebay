(defpackage #:arele
  (:use #:cl
	#:clog
	#:clog-gui
	#:models
	#:local-time
	)
  (:export start-app))

(in-package :arele)

(defun to-dollar (value)
  (multiple-value-bind (x result) (ppcre:scan-to-strings "(\\d+)(.(\\d{2}))?" value)
    (declare (ignore x))
    (+ (* 100 (parse-integer (or (aref result 0) "0"))) (parse-integer (or (aref result 2) "0")))
    )
  )

(defun display-dollar (value)
  (format nil "~$" value)
  )

(defun on-investor-list (obj)
  (let* ((app (connection-data-item obj "app-data"))
         (win (create-gui-window obj :title "Investors"))
         (panel (create-div (window-content win)))
	 (slots '(:id :name :percentage))
	 )
    (declare (ignorable app))
    (with-clog-create panel
	(table (:bind table)
	       (table-head (:bind th :style "text-align: center")
			      )
	       (table-body (:bind tb)
			   )
	       )
      (setf (attribute table :border) 1)
      (mapc (lambda (slot)
	      (create-table-column th :content (format nil "~:(~a~)" slot))
	      )
	    slots)
      (mapc (lambda (investor)
	      (with-clog-create tb
		  (table-row (:bind tr)
			     )
		(create-table-column tr :content (object-id investor))
		(create-table-column tr :content (investor-name investor))
		(create-table-column tr :content (investor-percentage investor) :style "text-align: right")
		)
	      )
	    (get-investors)
	    )
      )
    )
  )

(defun on-add-investor (obj)
  (let* ((app (connection-data-item obj "app-data"))
	 (win (create-gui-window obj :title "Add Investor" :width 400 :height 400))
	 (panel (create-div (window-content win)))
	 )
    (declare (ignore app))
    (with-clog-create panel
	(span ()
	      (form (:bind f1)
		    (table ()
			   (table-body ()
				       (table-row ()
						  (table-column () (label (:content "Name")))
						  (table-column () (form-element (:bind name :text)))
						  )
				       (table-row ()
						  (table-column () (label (:content "Percentage 0-100")))
						  (table-column () (select (:bind percentage)
								     (option (:value 0 :content 0))
								     (option (:value 30 :content 30 :selected t))
								     (option (:value 100 :content 100))
								     ))
						  )
				       (table-row ()
						  (table-column () (form-element (:submit :value "Submit")))
						  (table-column () (form-element (:reset :value "Reset")))
						  )
				       )
			   )
		    )
	      (div (:bind msg :content "Successfully added."))
	      )
      (setf (attribute name :required) t
	    (attribute percentage :required) t
	    (attribute msg :hidden) t
	    )
      (set-on-submit f1 (lambda (obj)
			  (declare (ignorable obj))
			  (add-investor :name (value name) :percentage (parse-integer (value percentage)))
			  (setf (attribute msg :hidden) nil)
			  (reset f1)
			  )
		     )
      )
    )
  )

(defun on-inventory-list (obj)
  (let* ((app (connection-data-item obj "app-data"))
	 (win (create-gui-window obj :title "Inventory"))
	 (inventory (create-div (window-content win)))
	 (slots '(:id :description))
	 )
    (declare (ignorable app inventory))
    (with-clog-create inventory
	(table (:bind table)
	       (table-head (:bind th :style "text-align: center")
			      )
	       (table-body (:bind tb)
			   )
	       )
      (setf (attribute table :border) 1)
      (mapc (lambda (slot)
	      (create-table-column th :content (format nil "~:(~a~)" slot))
	      )
	    slots)
      (mapc (lambda (item)
	      (with-clog-create tb
		  (table-row (:bind tr)
			     )
		(create-table-column tr :content (object-id item))
		(create-table-column tr :content (item-description item))
		)
	      )
	    (get-items)
	    )
      )
    )
  )

(defun on-add-item (obj)
  (let* ((app (connection-data-item obj "app-data"))
	 (win (create-gui-window obj :title "Add Item"))
	 (panel (create-div (window-content win)))
	 )
    (declare (ignore app))
    (with-clog-create panel
	(form (:bind f1)
	      (fieldset (:bind fs1)
			(table ()
			       (table-body ()
					   (table-row ()
						      (table-column () (label (:content "Description")))
						      (table-column () (form-element (:bind description :text)))
						      )
					   (table-row ()
						      (table-column () (form-element (:submit :value "Ok")))
						      (table-column () (form-element (:reset :value "Clear")))
						      )
					   )
			       )
			)
	      )
      (setf (attribute description :requiredp) t)
      (set-on-submit f1 (lambda (obj)
			  (declare (ignore obj))
			  (add-inventory-item :description (value description))
			  (reset f1)
			  )
		     )
      )
    )
  )

(defun on-purchases-list (obj)
  (let* ((app (connection-data-item obj "app-data"))
	 (win (create-gui-window obj :title "Purchases" :width 600))
	 (purchases (create-div (window-content win)))
	 (slots '(:id :investor :item :date :quantity :on-hand))
	 (dollar-slots '(:price))
	 )
    (declare (ignorable app purchases))
    (with-clog-create purchases
	(table (:bind table)
	       (table-head (:bind th :style "text-align: center")
			      )
	       (table-body (:bind tb)
			   )
	       )
      (setf (attribute table :border) 1)
      (mapc (lambda (slot)
	      (create-table-column th :content (format nil "~:(~a~)" slot))
	      )
	    slots)
      (mapc (lambda (slot)
	      (create-table-column th :content (format nil "~:(~a~)" slot))
	      )
	    dollar-slots)
      (mapc (lambda (purchase)
	      (with-clog-create tb
		  (table-row (:bind tr)
			     )
		(create-table-column tr :content (object-id purchase))
		(create-table-column tr :content (investor-name (purchase-investor purchase)))
		(create-table-column tr :content (item-description (purchase-item purchase)))
		(create-table-column tr :content (format-rfc3339-timestring nil (purchase-date purchase) :omit-time-part t))
		(create-table-column tr :content (purchase-quantity purchase) :style "text-align: right")
		(create-table-column tr :content (purchase-on-hand purchase) :style "text-align: right")
		(create-table-column tr :content (display-dollar (/ (purchase-price purchase) 100)) :style "text-align: right")
		)
	      )
	    (get-purchases)
	    )
      )
    )
  )

(defun on-add-purchase (obj)
  (let* ((app (connection-data-item obj "app-data"))
	 (win (create-gui-window obj :title "Add Purchase" :height 600))
	 (panel (create-div (window-content win)))
	 )
    (declare (ignorable app))
    (with-clog-create panel
	(div ()
	     (form (:bind f1)
		   (table (:bind tbl)
			  (table-body ()
				      (table-row ()
						 (table-column () (label (:content "Select item")))
						 (table-column () (select (:bind item)))
						 )
				      (table-row ()
						 (table-column () (label (:content "Date")))
						 (table-column () (form-element (:bind date :date)))
						 )
				      (table-row ()
						 (table-column () (label (:content "Price")))
						 (table-column () (form-element (:bind price :text)))
						 )
				      (table-row ()
						 (table-column () (label (:content "Quantity")))
						 (table-column () (form-element (:bind quantity :number)))
						 )
				      (table-row ()
						 (table-column () (label (:content "Select investor")))
						 (table-column () (select (:bind investor)))
						 )
				      (table-row ()
						 (table-column () (form-element (:submit :value "Ok")))
						 (table-column () (form-element (:reset :value "Clear")))
						 )
				      )
			     )
		   )
	     )
      (mapcar (lambda (i)
		(add-select-option item (object-id i) (item-description i)))
	      (get-items)
	      )
      (mapcar (lambda (i)
		(add-select-option investor (object-id i) (investor-name i)))
	      (get-investors)
	      )
      (setf (attribute price :pattern) "\\d+([.]\\d\\d)?"
	    (attribute date :required) t
	    (attribute item :required) t
	    (attribute price :required) t
	    (attribute quantity :required) t
	    (minimum quantity) 1
	    (attribute investor :required) t
	    )
      
      (set-on-submit f1
		     (lambda (obj)
		       (declare (ignore obj))
		       (add-purchase :item (value item)
				     :date (value date) :quantity (value quantity) :price (to-dollar (value price))
				     :investor (value investor))
		       (setf (attribute f1 :hidden) t)
		       )
		     )
      )
    )
  )

(defun on-listings-list (obj)
  (let* ((app (connection-data-item obj "app-data"))
	 (win (create-gui-window obj :title "Listings" :width 1200))
	 (listings (create-div (window-content win)))
	 (slots '(:id :item :investor :date :quantity))
	 (dollar-slots '(:price))
	 )
    (declare (ignorable app))
    (with-clog-create listings
	(table (:bind table)
	       (table-head (:bind th :style "text-align: center")
			      )
	       (table-body (:bind tb)
			   )
	       )
      (setf (attribute table :border) 1)
      (mapc (lambda (slot)
	      (create-table-column th :content (format nil "~:(~a~)" slot))
	      )
	    slots)
      (mapc (lambda (slot)
	      (create-table-column th :content (format nil "~:(~a~)" slot))
	      )
	    dollar-slots)
      (mapc (lambda (listing)
	      (with-clog-create tb
		  (table-row (:bind tr)
			     )
		(create-table-column tr :content (object-id listing))
		(let ((purchase (listing-purchase listing))
		      )
		  (create-table-column tr :content (item-description (purchase-item purchase)))
		  (create-table-column tr :content (investor-name (purchase-investor purchase)))
		  )
		(create-table-column tr :content (format-rfc3339-timestring nil (listing-date listing) :omit-time-part t))
		(create-table-column tr :content (listing-quantity listing) :style "text-align: right")
		(create-table-column tr :content (display-dollar (/ (listing-price listing) 100)) :style "text-align: right")
		)
	      )
	    (get-listings)
	    )
      )
    )
  )
(defun on-add-listing (obj)
  (let* ((app (connection-data-item obj "app-data"))
	 (win (create-gui-window obj :title "Add Listing" :height 600))
	 (panel (create-div (window-content win)))
	 (max)
	 )
    (declare (ignorable app))
    (with-clog-create panel
	(div ()
	     (form (:bind f1)
		   (table (:bind tbl)
			  (table-body ()
				      (table-row ()
						 (table-column () (label (:content "Select item")))
						 (table-column () (select (:bind item)))
						 )
				      (table-row ()
						 (table-column () (label (:content "Date")))
						 (table-column () (form-element (:bind date :date)))
						 )
				      (table-row ()
						 (table-column () (label (:content "Price")))
						 (table-column () (form-element (:bind price :text)))
						 )
				      (table-row ()
						 (table-column () (label (:content "Quantity")))
						 (table-column () (form-element (:bind quantity :number)))
						 )
				      (table-row ()
						 (table-column () (form-element (:submit :value "Ok")))
						 (table-column () (form-element (:reset :value "Clear")))
						 )
				      )
			     )
		   )
	     )
      (mapcar (lambda (i)
		(add-select-option item (object-id i) (item-description i)))
	      (get-items)
	      )
      (setf (attribute price :pattern) "\\d+(.\d\d)?"
	    (attribute item :required) t
	    (attribute price :required) t
	    (attribute date :required) t
	    (attribute quantity :required) t
	    (minimum quantity) 1
	    )
      (setf (maximum quantity) (get-max-on-hand-for-item (value item)))
      (set-on-change item (lambda (obj)
			    (declare (ignorable obj))
			    (setf (maximum quantity) (get-max-on-hand-for-item (value item)))
			    )
		     )
      (set-on-submit f1
		     (lambda (obj)
		       (declare (ignore obj))
		       (let ((q (parse-integer (value quantity)))
			     )
			 (setf max (get-max-on-hand-for-item (value item)))
			 (if (<= 1 q max)
			     (let ((purchase (get-first-purchase-with-enough (value item) q))
				   )
			       (when purchase
				 (add-listing :purchase purchase :date (value date) :quantity q :price (to-dollar (value price)))
				 (reset f1)
				 )
			       )
			     )
			 )
		       )
		     )
      )
    )
  )

(defun on-sales-list (obj)
  (let* ((app (connection-data-item obj "app-data"))
	 (win (create-gui-window obj :title "Sales" :width 1200))
	 (panel (create-div (window-content win)))
	 (slots '(:id :date :quantity :customer :investor :item))
	 (dollar-slots '(:price :fees :shipping))
	 )
    (declare (ignorable app))
    (with-clog-create panel
	(table (:bind table)
	       (table-head (:bind th :style "text-align: center")
			      )
	       (table-body (:bind tb)
			   )
	       )
      (setf (attribute table :border) 1)
      (mapc (lambda (slot)
	      (create-table-column th :content (format nil "~:(~a~)" slot))
	      )
	    slots)
      (mapc (lambda (slot)
	      (create-table-column th :content (format nil "~:(~a~)" slot))
	      )
	    dollar-slots)
      (mapc (lambda (sale)
	      (with-clog-create tb
		  (table-row (:bind tr)
			     )
		(create-table-column tr :content (object-id sale))
		(create-table-column tr :content (format-rfc3339-timestring nil (sale-date sale) :omit-time-part t))
		(create-table-column tr :content (sale-quantity sale) :style "text-align: right")
		(create-table-column tr :content (sale-customer sale))
		(create-table-column tr :content (investor-name (purchase-investor (listing-purchase (sale-listing sale)))))
		(create-table-column tr :content (item-description (purchase-item (listing-purchase (sale-listing sale)))))
		(create-table-column tr :content (display-dollar (/ (sale-price sale) 100)) :style "text-align: right")
		(create-table-column tr :content (display-dollar (/ (sale-fees sale) 100)) :style "text-align: right")
		(create-table-column tr :content (display-dollar (/ (sale-shipping sale) 100)) :style "text-align: right")
		)
	      )
	    (get-sales)
	    )
      )
    )
  )

(defun on-add-sale (obj)
  (let* ((app (connection-data-item obj "app-data"))
	 (win (create-gui-window obj :title "Add Sale" :height 350 :width 500))
	 (panel (create-div (window-content win)))
	 )
    (declare (ignorable app))
    (with-clog-create panel
	(form (:bind f1)
	      (table ()
		     (table-body ()
				 (table-row ()
					    (table-column () (label (:content "Select listing")))
					    (table-column () (select (:bind listing)))
					    )
				 (table-row ()
					    (table-column () (label (:content "Date")))
					    (table-column () (form-element (:bind date :date)))
					    )
				 (table-row ()
					    (table-column () (label (:content "Quantity")))
					    (table-column () (form-element (:bind quantity :number)))
					    )
				 (table-row ()
					    (table-column () (label (:content "Price")))
					    (table-column () (form-element (:bind price :text)))
					    )
				 (table-row ()
					    (table-column () (label (:content "Fees")))
					    (table-column () (form-element (:bind fees :text)))
					    )
				 (table-row ()
					    (table-column () (label (:content "Shipping")))
					    (table-column () (form-element (:bind shipping :text)))
					    )
				 (table-row ()
					    (table-column () (label (:content "Customer")))
					    (table-column () (form-element (:bind customer :text)))
					    )
				 (table-row ()
					    (table-column () (form-element (:submit :value "Ok")))
					    (table-column () (form-element (:reset :value "Reset")))
					    )
				 )
		     )
	      )
      (declare (ignorable date quantity price fees shipping customer))
      (mapc (lambda (l)
	      (add-select-option listing (object-id l)
				 (format nil "~a ~a ~a" (format-rfc3339-timestring t (listing-date l) :omit-time-part t) (item-description (purchase-item (listing-purchase l))) (listing-quantity l)))
	      )
	    (remove-if #'zerop (get-listings) :key 'listing-quantity)
	    )
      (setf (maximum quantity) (listing-quantity (get-listing (value listing))))
      (setf (attribute price :pattern) "\\d+([.].\\d\\d)?"
	    (attribute fees :pattern) "\\d+([.]\\d\\d)?"
	    (attribute shipping :pattern) "\\d+([.]\\d\\d)?"
	    (attribute listing :required) t
	    (attribute date :required) t
	    (attribute quantity :required) t
	    (attribute price :required) t
	    (attribute fees :required) t
	    (attribute customer :required) t
	    (minimum quantity) 1
	    )
      (set-on-change listing (lambda (obj)
			       (declare (ignorable obj))
			       (setf (maximum quantity) (listing-quantity (get-listing (value listing))))
			       )
		     )
      (set-on-submit f1 (lambda (obj)
			  (declare (ignore obj))
			  (let* ((l (get-listing (value listing)))
				 (ship (value shipping))
				 )
			    (when (<= 1 (parse-integer (value quantity)) (listing-quantity l))
			      (add-sale :listing l :date (value date) :quantity (parse-integer (value quantity)) :customer (value customer)
					:price (to-dollar (value price)) :fees (to-dollar (value fees))	:shipping (to-dollar (or (and (plusp (length ship)) ship) "0")))
			      (reset f1)
			      )
			    )
			  )
		     )
      )
    )
  )

(defun on-update-shipping (obj)
  (let* ((app (connection-data-item obj "app-data"))
	 (win (create-gui-window obj :title "Update Shipping"))
	 (panel (create-div (window-content win)))
	 )
    (declare (ignore app))
    (with-clog-create panel
	(form (:bind f1)
	      (table ()
		     (table-body ()
				 (table-row ()
					    (table-column () (label (:content "Select sale")))
					    (table-column () (select (:bind sale)))
					    )
				 (table-row ()
					    (table-column () (label (:content "Shipping")))
					    (table-column () (form-element (:bind shipping :text)))
					    )
				 (table-row ()
					    (table-column () (form-element (:submit :value "Ok")))
					    (table-column () (form-element (:reset :value "Reset")))
					    )
				 )
		     )
	      )
      (mapc (lambda (s)
	      (add-select-option sale (object-id s) (format nil "~a ~a ~a" (format-rfc3339-timestring nil (sale-date s) :omit-time-part t)
							    (item-description (purchase-item (listing-purchase (sale-listing s)))) (sale-quantity s)))
	      )
	    (get-sales)
	    )
      (setf (attribute shipping :pattern) "\\d+([.]\\d\\d)?"
	    (attribute sale :required) t
	    (attribute shipping :required) t
	    )
      (set-on-submit f1 (lambda (obj)
			  (declare (ignore obj))
			  (update-shipping :sale (get-sale (value sale)) :shipping (to-dollar (value shipping)))
			  (reset f1)
			  )
		     )
	)
    )
  )

(defun on-amounts-due (obj)
  (let* ((app (connection-data-item obj "app-data"))
	 (win (create-gui-window obj :title "Amounts Due" :width 400 :height 300))
	 (panel (create-div (window-content win)))
	 (encumbered 0)
	 )
    (declare (ignorable app))
    (with-clog-create panel
	(table (:bind tbl)
	       (table-head (:bind th :style "text-align: center")
			   )
	       (table-body (:bind tb)
			   )
	       )
      (setf (attribute tbl :border) 1)
      (mapc (lambda (name)
	      (create-table-column th :content (format nil "~:(~a~)" name))
	      )
	    '(:name :|amount due| :remitted)
	    )
      (mapc (lambda (investor)
	      (let ((tr (create-table-row tb))
		    (amount-due 0)
		    )
		(create-table-column tr :content (investor-name investor))
		(mapc (lambda (purchase)
			(let* ((unit-price (/ (purchase-price purchase) (purchase-quantity purchase)))
			       (cost-on-hand (* unit-price (purchase-on-hand purchase)))
			       )
			  (setf encumbered (+ encumbered cost-on-hand))
			  (mapc (lambda (listing)
				  (mapc (lambda (sale)
					  (let* ((cost-of-goods (* unit-price (sale-quantity sale)))
						 (profit (- (sale-price sale) cost-of-goods (or (sale-fees sale) 0) (or (sale-shipping sale) 0)))
						 (to-be-remitted (+ cost-of-goods (if (eql (investor-percentage investor) 30) ; standard split 30% on doubling money 50% on excess
											   (+ (* 30/100 (min profit cost-of-goods)) (* 50/100 (max 0 (- profit cost-of-goods))))
											   profit))) ; for 100% i.e. selling to pay rent
						 )
					    (incf encumbered to-be-remitted)
					    (incf amount-due to-be-remitted)
					    )
					  )
					(get-sales-for-listing listing)
					)
				  )
				(get-listings-for-purchase purchase)
				)
			  )
			)
		      (get-purchases-for-investor investor)
		      )
		(create-table-column tr :content (display-dollar (/ amount-due 100)) :style "text-align: right")
		(create-table-column tr :content (display-dollar (/ (get-total-remittances-for-investor investor) 100)) :style "text-align: right")
		)
	      )
	    (remove-if #'zerop (get-investors) :key 'investor-percentage)
	    )
      )
    (create-div panel :content (format nil "Amount encumbered is ~a." (display-dollar (/ encumbered 100))))
    )
  )

(defun on-add-remittance (obj)
  (let* ((app (connection-data-item obj "app-data"))
	 (win (create-gui-window obj :title "Add Remittance" :height 600))
	 (panel (create-div (window-content win)))
	 )
    (declare (ignorable app))
    (with-clog-create panel
	(form (:bind f1)
	      (table ()
		     (table-body ()
				 (table-row ()
					    (table-column () (label (:content "Select investor")))
					    (table-column () (select (:bind investor)))
					    )
				 (table-row ()
					    (table-column () (label (:content "Date")))
					    (table-column () (form-element (:bind date :date)))
					    )
				 (table-row ()
					    (table-column () (label (:content "Amount")))
					    (table-column () (form-element (:bind amount :text)))
					    )
				 (table-row ()
					    (table-column () (form-element (:submit :value "Ok")))
					    (table-column () (form-element (:reset :value "Reset")))
					    )
				 )
		     )
	      )
      (mapc (lambda (i)
	      (add-select-option investor (object-id i) (investor-name i))
	      )
	    (get-investors)
	    )
      (setf (attribute investor :required) t
	    (attribute date :required) t
	    (attribute amount :required) t
	    (attribute amount :pattern) "\\d+(.\d\d)?"
	    )
      (set-on-submit f1 (lambda (obj)
			  (declare (ignore obj))
			  (add-remittance :investor (get-investor (value investor))
					  :date (value date) :amount (to-dollar (value amount)))
			  (reset f1)
			  )
		     )
      )
    )
  )
(defun on-help-about (obj)
  (let* ((about (create-gui-window obj
                                   :title   "About"
                                   :content "<div class='w3-black'>
                                         <center><img src='/img/clogwicon.png'></center>
                                         <center>arele</center>
                                         <center>arele</center></div>
                                         <div><p><center>A New App</center>
                                         <center>(c) 2023 - Fila Kolodny</center></p></div>"
                                   :hidden  t
                                   :width   200
                                   :height  200)))
    (window-center about)
    (setf (visiblep about) t)
    (set-on-window-can-size about (lambda (obj)
                                    (declare (ignore obj))()))))

(defclass app-data ()
  ((data
    :accessor data)))

(defun on-new-window (body)
  (let ((app (make-instance 'app-data)))
    (setf (connection-data-item body "app-data") app)
    (setf (title (html-document body)) "EBay Store Software")
    (clog-gui-initialize body)
    (add-class body "w3-teal")
    (with-clog-create body
	(gui-menu-bar (:bind menu-bar)
		  (gui-menu-drop-down (:content "Investors")
				      (gui-menu-item (:content "Investor List" :on-click 'on-investor-list))
				      (gui-menu-item (:content "Add Investor" :on-click 'on-add-investor))
				      )
		  (gui-menu-drop-down (:content "Inventory")
				      (gui-menu-item (:content "Items" :on-click 'on-inventory-list))
				      (gui-menu-item (:content "Add Item" :on-click 'on-add-item))
				      )
		  (gui-menu-drop-down (:content "Purchases")
				      (gui-menu-item (:content "Purchases List" :on-click 'on-purchases-list))
				      (gui-menu-item (:content "Add Purchase" :on-click 'on-add-purchase))
				      )
		  (gui-menu-drop-down (:content "Listings")
				      (gui-menu-item (:content "Listings List" :on-click 'on-listings-list))
				      (gui-menu-item (:content "Add Listing" :on-click 'on-add-listing))
				      )
		  (gui-menu-drop-down (:content "Sales")
				      (gui-menu-item (:content "Sales List" :on-click 'on-sales-list))
				      (gui-menu-item (:content "Add Sale" :on-click 'on-add-sale))
				      (gui-menu-item (:content "Update Shipping" :on-click 'on-update-shipping))
				      )
		  (gui-menu-drop-down (:content "Remittances")
				      (gui-menu-item (:content "Add remittance" :on-click 'on-add-remittance))
				      )
		  (gui-menu-drop-down (:content "Reports")
				      (gui-menu-item (:content "Amounts due" :on-click 'on-amounts-due))
				      (gui-menu-item (:content "Investor List" :on-click 'on-investor-list))
				      (gui-menu-item (:content "Inventory items" :on-click 'on-inventory-list))
				      (gui-menu-item (:content "Purchases List" :on-click 'on-purchases-list))
				      (gui-menu-item (:content "Listings List" :on-click 'on-listings-list))
				      (gui-menu-item (:content "Sales List" :on-click 'on-sales-list))
				      )
		  (gui-menu-drop-down (:content "Help")
				      (gui-menu-item (:content "About" :on-click 'on-help-about))
				      )
		  )
      (create-gui-menu-full-screen menu-bar)
      )
    )
  )

(defun start-app ()
  (initialize-models)
  (initialize 'on-new-window
   :static-root (merge-pathnames "./www/"
				 (asdf:system-source-directory :arele)))
  (open-browser))
