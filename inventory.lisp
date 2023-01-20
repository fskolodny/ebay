;;;; CLOG Builder generated code - modify original clog file
(in-package :arele)
(defclass inventory (clog:clog-panel)
          ((items :reader items) (ebay-db :reader ebay-db)))
(defun create-inventory
       (clog-obj &key (hidden nil) (class nil) (html-id nil) (auto-place t))
  (let ((panel
         (change-class
          (clog:create-div clog-obj :content
                           "<div style=\"box-sizing: content-box; position: static; left: 95px; top: 53px;\" id=\"CLOGB3883048997\" data-clog-name=\"ebay-db\"></div><table style=\"box-sizing: content-box; position: static; left: 24px; top: 29px;\" id=\"CLOGB3883048998\" data-clog-name=\"items\"></table>"
                           :hidden hidden :class class :html-id html-id
                           :auto-place auto-place)
          'inventory)))
    (setf (slot-value panel 'items)
            (attach-as-child clog-obj "CLOGB3883048998" :clog-type
             'clog:clog-db-table :new-id t))
    (setf (slot-value panel 'ebay-db)
            (attach-as-child clog-obj "CLOGB3883048997" :clog-type
             'clog:clog-database :new-id t))
    (let ((target (ebay-db panel)))
      (declare (ignorable target))
      (setf (database-connection target)
              (dbi:connect :sqlite3 :database-name "arele.db")))
    (let ((target (items panel)))
      (declare (ignorable target))
      (setf (clog-database target) (clog-database (ebay-db panel)))
      (setf (table-name target) "inventory")
      (setf (where-clause target) "")
      (setf (order-by target) "")
      (setf (limit target) "")
      (setf (row-id-name target) "rowid")
      (setf (table-columns target) '(rowid description))
      (setf (attribute target :border) 1)
      (get-row target panel))
    panel))