;;;; CLOG Builder generated code - modify original clog file
(in-package :arele)
(defclass investors (clog:clog-panel)
          ((db-table :reader db-table) (ebay-db :reader ebay-db)))
(defun create-investors
       (clog-obj &key (hidden nil) (class nil) (html-id nil) (auto-place t))
  (let ((panel
         (change-class
          (clog:create-div clog-obj :content
                           "<div style=\"box-sizing: content-box; position: static; left: 67px; top: 29px;\" id=\"CLOGB3882995772\" data-clog-name=\"ebay-db\"></div><table style=\"box-sizing: content-box; position: static;\" id=\"CLOGB3882995773\" data-clog-name=\"db-table\"></table>"
                           :hidden hidden :class class :html-id html-id
                           :auto-place auto-place)
          'investors)))
    (setf (slot-value panel 'db-table)
            (attach-as-child clog-obj "CLOGB3882995773" :clog-type
             'clog:clog-db-table :new-id t))
    (setf (slot-value panel 'ebay-db)
            (attach-as-child clog-obj "CLOGB3882995772" :clog-type
             'clog:clog-database :new-id t))
    (let ((target (ebay-db panel)))
      (declare (ignorable target))
      )
    panel))