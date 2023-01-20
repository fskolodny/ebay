;;;; CLOG Builder generated code - modify original clog file
(in-package :arele)
(defclass purchases (clog:clog-panel)
          ((db-table :reader db-table) (database :reader database)))
(defun create-purchases
       (clog-obj &key (hidden nil) (class nil) (html-id nil) (auto-place t))
  (let ((panel
         (change-class
          (clog:create-div clog-obj :content
                           "<div style=\"box-sizing: content-box; position: static; left: 50px; top: 13px;\" id=\"CLOGB3883140620\" data-clog-name=\"database\"></div><table style=\"box-sizing: content-box; position: static; left: 22px; top: 61px;\" id=\"CLOGB3883140621\" data-clog-name=\"db-table\"></table>"
                           :hidden hidden :class class :html-id html-id
                           :auto-place auto-place)
          'purchases)))
    (setf (slot-value panel 'db-table)
            (attach-as-child clog-obj "CLOGB3883140621" :clog-type
             'clog:clog-db-table :new-id t))
    (setf (slot-value panel 'database)
            (attach-as-child clog-obj "CLOGB3883140620" :clog-type
             'clog:clog-database :new-id t))
    panel))
