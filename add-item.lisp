;;;; CLOG Builder generated code - modify original clog file
(in-package :arele)
(defclass panel-1 (clog:clog-panel) nil)
(defun create-panel-1
       (clog-obj &key (hidden nil) (class nil) (html-id nil) (auto-place t))
  (let ((panel
         (change-class
          (clog:create-div clog-obj :content "" :hidden hidden :class class
                           :html-id html-id :auto-place auto-place)
          'panel-1)))
    panel))