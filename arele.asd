
(asdf/parse-defsystem:defsystem #:arele
  :description
  "Ebay listing control system"
  :author
  "fskolodny@gmail.com"
  :license
  "BSD"
  :version
  "0.0.0"
  :serial
  t
  :entry-point
  "arele:start-app"
  :depends-on
  (#:clog #:mito)
  :components
  ((:file "arele") (:file "investors") (:file "inventory") (:file "purchases")
   (:file "listings") (:file "sales") (:file "add-item") (:file "add-purchase")
   (:file "add-listing")))
(asdf/parse-defsystem:defsystem #:arele/tools
  :defsystem-depends-on
  (:clog)
  :depends-on
  (#:arele #:clog/tools)
  :components
  ((:clog-file "investors") (:clog-file "inventory") (:clog-file "purchases")
   (:clog-file "listings") (:clog-file "sales") (:clog-file "add-item")
   (:clog-file "add-purchase") (:clog-file "add-listing")))