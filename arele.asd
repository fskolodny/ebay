
(asdf/parse-defsystem:defsystem #:arele
  :description "Ebay listing control system"
  :author "fskolodny@gmail.com"
  :license "BSD"
  :version "1.0.0"
  :serial t
  :entry-point "arele:start-app"
  :depends-on (
	       #:clog
	       #:clobber
	       #:dexador
               #:mito
	       #:alexandria
	       #:json
               )
  :components (
	       (:file "models")
	       (:file "arele")
               )
  )
