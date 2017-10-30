;;;; what3words.lisp

(in-package #:what3words)

;;; "what3words" goes here. Hacks and glory await!

(defmacro cdr-assoc (name alist)
  "Replaces '(cdr (assoc name alist))' because it's used a bajillion
times when doing API stuff."
  `(cdr (assoc ,name ,alist :test #'equal)))

(defun words-to-loc (api-key words &key (lang "en") (display "full") debug)
  "Given an API key and a word location (like
'armchairs.reduced.flannel'), return information about that location
as a JSON blob."
  (let ((uri
	 (concatenate
	  'string
	  "https://api.what3words.com/v2/forward?addr=" words
	  "&key=" api-key
	  "&lang=" lang
	  "&format=json&display=full")))
    (when debug (print uri))
    (json:decode-json-from-string
     (babel:octets-to-string
      (nth-value 0
		 (drakma:http-request
		  uri
		  :accept "application/json"
		  :content-type "application/json"
		  :method :get))))))
  
(defun loc-to-words (api-key lat lon &key (lang "en") (display "full") debug)
  "Given an API key and a floating point latitude and longitude,
return a set of words that map that location as a JSON blob."
  (let ((uri
	 (concatenate
	  'string
	  "https://api.what3words.com/v2/reverse?coords="
	  (format nil "~A,~A" lat lon)
	  "&key=" api-key
	  "&lang=" lang
	  "&format=json&display=full")))
    (when debug (print uri))
    (json:decode-json-from-string
     (babel:octets-to-string
      (nth-value 0
		 (drakma:http-request
		  uri
		  :accept "application/json"
		  :content-type "application/json"
		  :method :get))))))

(defun autosuggest (api-key addr &key lang ml focus-lat focus-lon
				   clip-lat clip-lon radius
				   count (display "full")
				   clip-ne-lat clip-ne-lon
				   clip-sw-lat clip-sw-lon
				   debug)
  (let* ((language (if (and (not ml) (not lang)) "en" lang))
	 (uri
	  (concatenate
	   'string
	   "https://api.what3words.com/v2/"
	   (if ml "autosuggest-ml?" "autosuggest?")
	   "key=" api-key
	   "&addr=" addr
	   (if language (format nil "&lang=~A" language) "")
	   (if count (format nil "&count=~A" count) "")
	   "&display=" display
	   (if (and focus-lat focus-lon) (format nil "&focus=~A,~A" focus-lat focus-lon) "")
	   (cond
	     ((and clip-lat clip-lon radius)
	      (format nil "&clip=radius(~A,~A,~A)" clip-lat clip-lon radius))
	     (radius
	      (format nil "&clip=focus(~A)" radius))
	     ((and clip-ne-lat clip-ne-lon clip-sw-lat clip-sw-lon)
	      (format nil "&clip=bbox(~A,~A,~A,~A)" clip-ne-lat clip-ne-lon clip-sw-lat clip-sw-lon))
	     (t
	      "&clip=none")))))
    (when debug (print uri))
    (json:decode-json-from-string
     (babel:octets-to-string
      (nth-value 0 (drakma:http-request
		    uri
		    :accept "application/json"
		    :content-type "application/json"
		    :method :get))))))

(defun standardblend (api-key addr &key lang ml focus-lat focus-lon debug)
  (let* ((language (if (and (not ml) (not lang)) "en" lang))
	 (uri
	  (concatenate
	   'string
	   "https://api.what3words.com/v2/"
	   (if ml "standardblend-ml?" "standardblend?")
	   "key=" api-key
	   "&addr=" addr
	   (if language (format nil "&lang=~A" language) "")
	   (format nil "&focus=~A,~A" focus-lat focus-lon))))
    (when debug (print uri))
    (json:decode-json-from-string
     (babel:octets-to-string
      (nth-value 0 (drakma:http-request
		    uri
		    :accept "application/json"
		    :content-type "application/json"
		    :method :get))))))

(defun grid (api-key ne-lat ne-lon sw-lat sw-lon &key debug)
  (let ((uri
	 (format nil "https://api.what3words.com/v2/grid?key=~A&bbox=~A,~A,~A,~A"
		 api-key ne-lat ne-lon sw-lat sw-lon)))
    (when debug (print uri))
    (json:decode-json-from-string
     (babel:octets-to-string
      (nth-value 0
		 (drakma:http-request
		  uri
		  :accept "application/json"
		  :content-type "application/json"
		  :method :get))))))

(defun get-languages (api-key &key debug)
  (let ((uri 
	 (concatenate
	  'string
	  "https://api.what3words.com/v2/languages?"
	  "key=" api-key)))
    (when debug (print uri))
    (json:decode-json-from-string
     (babel:octets-to-string
      (nth-value 0
		 (drakma:http-request
		  uri
		  :accept "application/json"
		  :content-type "application/json"
		  :method :get))))))
