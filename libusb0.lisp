#.(require :cffi)
(defpackage :libusb0
  (:use :cl :cffi)
  (:export
   :get-vendor-id
   :get-product-id
   :get-devices-by-ids
   :ensure-libusb0-initialized))

(in-package :libusb0)

(defparameter *libusb*
  (define-foreign-library libusb0
    ;; 
    (:windows "C:/Users/martin/Downloads/libusb-win32-bin-1.2.6.0/bin/amd64/libusb0.dll")
    (t (:default "libusb"))))

(use-foreign-library libusb0)

(defcfun "usb_init" :void)
#+nil
(usb-init)
(defcfun "usb_find_busses" :int)
(defcfun "usb_find_devices" :int)

(defctype u_int8_t :uint8)
(defctype u_int16_t :uint16)
(defctype u_int32_t :uint32)
(defcstruct usb_device_descriptor
  (bLength u_int8_t)  
  (bDescriptorType u_int8_t)  
  (bcdUSB u_int16_t) 
  (bDeviceClass u_int8_t)  
  (bDeviceSubClass u_int8_t)  
  (bDeviceProtocol u_int8_t)  
  (bMaxPacketSize0 u_int8_t)  
  (idVendor u_int16_t) 
  (idProduct u_int16_t) 
  (bcdDevice u_int16_t) 
  (iManufacturer u_int8_t)  
  (iProduct u_int8_t)  
  (iSerialNumber u_int8_t)  
  (bNumConfigurations u_int8_t))

;;find /usr/include/|grep -ir limits|xargs fgrep PATH_MAX
#+win32 #.(defconstant +path-max+ (1- 512)) ;; windows
#+linux #.(defconstant +path-max+ 4096)


(defcstruct usb_device
  (next (:pointer usb_device_descriptor))
  (prev (:pointer usb_device_descriptor))
  (filename :char :count #.(1+ +path-max+))
  (bus :pointer)
  (descriptor usb_device_descriptor)
  (config :pointer)
  (dev (:pointer :void))
  (devnum u_int8_t)
  (num-children :unsigned-char)
  (children (:pointer ;usb_device
	     )))

(defcstruct usb_bus
  (next (:pointer ;:usb_bus
	 ))
  (prev (:pointer ;:usb_bus
	 ))
  (dirname :char :count #.(1+ +path-max+))
  (devices (:pointer usb_device))
  (location u_int32_t)
  (root-dev (:pointer usb_device)))


(defcfun "usb_get_busses" (:pointer usb_bus))
(defctype phandle :pointer)
(defcfun "usb_open" phandle (dev (:pointer usb_device)))
(defcfun "usb_close" :int (handle phandle))
(defcfun "usb_set_configuration" :int
  (handle phandle)
  (configuration :int))
(defcfun "usb_claim_interface" :int
  (handle phandle)
  (interface :int))
(defcfun "usb_release_interface" :int
  (handle phandle)
  (interface :int))
(defcfun "usb_bulk_read" :int
  (handle phandle)
  (endpoint :int)
  (bytes (:pointer :uint8))
  (size :int)
  (timout_ms :int))
(defcfun "usb_bulk_write" :int
  (handle phandle)
  (endpoint :int)
  (bytes (:pointer :uint8))
  (size :int)
  (timout_ms :int))
(defcfun "usb_control_msg" :int
  (handle phandle)
  (request-type :int)
  (request :int)
  (value :int)
  (index :int)
  (bytes (:pointer :uint8))
  (size :int)
  (timeout_ms :int))

#+nil
(usb-init)
#+nil
(usb-find-busses)
#+nil
(usb-find-devices)

(defmacro check (&body body)
  `(let ((val (progn ,@body)))
     (if (< val 0)
	 (error (format nil "libusb error: ~a returned ~a." ',@body val))
	 val)))

(defvar *libusb0-initialized* nil)
(defvar *libusb0-shared-object* nil)
(defun ensure-libusb0-initialized ()
  (unless *libusb0-initialized*
    (setf *libusb0-shared-object* (use-foreign-library libusb0))
    (usb-init)
    (setf *libusb0-initialized* t))
  (list (usb-find-busses)
	(usb-find-devices)))

(defun get-busses ()
  (ensure-libusb0-initialized)
  (loop with bus = (usb-get-busses)
       until (null-pointer-p bus)
       collect bus
       do (setf bus (foreign-slot-value bus 'usb_bus 'next))))

#+nil
(get-busses)

(defun get-devices* (bus)
 (loop with dev = (foreign-slot-value 
		   bus 'usb_bus
		   'devices)
    until (null-pointer-p dev)
    collect dev
    do (setf dev (foreign-slot-value 
		  dev 'usb_device 
		  'next))))

(defun get-devices (&optional (bus-or-list (get-busses)))
  (if (listp bus-or-list)
      (loop for bus in bus-or-list
	   nconcing (get-devices* bus))
      (get-devices* bus-or-list)))

#+nil
(get-devices)

(defun get-vendor-id (dev)
  (foreign-slot-value 
  (foreign-slot-value dev 'usb_device 
		       'descriptor)
   'usb_device_descriptor
   'idvendor))

(defun get-product-id (dev)
  (foreign-slot-value 
   (foreign-slot-value dev 'usb_device 
		       'descriptor)
   'usb_device_descriptor
   'idproduct))

#+nil
(loop for e in (get-devices) collect
     (format nil "~4,'0x:~4,'0x" (get-vendor-id e)
	     (get-product-id e)))

(defun get-devices-by-ids (&key (vendor-id nil) (product-id nil))
  (flet ((ids-match (dev)
	   (and (or (null vendor-id)
		    (= vendor-id (get-vendor-id dev)))
		(or (null product-id)
		    (= product-id (get-product-id dev))))))
    (delete-if-not #'ids-match (get-devices))))

#+nil
(get-devices-by-ids :vendor-id #x067b :product-id #x2303)

(defclass usb-connection ()
  ((device :reader device :initarg :device :type 'fixnum)
   (handle :reader handle :initarg :handle :type 'fixnum)
   (configuration :reader configuration :initarg :configuration :type 'fixnum)
   (interface :reader interface :initarg :interface :initform nil :type '(or null fixnum))))

(defmethod initialize-instance :after ((c usb-connection) &key (vendor-id nil)
				       (product-id nil) 
				       (device (first (get-devices-by-ids :vendor-id vendor-id
									  :product-id product-id)))
				       (configuration 0)
				       (interface nil))
  (with-slots ((d device) (h handle) (conf configuration) (int interface))
      c
    (when (null-pointer-p device)
      (error "device is null."))
    
    (setf d device
	  h (usb-open d))
    (when (null-pointer-p h)
      (error "usb-open didn't succeed."))
    
    (when (< (usb-set-configuration h configuration) 0)
      (error "make sure the pl2303 is removed and you have permission to write to the usb device. SYSFS{idVendor}==\"067b\", MODE=\"0666\" to udev rules."))
    
    (setf conf configuration)
    (when interface 
      (when (< (usb-claim-interface h interface) 0)
	(error "can't claim interface")))
    (setf int interface)
    c))



(defmethod close-connection ((c usb-connection))
  (with-slots (handle interface) c
    (usb-release-interface handle interface)
    (usb-close handle)))

#+nil
(progn
  (defparameter *bla*
    (make-instance 'usb-connection
		   :vendor-id #x067b
		   :product-id #x2303
		   :configuration 1
		   :interface 0
					;:endpoint #x83 ; 2 #x81 #x83qu
		   ))
  (prepare-zeiss))
;;if (ps->dev->state != USB_STATE_CONFIGURED)
;;                retval = -EHOSTUNREACH;
;;[pid 30032] ioctl(5, USBDEVFS_IOCTL or USBDEVFS_IOCTL32, 0x7ffff5595790) = -1 EHOSTUNREACH (No route to host)
;;[pid 30032] ioctl(5, USBDEVFS_SETCONFIGURATION, 0x7ffff559593c) = 0

#+nil
(usb-set-configuration (slot-value *bla* 'handle)
		       1)


#+nil
(usb-claim-interface (slot-value *bla* 'handle) 0)


(defmethod bulk-write ((c usb-connection) data 
		       &key (endpoint 0) (timeout-ms 1000))
  (declare (type (simple-array (unsigned-byte 8) 1) data))
  (let ((len (with-pointer-to-vector-data (ptr data)
	       (check 
		 (usb-bulk-write (slot-value c 'handle)
				 endpoint ptr (length data)
				 timeout-ms)))))
    (unless (= len (length data))
      (error "libusb0: only wrote ~d of ~d bytes." len (length data)))
    len))

(defmethod bulk-read ((c usb-connection) bytes-to-read 
		      &key (endpoint 0) (timeout-ms 1000))
  (let ((data (make-array bytes-to-read :element-type '(unsigned-byte 8))))
    (let ((len (with-pointer-to-vector-data (ptr data)
		 (check
		   (usb-bulk-read (slot-value c 'handle)
				  endpoint
				  ptr
				  bytes-to-read
				  timeout-ms)))))
      (if (= len bytes-to-read)
	  data
	  (subseq data 0 len)))))


(defmethod control-msg ((c usb-connection) request-type
			request &key (value 0) (index 0) (data nil) 
			(timeout-ms 1000))
  (let* ((len 
	  (if data
	      (with-pointer-to-vector-data (ptr data)
		(usb-control-msg (slot-value c 'handle)
				 request-type request value index 
				 ptr (length data) timeout-ms))
	      (usb-control-msg (slot-value c 'handle)
			       request-type request value index 
			       (null-pointer) 0 timeout-ms))))
    (unless (= len (length data))
	(error "control-msg: only ~d of ~d bytes sent" len (length data)))
    len))

#+nil
(defparameter *dobla* t)
#+nil
(defparameter *dobla* nil)
#+nil
(defparameter *val* #b01010101)
#+nil
(loop while *dobla* do
 (let ((l (loop for i below #x40 collect *val* ;#b01010101
	       )))
   (bulk-write *bla* (make-array (length l)
				 :element-type '(unsigned-byte 8)
				 :initial-contents l)
	       :endpoint 2)))

#+nil
(let* ((str "!!ZA98;")
       (a (make-array (length str)
		      :element-type '(unsigned-byte 8)
		      :initial-contents (map 'list #'char-code str))))
  (bulk-write *bla* a :endpoint 2))

#+nil
(let* ((str "ZM1;")
       (a (make-array (length str)
		      :element-type '(unsigned-byte 8)
		      :initial-contents (map 'list #'char-code str))))
  (bulk-write *bla* a :endpoint 2))

#+nil
(loop for e in '(X Y Z) collect
 (let* ((str (format nil "~Ai;" e))
	(a (make-array (length str)
		       :element-type '(unsigned-byte 8)
		       :initial-contents (map 'list #'char-code str))))
   (bulk-write *bla* a :endpoint 2)
   (sleep .1)
   (list e
    (map 'string #'code-char
	 (bulk-read *bla* #x5 :endpoint #x83)))))

#+nil
(map 'string #'code-char
     (bulk-read *bla* #x5 :endpoint #x83))




(defmacro define-pl2303-constants ()
  `(progn 
     ,@(loop for (e f) in '((set-line-request-type #x21)
			    (set-line-request #x20)
			    (set-control-request-type #x21)
			    (set-control-request #x22)
			    (control-dtr 1)
			    (control-rts 2)
			    (break-request-type #x21)
			    (break-request #x23)
			    (break-on #xffff)
			    (break-off #x0000)
			    (get-line-request-type #xa1)
			    (get-line-request #x21)
			    (vendor-write-request-type #x40)
			    (vendor-write-request 1)
			    (vendor-read-request-type #xc0)
			    (vendor-read-request 1))
	 collect
	    `(defconstant ,(intern (format nil "+~A+" e)) ,f))))

(define-pl2303-constants)

(defmethod pl2303-vendor-write ((c usb-connection) value index)
  (declare (type (unsigned-byte 16) value index))
  (control-msg c +vendor-write-request-type+ +vendor-write-request+
	       :value value :index index))

(defmethod pl2303-vendor-read ((c usb-connection) value index)
  (declare (type (unsigned-byte 16) value index))
  (let ((data (make-array 1 :element-type '(unsigned-byte 8))))
   (values (control-msg c +vendor-read-request-type+ +vendor-read-request+
			:value value :index index :data data)
	   (aref data 0))))

(defmethod get-line ((c usb-connection))
 (let ((data (make-array 7 :element-type '(unsigned-byte 8))))
   (control-msg c
		+get-line-request-type+ +get-line-request+
		:data data)
   data))
#+nil
(get-line *bla*)

(defmethod set-line ((c usb-connection))
  (let ((data (make-array 7
			  :element-type '(unsigned-byte 8)
			  :initial-contents '(#x80 #x25 #x00 #x00 #x02 #x00 #x08))))
    (control-msg c +set-line-request-type+ +set-line-request+
		 :data data)
   data))
;; 9600   80 25
;; 115200 00 c2 01 00
;; 230400 00 84 03 00
;; 921600 00 10 0e
;; 1228800 00 c0 12 ;; 580kHz when sending #b01010101 
;; 2457600 00 80 25 00 ;; this doesn't look right anymore
;; 3000000 c0 c6 2d
;; 6000000 005B8D80 ;; kind of works for #b11110000

;; for the 6e6 baud setting i measure 4 bit are approximately 1us (or
;; 1byte per 2us), i see bursts of 140us with 112us gaps. this means
;; that the usb isn't able to keep up. i send 64byte packets, that
;; would correspond to 96us (with 1 stop bit every byte)
(defvar *bla* nil)
(defmethod prepare-zeiss ((con usb-connection))
  (set-line con)
  (loop for (e f g) in '((r #x8484 0)
			 (w #x0404 0)
			 (r #x8484 0)
			 (r #x8383 0)
			 (r #x8484 0)
			 (w #x0404 1)
			 (r #x8484 0)
			 (r #x8383 0)
			 (w 0 1)
			 (w 1 0)
			 (w 2 #x44)
			 (w #x0606 0)
			 (w 8 0)
			 (w 9 0))
     collect
       (ecase e
	 (r (pl2303-vendor-read con f g))
	 (w (pl2303-vendor-write con f g))))
  (pl2303-vendor-write con 0 #x61) ;; crtscts
  )

#+nil
(pl2303-vendor-write *bla* 0 #x61) ;; crtscts


(defmethod set-control-lines ((c usb-connection) value)
  (declare (type (unsigned-byte 8) value))
  (control-msg c +set-control-request-type+ +set-control-request+ :value value))

#+nil
(loop for i below 100001 do
     (set-control-lines *bla* (if (evenp i) #x00 #xff)))



(defvar *forthdd* nil)
#+nil
(progn
 (defparameter *forthdd*
   (make-instance 'usb-connection
		  :vendor-id #x19ec
		  :product-id #x0300
		  :configuration 1
		  :interface 0)))

(defmethod forthdd-write ((c usb-connection) data)
  (bulk-write c data :endpoint #x04))

(defmethod forthdd-read ((c usb-connection) bytes-to-read)
  (bulk-read c bytes-to-read :endpoint #x83))

(defun make-byte-array (n &optional initial-contents)
  (let ((a (make-array n :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop for i below (min n (length initial-contents)) do
	 (setf (aref a i) (elt initial-contents i)))
    a))

(defun checksum (a &optional (n (length a)))
  (declare (type (simple-array (unsigned-byte 8) 1) a))
  (let ((r 0))
   (dotimes (i n)
      (incf r (aref a i))
      (when (< 255 r)
	(decf r 255)))
    r))

(defun pkg-read (address16 length-1)
  (declare (type (unsigned-byte 16) address16)
	   (type (unsigned-byte 8) length-1))
  (let* ((content (list (char-code #\R)
			(ldb (byte 8 8) address16)
			(ldb (byte 8 0) address16)
			length-1))
	 (n (length content))
	 (a (make-byte-array (1+ n) content)))
    (setf (aref a n) (checksum a n))
    a))

(defun pkg-write (address16 data)
  (declare (type (unsigned-byte 16) address16))
  (let* ((nd (length data))
	 (content (list (char-code #\W)
			(ldb (byte 8 8) address16)
			(ldb (byte 8 0) address16)
			(1- nd)))
	 (n (length content))
	 (a (make-byte-array (+ 1 n nd) content)))
    (dotimes (i nd)
      (setf (aref a (+ i n)) (aref data i)))
    (setf (aref a (+ n nd)) (checksum a (+ n nd)))
    a))

;; write(0x1234, 2, [0x02, 0x41, 0xF3])
;; 57 12 34
;; 02
;; 02 41 F3 
;; D6

#+nil
(format nil "~{~x ~}" (map 'list #'identity 
			   (pkg-write #x1234 #(2 #x41 #xf3))))
#+nil
(pkg-read #x0101 01)

(defun pkg-grab-or-burn (code blocknum32)
  (declare (type (unsigned-byte 32) blocknum32))
  (declare (type (unsigned-byte 8) code))
  (let* ((content (list code
			(ldb (byte 8 24) blocknum32)
			(ldb (byte 8 16) blocknum32)
			(ldb (byte 8 8) blocknum32)
			(ldb (byte 8 0) blocknum32)))
	 (n (length content))
	 (a (make-byte-array (1+ n) content)))
    (setf (aref a n) (checksum a n))
    a))

(defun pkg-grab (blocknum32)
  ;; fetch one page from flash into ram
  (pkg-grab-or-burn (char-code #\G) blocknum32))

(defun pkg-burn (blocknum32)
  ;; write one page of data from forthdd controlers ram into flash
  (pkg-grab-or-burn (char-code #\B) blocknum32))

(defun pkg-call (function &optional data)
  (declare (type (unsigned-byte 8) function))
  (push (length data) data)
  (push function data)
  (push (char-code #\C) data)
  (let* ((n (length data))
	 (a (make-byte-array (1+ n) data)))
    (setf (aref a n) (checksum a n))
    a))

#+nil
(pkg-call #x01 '(1 2 3))

#+ni
(forthdd-write *forthdd* (pkg-call 2))
#+nil
(forthdd-write (pkg-call 1)) ;; reboot

#+nil
(forthdd-write (pkg-call #x23 '(3)))

(defmethod forthdd-talk ((c usb-connection) function &optional data)
  (forthdd-write c (pkg-call function data))
  (forthdd-read c 1024))

#+nil
(forthdd-talk #x0)

#+nil
(progn ;; get number of bitplanes
  (forthdd-talk *forthdd* #x17))
#+nil
(progn ;; get number of ro
  (forthdd-talk *forthdd* #x20))
#+nil
(progn ;; get selected ro
  (forthdd-talk #x21))
#+nil
(progn ;; get default ro
  (forthdd-talk #x22))
;; image 41 is default
#+nil
(progn ;;activate
  (forthdd-talk *forthdd* #x27))
#+nil
(progn ;;deactivate
  (forthdd-talk #x28))
#+nil
(progn ;; reload repertoir
  (forthdd-talk #x29))
#+NIL
(progn ;; get activation type
  (forthdd-talk #x25))
#+nil
(progn ;; get activation state
  (forthdd-talk #x26))

#+nil
(progn ;; switch image/running order
  (forthdd-talk *forthdd* #x23 '(102)))
#+nil
(dotimes (i 100)
  (sleep .1)
  (format t "~d~%" i)
  (progn ;; switch image/running order
    (forthdd-talk #x23 (list i))))

#+nil
(dotimes (i 10)
 (loop for i below 40 do
      (sleep .3)
      (forthdd-talk #x23 (list (random 36)))))

#+nil
(defparameter *resp* (forthdd-read 1024))

#+nil ;; timestamp
(map 'string #'code-char (forthdd-talk 2))

;; => "rTue Jan  5 10:20:15 2010
;; "


(defun slave-package (pkg)
  (declare (type (simple-array (unsigned-byte 8) 1) pkg))
  (ecase (aref pkg 0)
    (97 'ack) ;; a 
    (101 'error) ;; e 
    (112 'pro) ;; p
    (120 'exc) ;; x 
    (114 'ret) ;; r
    (108 'log) ;; l
    ))

#|
#x17 getNumBitplanes
#x20 getROCount
#x21 getSelectedRO
#x22 getDefaultRO
#x23 setSelectedRO byte
#x24 setDefaultRO byte

|#


(defparameter cmds
  `((0 is-ap)
    (1 reboot)
    (2 timestamp)
    (3 version)
    (5 erase-block (blocknum 4))
    (#x17 get-num-bitplanes)
    (#x20 get-ro-count)
    (#x21 get-selected-ro)
    (#x22 get-default-ro)
    (#x23 set-selected-ro (num 1))
    (#x24 set-default-ro (num 1))
    (#x25 get-activation-type)
    (#x26 get-activation-state)
    (#x27 activate)
    (#x28 deactivate)
    (#x29 reload-repertoire)
    (#x30 save-settings)
    (#x31 set-led (brightness 1))
    (#x32 get-led)
    (#x33 set-flip-testpattern (out-ctrl 1))
    (#x34 get-flip-testpattern)
    (#x35 get-daughterboard-type)
    (#x36 adc-read (channel 1))
    (#x37 board-id)
    (#x38 display-type)
    (#x39 display-temp)
    (#x3b get-serial-num)))

(defconstant +EXT-FLASH-BASE+ #x01000000) ;; first page
(defconstant +EXT-FLASH-BUFFER+ #x0400) ;; start of flash buffer in RAM
(defconstant +EXT-FLASH-PAGE-SIZE+ #x0800)

#+nil
(time
 (progn ;; erase all, takes 43s
   (loop for page from #x01000000 below #x0100f000 by 64 do
	(check-ack
	 (erase-block page)))))
#+nil
(erase-block #x01000040)

#+nil
(loop for page from #x01000000 below #x01000f00 by 64 do
	(check-ack
	 (erase-block page)))

(defun erase-block (blocknum)
  "Erase the Flash block."
  (declare (type (unsigned-byte 32) blocknum))
  (forthdd-talk 5
		(loop for i below 32 by 8 collect
		   ;; msb first
		     (ldb (byte 8 (- 24 i)) blocknum))))

(defun check-ack (pkg)
  (unless (eq 'ack (slave-package pkg))
    (break "error, device didnt acknowledge: ~a" pkg)))

(defun erase-bitplane (&optional (image-number 0))
  (check-ack
   (erase-block (+ (* image-number #x40)
		   +EXT-FLASH-BASE+)))
  (check-ack
   (erase-block (+ (* (1+ image-number) #x40) 
		   +EXT-FLASH-BASE+))))

#+nil
(erase-bitplane)


(defmethod write-ex ((c usb-connection) address16 data)
  (declare (type (unsigned-byte 16) address16))
  (forthdd-write c (pkg-write address16 data))
  (check-ack (forthdd-read c 1024)))

(defmethod burn-ex ((c usb-connection) blocknum32)
  (declare (type (unsigned-byte 32) blocknum32))
  (forthdd-write c (pkg-burn blocknum32))
  (check-ack (forthdd-read c 1024)))

(defmethod write-page ((c usb-connection) blocknum32 page)
  (declare (type (simple-array (unsigned-byte 8) 1) page)
	   (type (unsigned-byte 32) blocknum32))
  ;; write in chunks of 256 bytes
  ;; one page in external flash is 2048 bytes (8 packets)
  (dotimes (i 8)
    (write-ex c
	      (+ (* i 256) +EXT-FLASH-BUFFER+)
	      (subseq page 
		      (* 256 i)
		      (* 256 (1+ i)))))
  (burn-ex c blocknum32))

#+nil
(forthdd-write (pkg-write (+ (* 0 256) +EXT-FLASH-BUFFER+)
			  (make-array 256 :element-type '(unsigned-byte 8))))
#+nil
(forthdd-read 1024)
#+nil
(pkg-write (+ (* 0 256) +EXT-FLASH-BUFFER+)
	   (make-array 256 :element-type '(unsigned-byte 8)))

;; from 0x01 00 00 00 there are 960 blocks for images (120 MB)
;; from 0x01 00 F1 00 there are 60 blocks for more data (7.5 MB)

;; the image is filled from right to left and top to bottom
;; the first 160 bytes are the top row

;; each group of 8 bytes appear from left to right
;; each byte is displayed with the least significant
;; bit on the left

;; bytes: 152 153 154 .. 8 9 10 11 12 13 14 15 0 1 2 3 4 5 6 7
;; bits: ... 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7

(defun create-bitplane (img)
  (declare (type (simple-array (unsigned-byte 8) (1024 1280)) img)
	   (values (simple-array (unsigned-byte 8) (1024 160)) &optional))
  (let* ((w 160)
	 (h 1024)
	 (bits (make-array (list h w)
			   :element-type '(unsigned-byte 8))))
    (dotimes (j h)
      (dotimes (ii 20)
	(dotimes (i 8)
	  (let ((iii (+ i (* 8 ii)))
		(iii2 (+ (- 7 i) (* 8 ii))))
	    (dotimes (k 8)
	      (setf (ldb (byte 1 k) (aref bits j iii))
		    (if (= 0 (aref img j (+ k (* 8 iii2))))
			  0 1)))))))
    bits))



(defmethod write-bitplane ((c usb-connection) img &key (image-number 0))
  ;; one bitplane contains 80 pages (smallest write unit) or 1.25
  ;; blocks (smallest erase unit)
  ;; 5 images are stored in 4 blocks
  ;; 1280 x 1024 / 8 = 163840 bytes
  ;; 1 page = 2048 bytes
  ;; 1 block = 131072 bytes
  (declare (type (simple-array (unsigned-byte 8) (1024 160)) img))
  (let* ((img1 (sb-ext:array-storage-vector img))
	 (n (length img1))
	 (p +EXT-FLASH-PAGE-SIZE+))
    (dotimes (i (floor n p))
      (write-page c
		  (+ i (* 80 image-number) +EXT-FLASH-BASE+)
		  (subseq img1
			  (* i p)
			  (* (1+ i) p))))))

#+nil
(progn ;; write some 8pixel stripes
 (let* ((w 160)
	(h 1024)
	(a (make-array (list h w) :element-type '(unsigned-byte 8))))
   (dotimes (j h)
     (dotimes (i w)
       (when (oddp i)
	 (setf (aref a j i) #xff))))
   (write-bitplane a)))

#+nil
(progn ;; write white image
  (let* ((a (make-array '(1024 160)
			:element-type '(unsigned-byte 8)
			:initial-element #xff)))
    (write-bitplane a)))

#+nil
(erase-bitplane 0)
#+nil
(time 
 (let* ((h 1024)
	(w 1280)
	(a 
	 (make-array (list h w)
		     :element-type '(unsigned-byte 8))))
   (dotimes (i w)
     (dotimes (j h)
       (let ((r (sqrt (+ (expt (- i (floor w 2)) 2)
			 (expt (- j (floor h 2)) 2)))))
	 (when (< r 400)
	   (setf (aref a j i) 1)))))
   (write-bitplane (create-bitplane a)
		   :image-number 0)))
;; after uploading a bitplane, issue reload-repertoir rpc call
#+nil
(progn
 (progn ;;deactivate
   (forthdd-talk #x28))
 (progn ;; reload repertoir
   (forthdd-talk #x29))
 (progn ;;activate
   (forthdd-talk #x27))
 (progn ;; switch image/running order
   (forthdd-talk #x23 '(0))))

#+nil
(progn ;;deactivate
  (forthdd-talk #x28))
#+nil
(progn ;; reload repertoir
  (forthdd-talk #x29))
#+nil
(progn ;;activate
  (forthdd-talk #x27))
#+nil
(dotimes (i 103)
  (sleep .3)
 (progn ;; switch image/running order
   (forthdd-talk #x23 (list i))))

#+nil
(progn ;; switch image/running order
  (forthdd-talk #x23 '(19)))
#+nil
(progn ;; switch image/running order
  (forthdd-talk #x23 '(0)))
