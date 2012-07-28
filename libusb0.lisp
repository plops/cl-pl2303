(eval-when (:compile-toplevel)
 (require :cffi))
(defpackage :libusb0
  (:use :cl :cffi)
  (:export
   :get-vendor-id
   :get-product-id
   :get-devices-by-ids
   :ensure-libusb0-initialized))

(in-package :libusb0)

(define-foreign-library libusb0
  (:windows "C:/Users/martin/Downloads/libusb-win32-bin-1.2.6.0/libusb-win32-bin-1.2.6.0/bin/amd64/libusb0.dll")
  (t (:default "libusb")))
 
(defcfun "usb_init" :void)
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
(defparameter *bla*
  (make-instance 'usb-connection
		 :vendor-id #x067b
		 :product-id #x2303
		 :configuration 1
		 :interface 0
		 ;:endpoint #x83 ; 2 #x81 #x83qu
		 ))
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

(let* ((str "ZA1000;")
       (a (make-array (length str)
		      :element-type '(unsigned-byte 8)
		      :initial-contents (map 'list #'char-code str))))
  (bulk-write *bla* a :endpoint 2))

(let* ((str "Zi;")
       (a (make-array (length str)
		      :element-type '(unsigned-byte 8)
		      :initial-contents (map 'list #'char-code str))))
  (bulk-write *bla* a :endpoint 2))

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

#+nil
(set-line *bla*)

#+nil
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
       ('r (pl2303-vendor-read *bla* f g))
       ('w (pl2303-vendor-write *bla* f g))))

#+nil
(pl2303-vendor-write *bla* 0 #x61) ;; crtscts

(defmethod set-control-lines ((c usb-connection) value)
  (declare (type (unsigned-byte 8) value))
  (control-msg c +set-control-request-type+ +set-control-request+ :value value))

#+nil
(loop for i below 100001 do
     (set-control-lines *bla* (if (evenp i) #x00 #xff)))

