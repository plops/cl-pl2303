#+nil
(require :cffi)
(defpackage :libusb0
  (:use :cl :cffi))

(in-package :libusb0)

(define-foreign-library libusb0
  (t (:default "libusb")))

(use-foreign-library libusb0)

(defcfun "usb_init" :void)
(defcfun "usb_find_busses" :int)
(defcfun "usb_find_devices" :int)

;; struct usb_device_descriptor {
;;         u_int8_t  bLength;
;;         u_int8_t  bDescriptorType;
;;         u_int16_t bcdUSB;
;;         u_int8_t  bDeviceClass;
;;         u_int8_t  bDeviceSubClass;
;;         u_int8_t  bDeviceProtocol;
;;         u_int8_t  bMaxPacketSize0;
;;         u_int16_t idVendor;
;;         u_int16_t idProduct;
;;         u_int16_t bcdDevice;
;;         u_int8_t  iManufacturer;
;;         u_int8_t  iProduct;
;;         u_int8_t  iSerialNumber;
;;         u_int8_t  bNumConfigurations;
;; };

(defctype :u_int8_t :uint8)
(defctype :u_int16_t :uint16)
(defctype :u_int32_t :uint32)
(defcstruct :usb_device_descriptor
  (bLength :u_int8_t)  
  (bDescriptorType :u_int8_t)  
  (bcdUSB :u_int16_t) 
  (bDeviceClass :u_int8_t)  
  (bDeviceSubClass :u_int8_t)  
  (bDeviceProtocol :u_int8_t)  
  (bMaxPacketSize0 :u_int8_t)  
  (idVendor :u_int16_t) 
  (idProduct :u_int16_t) 
  (bcdDevice :u_int16_t) 
  (iManufacturer :u_int8_t)  
  (iProduct :u_int8_t)  
  (iSerialNumber :u_int8_t)  
  (bNumConfigurations :u_int8_t))

;; struct usb_device {
;;   struct usb_device *next, *prev;
;;   char filename[PATH_MAX + 1];
;;   struct usb_bus *bus;
;;   struct usb_device_descriptor descriptor;
;;   struct usb_config_descriptor *config;
;;   void *dev;            /* Darwin support */
;;   u_int8_t devnum;
;;   unsigned char num_children;
;;   struct usb_device **children;
;; };

;;find /usr/include/|grep -ir limits|xargs fgrep PATH_MAX
(defconstant +path-max+ 4096)

(defcstruct :usb_device
  (next (:pointer :usb_device_descriptor))
  (prev (:pointer :usb_device_descriptor))
  (filename :char :count #.(1+ +path-max+))
  (bus :pointer)
  (descriptor :usb_device_descriptor)
  (config :pointer)
  (dev (:pointer :void))
  (devnum :u_int8_t)
  (num-children :unsigned-char)
  (children (:pointer :usb_device)))

;; struct usb_bus {
;;   struct usb_bus *next, *prev;
;;   char dirname[PATH_MAX + 1];
;;   struct usb_device *devices;
;;   u_int32_t location;
;;   struct usb_device *root_dev;
;; };

(defcstruct :usb_bus
  (next (:pointer :usb_bus))
  (prev (:pointer :usb_bus))
  (dirname :char :count #.(1+ +path-max+))
  (devices (:pointer :usb_device))
  (location :u_int32_t)
  (root-dev (:pointer :usb_device)))

(usb-init)
(usb-find-busses)
(usb-find-devices)


