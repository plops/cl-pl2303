
# cl-pl2303

A Common Lisp library providing CFFI bindings to libusb0 and control code for the PL2303 USB-to-serial converter chip.

## Overview

This library enables direct communication with PL2303-based USB-to-serial adapters from Common Lisp without relying on kernel drivers. It was developed by reverse-engineering the Linux kernel module to understand the chip's initialization sequence and communication protocol.  

## Features

- **Complete libusb0 CFFI bindings** - Low-level USB device access through libusb0
- **PL2303 chip support** - Initialization and configuration for PL2303 USB-to-serial converters
- **Serial port configuration** - Set baud rate, data bits, stop bits, and parity
- **Control line management** - DTR and RTS control
- **Bulk transfers** - Read and write data through bulk endpoints
- **Device enumeration** - Find USB devices by vendor and product ID  

## Supported Devices

The library has been tested with PL2303 USB-to-serial adapters:
- **Vendor ID**: `0x067b` (Prolific Technology, Inc.)
- **Product ID**: `0x2303` (PL2303 Serial Port)  

## Requirements

- Common Lisp implementation with CFFI support
- libusb0 library installed on your system
  - Linux: `libusb` (version 0.1.x)
  - Windows: libusb-win32

## Installation

1. Ensure libusb0 is installed on your system
2. Clone this repository
3. Load the library in your Lisp environment:

```lisp
(require :cffi)
(load "libusb0.lisp")
```

## Usage

### Basic Connection

```lisp
;; Initialize and find PL2303 device
(libusb0:ensure-libusb0-initialized)

;; Create connection to PL2303
(defparameter *serial*
  (make-instance 'libusb0:usb-connection
                 :vendor-id #x067b
                 :product-id #x2303
                 :configuration 1
                 :interface 0))

;; Initialize the PL2303 chip
(libusb0:prepare-zeiss *serial*)
```  

### Serial Communication

```lisp
;; Write data to serial port
(let* ((message "Hello, Serial!")
       (data (make-array (length message)
                         :element-type '(unsigned-byte 8)
                         :initial-contents (map 'list #'char-code message))))
  (libusb0:bulk-write *serial* data :endpoint 2))

;; Read data from serial port
(let ((response (libusb0:bulk-read *serial* 64 :endpoint #x83)))
  (map 'string #'code-char response))
```  

### Serial Port Configuration

The library supports various baud rates and serial configurations:

```lisp
;; Set line parameters (baud rate, stop bits, parity, data bits)
(libusb0:set-line *serial*)

;; Control DTR and RTS lines
(libusb0:set-control-lines *serial* value)
```  

Supported baud rates include: 75, 150, 300, 600, 1200, 1800, 2400, 3600, 4800, 7200, 9600, 14400, 19200, 28800, 38400, 57600, 115200, 230400, 460800, 614400, 921600, 1228800, 2457600, 3000000, and 6000000 baud.  

### Closing Connection

```lisp
(libusb0:close-connection *serial*)
```  

## Permissions (Linux)

On Linux systems, you may need to configure udev rules to allow non-root access to USB devices:

Create `/etc/udev/rules.d/99-pl2303.rules`:
```
SYSFS{idVendor}=="067b", MODE="0666"
```  

## Technical Details

### PL2303 Initialization Sequence

The library implements the proper initialization sequence for PL2303 chips (particularly the HX variant) as documented in the Linux kernel driver. This includes:

1. Chip type detection based on `bMaxPacketSize0`
2. Vendor read/write commands for initialization
3. Line configuration setup
4. Control line initialization  

### GPIO Support

The PL2303HX chip includes GPIO pins that can be controlled through vendor-specific commands. The documentation includes information about GPIO control register bits and access methods.  

## Additional Features

The library also includes support for a ForthDD device controller, which extends the functionality beyond basic serial communication to include flash memory operations and advanced device control.  

## Documentation Files

- `pl2303.org` - Detailed reverse-engineering notes on PL2303 protocol
- `pl2303-black.lsusb` and `pl2303-blue.lsusb` - USB device descriptors for reference

## License

GPL 

## Author

This library was created by reverse-engineering the Linux PL2303 kernel module to enable direct USB communication with PL2303 chips from Common Lisp.



---

**Note**: This is a low-level library that bypasses the operating system's serial port drivers. Make sure to understand the implications of direct USB access before using it in production environments.
