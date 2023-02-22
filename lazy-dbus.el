;;; lazy-dbus.el --- DBUS communications module for lazy

;; Copyright (C) 2011-2017 Andreas Raster <lazor at affenbande dot org>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'lazy)

(require 'cl-lib)
(require 'dbus)

(defvar lazy-dbus-node "LazyEl")
(defvar lazy-dbus-path (concat "/" lazy-dbus-node))
(defvar lazy-dbus-interface "org.gnu.Emacs.LazyEl")

(defun lazy-dbus-service-node-names (&optional service)
  ;; alle nodenames zusammen suchen um in zukunft keine
  ;; probleme zu kriegen mit zeug das emacs definiert
  (concat "\n" "<node name='" lazy-dbus-node "'></node>"))

(defun lazy-dbus-root-introspect ()
  (concat "<node name='/'>
  <interface name='org.freedesktop.DBus.Introspectable'>
  <method name='Introspect'>
  <arg name='xml_data' type='s' direction='out'/>
  </method>
  </interface>"
  (lazy-dbus-service-node-names)
  "</node>"))

(defun lazy-dbus-lazy-introspect ()
  (concat "<node name='" lazy-dbus-path "'>
  <interface name='org.freedesktop.DBus.Introspectable'>
  <method name='Introspect'>
  <arg name='xml_data' type='s' direction='out'/>
  </method>
  </interface>
  <interface name='" lazy-dbus-interface "'>
  <method name='CurrentProject'>
  <arg name='' direction='out' type='s' />
  </method>
  <method name='ProjectNames'>
  <arg name='' direction='out' type='as' />
  </method>
  <method name='ProjectLoad'>
  <arg name='name' direction='in' type='s' />
  <arg name='' direction='out' type='b' />
  </method>
  <method name='ProjectUnload'>
  <arg name='' direction='out' type='b' />
  </method>
  </interface>
  </node>"))

(defvar lazy-dbus-root-introspect-object (dbus-register-method
                                        :session
                                        dbus-service-emacs
                                        "/"
                                        dbus-interface-introspectable
                                        "Introspect"
                                        'lazy-dbus-root-introspect))

(defvar lazy-dbus-lazy-introspect-object (dbus-register-method
                                             :session
                                             dbus-service-emacs
                                             lazy-dbus-path
                                             dbus-interface-introspectable
                                             "Introspect"
                                             'lazy-dbus-lazy-introspect))

(defun encode-umlauts (string)
  (apply #'concat (mapcar (lambda (c)
                            (cond ((eq c ?ü)
                                   "&uuml;")
                                  ((eq c ?ä)
                                   "&auml;")
                                  ((eq c ?ö)
                                   "&ouml;")
                                  ((eq c ?Ü)
                                   "&Uuml;")
                                  ((eq c ?Ä)
                                   "&Auml;")
                                  ((eq c ?Ö)
                                   "&Ouml;")
                                  (t (string c))))
                          string)))

(defun decode-umlauts (string)
  (let ((case-fold-search nil))
    (replace-regexp-in-string
     "&uuml;" "ü"
     (replace-regexp-in-string
      "&ouml;" "ö"
      (replace-regexp-in-string
       "&auml;" "ä"
       (replace-regexp-in-string
        "&Uuml;" "Ü"
        (replace-regexp-in-string
         "&Ouml;" "Ö"
         (replace-regexp-in-string
          "&Auml;" "Ä"
          string))))))))

(defun lazy-dbus-current-project ()
  (encode-umlauts (or lazy-name "")))

(defun lazy-dbus-project-names ()
  (mapcar #'encode-umlauts (lazy-project-names)))

(cl-defun lazy-dbus-project-load (name)
  (condition-case nil
      (lazy-load name)
    (error (cl-return-from "lazy-dbus-project-load" (list :boolean nil))))
  (list :boolean t))

(defun lazy-dbus-project-unload ()
  (if lazy-name
      (progn
       (lazy-unload)
       (list :boolean t))
    (list :boolean nil)))

(with-eval-after-load 'lazy-dbus
  (progn
       (dbus-register-method :session dbus-service-emacs lazy-dbus-path lazy-dbus-interface "CurrentProject" 'lazy-dbus-current-project)
       (dbus-register-method :session dbus-service-emacs lazy-dbus-path lazy-dbus-interface "ProjectNames" 'lazy-dbus-project-names)
       (dbus-register-method :session dbus-service-emacs lazy-dbus-path lazy-dbus-interface "ProjectLoad" 'lazy-dbus-project-load)
       (dbus-register-method :session dbus-service-emacs lazy-dbus-path lazy-dbus-interface "ProjectUnload" 'lazy-dbus-project-unload)))
