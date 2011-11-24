;;; mk-project-dbus.el --- DBUS communications module for mk-project

;; Copyright (C) 2010 Andreas Raster <lazor at affenbande dot org>
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

(require 'dbus)

(defvar mk-dbus-node "MkProject")
(defvar mk-dbus-path (concat "/" mk-dbus-node))
(defvar mk-dbus-interface "org.gnu.Emacs.MkProject")

(defun mk-dbus-service-node-names (&optional service)
  ;; alle nodenames zusammen suchen um in zukunft keine
  ;; probleme zu kriegen mit zeug das emacs definiert
  (concat "\n" "<node name='" mk-dbus-node "'></node>"))

(defun mk-dbus-root-introspect ()
  (concat "<node name='/'>
  <interface name='org.freedesktop.DBus.Introspectable'>
  <method name='Introspect'>
  <arg name='xml_data' type='s' direction='out'/>
  </method>
  </interface>"
  (mk-dbus-service-node-names)
  "</node>"))

(defun mk-dbus-mkproject-introspect ()
  (concat "<node name='" mk-dbus-path "'>
  <interface name='org.freedesktop.DBus.Introspectable'>
  <method name='Introspect'>
  <arg name='xml_data' type='s' direction='out'/>
  </method>
  </interface>
  <interface name='" mk-dbus-interface "'>
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

(defvar mk-dbus-root-introspect-object (dbus-register-method
                                        :session
                                        dbus-service-emacs
                                        "/"
                                        dbus-interface-introspectable
                                        "Introspect"
                                        'mk-dbus-root-introspect))

(defvar mk-dbus-mkproject-introspect-object (dbus-register-method
                                             :session
                                             dbus-service-emacs
                                             mk-dbus-path
                                             dbus-interface-introspectable
                                             "Introspect"
                                             'mk-dbus-mkproject-introspect))

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

(defun mk-dbus-current-project ()
  (encode-umlauts (or mk-proj-name "")))

(defun mk-dbus-project-names ()
  (mapcar #'encode-umlauts (mk-proj-names)))

(defun* mk-dbus-project-load (name)
  (condition-case nil
      (mk-proj-load name)
    (error (return-from "mk-dbus-project-load" (list :boolean nil))))
  (list :boolean t))

(defun mk-dbus-project-unload ()
  (if mk-proj-name
      (progn
       (project-unload)
       (list :boolean t))
    (list :boolean nil)))

(eval-after-load
    '(progn
       (dbus-register-method :session dbus-service-emacs mk-dbus-path mk-dbus-interface "CurrentProject" 'mk-dbus-current-project)
       (dbus-register-method :session dbus-service-emacs mk-dbus-path mk-dbus-interface "ProjectNames" 'mk-dbus-project-names)
       (dbus-register-method :session dbus-service-emacs mk-dbus-path mk-dbus-interface "ProjectLoad" 'mk-dbus-project-load)
       (dbus-register-method :session dbus-service-emacs mk-dbus-path mk-dbus-interface "ProjectUnload" 'mk-dbus-project-unload)))
