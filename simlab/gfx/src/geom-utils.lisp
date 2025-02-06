;;;   -*- Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10; Mode: LISP -*-

(in-package "COMMON-LISP-USER")

;;; ***********************************************************************
;;; GEOMETRIC UTILITIES USED BY GFX, NON-GEOM VERSION
;;; ***********************************************************************

;;; Rewrite of the geometric utilities for GFX, not using the GEOMETRY
;;; package.  This file contains vertex, cell, and geom classes and
;;; macros for accessing information about these objects.  Should
;;; be equivalent in functionality to the functions in GEOMETRY.

;;; The following classes must be defined:
;;;    VERTEX
;;;    GEOM
;;;    CELL
;;; The macros necessary are:
;;;    GEOM-SPACE-OF
;;;    GEOM-SPACE-DIMENSION
;;;    GEOM-EDGES-OF
;;;    GEOM-VERTS-OF
;;;    GEOM-VERT-COORDS

(defmacro geom-space-of (geom)
  `(max-dimension-of ,geom))

(defmacro geom-space-dimension (geom)
  geom)


(defmacro geom-faces-of (geom)
  `(2-skel ,geom))

(defmacro geom-edges-of (geom)
  `(1-skel ,geom))

(defmethod geom-verts-of ((geom t))
  (verts-of geom))

(defmethod geom-verts-of ((l list))
  l)

(defmacro geom-vert-coords (geom)
  geom)

(defmacro coord (vert i)
  `(nth ,i ,vert))
