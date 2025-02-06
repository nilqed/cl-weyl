(in-package "COMMON-LISP-USER")


(defsystem gfx ()
  #+Lucid  new-windows ;system definition for xox-windows
  algebra-utils
  geom-utils
  transforms
  #+lucid basic-graphics
  views
  ;;objects
  rendering
  #+APPLE drawmac
  init
  )

(export '(object primitive-object assembly scene camera light view
	  display viewport  initialize-graphics update-display
	  set-point-style set-point-diameter set-line-width
	  translation-matrix scale-matrix rotation-matrix
	  make-primitive-object make-assembly set-object-visibility
	  set-object-colors insert-object remove-object
	  transform-object simplify-object draw flash display
	  make-scene set-scene-objects set-scene-space clear-scene
	  create-display move-display resize-display
	  set-display-colors show-display hide-display destroy-display
	  make-light set-light-color set-light-direction make-camera
	  set-camera-position set-camera-target set-camera-lens
	  set-camera-orientation zoom-camera pan-camera roll-camera
	  orbit-camera make-view set-scene set-camera insert-light
	  remove-light make-viewport set-viewport-rect insert-viewport
	  remove-viewport))






