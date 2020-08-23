
(define screen-width 640)
(define screen-height 480)

;; ### Rendering ################################################################

;; define default vertex shader
(define default-vertex-code
#<<END
#version 330 core

layout(location = 0) in vec2 position;
layout(location = 1) in vec3 color;
layout(location = 2) in vec2 coords;

out vec3 c;
out vec2 fragCoords;

uniform mat4 MVP;

void main(){
   gl_Position = MVP * vec4(position, 0.0, 1.0);
   c = color;
   fragCoords = coords;
}
END
)

;; define default fragment shader
(define default-fragment-code
#<<END
#version 330 core

in vec3 c;
in vec2 fragCoords;

uniform sampler2D tex;

void main(){
  gl_FragColor = texture(tex, fragCoords);
}
END
)

(define default-vertex   #f)
(define default-fragment #f)

(define current-program (make-parameter #f))

(define (compile-shaders)
  (set! default-vertex   (make-shader gl:+vertex-shader+ default-vertex-code))
  (set! default-fragment (make-shader gl:+fragment-shader+ default-fragment-code))
  ;; setup default program
  (current-program (make-program (list default-vertex default-fragment))))

(define default-texture #f)

(define (load-texure name)
  (let* [(texture (soil:load-ogl-texture name
                                         soil:force-channels/rgba
                                         soil:texture-id/create-new-id
                                         (bitwise-ior soil:texture/power-of-two
                                                      soil:texture/multiply-alpha)))]
    (gl:bind-texture gl:+texture-2d+ texture)
    ;; add NN filtering
    (gl:tex-parameteri gl:+texture-2d+ gl:+texture-min-filter+ gl:+nearest+)
    (gl:tex-parameteri gl:+texture-2d+ gl:+texture-mag-filter+ gl:+nearest+)
    (print "Loaded " name ".")
    texture))


(define rect
  (make-mesh
   vertices: '(attributes: ((position #:float 2)
                            (color #:unsigned-byte 3 normalized: #t)
                            (coords #:float 2))

               initial-elements: ((position . (-0.5 -0.5
                                                0.5 -0.5
                                                0.5  0.5
                                               -0.5  0.5))
                                  (color . (255 0   0
                                            0   255 0
                                            0   0   255
                                            255 0   255))
                                  (coords . (0 1
                                             1 1
                                             1 0
                                             0 0))))
   indices: '(type: #:ushort
              initial-elements: (0 1 2
                                 0 2 3))))

(define (min-unit-viewer min-units)
  (if (> screen-height screen-width)
      ;; tall window
      (ortho min-units
             (truncate (/ (* screen-height min-units) screen-width)) 0.1 100)
      ;; wide window
      (ortho (truncate (/ (* screen-width min-units) screen-height))
             min-units -20 20)
      ))

(define projection-matrix
  (min-unit-viewer 20))

(define view-matrix (mat4-identity))

(define (set-camera-pos x y)
  (set! view-matrix
    (look-at (make-point x y 10)
             (make-point x y 0)
             (make-point 0 1 0))))

(define (camera-left-bound)
  (let* [(combined (m* projection-matrix view-matrix))
         (inversed (inverse combined))
         (left (m* inversed (make-point -1 0 0)))]
    (point-x left)))

(define (camera-right-bound)
  (let* [(combined (m* projection-matrix view-matrix))
         (inversed (inverse combined))
         (right (m* inversed (make-point 1 0 0)))]
    (point-x right)))

(set-camera-pos 0 10)

(define model-matrix (mat4-identity))

(define (render tex x y w h)
  ;; bind texture
  (gl:active-texture gl:+texture0+)
  (gl:bind-texture gl:+texture-2d+ tex)

  (gl:enable gl:+blend+)
  (gl:blend-func gl:+src-alpha+ gl:+one-minus-src-alpha+)

  (gl:use-program (current-program))
  ;; rotate model a little
  (rotate-y 0.03 model-matrix)
  (let [(framed (2d-scaling (* 2 w) (* 2 h)))]
    (translate (make-point x y 0) framed)
    ;; update mvp matrix
    (gl:uniform-matrix4fv (gl:get-uniform-location (current-program) "MVP")
                          1 #f
                          (m* projection-matrix (m* view-matrix framed))))

  (gl:uniform1i (gl:get-uniform-location (current-program) "tex") 0)

  ;; render rect
  (gl:bind-vertex-array (mesh-vao rect))
  (gl:draw-elements (mode->gl (mesh-mode rect))
                    (mesh-n-indices rect)
                    (type->gl (mesh-index-type rect))
                    #f)

  (check-error)
  (gl:bind-vertex-array 0))
