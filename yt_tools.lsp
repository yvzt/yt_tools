;--------------------------------------------
; YT Straight Arrow (shaft + wedge arrowhead)
;--------------------------------------------

(vl-load-com)

; Persistent defaults (kept across calls)
(if (not *yt_arrow_len*) (setq *yt_arrow_len* 7.0))
(if (not *yt_arrow_w*)   (setq *yt_arrow_w* 2.0))

(defun yt:vsub (a b) (mapcar '- a b))
(defun yt:vadd (a b) (mapcar '+ a b))
(defun yt:vmul (s v) (mapcar '(lambda (x) (* s x)) v))

(defun yt:vlen (v)
  (sqrt (+ (* (car v) (car v)) (* (cadr v) (cadr v))))
)

(defun yt:vunit (v / L)
  (setq L (yt:vlen v))
  (if (> L 1e-9)
    (yt:vmul (/ 1.0 L) v)
    (list 0.0 0.0 0.0)
  )
)

(defun yt:dist (a b)
  (yt:vlen (yt:vsub b a))
)

(defun yt:mkline (p1 p2)
  (entmakex
    (list
      (cons 0 "LINE")
      (cons 10 p1)
      (cons 11 p2)
    )
  )
)

; Make a 2-vertex polyline wedge: base -> tip
; Start width = 2*W, End width = 0
(defun yt:mkwedge (base tip w)
  (entmakex
    (append
      (list
        (cons 0 "LWPOLYLINE")
        (cons 100 "AcDbEntity")
        (cons 100 "AcDbPolyline")
        (cons 90 2)          ; number of vertices
        (cons 70 0)          ; open polyline
      )
      (list
        (cons 10 (list (car base) (cadr base)))
        (cons 40 (* 2.0 w))  ; start width at base
        (cons 41 0.0)        ; end width at base (segment end width handled at next vertex)
      )
      (list
        (cons 10 (list (car tip) (cadr tip)))
        (cons 40 0.0)        ; start width at tip
        (cons 41 0.0)        ; end width at tip
      )
    )
  )
)

(defun yt:mkwedge-tip (tip base w)
  ; Wedge arrowhead as a 2-vertex LWPOLYLINE: tip -> base
  ; Tip width = 0, base width = 2*w
  (entmakex
    (list
      (cons 0 "LWPOLYLINE")
      (cons 100 "AcDbEntity")
      (cons 100 "AcDbPolyline")
      (cons 90 2)
      (cons 70 0)

      ; Vertex 1: TIP (narrow)
      (cons 10 (list (car tip) (cadr tip)))
      (cons 40 0.0)
      (cons 41 (* 2.0 w))

      ; Vertex 2: BASE (wide)
      (cons 10 (list (car base) (cadr base)))
      (cons 40 (* 2.0 w))
      (cons 41 0.0)
    )
  )
)


(defun yt:getopts (/ kw val)
  (while
    (progn
      (initget "Length Width Done")
      (setq kw (getkword
        (strcat
          "\nOptions [Length/Width/Done]"
          " <Done>: "
        )
      ))
      (cond
        ((or (not kw) (= kw "Done"))
          nil
        )
        ((= kw "Length")
          (setq val (getreal (strcat "\nArrow length <" (rtos *yt_arrow_len* 2 3) ">: ")))
          (if val (setq *yt_arrow_len* val))
          T
        )
        ((= kw "Width")
          (setq val (getreal (strcat "\nArrow width <" (rtos *yt_arrow_w* 2 3) ">: ")))
          (if val (setq *yt_arrow_w* val))
          T
        )
        (T T)
      )
    )
  )
)

(defun c:YTARROW (/ p1 p2 d u L W headBase tailBase bothAns bothFlag shaftStart shaftEnd dist12)
  (yt:getopts)

  (setq p1 (getpoint "\nPick first point (tail if single-head): "))
  (if (not p1) (progn (princ "\nCancelled.") (princ) (exit)))

  (setq p2 (getpoint p1 "\nPick second point (head): "))
  (if (not p2) (progn (princ "\nCancelled.") (princ) (exit)))

  (setq dist12 (yt:dist p1 p2))
  (if (< dist12 1e-9)
    (progn
      (princ "\nPoints are identical. Cancelled.")
      (princ)
      (exit)
    )
  )

  (setq L *yt_arrow_len*)
  (setq W *yt_arrow_w*)

  ; Ask both ends?
  (initget "Yes No")
  (setq bothAns (getkword "\nBoth-end arrow? [Yes/No] <No>: "))
  (setq bothFlag (= bothAns "Yes"))

  (setq d (yt:vsub p2 p1))
  (setq u (yt:vunit d))

  (if bothFlag
    (progn
      ; Ensure the two arrowheads do not overlap
      (if (<= dist12 (* 2.0 L))
        (progn
          (setq L (/ dist12 2.0))
          (princ (strcat "\nNote: Distance too short; arrow length reduced to " (rtos L 2 3) "."))
        )
      )

      (setq tailBase (yt:vadd p1 (yt:vmul L u)))
      (setq headBase (yt:vsub p2 (yt:vmul L u)))

      ; Shaft
      (yt:mkline tailBase headBase)

      ; Arrowheads as wedges
      (yt:mkwedge headBase p2 W)   ; head
      (yt:mkwedge tailBase p1 W)   ; tail (base->tip towards p1)
    )
    (progn
      ; Single-head (p2 is head)
      (if (<= dist12 L)
        (progn
          (setq L (* 0.9 dist12))
          (princ (strcat "\nNote: Distance too short; arrow length reduced to " (rtos L 2 3) "."))
        )
      )

      (setq headBase (yt:vsub p2 (yt:vmul L u)))

      ; Shaft
      (yt:mkline p1 headBase)

      ; Arrowhead
      (yt:mkwedge headBase p2 W)
    )
  )

  (princ "\nDone.")
  (princ)
)



(vl-load-com)

; Persistent defaults
(if (not *yt_n_head_len*) (setq *yt_n_head_len* 7.0))
(if (not *yt_n_head_w*)   (setq *yt_n_head_w* 2.0))
(if (not *yt_n_reach*)    (setq *yt_n_reach* 50.0))

(defun yt:vadd (a b) (mapcar '+ a b))
(defun yt:vsub (a b) (mapcar '- a b))
(defun yt:vmul (s v) (mapcar '(lambda (x) (* s x)) v))

(defun yt:dot (a b)
  (+ (* (car a) (car b)) (* (cadr a) (cadr b)) (* (caddr a) (caddr b)))
)

(defun yt:vlen (v)
  (sqrt (+ (* (car v) (car v)) (* (cadr v) (cadr v)) (* (caddr v) (caddr v))))
)

(defun yt:vunit (v / L)
  (setq L (yt:vlen v))
  (if (> L 1e-9) (yt:vmul (/ 1.0 L) v) (list 0.0 0.0 0.0))
)

(defun yt:rot90 (v)
  (list (- (cadr v)) (car v) 0.0)
)

(defun yt:mkline (p1 p2)
  (entmakex (list (cons 0 "LINE") (cons 10 p1) (cons 11 p2)))
)

; Wedge arrowhead as a 2-vertex LWPOLYLINE: base -> tip
; Start width at base = 2*W, end width at tip = 0
(defun yt:mkwedge (base tip w)
  (entmakex
    (list
      (cons 0 "LWPOLYLINE")
      (cons 100 "AcDbEntity")
      (cons 100 "AcDbPolyline")
      (cons 90 2)
      (cons 70 0)
      (cons 10 (list (car base) (cadr base)))
      (cons 40 (* 2.0 w))
      (cons 41 0.0)
      (cons 10 (list (car tip) (cadr tip)))
      (cons 40 0.0)
      (cons 41 0.0)
    )
  )
)

(defun yt:getopts-normal (/ kw val)
  (while
    (progn
      (initget "HeadLen Width Reach Done")
      (setq kw
        (getkword
          (strcat
            "\nOptions [HeadLen/Width/Reach/Done] <Done>: "
          )
        )
      )
      (cond
        ((or (not kw) (= kw "Done"))
          nil
        )
        ((= kw "HeadLen")
          (setq val (getreal (strcat "\nArrowhead length <" (rtos *yt_n_head_len* 2 3) ">: ")))
          (if val (setq *yt_n_head_len* val))
          T
        )
        ((= kw "Width")
          (setq val (getreal (strcat "\nArrow width <" (rtos *yt_n_head_w* 2 3) ">: ")))
          (if val (setq *yt_n_head_w* val))
          T
        )
        ((= kw "Reach")
          (setq val (getreal (strcat "\nArrow reach from curve <" (rtos *yt_n_reach* 2 3) ">: ")))
          (if val (setq *yt_n_reach* val))
          T
        )
        (T T)
      )
    )
  )
)

(defun c:YTARROWN (/ ent pickU pick p0 param deriv tan n0 n L W reach
                     bothAns both tipA tipB baseA baseB)
  (yt:getopts-normal)

  (setq ent (car (entsel "\nSelect curve (line/arc/polyline/spline): ")))
  (if (not ent) (progn (princ "\nCancelled.") (princ) (exit)))

  (setq pickU (getpoint "\nPick point near curve (sets side): "))
  (if (not pickU) (progn (princ "\nCancelled.") (princ) (exit)))

  ; WCS for vlax-curve
  (setq pick (trans pickU 1 0))

  ; Closest point ON curve
  (setq p0 (vlax-curve-getClosestPointTo ent pick))

  ; Tangent via first derivative
  (setq param (vlax-curve-getParamAtPoint ent p0))
  (setq deriv (vlax-curve-getFirstDeriv ent param))
  (setq tan   (yt:vunit deriv))

  (if (< (yt:vlen tan) 1e-9)
    (progn
      (princ "\nCould not compute tangent at this location.")
      (princ)
      (exit)
    )
  )

  ; Normal candidates
  (setq n0 (yt:rot90 tan))

  ; Choose normal direction toward user's side click
  (if (< (yt:dot n0 (yt:vsub pick p0)) 0.0)
    (setq n (yt:vmul -1.0 n0))
    (setq n n0)
  )
  (setq n (yt:vunit n))

  ; Parameters
  (setq L *yt_n_head_len*)
  (setq W *yt_n_head_w*)
  (setq reach *yt_n_reach*)

  ; Keep head shorter than reach
  (if (> L (* 0.95 reach))
    (setq L (* 0.95 reach))
  )

  ; Both-end?
  (initget "Yes No")
  (setq bothAns (getkword "\nBoth-end arrow? [Yes/No] <No>: "))
  (setq both (= bothAns "Yes"))

  (if both
    (progn
      ; Tips
      (setq tipA (yt:vadd p0 (yt:vmul reach n)))
      (setq tipB (yt:vsub p0 (yt:vmul reach n)))

      ; Bases (shorten shaft)
      (setq baseA (yt:vsub tipA (yt:vmul L n)))
      (setq baseB (yt:vadd tipB (yt:vmul L n)))

      ; Shaft
      (yt:mkline baseB baseA)

      ; Heads
      (yt:mkwedge baseA tipA W)
      (yt:mkwedge baseB tipB W)
    )
    (progn
      ; Single-head: from curve point outward
      (setq tipA  (yt:vadd p0 (yt:vmul reach n)))
      (setq baseA (yt:vsub tipA (yt:vmul L n)))

      ; Shaft
      (yt:mkline p0 baseA)

      ; Head
      (yt:mkwedge baseA tipA W)
    )
  )

  (princ "\nDone.")
  (princ)
)


;============================================================
; YTDIMA — aligned-dimension-like geometry (NO TEXT)
; - Picks: p1, p2, then a point to define offset side/amount
; - Draws: two extension lines + one dimension line + 2 arrowheads
; - Arrowheads are wedges (will later be replaced by ticks)
;============================================================

; Persistent defaults for YTDIMA
(if (not *yt_dima_head_len*) (setq *yt_dima_head_len* 7.0))
(if (not *yt_dima_head_w*)   (setq *yt_dima_head_w* 2.0))

(defun yt:dima-getopts (/ kw val)
  (while
    (progn
      (initget "HeadLen Width Done")
      (setq kw (getkword "\nOptions [HeadLen/Width/Done] <Done>: "))
      (cond
        ((or (not kw) (= kw "Done"))
          nil)
        ((= kw "HeadLen")
          (setq val (getreal (strcat "\nArrowhead length <" (rtos *yt_dima_head_len* 2 3) ">: ")))
          (if (and val (> val 1e-9)) (setq *yt_dima_head_len* val))
          T)
        ((= kw "Width")
          (setq val (getreal (strcat "\nArrow width <" (rtos *yt_dima_head_w* 2 3) ">: ")))
          (if (and val (> val 1e-9)) (setq *yt_dima_head_w* val))
          T)
        (T T)))
  )
)

(defun c:YTDIMA (/ p1u p2u p3u p1 p2 p3 d u n off p1d p2d
                 L W span halfTick
                 tipL tipR baseL baseR
                 tickL1 tickL2 tickR1 tickR2)
  ; Requires existing helpers:
  ; yt:vsub, yt:vadd, yt:vmul, yt:vunit, yt:rot90, yt:dot, yt:mkline

  (yt:dima-getopts)

  (setq p1u (getpoint "\nPick extension point 1: "))
  (if (not p1u) (progn (princ "\nCancelled.") (princ) (exit)))

  (setq p2u (getpoint p1u "\nPick extension point 2: "))
  (if (not p2u) (progn (princ "\nCancelled.") (princ) (exit)))

  (setq p3u (getpoint "\nPick dimension line location (offset side): "))
  (if (not p3u) (progn (princ "\nCancelled.") (princ) (exit)))

  ; Work in WCS (UCS->WCS)
  (setq p1 (trans p1u 1 0))
  (setq p2 (trans p2u 1 0))
  (setq p3 (trans p3u 1 0))

  (setq span (distance p1 p2))
  (if (< span 1e-9)
    (progn (princ "\nPoints are identical. Cancelled.") (princ) (exit)))

  ; Along measured direction
  (setq d (yt:vsub p2 p1))
  (setq u (yt:vunit d))

  ; Normal for offset
  (setq n (yt:rot90 u))
  (if (< (yt:dot n (yt:vsub p3 p1)) 0.0)
    (setq n (yt:vmul -1.0 n))
  )

  ; Signed perpendicular offset (distance from p1 to picked dim-line location)
  (setq off (yt:dot (yt:vsub p3 p1) n))

  ; Dimension line endpoints (tips) at offset
  (setq p1d (yt:vadd p1 (yt:vmul off n)))
  (setq p2d (yt:vadd p2 (yt:vmul off n)))

  ; Parameters
  (setq L *yt_dima_head_len*)
  (setq W *yt_dima_head_w*)

  ; Keep arrowheads inside: L must be <= span/2 (with margin)
  (if (> (* 2.0 L) (* 0.9 span))
    (setq L (* 0.45 span))
  )

  ; Dimension line (between tips)
  (yt:mkline p1d p2d)

  ; Short extension markers: length = L, centred at each tip, along normal n
  (setq halfTick (/ L 2.0))

  (setq tickL1 (yt:vadd p1d (yt:vmul halfTick n)))
  (setq tickL2 (yt:vsub p1d (yt:vmul halfTick n)))
  (yt:mkline tickL1 tickL2)

  (setq tickR1 (yt:vadd p2d (yt:vmul halfTick n)))
  (setq tickR2 (yt:vsub p2d (yt:vmul halfTick n)))
  (yt:mkline tickR1 tickR2)

  ; Arrowheads INSIDE:
  ; Left tip at p1d, base goes inward along +u
  (setq tipL  p1d)
  (setq baseL (yt:vadd p1d (yt:vmul L u)))
  (yt:mkwedge-tip tipL baseL W)

  ; Right tip at p2d, base goes inward along -u
  (setq tipR  p2d)
  (setq baseR (yt:vsub p2d (yt:vmul L u)))
  (yt:mkwedge-tip tipR baseR W)

  (princ "\nDone.")
  (princ)
)


;============================================================
; YTDIM — aligned-dimension-like geometry (NO TEXT, NO ARROWS)
; - Picks: p1, p2, then a point to define offset side/amount
; - Draws:
;   * shaft line (between tips)
;   * simple extension markers at both ends (length = Lext, centred at each tip, along normal n)
;   * tick line at each tip: 30° to extension line, centred at the tip, length = 0.5*Lext
;============================================================

(if (not *yt_dim_ext_len*) (setq *yt_dim_ext_len* 7.0))   ; extension marker length
(if (not *yt_dim_tick_ang*) (setq *yt_dim_tick_ang* 30.0)) ; degrees (fixed per your definition)

(defun yt:rot2d (v angDeg / a c s x y)
  ; Rotate vector v by angDeg in XY plane
  (setq a (* pi (/ angDeg 180.0)))
  (setq c (cos a))
  (setq s (sin a))
  (setq x (car v))
  (setq y (cadr v))
  (list
    (- (* x c) (* y s))
    (+ (* x s) (* y c))
    0.0
  )
)

(defun yt:dim-getopts (/ kw val)
  (while
    (progn
      (initget "ExtLen Done")
      (setq kw (getkword "\nOptions [ExtLen/Done] <Done>: "))
      (cond
        ((or (not kw) (= kw "Done"))
          nil)
        ((= kw "ExtLen")
          (setq val (getreal (strcat "\nExtension marker length <" (rtos *yt_dim_ext_len* 2 3) ">: ")))
          (if (and val (> val 1e-9)) (setq *yt_dim_ext_len* val))
          T)
        (T T)))
  )
)

(defun c:YTDIM (/ p1u p2u p3u p1 p2 p3 d u n off p1d p2d
                 Lext halfExt Lt halfTick t1 t2 tdir
                 eShaft eExt1 eExt2 eTick1 eTick2)
  (yt:dim-getopts)

  (setq p1u (getpoint "\nPick extension point 1: "))
  (if (not p1u) (progn (princ "\nCancelled.") (princ) (exit)))

  (setq p2u (getpoint p1u "\nPick extension point 2: "))
  (if (not p2u) (progn (princ "\nCancelled.") (princ) (exit)))

  (setq p3u (getpoint "\nPick shaft (dimension line) location (offset side): "))
  (if (not p3u) (progn (princ "\nCancelled.") (princ) (exit)))

  ; Work in WCS
  (setq p1 (trans p1u 1 0))
  (setq p2 (trans p2u 1 0))
  (setq p3 (trans p3u 1 0))

  (if (< (distance p1 p2) 1e-9)
    (progn (princ "\nPoints are identical. Cancelled.") (princ) (exit)))

  ; Along measured direction
  (setq d (yt:vsub p2 p1))
  (setq u (yt:vunit d))

  ; Normal (offset direction)
  (setq n (yt:rot90 u))
  (if (< (yt:dot n (yt:vsub p3 p1)) 0.0)
    (setq n (yt:vmul -1.0 n))
  )
  (setq n (yt:vunit n))

  ; Signed perpendicular offset magnitude
  (setq off (yt:dot (yt:vsub p3 p1) n))

  ; Shaft endpoints (tips)
  (setq p1d (yt:vadd p1 (yt:vmul off n)))
  (setq p2d (yt:vadd p2 (yt:vmul off n)))

  ; Shaft line
  (setq eShaft (yt:mkline p1d p2d))

  ; Extension markers (centred at tips, length = Lext, along n)
  (setq Lext *yt_dim_ext_len*)
  (setq halfExt (/ Lext 2.0))

  (setq eExt1 (yt:mkline (yt:vadd p1d (yt:vmul halfExt n))
                         (yt:vsub p1d (yt:vmul halfExt n))))
  (setq eExt2 (yt:mkline (yt:vadd p2d (yt:vmul halfExt n))
                         (yt:vsub p2d (yt:vmul halfExt n))))

  ; Tick line: 30° to extension direction, centred at the tip
  ; Tick length = half of extension marker length
  (setq Lt (* 0.5 Lext))
  (setq halfTick (/ Lt 2.0))

  ; Choose a consistent slant direction:
  ; rotate n by ±30°, pick the one that "leans" toward the shaft direction u.
  (setq t1 (yt:vunit (yt:rot2d n *yt_dim_tick_ang*)))
  (setq t2 (yt:vunit (yt:rot2d n (- *yt_dim_tick_ang*))))

  (if (> (yt:dot t1 u) (yt:dot t2 u))
    (setq tdir t1)
    (setq tdir t2)
  )

  ; Tick at left tip (p1d)
  (setq eTick1 (yt:mkline (yt:vsub p1d (yt:vmul halfTick tdir))
                          (yt:vadd p1d (yt:vmul halfTick tdir))))

  ; Tick at right tip (p2d)
  (setq eTick2 (yt:mkline (yt:vsub p2d (yt:vmul halfTick tdir))
                          (yt:vadd p2d (yt:vmul halfTick tdir))))

  (princ "\nDone.")
  (princ)
)


;============================================================
; YTDLOAD — Linear distributed load along a baseline
; - Picks: p1, p2, then a point to choose side
; - Inputs:
;     L1 = arrow length at p1 side (can be 0)
;     L2 = arrow length at p2 side (can be 0)
;     n  = number of divisions (arrows are at i=0..n)
; - Logic:
;     arrow count = n+1
;     but if L1=0 => no arrow at i=0
;        if L2=0 => no arrow at i=n
; - Draws:
;     baseline line from p1 to p2
;     each arrow: shaft line + wedge head pointing to baseline
;============================================================

(if (not *yt_load_head_len*) (setq *yt_load_head_len* 7.0))
(if (not *yt_load_head_w*)   (setq *yt_load_head_w* 2.0))

(defun yt:load-getopts (/ kw val)
  (while
    (progn
      (initget "HeadLen Width Done")
      (setq kw (getkword "\nOptions [HeadLen/Width/Done] <Done>: "))
      (cond
        ((or (not kw) (= kw "Done"))
          nil)
        ((= kw "HeadLen")
          (setq val (getreal (strcat "\nArrowhead length <" (rtos *yt_load_head_len* 2 3) ">: ")))
          (if (and val (> val 1e-9)) (setq *yt_load_head_len* val))
          T)
        ((= kw "Width")
          (setq val (getreal (strcat "\nArrow width <" (rtos *yt_load_head_w* 2 3) ">: ")))
          (if (and val (> val 1e-9)) (setq *yt_load_head_w* val))
          T)
        (T T)))
  )
)

(defun c:YTDLOAD (/ p1u p2u psu p1 p2 ps
                  u n0 n d span i s p Li
                  L1 L2 ndv
                  headW headLen headL
                  tail headBase)
  ; Requires your existing helpers:
  ; yt:vadd yt:vsub yt:vmul yt:vunit yt:rot90 yt:dot yt:mkline yt:mkwedge

  (yt:load-getopts)

  (setq p1u (getpoint "\nPick baseline start point: "))
  (if (not p1u) (progn (princ "\nCancelled.") (princ) (exit)))

  (setq p2u (getpoint p1u "\nPick baseline end point: "))
  (if (not p2u) (progn (princ "\nCancelled.") (princ) (exit)))

  (setq psu (getpoint "\nPick side for the load (near the baseline): "))
  (if (not psu) (progn (princ "\nCancelled.") (princ) (exit)))

  ; WCS (UCS->WCS)
  (setq p1 (trans p1u 1 0))
  (setq p2 (trans p2u 1 0))
  (setq ps (trans psu 1 0))

  (setq span (distance p1 p2))
  (if (< span 1e-9)
    (progn (princ "\nBaseline too short. Cancelled.") (princ) (exit)))

  ; Along-baseline direction
  (setq d (yt:vsub p2 p1))
  (setq u (yt:vunit d))

  ; Side normal (choose sign by side pick)
  (setq n0 (yt:rot90 u))
  (if (< (yt:dot n0 (yt:vsub ps p1)) 0.0)
    (setq n0 (yt:vmul -1.0 n0))
  )
  (setq n (yt:vunit n0))

  ; Inputs
  (setq L1 (getreal "\nArrow length at start (p1) <0>: "))
  (if (not L1) (setq L1 0.0))

  (setq L2 (getreal "\nArrow length at end (p2) <0>: "))
  (if (not L2) (setq L2 0.0))

  (setq ndv (getint "\nNumber of divisions <10>: "))
  (if (not ndv) (setq ndv 10))
  (if (< ndv 1) (setq ndv 1))

  (setq headLen *yt_load_head_len*)
  (setq headW   *yt_load_head_w*)

  ; Draw baseline
  (yt:mkline p1 p2)

  ; Draw arrows at i=0..ndv
  (setq i 0)
  (while (<= i ndv)
    (setq s (/ (float i) (float ndv)))                ; fraction along baseline (0..1)
    (setq p (yt:vadd p1 (yt:vmul (* s span) u)))      ; point on baseline = arrow TIP

    ; Linear interpolation of arrow length
    (setq Li (+ L1 (* (- L2 L1) s)))

    ; Skip arrow at start/end if that end is zero (as requested)
    (cond
      ((and (= i 0)   (< (abs L1) 1e-9)) nil)
      ((and (= i ndv) (< (abs L2) 1e-9)) nil)
      ((< (abs Li) 1e-9) nil)
      (T
        ; Ensure head length does not exceed arrow length
        (setq headL headLen)
        (if (> headL (* 0.95 Li)) (setq headL (* 0.95 Li)))
        (if (< headL 1e-6) (setq headL (min headLen Li)))

        ; Arrow geometry: tail away from baseline, tip on baseline
        (setq tail     (yt:vadd p (yt:vmul Li n)))          ; tail
        (setq headBase (yt:vadd p (yt:vmul headL n)))       ; base of head (away from tip)

        (yt:mkline tail headBase)                           ; shaft
        (yt:mkwedge headBase p headW)                       ; head (base->tip)
      )
    )

    (setq i (1+ i))
  )

  (princ "\nDone.")
  (princ)
)
