;;; ==================================================================
;;; YT TOOLS - EDUCATIONAL DRAWING SUITE
;;; ==================================================================

(vl-load-com)

;;; ==================================================================
;;; SECTION 1: GLOBAL CONFIGURATION
;;; ==================================================================

;;; --- 1. System Settings ---
(if (not *yt_global_scale*) (setq *yt_global_scale* 1.0))
(if (not *yt_auto_layer*)   (setq *yt_auto_layer* T))

;;; --- 2. Default Sizes (Base units) ---
(if (not *yt_def_arrow_len*) (setq *yt_def_arrow_len* 7.0))
(if (not *yt_def_arrow_w*)   (setq *yt_def_arrow_w* 2.0))
(if (not *yt_def_reach*)     (setq *yt_def_reach* 50.0))

;;; --- 3. Dimension Defaults ---
(if (not *yt_def_dim_ext*)   (setq *yt_def_dim_ext* 7.0))  ; Extension line length
(if (not *yt_def_dim_tick*)  (setq *yt_def_dim_tick* 3.5)) ; Tick mark length

;;; --- 4. Layer Settings ---
(if (not *yt_lay_annot*) (setq *yt_lay_annot* "YT_ANNOTATION"))
(if (not *yt_col_annot*) (setq *yt_col_annot* 7))

(if (not *yt_lay_dim*)   (setq *yt_lay_dim* "YT_DIM"))
(if (not *yt_col_dim*)   (setq *yt_col_dim* 8))

(if (not *yt_lay_load*)  (setq *yt_lay_load* "YT_LOAD"))
(if (not *yt_col_load*)  (setq *yt_col_load* 1))

;;; =================================================================
;;; ERROR HANDLER
;;; ==================================================================
(defun yt:error (msg)
  ;; Only print "Real" errors, ignore Cancel/Exit messages
  (if (and msg (not (member msg '("Function cancelled" "quit / exit abort"))))
    (princ (strcat "\nYT Error: " msg))
  )
  (setvar "CMDECHO" 1)
  (setq *error* nil) ; Restore default handler
  (princ)
)

;;; ==================================================================
;;; SECTION 2: CONFIGURATION COMMAND
;;; ==================================================================
(defun c:YTCONFIG (/ *error* kw val subkw col strMode)
  (setq *error* yt:error) ; Enable silent cancel
  
  (princ "\n--- YT Tools Configuration ---")
  (setq kw "Start")
  
  (while kw
    ;; Main Status Line
    (princ (strcat "\nCurrent Global Scale: " (rtos *yt_global_scale* 2 1)))
    (princ (strcat " | Layering: " (if *yt_auto_layer* "Auto (Smart)" "Current Layer")))
    
    (initget "Scale Layers Params Reset Done")
    (setq kw (getkword "\nMain Settings [Scale/Layers/Params/Reset/Done] <Done>: "))
    
    (cond
      ;; --- SCALE ---
      ((= kw "Scale")
       (setq val (getreal (strcat "\nNew Global Scale Factor <" (rtos *yt_global_scale* 2 2) ">: ")))
       (if (and val (> val 0.0)) (setq *yt_global_scale* val))
      )

      ;; --- LAYERS ---
      ((= kw "Layers")
       (initget "Mode Loads Dims Done")
       (setq strMode (if *yt_auto_layer* "Auto" "Current"))
       ;; Prompt separates status from keywords
       (setq subkw (getkword (strcat "\nLayer Settings (Mode=" strMode ") [Mode/Loads/Dims/Done] <Done>: ")))
       
       (cond
         ((= subkw "Mode") 
          (setq *yt_auto_layer* (not *yt_auto_layer*))
          (princ (strcat "\nAuto-Layering is now " (if *yt_auto_layer* "ON." "OFF."))))
         
         ((= subkw "Loads")
          (setq col (getint (strcat "\nColor for Loads (1-255) <" (itoa *yt_col_load*) ">: ")))
          (if (and col (> col 0) (< col 256)) (setq *yt_col_load* col)))
          
         ((= subkw "Dims")
          (setq col (getint (strcat "\nColor for Dims (1-255) <" (itoa *yt_col_dim*) ">: ")))
          (if (and col (> col 0) (< col 256)) (setq *yt_col_dim* col)))
       )
      )

      ;; --- PARAMS ---
      ((= kw "Params")
       (initget "ArrowLen ArrowWidth ExtLen TickLen Done")
       (setq subkw (getkword "\nDefault Params [ArrowLen/ArrowWidth/ExtLen/TickLen/Done] <Done>: "))
       (cond
         ((= subkw "ArrowLen")   
          (setq val (getreal (strcat "\nBase Arrow Len <" (rtos *yt_def_arrow_len* 2 2) ">: "))) 
          (if val (setq *yt_def_arrow_len* val)))
         
         ((= subkw "ArrowWidth") 
          (setq val (getreal (strcat "\nBase Arrow Width <" (rtos *yt_def_arrow_w* 2 2) ">: "))) 
          (if val (setq *yt_def_arrow_w* val)))
         
         ((= subkw "ExtLen")     
          (setq val (getreal (strcat "\nBase Dim Ext Len <" (rtos *yt_def_dim_ext* 2 2) ">: "))) 
          (if val (setq *yt_def_dim_ext* val)))
         
         ((= subkw "TickLen")    
          (setq val (getreal (strcat "\nBase Dim Tick Len <" (rtos *yt_def_dim_tick* 2 2) ">: "))) 
          (if val (setq *yt_def_dim_tick* val)))
       )
      )

      ;; --- RESET ---
      ((= kw "Reset")
       (setq *yt_global_scale* 1.0 *yt_auto_layer* T
             *yt_def_arrow_len* 7.0 *yt_def_arrow_w* 2.0 
             *yt_def_dim_ext* 7.0 *yt_def_dim_tick* 3.5
             *yt_col_load* 1 *yt_col_dim* 8 *yt_col_annot* 7)
       (princ "\nDefaults restored.")
      )

      ((or (= kw "Done") (= kw nil)) (setq kw nil))
    )
  )
  (princ)
)

;;; ==================================================================
;;; SECTION 3: CORE HELPERS
;;; ==================================================================

(defun yt:error (msg)
  (if (and msg (not (member msg '("Function cancelled" "quit / exit abort"))))
    (princ (strcat "\nYT Error: " msg))
  )
  (setvar "CMDECHO" 1)
  (setq *error* nil)
  (princ)
)

(defun yt:vadd (a b) (mapcar '+ a b))
(defun yt:vsub (a b) (mapcar '- a b))
(defun yt:vmul (s v) (mapcar '(lambda (x) (* s x)) v))
(defun yt:dot  (a b) (apply '+ (mapcar '* a b)))
(defun yt:vlen (v)   (sqrt (apply '+ (mapcar '* v v))))
(defun yt:dist (a b) (yt:vlen (yt:vsub b a)))
(defun yt:vunit (v / L) (setq L (yt:vlen v)) (if (> L 1e-9) (yt:vmul (/ 1.0 L) v) (list 0.0 0.0 0.0)))
(defun yt:rot90 (v) (list (- (cadr v)) (car v) 0.0))
(defun yt:rot2d (v angDeg / a c s x y) (setq a (* pi (/ angDeg 180.0)) c (cos a) s (sin a) x (car v) y (cadr v)) (list (- (* x c) (* y s)) (+ (* x s) (* y c)) 0.0))

(defun yt:get-layer (name color / tbl)
  (if *yt_auto_layer*
    (progn (if (not (tblsearch "LAYER" name)) (entmake (list (cons 0 "LAYER") (cons 100 "AcDbSymbolTableRecord") (cons 100 "AcDbLayerTableRecord") (cons 2 name) (cons 62 color) (cons 70 0)))) name)
    nil
  )
)

(defun yt:ent-line (p1 p2 layer)
  (entmake (append (list (cons 0 "LINE") (cons 10 p1) (cons 11 p2)) (if layer (list (cons 8 layer)) '())))
)
(defun yt:ent-wedge (base tip w layer)
  (entmake (append (list (cons 0 "LWPOLYLINE") (cons 100 "AcDbEntity") (cons 100 "AcDbPolyline") (cons 90 2) (cons 70 0) (cons 10 (list (car base) (cadr base))) (cons 40 (* 2.0 w)) (cons 41 0.0) (cons 10 (list (car tip) (cadr tip))) (cons 40 0.0) (cons 41 0.0)) (if layer (list (cons 8 layer)) '())))
)

;;; ==================================================================
;;; SECTION 4: COMMANDS (Prompts Updated)
;;; ==================================================================

;;; --- YTARROW ---
(defun c:YTARROW (/ *error* p1 p2 L W bothAns bothFlag dist12 u tailBase headBase lay kw val)
  (setq *error* yt:error)
  (setq lay (yt:get-layer *yt_lay_annot* *yt_col_annot*)) 

  (setq L (* *yt_def_arrow_len* *yt_global_scale*) 
        W (* *yt_def_arrow_w* *yt_global_scale*))

  ;; Options Loop
  (setq kw "Start")
  (while kw
    (princ (strcat "\nCurrent: Length=" (rtos L 2 2) ", Width=" (rtos W 2 2)))
    (initget "Length Width Done")
    (setq kw (getkword "\nArrow [Length/Width/Done] <Done>: "))
    (cond
      ((= kw "Length") 
       (setq val (getreal (strcat "\nLen <" (rtos L 2 2) ">: "))) 
       (if val (setq L val)))
       
      ((= kw "Width")  
       (setq val (getreal (strcat "\nWidth <" (rtos W 2 2) ">: "))) 
       (if val (setq W val)))
       
      ((or (= kw "Done") (= kw nil)) (setq kw nil))
    )
  )

  (if (and (setq p1 (getpoint "\nTail point: ")) (setq p2 (getpoint p1 "\nHead point: ")))
    (progn
      (setq dist12 (yt:dist p1 p2))
      (initget "Yes No") (setq bothFlag (= (getkword "\nDouble-ended? [Yes/No] <No>: ") "Yes"))

      (if bothFlag
        (if (< dist12 (* 2.2 L)) (setq L (/ dist12 2.2)))
        (if (< dist12 L) (setq L (* 0.9 dist12)))
      )

      (setq u (yt:vunit (yt:vsub p2 p1)))
      (setq headBase (yt:vsub p2 (yt:vmul L u)))

      (if bothFlag
        (progn (setq tailBase (yt:vadd p1 (yt:vmul L u))) (yt:ent-line tailBase headBase lay) (yt:ent-wedge headBase p2 W lay) (yt:ent-wedge tailBase p1 W lay))
        (progn (yt:ent-line p1 headBase lay) (yt:ent-wedge headBase p2 W lay))
      )
      (princ "\nDone.")
    )
  )
  (princ)
)

;;; --- YTARROWN (Normal Arrow) ---
(if (not *yt_n_dir*) (setq *yt_n_dir* "Away"))

(defun c:YTARROWN (/ *error* ent pickU pick p0 param deriv tan n0 n L W reach 
                     tipA tipB baseA baseB tailA tailB bothAns both lay kw statusStr val)
  (setq *error* yt:error)
  (setq lay (yt:get-layer *yt_lay_annot* *yt_col_annot*))

  (setq L (* *yt_def_arrow_len* *yt_global_scale*) 
        W (* *yt_def_arrow_w* *yt_global_scale*)
        reach (* *yt_def_reach* *yt_global_scale*))

  (setq kw "Start")
  (while kw
    (setq statusStr (if (= *yt_n_dir* "To") "To Curve" "Away from Curve"))
    (princ (strcat "\nCurrent: " statusStr " | Reach=" (rtos reach 2 2)))
    
    (initget "Reach Length Width To Away Done")
    (setq kw (getkword "\nOptions [Reach/Length/Width/To/Away/Done] <Done>: "))
    
    (cond
      ((= kw "Reach") 
       (setq val (getdist (strcat "\nReach dist <" (rtos reach 2 2) ">: "))) 
       (if val (setq reach val)))
       
      ((= kw "Length") 
       (setq val (getreal (strcat "\nArrowhead Length <" (rtos L 2 2) ">: "))) 
       (if val (setq L val)))
       
      ((= kw "Width") 
       (setq val (getreal (strcat "\nArrowhead Width <" (rtos W 2 2) ">: "))) 
       (if val (setq W val)))
       
      ((= kw "To") (setq *yt_n_dir* "To") (princ "\nSet: To Curve."))
      ((= kw "Away") (setq *yt_n_dir* "Away") (princ "\nSet: Away from Curve."))
      ((or (= kw "Done") (= kw nil)) (setq kw nil))
    )
  )

  (if (and (setq ent (car (entsel "\nSelect curve: "))) (setq pickU (getpoint "\nPick side point: ")))
    (progn
      (setq pick (trans pickU 1 0)) (setq p0 (vlax-curve-getClosestPointTo ent pick))
      (setq param (vlax-curve-getParamAtPoint ent p0) deriv (vlax-curve-getFirstDeriv ent param) tan (yt:vunit deriv) n0 (yt:rot90 tan))
      (if (< (yt:dot n0 (yt:vsub pick p0)) 0.0) (setq n (yt:vmul -1.0 n0)) (setq n n0))
      
      (if (> L (* 0.95 reach)) (setq L (* 0.95 reach)))
      (initget "Yes No") (setq bothAns (getkword "\nDouble-ended? [Yes/No] <No>: ")) (setq both (= bothAns "Yes"))

      (cond
        ((= *yt_n_dir* "To")
           (if both
             (progn (setq tailA (yt:vadd p0 (yt:vmul reach n)) tailB (yt:vsub p0 (yt:vmul reach n)) baseA (yt:vadd p0 (yt:vmul L n)) baseB (yt:vsub p0 (yt:vmul L n)))
               (yt:ent-line tailA baseA lay) (yt:ent-wedge baseA p0 W lay) (yt:ent-line tailB baseB lay) (yt:ent-wedge baseB p0 W lay))
             (progn (setq tailA (yt:vadd p0 (yt:vmul reach n)) baseA (yt:vadd p0 (yt:vmul L n)))
               (yt:ent-line tailA baseA lay) (yt:ent-wedge baseA p0 W lay))))
        (T 
           (if both
             (progn (setq tipA (yt:vadd p0 (yt:vmul reach n)) tipB (yt:vsub p0 (yt:vmul reach n)) baseA (yt:vsub tipA (yt:vmul L n)) baseB (yt:vadd tipB (yt:vmul L n)))
               (yt:ent-line p0 baseA lay) (yt:ent-wedge baseA tipA W lay) (yt:ent-line p0 baseB lay) (yt:ent-wedge baseB tipB W lay))
             (progn (setq tipA (yt:vadd p0 (yt:vmul reach n)) baseA (yt:vsub tipA (yt:vmul L n)))
               (yt:ent-line p0 baseA lay) (yt:ent-wedge baseA tipA W lay))))
      )
      (princ (strcat "\nArrow created " (if (= *yt_n_dir* "To") "TO" "AWAY from") " the curve."))
    )
  )
  (princ)
)

;;; --- YTDIMA (Arrows Dim) ---
(defun c:YTDIMA (/ *error* p1u p2u p3u p1 p2 p3 u n off p1d p2d L W span halfTick lay kw val)
  (setq *error* yt:error)
  (setq lay (yt:get-layer *yt_lay_dim* *yt_col_dim*))

  (setq L (* *yt_def_arrow_len* *yt_global_scale*) 
        W (* *yt_def_arrow_w* *yt_global_scale*))

  (setq kw "Start")
  (while kw
    (princ (strcat "\nCurrent: Length=" (rtos L 2 2) ", Width=" (rtos W 2 2)))
    (initget "Length Width Done")
    (setq kw (getkword "\nDim Arrow [Length/Width/Done] <Done>: "))
    (cond
      ((= kw "Length") 
       (setq val (getreal (strcat "\nLen <" (rtos L 2 2) ">: "))) 
       (if val (setq L val)))
       
      ((= kw "Width")  
       (setq val (getreal (strcat "\nWidth <" (rtos W 2 2) ">: "))) 
       (if val (setq W val)))
       
      ((or (= kw "Done") (= kw nil)) (setq kw nil))
    )
  )

  (if (and (setq p1u (getpoint "\nExt point 1: ")) (setq p2u (getpoint p1u "\nExt point 2: ")) (setq p3u (getpoint "\nDim line offset: ")))
    (progn
      (setq p1 (trans p1u 1 0) p2 (trans p2u 1 0) p3 (trans p3u 1 0))
      (setq span (yt:dist p1 p2) u (yt:vunit (yt:vsub p2 p1)) n (yt:rot90 u))
      (if (< (yt:dot n (yt:vsub p3 p1)) 0.0) (setq n (yt:vmul -1.0 n)))
      (setq off (yt:dot (yt:vsub p3 p1) n))

      (setq p1d (yt:vadd p1 (yt:vmul off n)) p2d (yt:vadd p2 (yt:vmul off n)))
      (if (> (* 2.0 L) (* 0.9 span)) (setq L (* 0.45 span)))

      (yt:ent-line p1d p2d lay)
      (setq halfTick (/ L 2.0))
      (yt:ent-line (yt:vadd p1d (yt:vmul halfTick n)) (yt:vsub p1d (yt:vmul halfTick n)) lay)
      (yt:ent-line (yt:vadd p2d (yt:vmul halfTick n)) (yt:vsub p2d (yt:vmul halfTick n)) lay)
      (yt:ent-wedge (yt:vadd p1d (yt:vmul L u)) p1d W lay)
      (yt:ent-wedge (yt:vsub p2d (yt:vmul L u)) p2d W lay)
    )
  )
  (princ)
)

;;; --- YTDIM (Ticks Dim) ---
(defun c:YTDIM (/ *error* p1u p2u p3u p1 p2 p3 u n off p1d p2d Lext Ltick halfExt halfTick t1 t2 tdir lay kw val)
  (setq *error* yt:error)
  (setq lay (yt:get-layer *yt_lay_dim* *yt_col_dim*))
  
  (setq Lext  (* *yt_def_dim_ext* *yt_global_scale*)
        Ltick (* *yt_def_dim_tick* *yt_global_scale*))

  (setq kw "Start")
  (while kw
    (princ (strcat "\nCurrent: ExtLen=" (rtos Lext 2 2) ", TickLen=" (rtos Ltick 2 2)))
    (initget "ExtLen TickLen Done")
    (setq kw (getkword "\nDim Ticks [ExtLen/TickLen/Done] <Done>: "))
    (cond
      ((= kw "ExtLen")  
       (setq val (getreal (strcat "\nExtension Line Length <" (rtos Lext 2 2) ">: "))) 
       (if val (setq Lext val)))
       
      ((= kw "TickLen") 
       (setq val (getreal (strcat "\nTick Mark Length <" (rtos Ltick 2 2) ">: "))) 
       (if val (setq Ltick val)))
       
      ((or (= kw "Done") (= kw nil)) (setq kw nil))
    )
  )

  (if (and (setq p1u (getpoint "\nExt point 1: ")) (setq p2u (getpoint p1u "\nExt point 2: ")) (setq p3u (getpoint "\nDim line offset: ")))
    (progn
      (setq p1 (trans p1u 1 0) p2 (trans p2u 1 0) p3 (trans p3u 1 0))
      (setq u (yt:vunit (yt:vsub p2 p1)) n (yt:rot90 u))
      (if (< (yt:dot n (yt:vsub p3 p1)) 0.0) (setq n (yt:vmul -1.0 n)))
      (setq off (yt:dot (yt:vsub p3 p1) n))

      (setq p1d (yt:vadd p1 (yt:vmul off n)) p2d (yt:vadd p2 (yt:vmul off n)))
      (yt:ent-line p1d p2d lay)
      (setq halfExt (/ Lext 2.0))
      (yt:ent-line (yt:vadd p1d (yt:vmul halfExt n)) (yt:vsub p1d (yt:vmul halfExt n)) lay)
      (yt:ent-line (yt:vadd p2d (yt:vmul halfExt n)) (yt:vsub p2d (yt:vmul halfExt n)) lay)

      (setq halfTick (/ Ltick 2.0) t1 (yt:vunit (yt:rot2d n 30.0)) t2 (yt:vunit (yt:rot2d n -30.0)))
      (if (> (yt:dot t1 u) (yt:dot t2 u)) (setq tdir t1) (setq tdir t2))
      (yt:ent-line (yt:vsub p1d (yt:vmul halfTick tdir)) (yt:vadd p1d (yt:vmul halfTick tdir)) lay)
      (yt:ent-line (yt:vsub p2d (yt:vmul halfTick tdir)) (yt:vadd p2d (yt:vmul halfTick tdir)) lay)
    )
  )
  (princ)
)

;;; --- YTDLOAD (Distributed Load) ---
(defun c:YTDLOAD (/ *error* p1u p2u psu p1 p2 ps u n span i s p Li L1 L2 ndv headLen headW headL tail headBase lay)
  (setq *error* yt:error)
  (setq lay (yt:get-layer *yt_lay_load* *yt_col_load*))

  (setq headLen (* *yt_def_arrow_len* *yt_global_scale*) 
        headW   (* *yt_def_arrow_w* *yt_global_scale*))

  (if (and (setq p1u (getpoint "\nStart point: ")) (setq p2u (getpoint p1u "\nEnd point: ")) (setq psu (getpoint "\nPick Load Direction: ")))
    (progn
      (setq p1 (trans p1u 1 0) p2 (trans p2u 1 0) ps (trans psu 1 0))
      (setq span (yt:dist p1 p2) u (yt:vunit (yt:vsub p2 p1)) n (yt:rot90 u))
      (if (< (yt:dot n (yt:vsub ps p1)) 0.0) (setq n (yt:vmul -1.0 n)))

      (setq L1 (getreal "\nArrow length at start <0>: ")) (if (not L1) (setq L1 0.0))
      (setq L2 (getreal "\nArrow length at end <0>: "))   (if (not L2) (setq L2 0.0))
      (setq ndv (getint "\nNumber of divisions <10>: "))  (if (not ndv) (setq ndv 10))
      
      (yt:ent-line p1 p2 lay)

      (setq i 0)
      (while (<= i ndv)
        (setq s (/ (float i) (float ndv)) p (yt:vadd p1 (yt:vmul (* s span) u)) Li (+ L1 (* (- L2 L1) s)))
        (if (> (abs Li) 1e-4)
          (progn
             (setq headL (min headLen Li))
             (if (> headL (* 0.95 Li)) (setq headL (* 0.95 Li)))
             (setq tail (yt:vadd p (yt:vmul Li n)) headBase (yt:vadd p (yt:vmul headL n)))
             (yt:ent-line tail headBase lay) (yt:ent-wedge headBase p headW lay)
          )
        )
        (setq i (1+ i))
      )
      (princ "\nDistributed load created.")
    )
  )
  (princ)
)

(princ "\nYT Tools Loaded. Type YTCONFIG to setup Scale, Layers, and Defaults.")
(princ)
