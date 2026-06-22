; ============================================================
; PantherX - GUI Viewport Layout Generator
; ============================================================
; FEATURES
; ------------------------------------------------------------
; ✔ DCL GUI
; ✔ Base Layout dropdown
; ✔ Viewport Layer dropdown
; ✔ Strict layer filtering
; ✔ Closed polyline validation
; ✔ Auto rename duplicate layouts
; ✔ Settings persistence (INI beside LSP)
; ✔ Auto creates DCL if missing
; ✔ Uses selected layout as template
; ✔ Locks viewport automatically
; ============================================================

; ============================================================
; PantherX - GUI Viewport Layout GeneratorS
; Version : x9.1.1
; Author  : Harshit
; Last Updated : 2026-06-22
; ============================================================
(setq *PX_NAME* "PantherX")
(setq *PX_VERSION* "x9.1.1")

(princ (strcat "\nA Tool handcrafted by Harshit.\nPantherX " *PX_VERSION* " Loaded.\n"))  
; ============================================================
; Get Help use command - ineedhelp
; ============================================================


(defun C:ineedhelp()
   (setq url "https://forms.office.com/pages/responsepage.aspx?id=gruQGTzV-06HamayWZBzCBbhiXhr-iBMlaM6LmJQ0LNUMFcwVzBFVFA3QlJGNDVRV1pQTEo3VDBFQi4u&route=shorturl")
   (command "_.browser" url)
)

(vl-load-com)

;; -----------------------------
;; GLOBAL CACHE
;; -----------------------------
(setq *PX_NET_CACHE* nil)
(setq *PX_ACCESS_CACHE* nil)


;; -----------------------------
;; Generic HTTP GET (with cache-busting) ;for inta
;; -----------------------------



;; -----------------------------
;; Generic HTTP GET (with cache-busting) ;for inta
;; -----------------------------
;----------For version----------------------------------


(defun PX:HttpGet (url / http response i)
  (vl-load-com)

  (setq http
    (vl-catch-all-apply
      'vlax-create-object
      (list "MSXML2.XMLHTTP.6.0")
    )
  )

  (if (not (vl-catch-all-error-p http))
    (progn
      ;; Open request
      (vlax-invoke-method http 'open "GET" url :vlax-false)

      ;; ❌ REMOVE ALL HEADERS (important for GitHub raw)

      ;; Send request
      (vlax-invoke-method http 'send)

      ;; Wait (~5 sec max)
      (setq i 0)
      (while (and (/= (vlax-get-property http 'readyState) 4)
                  (< i 100))
        (setq i (1+ i))
        (vlax-sleep 50)
      )

      ;; Read response
      (if (= (vlax-get-property http 'status) 200)
        (setq response (vlax-get-property http 'responseText))
      )

      ;; Cleanup
      (vlax-release-object http)
    )
  )

  response
)


;-------------------------------------------------
;; -----------------------------
;; Internet Check (CACHED)
;; -----------------------------
(defun PX:InternetAvailable (/ response)

  (if (null *PX_NET_CACHE*)
    (progn
      (setq response (PX:HttpGet "https://www.google.com"))
      (setq *PX_NET_CACHE* (if response T nil))
    )
  )

  *PX_NET_CACHE*
)


(defun PX:GetURL ()
  (apply 'strcat
    (mapcar 'chr
      '(104 116 116 112 115 58 47 47 114 97 119 46 103 105 116 104 117 98 117 115 101 114 99 111 110 116 101 110 116 46 99 111 109 47 104 97 114 115 104 105 116 104 57 119 111 114 108 100 47 97 117 116 111 109 111 116 105 118 101 47 109 97 115 116 101 114 47 80 97 110 120 57 46 116 120 116)
    )
  )
)

;; -----------------------------
;; Access Check (CACHED + FIXED)
;; -----------------------------
(defun PX:CheckAccess (/ response expected)
  (setq response
    (PX:HttpGet
      (strcat
		(PX:GetURL)
        "?t=" (rtos (getvar "DATE") 2 6)
      )
    )
  )

(setq response (vl-string-subst "\n" "\r\n" response))

  ;; store raw for later use
  (setq *PX_LAST_RESPONSE* response)

  ;; build expected pattern
  (setq expected (strcat *PX_NAME* "=" *PX_VERSION*))

;(prompt (strcat "\nEXPECTED: " *PX_VERSION*))
;(prompt (strcat "\nACTUAL: " (PX:GetLatestVersion)))



  ;; correct matching
  (if (and response
           (vl-string-search "STATUS=LIVE" response)
           (vl-string-search expected response)
      )
    T
    nil
  )
)

(defun PX:GetLatestVersion (/ response name pos start end latest)

  (setq response *PX_LAST_RESPONSE*)

  (if response
    (progn
      (setq name *PX_NAME*)

      (setq pos (vl-string-search (strcat name "=") response))

      (if pos
        (progn
          (setq start (+ pos (strlen name) 1))

          ;; handle CRLF properly
          (setq end (vl-string-search "\n" response start))
          (if (not end)
            (setq end (strlen response))
          )

          ;; ✅ FIXED extraction (no +1)
          (setq latest
            (substr response start (- end start))
          )

          ;; ✅ trim CR/LF/spaces
          (setq latest (vl-string-trim "\r\n " latest))
        )
      )
    )
  )

  latest
)
``

;----------greetings------------------------------------
(defun PX:Greetings (/ usr hr greet)
  (setq usr (getvar "LOGINNAME"))
  (setq hr (atoi (substr (menucmd "M=$(edtime,$(getvar,date),HH)") 1 2)))

  ;; Determine greeting
  (setq greet
    (cond
      ((< hr 12) "Good Morning ")
      ((< hr 17) "Good Afternoon ")
      (T          "Good Evening ")
    )
  )

  (strcat greet usr)
)


;-------------------------------------------------------------
(setq *PX_ConfigFile*
      (if (findfile "PantherX.lsp")
        (strcat
          (vl-filename-directory
            (findfile "PantherX.lsp")
          )
          "\\PantherX_Settings.ini"
        )
        (strcat
          (getvar "TEMPPREFIX")
          "PantherX_Settings.ini"
        )
      )
)
(setq *PX_DCLFile*
      (strcat
        (getvar "TEMPPREFIX")
        "PantherX.dcl"
      ) 
)
   
; ------------------------------------------------------------
; WRITE DCL FILE
; ------------------------------------------------------------

(defun PX:WriteDCL (/ f)

  (setq f (open *PX_DCLFile* "w"))

  (foreach x
    '(
      "PantherX : dialog {"
      "label = \"PantherX Setup\" ;"
      "spacer;"
	  "spacer;"
	  ": text { label = \"===================================================\"; }"
	  ": text { key = \"welcome\"; width = 42; alignment = centered; }"
	  ": text { label = \"===================================================\"; }"
	  "spacer;"
	  "spacer;"
	  ": boxed_column {"	
	  "  label = \"Prerequisite\";" 
	  
	  ": row {"
      "  : text { label = \"Base Layout:\"; width = 18; }"
      "  : popup_list { key = \"layout\"; width = 30; }"
      "}"
      ": row {"
      "  : text { label = \"Viewport Layer:\"; width = 18; }"
      "  : popup_list { key = \"layer\"; width = 30; }"
      "}"

	  ": row {"
      "  : text { label = \"Scale (1:X):\"; width = 18; }"
      "  : edit_box { key = \"scale\"; width = 30; }"
	  "}"
	  "}"
	  
	  "spacer;"        

	  ": boxed_column {"	
	  "  label = \"Layout Name\";"
	  
      ": row {"
      "  : text { label = \"Prefix:\"; width = 18; }"
      "  : edit_box { key = \"prefix\"; width = 30; }"
      "}" 
      ": row {"
      "  : text { label = \"Suffix:\"; width = 18; }"
      "  : edit_box { key = \"suffix\"; width = 30; }"
      "}"
      ": row {"
      "  : text { label = \"Preview:\"; width = 18; }"
      "  : text { key = \"preview\"; width = 30; }"
      "}"
	  "}"

	  "spacer;" 	  
	  
	  ": boxed_column {"
	  "  label = \"Settings\";"
	  "  : text { label = \"Multi-Viewport ?\"; }"
	  "	 : row {"
	  "		: radio_button { key = \"yes\"; label = \"Yes\"; }"
	  "		: radio_button { key = \"no\"; label = \"No\"; }"
	  "}"
	  "}"

	  ": text { key = \"version\"; }"	  

      "spacer;"
      "ok_cancel;"
      "}"
     )
    (write-line x f)
  )

  (close f)
)

; ------------------------------------------------------------
; SAVE SETTINGS
; ------------------------------------------------------------

(defun PX:SaveSettings
  (layout layer scale prefix suffix / f)

  (setq f (open *PX_ConfigFile* "w"))

  (write-line layout f)
  (write-line layer f)
  (write-line scale f)
  (write-line prefix f)
  (write-line suffix f)

  (close f)
)

; ------------------------------------------------------------
; LOAD SETTINGS
; ------------------------------------------------------------

(defun PX:LoadSettings (/ f lst)

  (if (findfile *PX_ConfigFile*)
    (progn

      (setq f (open *PX_ConfigFile* "r"))

      (while (setq x (read-line f))
        (setq lst (append lst (list x)))
      )

      (close f)

      lst
    )
  )
)

; ------------------------------------------------------------
; GET LAYOUT LIST
; ------------------------------------------------------------

(defun PX:GetLayouts (/ lst)

  (foreach x (layoutlist)

    (if (/= (strcase x) "MODEL")
      (setq lst (append lst (list x)))
    )
  )

  lst
)

; ------------------------------------------------------------
; GET LAYER LIST
; ------------------------------------------------------------

(defun PX:GetLayers (/ lst e)

  (setq e (tblnext "LAYER" T))

  (while e

    (setq lst
          (append
            lst
            (list (cdr (assoc 2 e)))
          )
    )

    (setq e (tblnext "LAYER"))
  )

  (acad_strlsort lst)
)

; ------------------------------------------------------------
; UNIQUE LAYOUT NAME
; ------------------------------------------------------------

(defun PX:GetUniqueLayoutName (name / n test)

  (setq test name)

  (setq n 1)

  (while (member test (layoutlist))

    (setq test
          (strcat
            name
            "_"
            (itoa n)
          )
    )

    (setq n (1+ n))
  )

  test
)
;------------------------------------------------------------
;Greatings
;-------------------------------------------------------------

(defun PX:GetGreeting (/ usr mail)
  (setq usr (getvar "LOGINNAME"))
  (setq mail (getvar "LOGINID"))
  (strcat usr "!! Welcome to PantherX, your layout expert.")
)

; ------------------------------------------------------------
; GUI
; ------------------------------------------------------------

(defun PX:Dialog
  (/ dcl id layouts layers
     lay layer scale prefix suffix
     result saved idx)

  
  (PX:WriteDCL)

  (setq layouts (PX:GetLayouts))
  (setq layers  (PX:GetLayers))

  (setq saved (PX:LoadSettings))

  (setq lay
        (if saved
          (nth 0 saved)
          (car layouts)
        )
  )

  (setq layer
        (if saved
          (nth 1 saved)
          (if (member "VIEWPORT HIDE" layers)
            "VIEWPORT HIDE"
            (car layers)
          )
        )
  )

  (setq scale
        (if saved
          (nth 2 saved)
          "50"
        )
  )

  (setq prefix
        (if saved
          (nth 3 saved)
          ""
        )
  )

  (setq suffix
        (if saved
          (nth 4 saved)
          ""
        )
  )

  (setq dcl (load_dialog *PX_DCLFile*))

  (if (not (new_dialog "PantherX" dcl))
    (exit)
  )
  
  (set_tile "welcome" (PX:GetGreeting))
  
  ; -------------------------
  ; Populate layouts
  ; -------------------------

  (start_list "layout")

  (foreach x layouts
    (add_list x)
  )

  (end_list)

  (setq idx (vl-position lay layouts))

  (if idx
    (set_tile "layout" (itoa idx))
    (set_tile "layout" "0")
  )

  ; -------------------------
  ; Populate layers
  ; -------------------------

  (start_list "layer")

  (foreach x layers
    (add_list x)
  )

  (end_list)

  (setq idx (vl-position layer layers))

  (if idx
    (set_tile "layer" (itoa idx))
    (set_tile "layer" "0")
  )

  ; -------------------------
  ; Values
  ; -------------------------

  (set_tile "scale" scale)
  (set_tile "prefix" prefix)
  (set_tile "suffix" suffix)

  (set_tile
    "preview"
    (strcat prefix "1" suffix)
  )

  ; -------------------------
  ; Live preview
  ; -------------------------

  (action_tile
    "prefix"
    "(set_tile \"preview\" (strcat $value \"1\" (get_tile \"suffix\")))"
  )

  (action_tile
    "suffix"
    "(set_tile \"preview\" (strcat (get_tile \"prefix\") \"1\" $value))"
  )

  ; -------------------------
  ; OK
  ; -------------------------

  (action_tile
    "accept"

    (strcat
      "(setq result "
      "(list "
      "(nth (atoi (get_tile \"layout\")) layouts) "
      "(nth (atoi (get_tile \"layer\")) layers) "
      "(get_tile \"scale\") "
      "(get_tile \"prefix\") "
      "(get_tile \"suffix\")"
      "))"
      "(done_dialog 1)"
    )
  )

  (action_tile
    "cancel"
    "(done_dialog 0)"
  )

  (setq id (start_dialog))

  (unload_dialog dcl)

  (if (= id 1)

    (progn

      (PX:SaveSettings
        (nth 0 result)
        (nth 1 result)
        (nth 2 result)
        (nth 3 result)
        (nth 4 result)
      )

      result
    )
  )
)


; ------------------------------------------------------------
; MAIN COMMAND
; ------------------------------------------------------------

(defun c:PantherX
  (/ cfg
     baseLayout
     vpLayer
     scl
     prefix
     suffix
     ss i ent obj
     minpt maxpt
     width height
     vpW vpH
     acadApp doc layouts
     layName
     layBlock vpObj
     vpCtr
     oldCmdEcho
     closedFlag)



  (vl-load-com)


 
  ;; ✅ RESET CACHE HERE (IMPORTANT)
  (setq *PX_LAST_RESPONSE* nil)

  ;; Save environment
  (setq oldCmdEcho (getvar 'CMDECHO))
  (setvar 'CMDECHO 0)

  ;; -----------------------------
  ;; INTERNET CHECK
  ;; -----------------------------
  (if (not (PX:InternetAvailable))
    (progn
      (alert "Application Error [PX-1042] - No Internet")
      (setvar 'CMDECHO oldCmdEcho)
      (princ)
      (exit)
    )
  )

  ;; -----------------------------
  ;; VERSION + ACCESS CHECK
  ;; -----------------------------
(setq check (PX:CheckAccess))

(if (not (eq check T))
  (progn
    (setq latestVer (PX:GetLatestVersion))

    ;; Show version info
    (alert
      (strcat
        "Application Error [PX-1041] - Update available\n\n"
        "Current Version: " *PX_VERSION* "\n"
        "Latest Version: " (if latestVer latestVer "Unknown") "\n\n"
        "Do you want to download the latest version?"
      )
    )

    ;; ✅ Ask user input
    (initget "Yes No")
    (setq userChoice (getkword "\nClick on Yes to process to Download link [Yes/No]: "))

    ;; ✅ Handle response
    (if (= userChoice "Yes")
      (progn
        ;; Open browser
        (startapp
          "cmd.exe"
          (strcat "/c start \"\" \""
            "https://psixbox.sharepoint.com/:f:/r/sites/AutomationRepo/Shared%20Documents/Cover/All%20Accounts/Tools/PantherX?csf=1&web=1&e=bGZ8Yp"
          "\"")
        )
      )
    )

    ;; Exit regardless of choice
    (setvar 'CMDECHO oldCmdEcho)
    (exit)
  )
)

  ;; ✅ Continue main logic here

  (setvar 'CMDECHO oldCmdEcho)
  (princ)


  ;; -----------------------------
  ;; MAIN LOGIC STARTS HERE
  ;; -----------------------------
  (alert (PX:Greetings))

  ;; your actual PantherX logic continues here


  ;; Restore environment
  (setvar 'CMDECHO oldCmdEcho)

  (princ)


 
(princ (strcat "\nLaunching PantherX " *PX_VERSION* "......"))   
  ; -------------------------
  ; GUI
  ; -------------------------

  (setq cfg (PX:Dialog))

  (if (null cfg)
    (progn
      (princ "\nCancelled.")
      (exit)
    )
  )

  (setq baseLayout (nth 0 cfg))
  (setq vpLayer    (nth 1 cfg))
  (setq scl        (atoi (nth 2 cfg)))
  (setq prefix     (nth 3 cfg))
  (setq suffix     (nth 4 cfg))

  ; -------------------------
  ; Validation
  ; -------------------------

  (if (<= scl 0)
    (progn
      (alert "Invalid scale.")
      (exit)
    )
  )

  ; -------------------------
  ; Save vars
  ; -------------------------

  (setq oldCmdEcho (getvar "CMDECHO"))

  (setvar "CMDECHO" 0)

  ; -------------------------
  ; Select objects
  ; -------------------------

  (princ
    (strcat
      "\nSelect viewport rectangles on layer: "
      vpLayer
    )
  )

  (setq ss
        (ssget
          (list
            '(0 . "LWPOLYLINE")
            (cons 8 vpLayer)
          )
        )
  )

  (if ss

    (progn

      (setq acadApp (vlax-get-acad-object))
      (setq doc (vla-get-ActiveDocument acadApp))
      (setq layouts (vla-get-Layouts doc))

      (setvar "TILEMODE" 0)

      (setq i 0)

      (while (< i (sslength ss))

        (setq ent (ssname ss i))

        (setq obj (vlax-ename->vla-object ent))

        ; --------------------------------
        ; Closed polyline check
        ; --------------------------------

        (setq closedFlag
              (vlax-get obj 'Closed)
        )

        (if (not closedFlag)

          (progn
            (princ
              "\nSkipped open polyline."
            )
          )

          (progn

            ; ----------------------------
            ; Bounding box
            ; ----------------------------

            (vla-GetBoundingBox obj 'minpt 'maxpt)

            (setq minpt
                  (vlax-safearray->list minpt)
            )

            (setq maxpt
                  (vlax-safearray->list maxpt)
            )

            (setq width
                  (- (car maxpt)
                     (car minpt))
            )

            (setq height
                  (- (cadr maxpt)
                     (cadr minpt))
            )

            (setq vpW (/ width scl))
            (setq vpH (/ height scl))

            ; ----------------------------
            ; Layout name
            ; ----------------------------

            (setq layName
                  (PX:GetUniqueLayoutName
                    (strcat
                      prefix
                      (itoa (1+ i))
                      suffix
                    )
                  )
            )

            ; ----------------------------
            ; Copy layout
            ; ----------------------------

            (command
              "-LAYOUT"
              "Copy"
              baseLayout
              layName
            )

            ; ----------------------------
            ; Activate layout
            ; ----------------------------

            (setvar "CTAB" layName)

            (command "REGENALL")

            ; ----------------------------
            ; Find viewport
            ; ----------------------------

            (setq vpObj nil)

            (setq layBlock
                  (vla-get-Block
                    (vla-item layouts layName)
                  )
            )

            (vlax-for x layBlock

              (if
                (= (vla-get-ObjectName x)
                   "AcDbViewport")

                (setq vpObj x)
              )
            )

            ; ----------------------------
            ; Configure viewport
            ; ----------------------------

            (if vpObj

              (progn

                (vla-put-DisplayLocked
                  vpObj
                  :vlax-false
                )

                (setq vpCtr
                      (vlax-get
                        vpObj
                        'Center
                      )
                )

                (vla-put-Width vpObj vpW)
                (vla-put-Height vpObj vpH)

                (vla-put-Center
                  vpObj
                  (vlax-3D-point vpCtr)
                )

                ; ------------------------
                ; Zoom viewport
                ; ------------------------

                (command "_MSPACE")

                (command
                  "_ZOOM"
                  "_W"
                  (list
                    (car minpt)
                    (cadr minpt)
                  )
                  (list
                    (car maxpt)
                    (cadr maxpt)
                  )
                )

                (command
                  "_ZOOM"
                  (strcat
                    "1/"
                    (itoa scl)
                    "XP"
                  )
                )

                (command "_PSPACE")

                ; ------------------------
                ; Lock viewport
                ; ------------------------

                (vla-put-DisplayLocked
                  vpObj
                  :vlax-true
                )
              )
            )
          )
        )

        (setq i (1+ i))
      )

      (princ
        (strcat
          "\nFinished creating layouts."
        )
      )
    )

    (princ "\nNothing selected.")
  )

  ; -------------------------
  ; Restore vars
  ; -------------------------

  (setvar "CMDECHO" oldCmdEcho)
 

  (princ)
)


 
