<FILE file_path="/home/workdir/attachments/Plot - AllPublish.lsp" size="11673 bytes">(defun PlotAllLayoutsToPDF (/ doc layouts outputFile layoutList dsdFile err oldFileDia pdebug)
  (vl-load-com)
  ;; Set debug toggle (1 = on, 0 = off)
  (setq pdebug nil)  ; Change this to nil to disable debug messages
  
  ;; Error handler
  (setq err *error*)
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg))
    )
    (if dsdFile (vl-file-delete dsdFile))  ; Clean up DSD file on error
    (if oldFileDia (setvar "FILEDIA" oldFileDia))  ; Restore FILEDIA
    (princ)
  )
  
  ;; Get the active document
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (if pdebug (princ "\nStatus: Retrieved active document."))
  
  ;; Get the layouts collection
  (setq layouts (vla-get-Layouts doc))
  (if pdebug (princ "\nStatus: Retrieved layouts collection."))
  
  ;; Set output PDF file name (single file for all layouts)
  (setq outputFile 
    (strcat 
      (getvar "DWGPREFIX") 
      (vl-filename-base (getvar "DWGNAME")) 
      ".pdf"
    )
  )
  (if pdebug (princ (strcat "\nStatus: Output file set to " outputFile)))
  
  ;; Apply plot settings to all paperspace layouts
  (vlax-for layout layouts
    (if (/= (vla-get-Name layout) "Model")
      (progn
        ;; Activate the layout to ensure extents are calculated correctly
        (vla-put-ActiveLayout doc layout)
        (if pdebug (princ (strcat "\nStatus: Switched to layout " (vla-get-Name layout))))
        
        ;; Get extents for the current layout
        (setq minPt (getvar "EXTMIN")) ; Lower-left corner of extents
        (setq maxPt (getvar "EXTMAX")) ; Upper-right corner of extents
        
        ;; Calculate width (X difference) and height (Y difference)
        (setq width (- (car maxPt) (car minPt))) ; X max - X min
        (setq height (- (cadr maxPt) (cadr minPt))) ; Y max - Y min
        
        ;; Determine orientation
        (if (> width height)
          (progn
            (if pdebug (princ (strcat "\nStatus: " (vla-get-Name layout) " is wider than tall (Landscape)\n")))
            (setq plotRotation 1) ; Landscape
          )
          (progn
            (if pdebug (princ (strcat "\nStatus: " (vla-get-Name layout) " is taller than or equal to width (Portrait)\n")))
            (setq plotRotation 0) ; Portrait
          )
        )
        
        ;; Apply plot settings with dynamic rotation
        (if (vl-catch-all-error-p 
              (vl-catch-all-apply 
                '(lambda ()
                   (vla-put-ConfigName layout "AutoCAD PDF (General Documentation).pc3")
                   (vla-put-CanonicalMediaName layout "ANSI_expand_B_(11.00_x_17.00_Inches)")
                   (vla-put-PlotRotation layout plotRotation) ; Use calculated rotation
                   (vla-put-PlotType layout 1)
                   (vla-put-ScaleLineweights layout :vlax-false)
                   (vla-put-PlotWithPlotStyles layout :vlax-true)
                   (vla-put-StyleSheet layout "MSY_V2004.ctb")
                   (vla-put-CenterPlot layout :vlax-true)
                   (vla-put-StandardScale layout 0)
                 )
              )
            )
          (princ (strcat "\nError: Failed to apply plot settings to " (vla-get-Name layout)))
          (if pdebug (princ (strcat "\nStatus: Applied plot settings to " (vla-get-Name layout))))
        )
      )
    )
  )
  
  ;; Build list of paperspace layout names into a list
  (setq layoutList '())
  (vlax-for layout layouts
    (if (/= (vla-get-Name layout) "Model")
      (setq layoutList (append layoutList (list (vla-get-Name layout))))
    )
  )
  (if pdebug (princ (strcat "\nStatus: Layout list built - " (vl-princ-to-string layoutList))))
  
  ;; Check for layouts
  (if (null layoutList)
    (progn
      (princ "\nError: No paperspace layouts found in the drawing.")
      (exit)
    )
  )
  
  ;; Create temporary DSD file for multi-page PDF
  ;(setq dsdFile (strcat (getvar "DWGPREFIX") "temp.dsd"))
  (setq dsdFile (strcat "C:\\Temp\\" (vl-filename-base (getvar "DWGNAME")) "temp.dsd"))
  (write-dsd-file dsdFile outputFile layoutList)
  (if pdebug (princ (strcat "\nStatus: Created DSD file at " dsdFile)))
  
  ;; Temporarily set FILEDIA to 0 to suppress dialogs
  (setq oldFileDia (getvar "FILEDIA"))
  (setvar "FILEDIA" 0)
  (if pdebug (princ "\nStatus: Set FILEDIA to 0"))
  
  ;; Publish to single PDF
  (command "-PUBLISH" dsdFile)
  (if pdebug (princ "\nStatus: Executed -PUBLISH command"))
  
  ;; Restore FILEDIA
  (setvar "FILEDIA" oldFileDia)
  (if pdebug (princ "\nStatus: Restored FILEDIA"))
  
  ;; Check if publish succeeded
  (if (findfile outputFile)
    (progn
      (princ "\nAll layouts plotted to: ")
      (princ outputFile)
      (if pdebug (princ (strcat "\nStatus: Successfully plotted to " outputFile)))
    )
    (princ "\nError: Failed to publish layouts to PDF.")
  )
  
  ;; Clean up temporary DSD file
  (vl-file-delete dsdFile)
  (if pdebug (princ "\nStatus: Deleted temporary DSD file"))
  
  ;; Reset error handler
  (setq *error* err)
  (princ)
)

;; Helper function to create DSD file matching provided format
(defun write-dsd-file (dsdFile pdfFile layoutNames / file dwgPath pdebug sheetIndex)
  (setq pdebug nil)  ; Debug toggle, consistent with parent function
  (setq file (open dsdFile "w"))
  (setq dwgPath (strcat (getvar "DWGPREFIX") (getvar "DWGNAME")))
  
  ;; DSD Version
  (write-line "[DWF6Version]" file)
  (if pdebug (princ "\nDebug: Wrote [DWF6Version]"))
  (write-line "Ver=1" file)
  (if pdebug (princ "\nDebug: Wrote Ver=1"))
  (write-line "[DWF6MinorVersion]" file)
  (if pdebug (princ "\nDebug: Wrote [DWF6MinorVersion]"))
  (write-line "MinorVer=1" file)
  (if pdebug (princ "\nDebug: Wrote MinorVer=1"))
  
  ;; Write individual sheet entries for each layout
  (setq sheetIndex 0)
  (foreach layout layoutNames
    (write-line (strcat "[DWF6Sheet:" (vl-filename-base (getvar "DWGNAME")) "-" layout "]") file)
    (if pdebug (princ (strcat "\nDebug: Wrote [DWF6Sheet:" (vl-filename-base (getvar "DWGNAME")) "-" layout "]")))
    (write-line (strcat "DWG=" dwgPath) file)
    (if pdebug (princ (strcat "\nDebug: Wrote DWG=" dwgPath)))
    (write-line (strcat "Layout=" layout) file)
    (if pdebug (princ (strcat "\nDebug: Wrote Layout=" layout)))
    (write-line "Setup=" file)
    (if pdebug (princ "\nDebug: Wrote Setup="))
    (write-line (strcat "OriginalSheetPath=" dwgPath) file)
    (if pdebug (princ (strcat "\nDebug: Wrote OriginalSheetPath=" dwgPath)))
    (write-line "Has Plot Port=0" file)
    (if pdebug (princ "\nDebug: Wrote Has Plot Port=0"))
    (write-line "Has3DDWF=0" file)
    (if pdebug (princ "\nDebug: Wrote Has3DDWF=0"))
    (setq sheetIndex (1+ sheetIndex))
  )
  
  ;; Target section
  (write-line "[Target]" file)
  (if pdebug (princ "\nDebug: Wrote [Target]"))
  (write-line "Type=6" file)
  (if pdebug (princ "\nDebug: Wrote Type=6"))
  (write-line (strcat "DWF=" pdfFile) file)
  (if pdebug (princ (strcat "\nDebug: Wrote DWF=" pdfFile)))
  (write-line (strcat "OUT=" (getvar "DWGPREFIX")) file)
  (if pdebug (princ (strcat "\nDebug: Wrote OUT=" (getvar "DWGPREFIX"))))
  (write-line "PWD=" file)
  (if pdebug (princ "\nDebug: Wrote PWD="))
  
  ;; MRU block template
  (write-line "[MRU block template]" file)
  (if pdebug (princ "\nDebug: Wrote [MRU block template]"))
  (write-line "MRU=0" file)
  (if pdebug (princ "\nDebug: Wrote MRU=0"))
  
  ;; MRU Local (simplified to one recent path)
  (write-line "[MRU Local]" file)
  (if pdebug (princ "\nDebug: Wrote [MRU Local]"))
  (write-line "MRU=1" file)
  (if pdebug (princ "\nDebug: Wrote MRU=1"))
  (write-line (strcat "File0=" (getvar "DWGPREFIX")) file)
  (if pdebug (princ (strcat "\nDebug: Wrote File0=" (getvar "DWGPREFIX"))))
  
  ;; MRU Sheet List
  (write-line "[MRU Sheet List]" file)
  (if pdebug (princ "\nDebug: Wrote [MRU Sheet List]"))
  (write-line "MRU=0" file)
  (if pdebug (princ "\nDebug: Wrote MRU=0"))
  
  ;; PDF Options
  (write-line "[PdfOptions]" file)
  (if pdebug (princ "\nDebug: Wrote [PdfOptions]"))
  (write-line "IncludeHyperlinks=FALSE" file)
  (if pdebug (princ "\nDebug: Wrote IncludeHyperlinks=FALSE"))
  (write-line "CreateBookmarks=TRUE" file)
  (if pdebug (princ "\nDebug: Wrote CreateBookmarks=TRUE"))
  (write-line "CaptureFontsInDrawing=TRUE" file)
  (if pdebug (princ "\nDebug: Wrote CaptureFontsInDrawing=TRUE"))
  (write-line "ConvertTextToGeometry=FALSE" file)
  (if pdebug (princ "\nDebug: Wrote ConvertTextToGeometry=FALSE"))
  (write-line "VectorResolution=1200" file)
  (if pdebug (princ "\nDebug: Wrote VectorResolution=1200"))
  (write-line "RasterResolution=400" file)
  (if pdebug (princ "\nDebug: Wrote RasterResolution=400"))
  
  ;; AutoCAD Block Data
  (write-line "[AutoCAD Block Data]" file)
  (if pdebug (princ "\nDebug: Wrote [AutoCAD Block Data]"))
  (write-line "IncludeBlockInfo=0" file)
  (if pdebug (princ "\nDebug: Wrote IncludeBlockInfo=0"))
  (write-line "BlockTmplFilePath=" file)
  (if pdebug (princ "\nDebug: Wrote BlockTmplFilePath="))
  
  ;; SheetSet Properties
  (write-line "[SheetSet Properties]" file)
  (if pdebug (princ "\nDebug: Wrote [SheetSet Properties]"))
  (write-line "IsSheetSet=FALSE" file)
  (if pdebug (princ "\nDebug: Wrote IsSheetSet=FALSE"))
  (write-line "IsHomogeneous=FALSE" file)
  (if pdebug (princ "\nDebug: Wrote IsHomogeneous=FALSE"))
  (write-line "SheetSet Name=" file)
  (if pdebug (princ "\nDebug: Wrote SheetSet Name="))
  (write-line "NoOfCopies=1" file)
  (if pdebug (princ "\nDebug: Wrote NoOfCopies=1"))
  (write-line "PlotStampOn=FALSE" file)
  (if pdebug (princ "\nDebug: Wrote PlotStampOn=FALSE"))
  (write-line "ViewFile=FALSE" file)
  (if pdebug (princ "\nDebug: Wrote ViewFile=FALSE"))
  (write-line "JobID=0" file)
  (if pdebug (princ "\nDebug: Wrote JobID=0"))
  (write-line "SelectionSetName=" file)
  (if pdebug (princ "\nDebug: Wrote SelectionSetName="))
  (write-line "AcadProfile=JacobsMetairie_2017_SaveAs_2010.arg" file)
  (if pdebug (princ "\nDebug: Wrote AcadProfile=JacobsMetairie_2017_SaveAs_2010.arg"))
  (write-line "CategoryName=" file)
  (if pdebug (princ "\nDebug: Wrote CategoryName="))
  (write-line "LogFilePath=" file)
  (if pdebug (princ "\nDebug: Wrote LogFilePath="))
  (write-line "IncludeLayer=FALSE" file)
  (if pdebug (princ "\nDebug: Wrote IncludeLayer=FALSE"))
  (write-line "LineMerge=FALSE" file)
  (if pdebug (princ "\nDebug: Wrote LineMerge=FALSE"))
  (write-line "CurrentPrecision=" file)
  (if pdebug (princ "\nDebug: Wrote CurrentPrecision="))
  (write-line "PromptForDwfName=TRUE" file)
  (if pdebug (princ "\nDebug: Wrote PromptForDwfName=TRUE"))
  (write-line "PwdProtectPublishedDWF=FALSE" file)
  (if pdebug (princ "\nDebug: Wrote PwdProtectPublishedDWF=FALSE"))
  (write-line "PromptForPwd=FALSE" file)
  (if pdebug (princ "\nDebug: Wrote PromptForPwd=FALSE"))
  (write-line "RepublishingMarkups=FALSE" file)
  (if pdebug (princ "\nDebug: Wrote RepublishingMarkups=FALSE"))
  (write-line "PublishSheetSetMetadata=FALSE" file)
  (if pdebug (princ "\nDebug: Wrote PublishSheetSetMetadata=FALSE"))
  (write-line "PublishSheetMetadata=FALSE" file)
  (if pdebug (princ "\nDebug: Wrote PublishSheetMetadata=FALSE"))
  (write-line "3DDWFOptions=0 1" file)
  (if pdebug (princ "\nDebug: Wrote 3DDWFOptions=0 1\n"))
  
  (close file)
  (command "_.delay" "100")
)

;; Run the function
(PlotAllLayoutsToPDF)</FILE>