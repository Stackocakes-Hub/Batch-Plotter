Dim blinkInterval
Dim isVisible: isVisible=True
Dim windowinit: windowinit=False

Sub Window_Onload()
    blinkInterval = window.setInterval("ToggleBlinkByClass", 400)
	RefreshFolders = window.setInterval("PopulateActiveFolders", 2000)
	appPath = Replace(Unescape(Document.Location.href), "file:///", "")
    appPath = Replace(appPath, "/", "\")
    folderPath = Ofs.GetParentFolderName(appPath)
    Window.document.title = "Batch Plotter - " & folderPath
	Version.Writehtml Id.Text.Version
    Write Id.Button.Quitbutton.Name, Id.Button.Quitbutton.Value
    Write Id.Button.Getfolder.Name, Id.Button.Getfolder.Value
    Write Id.Button.bRefresh.Name, Id.Button.bRefresh.Value
    Write Id.Button.aCancel.Name, Id.Button.aCancel.Value
    Write Id.Button.Batchplot.Name, Id.Button.Batchplot.Value
    Write Id.Button.Selectall.Name, Id.Button.Selectall.Value
    Write Id.Text.Program, Program_Name
    For I = 0 To Ubound(Plot_Routine)
        Set Objoption = Document.Createelement("Option")
        Objoption.Text = Plot_Routine(I)
        Objoption.Value = Plot_Routine(I)
        Document.Getelementbyid(Id.List.Plotroutine).Add(Objoption)
    Next
    
	Resizewindow Windowwidth, Windowheight
    'On Error Resume Next
    savedFolder = Oshell.RegRead("HKEY_CURRENT_USER\Software\BatchPlotter\LastFolder") ' Check for saved folder in registry
    If Err.Number = 0 And savedFolder <> "" And Ofs.FolderExists(savedFolder) Then
        ' Use saved folder if it exists
        Populatelist(savedFolder)
    Else
        ' Fall back to Desktop if no valid saved folder
        Populatelist(Oshellapp.Namespace(&H10&).Self.Path)
    End If
	PopulateActiveFolders
    
	' Load saved plot routine index from registry (default to 3 if not set)
	Dim savedPlotIndex
	On Error Resume Next
	savedPlotIndex = Oshell.RegRead("HKEY_CURRENT_USER\Software\BatchPlotter\PlotRoutineIndex")
	If Err.Number <> 0 Then
		savedPlotIndex = 3 ' Default to "Paper Space (Auto-All Layouts)"
	End If
	On Error GoTo 0
	Document.GetElementById(Id.List.Plotroutine).SelectedIndex = savedPlotIndex
	
	'On Error Goto 0
	windowinit = True
End Sub

Sub Plotroutine_Onchange()
    Select Case Document.Getelementbyid(Id.List.Plotroutine).Options(Document.Getelementbyid(Id.List.Plotroutine).Selectedindex).Text
        Case Plot_Routine(0)
            Hide Id.Text.Command.Name, True
        Case Plot_Routine(1)
            Hide Id.Text.Command.Name, True
        Case Plot_Routine(2)
            Hide Id.Text.Command.Name, True
        Case Plot_Routine(3)
            Hide Id.Text.Command.Name, True
		Case Plot_Routine(4)
            Hide Id.Text.Command.Name, False
    End Select
	' Save current plot routine index to registry
	If windowinit = True then
		Dim currentIndex
		currentIndex = Document.Getelementbyid(Id.List.Plotroutine).Selectedindex
		On Error Resume Next
		Oshell.RegWrite "HKEY_CURRENT_USER\Software\BatchPlotter\PlotRoutineIndex", currentIndex, "REG_SZ"
		On Error GoTo 0
	End If
End Sub

Sub Quitbut_Onclick()
    Windowclose = True
    Close()
End Sub

Sub Getfolder_Onclick()
    Dim selectedFolder
    selectedFolder = Pickfolder(Pickfolder_Title)
    If selectedFolder <> "" Then
        ' Save selected folder to registry
        On Error Resume Next
        Oshell.RegWrite "HKEY_CURRENT_USER\Software\BatchPlotter\LastFolder", selectedFolder, "REG_SZ"
        On Error Goto 0
        Populatelist(selectedFolder)
	Else
		Populatelist("")
    End If
End Sub

Sub Bbplot_Onclick()
    Batch_Plot()
End Sub

Sub bRefresh_Onclick()
    Populatelist Oshell.RegRead("HKEY_CURRENT_USER\Software\BatchPlotter\LastFolder")
End Sub

Sub Selall_Onclick()
    Selectall()
End Sub

Sub pBody_OnHelp()
    Msgbox Credit
End Sub

Sub aCancel_Onclick()
	CancelCommand = True
	Write Id.Text.Progress, Progress_Update_Cancel & CancelTask & Progress_Update5
End Sub

Sub ToggleBlinkByClass()
	Dim elements, i, elem
    Set elements = document.getElementsByTagName("*")
    
    isVisible = Not isVisible  ' Toggle state
    
    For i = 0 To elements.length - 1
        Set elem = elements(i)
        
        ' Check if element has class "blink"
        If InStr(1, " " & elem.className & " ", " blink ") > 0 Then
            If isVisible Then
                elem.style.filter = "alpha(opacity=100)"
                elem.style.visibility = "visible"
            Else
                elem.style.filter = "alpha(opacity=50)"
                elem.style.visibility = "visible"
            End If
        End If
    Next
	sleep 1
End Sub