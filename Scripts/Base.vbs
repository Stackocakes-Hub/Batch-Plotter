<FILE file_path="/home/workdir/attachments/Base.vbs" size="5155 bytes">Function Pickfolder(Strquestion)
	Dim F
	Set F = Oshellapp.Browseforfolder(0, Strquestion, 16+32+64+512, 0)
	If (Not F Is Nothing) Then
		If F = "Desktop" Then
			Pickfolder = Oshellapp.Namespace(&H10&).Self.Path  ' Get Desktop path
		Else
			Pickfolder = F.Items.Item.Path
		End If
	End If
	Set F = Nothing
End Function

Sub Populatelist(Spath)
	Dim cFileName
	SetCancelable True, "Populating List of Drawings"
	Write Id.Text.Directory, "<font style=""Color:Red"" class=""blink"">Reading Directory: </font>" & Spath & ""
	Sleep 1
	If Spath <> "" Then
		Currentpath = Spath
		For Each Objoption In Sfolderlist.Options
			Objoption.Removenode
		Next
		If Ofs.Folderexists(Spath) Then
			Set Ofolder = Ofs.Getfolder(Spath)
			For Each Ofile In Ofolder.Files
				If CancelCommand then Exit For
				cFileName = Ofile.Name
				If LCase(ofs.GetExtensionName(cFileName)) = "dwg" Then
					Set Objoption = Document.Createelement("Option")
					Objoption.Text = cFileName
					Objoption.Value = Spath & "\"
					Sfolderlist.Add(Objoption)
					'Sleep 1
				End If
			Next
		End If
		Write Id.Text.Directory, Directory_Listed1 & Spath & Directory_Listed2
	Else
		Write Id.Text.Directory, Directory_Listed1 & Directory_None & Directory_Listed2
	End If
	SetCancelable False, "Canceled Drawing list population."
End Sub

Sub Batch_Plot()
	Lock Id.List.Files, True
	Lock Id.List.Plotroutine, True
	Lock Id.Button.Getfolder.Name, True
	Lock Id.Button.Batchplot.Name, True
	Lock Id.Button.Selectall.Name, True
	Lock Id.Button.bRefresh.Name, True
	SetCancelable True, "Batch Plot Routine"
	' Removed printer selection loop
	
	Dim Numprints: Numprints = 0
	For Each Objoption In Sfolderlist.Options
		If Objoption.Selected Then Numprints = Numprints + 1
	Next
	
	Dim Curprint: Curprint = 0
	Dim Progress: Progress = 0
	Dim Autocloseacad: Autocloseacad = False
	Updateprogress Progress, Numprints
	
	On Error Resume Next
	Set Acadapp = GetObject(, "Autocad.Application")
	If Err.Number = 0 Then
		Dim acadVersion
		acadVersion = Acadapp.Caption
		Write "AcadVersion", "AutoCAD Linked: " & acadVersion
	Else
		Err.Clear
		Set Acadapp = CreateObject("Autocad.Application")
		If Err.Number = 0 Then
			acadVersion = Acadapp.Caption
			Write "AcadVersion", "AutoCAD Linked: " & acadVersion
			Autocloseacad = True
		Else
			Write "AcadVersion", "Error: Could not open AutoCAD"
			CancelCommand = True
		End If
	End If
	On Error Goto 0
	
	For Each Objoption In Sfolderlist.Options
		If Windowclose Then Exit Sub
		If CancelCommand then Exit For
		If Objoption.Selected Then
			Write Id.Text.Fileprint, Currently_Printing1 & Objoption.Text & Currently_Printing4
			Printdwg _
				Progress+.5, _
				Numprints, _
				Objoption.Value, _
				Objoption.Text
			Write Id.Text.Fileprint, Currently_Printing1 & Objoption.Text & Currently_Printing5
			Progress = Progress + 1
			Updateprogress Progress, Numprints
		End If
	Next
	Lock Id.List.Files, False
	Lock Id.List.Plotroutine, False
	Lock Id.Button.Getfolder.Name, False
	Lock Id.Button.Batchplot.Name, False
	Lock Id.Button.Selectall.Name, False
	Lock Id.Button.bRefresh.Name, False
	Write Id.Text.Fileprint, Finished_Printing
	SetCancelable False, "Canceled Batch Plot. Completed " & Progress & " of " & Numprints & " prints." 	
	If Autocloseacad Then
		For Each Objprocess In Objwmiservice.Execquery (Win32_Process & " Where Name='Acad.Exe'")
			Objprocess.Terminate()
		Next
	End If
End Sub

Sub Selectall()
	For Each Objoption In Sfolderlist.Options
		Objoption.Selected = True
	Next
End Sub

Sub Updateprogress(Scurrent, Stotal)
	If Scurrent = 0 Then
		Write Id.Text.Progress, Progress_Update1 & Round(Scurrent/Stotal*100) & Progress_Update2
	ElseIf Scurrent > 0 And Scurrent < Stotal Then
		Write Id.Text.Progress, Progress_Update1 & Round(Scurrent/Stotal*100) & Progress_Update2 & Progress_Update3 & Int(Scurrent+1) & Progress_Update4 & Stotal & Progress_Update5
	Else
		Write Id.Text.Progress, Progress_Update1 & Round(Scurrent/Stotal*100) & Progress_Update2
	End If
	Sleep 1
End Sub

Sub Lock(Id, Status)
	Document.Getelementbyid(Id).Disabled = Status
End Sub

Sub Hide(Id, Status)
	If Status Then Document.Getelementbyid(Id).Style.Visibility="hidden" Else Document.Getelementbyid(Id).Style.Visibility=""
End Sub

Sub Write(Id, Status)
	Document.Getelementbyid(Id).Innerhtml = Status
End Sub

Sub SetCancelable(Status, task)
	Lock Id.Button.Getfolder.Name, Status
	Lock Id.Button.Batchplot.Name, Status
	Lock Id.Button.Selectall.Name, Status
	Lock Id.Button.bRefresh.Name, Status
	Lock Id.List.ActiveFolders, Status
	TaskCancelable = Status
	Hide Id.Button.aCancel.Name, Not Status	
	CancelTask=task
	If Not IsNull(task) Then
		if status = True Then 
			Write Id.Text.Progress, "Executing command: <Font Color='#008900'>" & task & "</font>"
		Else
			If CancelCommand then 
				Write Id.Text.Progress, "<Font style='Color:Red'>" & task & "</font>"
			Else
				Write Id.Text.Progress, ""
			end if
		end if
	End If
	CancelCommand = Not Status
	Sleep 1
End Sub</FILE>