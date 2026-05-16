Sub Printdwg(Progress, Numprints, Sfoldername, Sfilename) ' Removed unused parameters
		Write Id.Text.Fileprint, Currently_Printing1 & Sfilename & Currently_Printing7
		Sleep 1
		Acadapp.Documents.Open(Sfoldername & Sfilename)
		Updateprogress Progress, Numprints
		Write Id.Text.Fileprint, Currently_Printing1 & Sfilename & Currently_Printing2
		Sleep 1
		' Get the Scripts folder path
		scriptFolder = folderPath & "\Scripts"
		
		With Acadapp.Activedocument
			Select Case Document.Getelementbyid(Id.List.Plotroutine).Options(Document.Getelementbyid(Id.List.Plotroutine).Options.Selectedindex).Value
				Case Plot_Routine(0) ' Previous Plot
					If .ActiveSpace = Acmodelspace Then
						.Sendcommand ( _
							"(command ""_SCRIPT"" """ & Replace(scriptFolder, "\", "\\") & "\\Model PreviousPlot.scr"")") & VbLf
						On Error Resume Next
						.Close False
						On Error GoTo 0
					ElseIf .ActiveSpace = Acpaperspace Then
						.Sendcommand ( _
							"(command ""_SCRIPT"" """ & Replace(scriptFolder, "\", "\\") & "\\Layout PreviousPlot.scr"")") & VbLf
						On Error Resume Next
						.Close False
						On Error GoTo 0						
					Else
						MsgBox "Error selecting script. Var:currentSpace undefined"
					End If
				Case Plot_Routine(1) ' Model Space
					.Sendcommand ( _
						"(command ""_SCRIPT"" """ & Replace(scriptFolder, "\", "\\") & "\\Model 11x17.scr"")") & VbLf
					On Error Resume Next
					.Close False
					On Error GoTo 0
				Case Plot_Routine(2) ' Paper Space
					.Sendcommand ( _
						"(command ""_SCRIPT"" """ & Replace(scriptFolder, "\", "\\") & "\\Layout 11x17.scr"")") & VbLf
					On Error Resume Next
					.Close False
					On Error GoTo 0
				Case Plot_Routine(3) ' Custom Paper Space Auto-All Script
					.Sendcommand ( _
						"(Load """ & Replace(scriptFolder, "\", "\\") & "\\Plot - AllPublish.lsp""")))" ) & VbLf
					On Error Resume Next
					.Close False
					On Error GoTo 0					
				Case Plot_Routine(4) ' Custom Command
					TCOM = Replace(Replace(Document.Getelementbyid(Id.Text.Command.Value).Value, ";", VbLf), "^C", Chr(27))
					.Sendcommand ( _
						"Set" & VbLf _
						& "sdi" & VbLf _
						& "0" & VbLf _
						& TCOM & VbLf _
						& "_QSAVE" & VbLf _
						& "_CLOSE" & VbLf)
					On Error Resume Next
					.Close False
					On Error GoTo 0
			End Select
		End With
		Sleep 1
End Sub

Function Sleep(Msec)
	Oshell.Run "ping -n 1 127.0.0.1", 0, True 
	'Note: Running an external app even if it fails accomplishes the goal of VBA-like DoEvents()
End Function

Function Iff(String1, String2, String3, String4)
	If String1=String2 Then Iff=String3 Else Iff=String4
End Function

Sub Resizewindow(Width, Height)
	For Each Objitem In Objwmiservice.Execquery(Win32_Desktopmonitor)
		Inthorizontal = Objitem.Screenwidth
		Intvertical = Objitem.Screenheight
	Next
	Window.Resizeto Width, Height 
	Window.Moveto (Inthorizontal - Width) / 2, (Intvertical - Height) / 2
End Sub