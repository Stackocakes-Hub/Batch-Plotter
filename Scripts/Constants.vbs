<FILE file_path="/home/workdir/attachments/Constants.vbs" size="5308 bytes">'Global Constants
Dim Ofs, Odw, Oshell, Oshellapp, Strcomputer, Spath, Acadapp, Currentpath
Dim Windowclose: Windowclose=False
Set Ofs = Createobject("Scripting.Filesystemobject")
Set Objwmiservice = Getobject("Winmgmts:\\.\Root\Cimv2")
Set Oshell = Createobject("Wscript.Shell")
Set Oshellapp = Createobject("Shell.Application")
Const Windowwidth = 900
Const Windowheight = 620
Dim Version: Set Version = New FileTracking
Dim appPath, folderPath
Dim savedFolder
        
'Document Object ID Class
Dim Id: Set Id = New Ids
Dim TaskCancelable: TaskCancelable=False
Dim CancelCommand: CancelCommand=False
Dim CancelTask: CancelTask="Blank"

'Autocad Activespace Constants
Const Acmodelspace = 1
Const Acpaperspace = 0

'Plot Array Variable
Dim Plot_Routine: Plot_Routine = Array("Previous Plot", "Model Space", "Paper Space", "Paper Space (Auto-All Layouts)", "Custom Command")
' Removed Plot_Orientation and Plot_Space arrays

'WMI Constant Queries
Const Win32_Process = "Select * From Win32_Process"
Const Win32_Desktopmonitor = "Select * From Win32_Desktopmonitor"
' Removed Win32_Printer and Win32_PrintJob

'Dynamic Text Constants
Const Currently_Printing1 = "Currently Printing: <Font Color='#0089FF'>"
Const Currently_Printing2 = "</Font> ... <Font Color='#0089FF'>Printing.</Font><Br>"
Const Currently_Printing3 = "</Font> ... <Font Color='#FF8900'>Spooling.</Font><Br>"
Const Currently_Printing4 = "</Font> ... <Br>"
Const Currently_Printing5 = "</Font> ... <Font Color='#008900'>Done.</Font><Br>"
Const Currently_Printing6 = "</Font> ... <Font Color='#FF8900'>Timed out.</Font><Br>"
Const Currently_Printing7 = "</Font> ... <Font Color='#0089FF' class='blink'>Opening Dwg.</Font><Br>"
Const Finished_Printing = "Currently Printing: <Font Color='#00ff00'>Finished.</Font><Br>"
Const Directory_Listed1 = "Directory Listed: "
Const Directory_Listed2 = "<Br>"
Const Directory_None = " <I>None</I>"
Const Progress_Update1 = "Current Progress: <Font Color='#0089ff'>"
Const Progress_Update2 = "%</font>"
Const Progress_Update3 = "&nbsp;&nbsp;Current Drawing: <Font Color='#0089ff'>"
Const Progress_Update4 = "</font> out of <Font Color='#0089ff'>"
Const Progress_Update5 = "</font>"
Const Progress_Update_Cancel = "<Font style='color:Red'>Canceling command: "
Const Program_Name = "<Font Color='#0089ff'>Batch Plotter</font>"

'Msgbox Variables
Const PickFolder_Title = "Choose A Folder To List"

Class Ids
	Dim List, Button, Text, Input
	Private Sub Class_Initialize
		Set List = New cList
		Set Button = New cButton
		Set Text = New cText
		Set Input = New cInput
	End Sub
End Class

Class cList
	Dim Files, Plotroutine, ActiveFolders ' Removed Papersizes, Printerlist, Plotorientation, Printspace
	Private Sub Class_Initialize
		Files = "Sfolderlist"
		Plotroutine = "Plotroutine"
		ActiveFolders = "ActiveFolders"
	End Sub
End Class

Class cInput
	' Removed Discipline
	Private Sub Class_Initialize
	End Sub
End Class

Class cButton
	Dim Getfolder, Batchplot, Selectall, QuitButton, bRefresh, aCancel ' Removed Printerpref
	Private Sub Class_Initialize
		Set Getfolder = NewSubClass("Getfolder", "Pick Folder")
		Set Batchplot = NewSubClass("Bbplot", "Batch Plot")
		Set bRefresh = NewSubClass("bRefresh", "&#8635;")
		Set aCancel = NewSubClass("aCancel", "Cancel")
		Set Selectall = NewSubClass("Selall", "Select All")
		Set QuitButton = NewSubClass("Quitbut", "Close")
	End Sub
End Class

Class cText
	Dim Directory, Fileprint, Progress, Version, Program, Command ' Removed Discipline
	Private Sub Class_Initialize
		Directory = "Foldertext"
		Fileprint = "Fprint"
		Progress = "Fprintprogress"
		Version = "dVersion"
		Program = "Program"
		Set Command = NewSubClass("sCommand","tCommand")
	End Sub
End Class

Class FileTracking
	Dim File()
	Private Sub Class_Initialize
		Redim File(6)
		Set File(0) = NewSubClass("Automation.vbs","")
		Set File(1) = NewSubClass("Base.vbs","")
		Set File(2) = NewSubClass("Constants.vbs","")
		Set File(3) = NewSubClass("Scripting.vbs","")
		Set File(4) = NewSubClass("ActiveFolders.vbs","")
		Set File(5) = NewSubClass("Window.css","")
		Set File(6) = NewSubClass("Batch Plot.Hta","")
	End Sub
	Public Sub WriteHTML(t)
		Dim i, fullpath, f, d, scriptPath
		scriptPath = folderPath & "\Scripts\"
		For i = 0 To Ubound(File)
			If i < 5 Then
				fullpath = scriptPath & File(i).Name
			Else
				fullpath = appPath
			End If
			Set f = Ofs.GetFile(fullpath)
			d = f.DateLastModified
			File(i).Value = CStr(Year(d)) & "." & Right("0" & Month(d), 2) & "." & Right("0" & Day(d), 2)
		Next
		Msg = ""
		Msg = Msg & "<Table>" & VbLf
		Msg = Msg & "<Tr><td class=cVersion align=right><b>File Versions</b></td></tr>" & VbLf
		For i = 0 To Ubound(File)
			Msg = Msg & "<Tr><td class=cVersion align=right>" & File(i).Name & "</td><td class=cVersion align=center>_</td><td class=cVersion align=left>" & File(i).Value & "</td></tr>" & VbLf
		Next
		Msg = Msg & "</table><br>"
		Write t, Msg
	End Sub
End Class

Function NewSubClass(Nm, Bd)
	Set NewSubClass = New SubClass
	NewSubClass.Value = Bd
	NewSubClass.Name = Nm
End Function

Class SubClass
	Dim Value
	Dim Name
End Class

Const Credit = "Created by: Scott Hawkinson"</FILE>