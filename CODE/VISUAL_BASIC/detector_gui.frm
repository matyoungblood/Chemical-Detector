VERSION 5.00
Begin VB.Form Form1 
   Caption         =   "Form1"
   ClientHeight    =   7335
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   9885
   LinkTopic       =   "Form1"
   ScaleHeight     =   7335
   ScaleWidth      =   9885
   StartUpPosition =   3  'Windows Default
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Form_Load()
 MSComm1.CommPort = 2 ' Place your comport here 1=com1; 2=com2
 ' 9600 baud, no parity, 8 data, and 1 stop bit.
 MSComm1.Settings = "9600,N,8,1"
 ' Tell the control to read entire buffer when Input
 ' is used.
 MSComm1.InputLen = 1
 ' Open the port.
 MSComm1.PortOpen = True
 ' Initialize DTR and RTS to OFF
 MSComm1.DTREnable = False
 MSComm1.RTSEnable = False
 'use QBColor to set label colors
 lblDTR.BackColor = QBColor(12) '12=red 10=green
 lblRTS.BackColor = QBColor(12) '12=red 10=green
End Sub

Private Sub Form_Unload(Cancel As Integer)
 MSComm1.PortOpen = False
End Sub
