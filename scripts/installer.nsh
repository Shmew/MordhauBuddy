!macro customUnInstall
    SetRegView 64
     DeleteRegValue HKCU "Software\Microsoft\Windows\CurrentVersion\Run" "MordhauBuddy"
    SetRegView 32
     DeleteRegValue HKCU "Software\Microsoft\Windows\CurrentVersion\Run" "MordhauBuddy"
    RMDir /r "$APPDATA\mordhau-buddy"
 !macroend
