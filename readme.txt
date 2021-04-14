ThunderTray 1.0.1

Minimize application to tray helper. Initially created for Thunderbird.

1. Why?
  - 88.0b1 crashed when restoring from tray and using "System Integration" -> "When Thunderbird is minimized, move it to the tray".
  - Starting from 88.0b1, the new message icon is not cleared if old messages are marked as unread.
  - Thunderbird hides the tray icon when restoring.

2. How to use
  - You can use ThunderTray.ini (see example in Thunderbird folder) or run with parameters:
    - ThunderTray C:\App.exe Param1 /Param2 ...
    - ThunderTray /SAFE C:\App.exe Param1 /Param2 ...
    - use custom ThunderTray.ini: ThunderTray C:\App.ini
    /SAFE means change EXSTYLE instead of calling ShowWindow
  - To use with Thunderbird:
    - Disable "System Integration" -> "When Thunderbird is minimized, move it to the tray"
    - Disable "Incoming mails" -> "Show a tray icon"
    - Enable "Incoming mails" -> "Show an alert" and set the alert for at least 5 secons (check the Customize button)

3. TODO:
  ? Use SetWindowsHookEx (unfortunately SetWindowsHookEx(WH_CBT, ...) doesn't give HCBT_MINMAX on Win+D https://github.com/AlekseyKolesnikov/SetWindowsHookEx-demo)
