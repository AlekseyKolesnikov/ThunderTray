Minimize Thunderbird to tray helper 0.2

1. Why?
  - 88.0b1 crashes when restoring from tray and using "System Integration" -> "When Thunderbird is minimized, move it to the tray".
  - In 88.0b1, the new message icon is not cleared if old messages are marked as unread.
  - Thunderbird hides the tray icon when restoring.

2. How to use
  - Disable "System Integration" -> "When Thunderbird is minimized, move it to the tray"
  - Disable "Incoming mails" -> "Show a tray icon"
  - Enable "Incoming mails" -> "Show an alert" and set the alert for at least 5 secons (check the Customize button)
  - Place in Thunderbird folder and run. If thunderbird.exe is not found, C:\Program Files\Mozilla Thunderbird\thunderbird.exe will start

3. TODO:
  - Use SetWindowsHookEx
  - Check the new message popup using dwStyle = 94000000
  - Settings
