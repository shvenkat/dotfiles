# Disable the Notification Center and remove the menu bar icon.
launchctl unload -w /System/Library/LaunchAgents/com.apple.notificationcenterui.plist 2> /dev/null
killall NotificationCenter

# Desktop background/wallpaper.
sudo ln -s '/Library/Desktop Pictures/Solid Colors/Solid Gray Light.png' \
    /System/Library/CoreServices/DefaultDesktop.jpg
