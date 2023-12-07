tell application "System Events"
    tell appearance preferences
        if (dark mode is true) then
            return "Dark"
        else
            return "Light"
        end if
    end tell
end tell
