#!/bin/bash

set -e -u -o pipefail

# Key remapping: CapsLock -> Control.
kb_properties="$(ioreg -c AppleHSSPIDevice -r -d1)"
kb_count="$(echo "$kb_properties" | grep -c idProduct)"
kb_vendor_id="$(echo "$kb_properties" | sed -n -e 's/^ *"idVendor" *= *//p')"
kb_product_id="$(echo "$kb_properties" | sed -n -e 's/^ *"idProduct" *= *//p')"


if [ "$kb_count" -eq 1 ] && [ ! -z "$kb_vendor_id" ] && [ ! -z "$kb_product_id" ]; then
    defaults -currentHost write NSGlobalDomain \
        "com.apple.keyboard.modifiermapping.${kb_vendor_id}-${kb_product_id}-0" -array \
        "<dict> \
            <key>HIDKeyboardModifierMappingSrc</key> \
            <integer>0</integer> \
            <key>HIDKeyboardModifierMappingDst</key> \
            <integer>2</integer> \
        </dict>"
else
    echo -e '\033[31mWARNING: Key remapping skipped.\033[0m' \
         'Unable to determine keyboard vendor and product IDs.'
fi

# Key codes (ref: http://apple.stackexchange.com/a/88096).
#     -1    None
#      0    Caps Lock
#      1    Shift (Left)
#      2    Control (Left)
#      3    Option (Left)
#      4    Command (Left)
#      5    Keypad 0
#      6    Help
#      9    Shift (Right)
#     10    Control (Right)
#     11    Option (Right)
#     12    Command (Right)
