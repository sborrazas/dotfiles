#!/bin/bash

keyboard_id="$(ioreg -n IOHIDKeyboard -r | grep -e VendorID\" -e ProductID | tr -d \"\|[:blank:] | cut -d\= -f2 | tr '\n' -)"

echo "Switching to emacs modifiers"
defaults -currentHost write -g com.apple.keyboard.modifiermapping.${keyboard_id}0 '(
            {
        HIDKeyboardModifierMappingDst = 2;
        HIDKeyboardModifierMappingSrc = 0; },
            {
        HIDKeyboardModifierMappingDst = "-1";
        HIDKeyboardModifierMappingSrc = 2; },
            {
        HIDKeyboardModifierMappingDst = "-1";
        HIDKeyboardModifierMappingSrc = 10; })'
