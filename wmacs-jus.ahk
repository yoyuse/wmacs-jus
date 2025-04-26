; --------------------------------------------------------------------
; wmacs-jus.ahk
; --------------------------------------------------------------------
;
; ----------------------------------------------------------------------------
; ¦    ¦F1  ¦F2  ¦F3  ¦F4  ¦F5  ¦F6  ¦F7  ¦F8  ¦F9  ¦F10 ¦F11 ¦F12 ¦    ¦    ¦
; ¦Esc ¦1 ! ¦2 @ ¦3 # ¦4 $ ¦5 % ¦6 ^ ¦7 & ¦8 * ¦9 ( ¦0 ) ¦- _ ¦= + ¦\ | ¦BS  ¦
; ----------------------------------------------------------------------------
; ¦      ¦Quot¦    ¦End ¦    ¦    ¦    ¦    ¦    ¦    ¦Up  ¦PgUp¦PgDn¦       ¦
; ¦Tab   ¦q Q ¦w W ¦e E ¦r R ¦t T ¦y Y ¦u U ¦i I ¦o O ¦p P ¦[ { ¦] } ¦Enter  ¦
; ---------------------------------------------------------------------      ¦
; ¦       ¦Home¦    ¦Del ¦Rt  ¦    ¦BS  ¦    ¦    ¦    ¦^Up ¦^Dn ¦    ¦      ¦
; ¦Eisu   ¦a A ¦s S ¦d D ¦f F ¦g G ¦h H ¦j J ¦k K ¦l L ¦; : ¦' " ¦\ | ¦      ¦
; ----------------------------------------------------------------------------
; ¦         ¦    ¦    ¦    ¦    ¦Lt  ¦Dn  ¦Ente¦^Hom¦^End¦Undo¦    ¦         ¦
; ¦Shift    ¦z Z ¦x X ¦c C ¦v V ¦b B ¦n N ¦m M ¦, < ¦. > ¦/ ? ¦` ~ ¦Shift    ¦
; ----------------------------------------------------------------------------
; ¦    ¦    ¦    ¦    ¦      ¦          ¦      ¦    ¦    ¦    ¦              ¦
; ¦Ctrl¦Fn  ¦Win ¦ALt ¦Mu    ¦          ¦He    ¦H/Z ¦Menu¦Ctrl¦              ¦
; ----------------------------------------------------------------------------
;

#Requires AutoHotkey v2.0

WmacsVersion := "2025-04-25"

; 2025-04-25 AltOneShotToMuHenkan with timeout
; 2025-04-24 remove remap Ins
; 2025-04-21 fix AltOneShotToMuHenkan
; 2025-04-20 WmacsBind; comment out: OnClipboardChange ClipChanged
; 2025-04-20 RemapRWinToRCtrl → RWinToRCtrl; !Use104On104 → JUSLayout
; 2025-04-20 AltOneShotToMuHenkan; !UseHHK → HankakuZenkakuToEsc
; 2025-04-19 wmacs-jus.ico
; 2025-04-17 fix remap Ins
; 2025-04-16 RemapRWinToRCtrl, UseHHK; remove TTT
; 2025-04-12 wmacs-jus.ahk from wim-jus-ahk2.ahk 2024-07-03

; --------------------------------------------------------------------
; Misc
; --------------------------------------------------------------------

#SingleInstance
#Warn
SendMode "Input"
SetWorkingDir A_ScriptDir

; XXX
; SendMode "Event"
SetKeyDelay 40

; --------------------------------------------------------------------
; Usehook
; --------------------------------------------------------------------

InstallKeybdHook
#UseHook

; --------------------------------------------------------------------
; 変数
; --------------------------------------------------------------------

C_q := 0

quoted_insert() {
    global C_q
    s := "C-q"
    if CaretGetPos(&x, &y) {
        ToolTip s, x, y + 20, 2
    } else {
        ToolTip s, , , 2
    }
    C_q := 1
}

SendBlind(key) {
    global RWinToRCtrl
    modif := ""
    if GetKeyState("RCtrl") {
        modif := modif "^"
    }
    if GetKeyState("Shift", "P") {
        modif := modif "+"
    }
    ; if GetKeyState(RWinToRCtrl ? "LAlt" : "Alt", "P") {
    if GetKeyState("Alt", "P") {
        modif := modif "!"
    }
    ; if GetKeyState("LWin", "P") || GetKeyState("RWin", "P") {
    if GetKeyState("LWin", "P") || !RWinToRCtrl && GetKeyState("RWin", "P") {
        modif := modif "#"
    }
    hkey := modif key
    Send hkey
}

; --------------------------------------------------------------------
; Group NoWmacs
; --------------------------------------------------------------------

GroupAdd "NoWmacs", "ahk_class Emacs"                 ; Emacs
GroupAdd "NoWmacs", "ahk_class gdkWindowToplevel"     ; GIMPPortable, Inkscpae
GroupAdd "NoWmacs", "ahk_class PuTTY"                 ; PuTTY
GroupAdd "NoWmacs", "ahk_class QWidget"               ; VirtualBox
GroupAdd "NoWmacs", "ahk_class Vim"                   ; GVim
GroupAdd "NoWmacs", "ahk_class VirtualConsoleClass"   ; ConEmu
GroupAdd "NoWmacs", "ahk_class VTWin32"               ; TeraTerm
GroupAdd "NoWmacs", "ahk_class 　"                    ; xyzzy
GroupAdd "NoWmacs", "ahk_class VNCMDI_Window"         ; UltraVNC
GroupAdd "NoWmacs", "ahk_class mintty"                ; Git Bash

; --------------------------------------------------------------------
; Group Explorer
; --------------------------------------------------------------------

GroupAdd "Explorer", "ahk_class CabinetWClass"   ; Explorer
GroupAdd "Explorer", "ahk_class ExploreWClass"   ; ???
GroupAdd "Explorer", "ahk_class Progman"         ; Desktop (Program Manager)

; --------------------------------------------------------------------
; Target
; --------------------------------------------------------------------

isWmacsTarget() {
    if WinActive("ahk_group NoWmacs") {
        return 0
    }
    return 1
}

isTargetExplorer() {
    if WinActive("ahk_group Explorer") {
        return 1
    }
    return 0
}

; --------------------------------------------------------------------
; OnClipboardChange
; --------------------------------------------------------------------

/*
OnClipboardChange ClipChanged

ClipChanged(Type) {
    if Type = 1 {
        ; long text
        maxLength := 100
        length := StrLen(A_Clipboard)
        if length > maxLength {
            str := SubStr(A_Clipboard, 1, maxLength)
            ToolTip "テキストをコピーしました`n" str "...`n(" length " 文字)"
        } else {
            ; short text
            ToolTip "テキストをコピーしました`n" A_Clipboard
        }
    } else if Type = 2 {
        ; non text
        ToolTip "テキストでないものをコピーしました"
    }
    SetTimer RemoveToolTip, 1500
}

RemoveToolTip() {
    ToolTip
}
*/

; --------------------------------------------------------------------
; Tray Icon / Menu
; --------------------------------------------------------------------

; wmacs-jus.ini
IniFile := A_Linefile "\..\wmacs-jus.ini"
Section := "wmacs-jus"

; wmacs-jus.ico
IconFile := A_Linefile "\..\wmacs-jus.ico"

strWmacsVersion := "Wmacs JUS " WmacsVersion
WmacsURL := "https://github.com/yoyuse/wmacs-jus"

EnableDateStamp := "1"
strEnableDateStamp := "Date Stamp"
EnableDateStamp := IniRead(IniFile, Section, "EnableDateStamp", EnableDateStamp)

EnableNaturalScroll := "1"
strEnableNaturalScroll := "Natural Scroll"
EnableNaturalScroll := IniRead(IniFile, Section, "EnableNaturalScroll", EnableNaturalScroll)

JUSLayout := "1"
strJUSLayout := "JUS Layout"
JUSLayout := IniRead(IniFile, Section, "JUSLayout", JUSLayout)

WmacsBind := "1"
strWmacsBind := "Wmacs Bind"
WmacsBind := IniRead(IniFile, Section, "WmacsBind", WmacsBind)

RWinToRCtrl := "0"
strRWinToRCtrl := "RWin to RCtrl"
RWinToRCtrl := IniRead(IniFile, Section, "RWinToRCtrl", RWinToRCtrl)

AltOneShotToMuHenkan := "0"
strAltOneShotToMuHenkan := "Alt One Shot to (Mu)Henkan"
AltOneShotToMuHenkan := IniRead(IniFile, Section, "AltOneShotToMuHenkan", AltOneShotToMuHenkan)

HankakuZenkakuToEsc := "0"
strHankakuZenkakuToEsc := "Hankaku/Zenkaku to Esc"
HankakuZenkakuToEsc := IniRead(IniFile, Section, "HankakuZenkakuToEsc", HankakuZenkakuToEsc)

if FileExist(IconFile) {
    TraySetIcon IconFile
}

MyMenu := A_TrayMenu
; 区切り線
MyMenu.Add
; バージョン情報
MyMenu.Add strWmacsVersion, menuWmacsVersion
; 日付入力を使うか
MyMenu.Add strEnableDateStamp, menuEnableDateStamp
if EnableDateStamp = 1 {
    MyMenu.Check strEnableDateStamp
}
; ナチュラルスクロールを使うか
MyMenu.Add strEnableNaturalScroll, menuEnableNaturalScroll
if EnableNaturalScroll = 1 {
    myMenu.Check strEnableNaturalScroll
}
; JUS 配列にするか
MyMenu.Add strJUSLayout, menuJUSLayout
if JUSLayout = 1 {
    myMenu.Check strJUSLayout
}
; Wmacs バインドにするか
MyMenu.Add strWmacsBind, menuWmacsBind
if WmacsBind = 1 {
    myMenu.Check strWmacsBind
}
; RWin を RCtrl にリマップするか
MyMenu.Add strRWinToRCtrl, menuRWinToRCtrl
if RWinToRCtrl = 1 {
    myMenu.Check strRWinToRCtrl
}
; LAlt/RAlt を Muhenkan/Henkan のワンショットモディファイアにするか
MyMenu.Add strAltOneShotToMuHenkan, menuAltOneShotToMuHenkan
if AltOneShotToMuHenkan = 1 {
    myMenu.Check strAltOneShotToMuHenkan
}
; HHK を使うか
MyMenu.Add strHankakuZenkakuToEsc, menuHankakuZenkakuToEsc
if HankakuZenkakuToEsc = 1 {
    MyMenu.Check strHankakuZenkakuToEsc
}

; Auto-execute Section の終わり
return

menuWmacsVersion(ItemName, ItemPos, MyMenu)
{
    global WmacsURL
    Run WmacsURL
}

menuEnableDateStamp(ItemName, ItemPos, MyMenu)
{
    global EnableDateStamp, IniFile, Section, strEnableDateStamp
    if EnableDateStamp = 1 {
        MyMenu.Uncheck strEnableDateStamp
        EnableDateStamp := 0
    } else {
        MyMenu.Check strEnableDateStamp
        EnableDateStamp := 1
    }
    IniWrite EnableDateStamp, IniFile, Section, "EnableDateStamp"
}

menuEnableNaturalScroll(ItemName, ItemPos, MyMenu)
{
    global EnableNaturalScroll, IniFile, Section, strEnableNaturalScroll
    if EnableNaturalScroll = 1 {
        MyMenu.Uncheck strEnableNaturalScroll
        EnableNaturalScroll := 0
    } else {
        MyMenu.Check strEnableNaturalScroll
        EnableNaturalScroll := 1
    }
    IniWrite EnableNaturalScroll, IniFile, Section, "EnableNaturalScroll"
}

menuJUSLayout(ItemName, ItemPos, MyMenu)
{
    global JUSLayout, IniFile, Section, strJUSLayout
    if JUSLayout = 1 {
        MyMenu.Uncheck strJUSLayout
        JUSLayout := 0
    } else {
        MyMenu.Check strJUSLayout
        JUSLayout := 1
    }
    IniWrite JUSLayout, IniFile, Section, "JUSLayout"
}

menuWmacsBind(ItemName, ItemPos, MyMenu)
{
    global WmacsBind, IniFile, Section, strWmacsBind
    if WmacsBind = 1 {
        MyMenu.Uncheck strWmacsBind
        WmacsBind := 0
    } else {
        MyMenu.Check strWmacsBind
        WmacsBind := 1
    }
    IniWrite WmacsBind, IniFile, Section, "WmacsBind"
}

menuRWinToRCtrl(ItemName, ItemPos, MyMenu)
{
    global RWinToRCtrl, IniFile, Section, strRWinToRCtrl
    if RWinToRCtrl = 1 {
        MyMenu.Uncheck strRWinToRCtrl
        RWinToRCtrl := 0
    } else {
        MyMenu.Check strRWinToRCtrl
        RWinToRCtrl := 1
    }
    IniWrite RWinToRCtrl, IniFile, Section, "RWinToRCtrl"
}

menuAltOneShotToMuHenkan(ItemName, ItemPos, MyMenu)
{
    global AltOneShotToMuHenkan, IniFile, Section, strAltOneShotToMuHenkan
    if AltOneShotToMuHenkan = 1 {
        MyMenu.Uncheck strAltOneShotToMuHenkan
        AltOneShotToMuHenkan := 0
    } else {
        MyMenu.Check strAltOneShotToMuHenkan
        AltOneShotToMuHenkan := 1
    }
    IniWrite AltOneShotToMuHenkan, IniFile, Section, "AltOneShotToMuHenkan"
}

menuHankakuZenkakuToEsc(ItemName, ItemPos, MyMenu)
{
    global HankakuZenkakuToEsc, IniFile, Section, strHankakuZenkakuToEsc
    if HankakuZenkakuToEsc = 1 {
        MyMenu.Uncheck strHankakuZenkakuToEsc
        HankakuZenkakuToEsc := 0
    } else {
        MyMenu.Check strHankakuZenkakuToEsc
        HankakuZenkakuToEsc := 1
    }
    IniWrite(HankakuZenkakuToEsc, IniFile, Section, "HankakuZenkakuToEsc")
}

; --------------------------------------------------------------------
; Reload Script
; --------------------------------------------------------------------

#HotIf !C_q

~RShift & Esc::Reload

#HotIf !C_q && HankakuZenkakuToEsc

~RShift & vkF3::Reload
~RShift & vkF4::Reload

#HotIf

; --------------------------------------------------------------------
; RWin to RCtrl
; --------------------------------------------------------------------

#HotIf !C_q && RWinToRCtrl

RWin::RCtrl

#HotIf

; --------------------------------------------------------------------
; カタカナ ひらがな → 半角/全角
; --------------------------------------------------------------------

#HotIf !C_q && JUSLayout

*vkF2::Send "{Blind}{vkF3}"

; --------------------------------------------------------------------
; 半角/全角 または `~
; --------------------------------------------------------------------

#HotIf HankakuZenkakuToEsc && JUSLayout

; 半角/全角 → ESC
*vkF3::Send "{Blind}{Esc}"
*vkF4::Send "{Blind}{Esc}"

#HotIf !HankakuZenkakuToEsc && JUSLayout

; 半角/全角 を IME のトグルにしない
*+vkF3::Send "{Blind}{~}"
*+vkF4::Send "{Blind}{~}"
 *vkF3::Send "{Blind}{``}"
 *vkF4::Send "{Blind}{``}"

; --------------------------------------------------------------------
; Explorer
; --------------------------------------------------------------------

CopyFileName() {
    A_Clipboard := ""
    Send "^c"
    ClipWait 2
    ;;
    paths := A_Clipboard
    names := ""
    Loop Parse,paths,"`n","`r"
    {
        SplitPath A_LoopField, &name
        If names != ""
            names .= "`r`n"
        names .= name
    }
    ;;
    A_Clipboard := names
}

CopyFilePath() {
    A_Clipboard := ""
    Send "^c"
    ClipWait 2
    ;;
    names := A_Clipboard
    A_Clipboard := names
}

#HotIf !C_q && isTargetExplorer() && WmacsBind

+^c::CopyFileName()
+^x::CopyFilePath()

#HotIf

; --------------------------------------------------------------------
; C-q
; --------------------------------------------------------------------

#HotIf C_q || !WmacsBind

~*1::
~*2::
~*3::
~*4::
~*5::
~*6::
~*7::
~*8::
~*9::
~*0::
~*a::
~*b::
~*c::
~*d::
~*e::
~*f::
~*g::
~*h::
~*i::
~*j::
~*k::
~*l::
~*m::
~*n::
~*o::
~*p::
~*q::
~*r::
~*s::
~*t::
~*u::
~*v::
~*w::
~*x::
~*y::
~*z::
~*vkBA::
~*vkBB::
~*vkBC::
~*vkBD::
~*vkBE::
~*vkBF::
~*VKC0::
~*vkDB::
~*vkDC::
~*vkDD::
~*vkDE::
~*vkE2::
~*Space::
~*Tab::
~*Enter::
~*BS::
~*Del::
~*Ins::
~*Left::
~*Right::
~*Up::
~*Down::
~*Home::
~*End::
~*PgUp::
~*PgDn::
~*vkF3::
~*vkF4::
~*vk1C::
~*vk1D::
~*vkF2::
~*vkF0::
~*F1::
~*F2::
~*F3::
~*F4::
~*F5::
~*F6::
~*F7::
~*F8::
~*F9::
~*F10::
~*F11::
~*F12::
~*F13::
~*F14::
~*F15::
~*F16::
~*F17::
~*F18::
~*F19::
~*F20::
~*F21::
~*F22::
~*F23::
~*F24::
~*Esc::
~*AppsKey::
~*PrintScreen::
~*Pause::
~*CtrlBreak::
~*Sleep::
~*Help::
~*CapsLock::
~*ScrollLock::
~*NumLock::
~*Numpad0::
~*Numpad1::
~*Numpad2::
~*Numpad3::
~*Numpad4::
~*Numpad5::
~*Numpad6::
~*Numpad7::
~*Numpad8::
~*Numpad9::
~*NumpadDot::
~*NumpadDel::
~*NumpadIns::
~*NumpadClear::
~*NumpadUp::
~*NumpadDown::
~*NumpadLeft::
~*NumpadRight::
~*NumpadHome::
~*NumpadEnd::
~*NumpadPgUp::
~*NumpadPgDn::
~*NumpadDiv::
~*NumpadMult::
~*NumpadAdd::
~*NumpadSub::
~*NumpadEnter::
{
    Global C_q
    C_q := 0
    ToolTip , , , 2
}

#HotIf

; --------------------------------------------------------------------
; wmacs
; --------------------------------------------------------------------

#HotIf !C_q && isWmacsTarget() && JUSLayout && WmacsBind

*<^vkDE::SendBlind("{F12}")
*<^vkC0::SendBlind("{PgUp}")
*<^vkDB::SendBlind("{PgDn}")

<^vkBB::Send "{Blind}^{Up}"
<^vkBA::Send "{Blind}^{Down}"

#HotIf !C_q && isWmacsTarget() && !JUSLayout && WmacsBind

*<^vkBB::SendBlind("{F12}")
*<^vkDB::SendBlind("{PgUp}")
*<^vkDD::SendBlind("{PgDn}")

#HotIf !C_q && isWmacsTarget() && WmacsBind

*<^1::SendBlind("{F1}")
*<^2::SendBlind("{F2}")
*<^3::SendBlind("{F3}")
*<^4::SendBlind("{F4}")
*<^5::SendBlind("{F5}")
*<^6::SendBlind("{F6}")
*<^7::SendBlind("{F7}")
*<^8::SendBlind("{F8}")
*<^9::SendBlind("{F9}")
*<^0::SendBlind("{F10}")
*<^-::SendBlind("{F11}")
 <^q::quoted_insert()
*<^e::SendBlind("{End}")
*<^p::SendBlind("{Up}")
*<^a::SendBlind("{Home}")
*<^d::SendBlind("{Del}")
*<^f::SendBlind("{Right}")
*<^h::SendBlind("{BS}")
*<^b::SendBlind("{Left}")
*<^n::SendBlind("{Down}")
*<^m::SendBlind("{Enter}")
*<^,::SendBlind("^{Home}")
*<^.::SendBlind("^{End}")
 <^/::Send "^z"
+<^/::Send "^y"

/*
#HotIf !C_q && isWmacsTarget() && WmacsBind

 *<^vkDC::SendBlind("{Ins}")
*<^sc07D::SendBlind("{Ins}")

#HotIf !C_q && isWmacsTarget() && JUSLayout && WmacsBind

*<^vkDD::SendBlind("{Ins}")
*/

#HotIf !C_q && JUSLayout

 +vk32::Send "{@}"
 +vk36::Send "{^}"
+*vk37::Send "{Blind}{&}"
+*vk38::Send "{Blind}{*}"
+*vk39::Send "{Blind}{(}"
+*vk30::Send "{Blind}{)}"
+*vkBD::Send "{Blind}{_}"
+*vkDE::Send "{Blind}{+}"
 *vkDE::Send "{Blind}{=}"
 *vkC0::Send "{Blind}{[}"
 *vkDB::Send "{Blind}{]}"
 +vkBB::Send "{:}"
+*vkBA::Send "{Blind}{`"}"
 *vkBA::Send "{Blind}{'}"
 *vkDD::Send "{Blind}{\}"
+*vkDD::Send "{Blind}{|}"
+*vkE2::Send "{Blind}{~}"
 *vkE2::Send "{Blind}{``}"

; --------------------------------------------------------------------
; date stamp
; --------------------------------------------------------------------

#HotIf !C_q && EnableDateStamp && JUSLayout && WmacsBind

+<^vkBB::Send A_YYYY "-" A_MM "-" A_DD
+<^vkBA::Send FormatTime(, "yyMMdd")

; --------------------------------------------------------------------
; natural scroll
; --------------------------------------------------------------------

#HotIf EnableNaturalScroll

WheelUp::WheelDown
WheelDown::WheelUp
WheelLeft::WheelRight
WheelRight::WheelLeft

#HotIf

; --------------------------------------------------------------------
; alt one shot to (mu)henkan
; --------------------------------------------------------------------

; - AHK v2で左右のaltキーをIMEの切り替えに割り当てる #AutoHotkey - Qiita
; - https://qiita.com/zhiqoo/items/126dae56542f3d451210
; - karakaram/alt-ime-ahk
; - https://github.com/karakaram/alt-ime-ahk

singlePress(lastKey, sendKey, timeout := 1000) {
    ; メニューがアクティブになるのを抑制
    Send "{Blind}{vkE8}"
    startTime := A_TickCount
    KeyWait lastKey, "L"
    ; KeyWait lastKey
    elapsedTime := A_TickCount - startTime
    if timeout < elapsedTime {
        return
    }
    if A_PriorKey = lastKey {
        Send "{Blind}" sendKey
    }
}

; 左右 Alt で IME OFF/ON
~*RAlt::singlePress("RAlt", "{vk1C}")
~*LAlt::singlePress("LAlt", "{vk1D}")

; --------------------------------------------------------------------
; EOF
; --------------------------------------------------------------------
