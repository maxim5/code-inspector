'FTP Client
'Matt Kilgore -- 2011/2013

'This program is free software, without any warranty of any kind.
'You are free to edit, copy, modify, and redistribute it under the terms
'of the Do What You Want Public License, Version 1, as published by Matt Kilgore
'See file COPYING that should have been included with this source.

'Basically -- You just do what you want. It's fairly simple.
'I wouldn't mind a mention if you use this code, but it's by no means required.

'DECLARE LIBRARY "SDLNet_Temp_Header"
'    FUNCTION bytes_left& (a AS LONG)
'END DECLARE

'DECLARE LIBRARY
'  FUNCTION fopen%& (file_name as string, mode as string) 'Mode "w" will create an empty file for writing
'END DECLARE

'Thanks to DAV and the Wiki for this code
'DECLARE LIBRARY
'  FUNCTION GetLogicalDriveStringsA(BYVAL nBuff AS LONG, lpbuff AS STRING)
'END DECLARE


$SCREENHIDE
$CONSOLE

CONST BOXES = 3 'Don't change
CONST g_menu_c = 5 'Number of global menu choices

CONST VER$ = "0.96"

'variable length strings via _MEM

TYPE string_type
  mem AS _MEM
  length AS LONG
  allocated AS LONG
  is_allocated AS _BYTE
END TYPE

TYPE array_type
  mem AS _MEM
  length AS LONG
  allocated AS LONG
  is_allocated AS _BYTE
  element_size AS INTEGER
END TYPE

TYPE box_type
  nam AS string_type

  row1 AS INTEGER 'location
  col1 AS INTEGER
  row2 AS INTEGER 'row2 not used for button/checkbox. for drop_down, it represents the number of rows in the selection box
  col2 AS INTEGER

  c1 AS _BYTE 'forcolor
  c2 AS _BYTE 'backcolor

  sc1 AS _BYTE 'selected color. -- not always used
  sc2 AS _BYTE

  text_box AS _BYTE '-1 then drawn as textbox (input box) -- always as row2 = row1 + 2
  text AS string_type 'text drawn inside the textbox
  text_position AS INTEGER 'position of the cursor
  text_offset AS INTEGER 'We display the string in the box starting at the text_offset character, to account for scrolling to the right
  hide AS _BYTE 'text will be drawn as "****" instead of "test"

  scroll AS _BYTE 'if -1 then scroll is drawn
  scroll_loc AS INTEGER 'various numbers needed to draw scroll

  multi_text_box AS _BYTE '-1 then drawn as a multiple line text-box (Not editable)
  selected AS INTEGER 'the line that is selected (Will be drawn in sc1,sc2

  length AS INTEGER 'number of options (now use multi_line.length

  offset AS INTEGER 'offset from the beginning that we will draw from
  shadow AS _BYTE 'if -1 then a shadow is drawn around the box

  button AS _BYTE 'if -1 then drawn as button.

  checkbox AS _BYTE 'if -1 then drawn as checkbox
  checked AS _BYTE 'represents checkbox state

  drop_down AS _BYTE 'drawn as drop_down.
  d_flag AS _BYTE 'if d_flag then dropdown box is drawn

  drop_row2 AS INTEGER 'bottom location of dropdown box
  
  menu as _byte

  updated AS INTEGER 'if set, then information about this box has been updated

  multi_line AS array_type 'string array for multi-line text boxes
END TYPE

TYPE filedir_type
  'NOTE to myself -- GET RID OF DIR, not needed (Just check the flags, replace DIR in the printing sub, etc..
  dir AS STRING * 3 ' = "DIR" or "LNK" or ""
  nam AS string_type
  flag_cwd AS INTEGER 'dir flag
  flag_retr AS INTEGER 'file flag
  size AS LONG
  lin AS string_type
END TYPE


CONST OK_BUTTON = 1 'for prompt_dialog
CONST NO_BUTTON = 2
CONST CANCEL_BUTTON = 4
CONST CLOSE_BUTTON = 8
CONST YES_BUTTON = 16

COMMON SHARED scrnw, scrnh, scrn&
COMMON SHARED command_connect&, data_connect&, server$, username$, password$, port$
COMMON SHARED Remote_dir$, Local_dir$, temp_dir$
COMMON SHARED server_syst$
COMMON SHARED opper$, mx AS INTEGER, my AS INTEGER, but, mtimer AS SINGLE, mscroll AS SINGLE
COMMON SHARED locrow, loccol, butflag AS INTEGER
COMMON SHARED is_connected AS INTEGER, main_menu_len AS INTEGER, status$, crlf$, cmd$
COMMON SHARED pasv_mode AS INTEGER, cmd_mode AS INTEGER, err_flag, cmd_count
COMMON SHARED show_hidden_local AS INTEGER, show_hidden_remote AS INTEGER

COMMON SHARED main_gui_c1 AS INTEGER, main_gui_c2 AS INTEGER
COMMON SHARED main_gui_sel_c1 AS INTEGER, main_gui_sel_c2 AS INTEGER
COMMON SHARED status_c1 AS INTEGER, status_c2 AS INTEGER
COMMON SHARED menu_c1 AS INTEGER, menu_c2 AS INTEGER
COMMON SHARED menu_sel_c1 AS INTEGER, menu_sel_c2 AS INTEGER
COMMON SHARED menu_char_c AS INTEGER, file_non_sel AS INTEGER
COMMON SHARED box_c1 AS INTEGER, box_c2 AS INTEGER
COMMON SHARED box_sel_c1 AS INTEGER, box_sel_c2 AS INTEGER
COMMON SHARED CLI, CONFIG

DIM SHARED boxes(BOXES) AS box_type, selected_box
DIM SHARED Remote_files(500) AS filedir_type, Local_files(1000) AS filedir_type, sep$ 'Change this number for more files
DIM SHARED global_menu_sel, menu_sel, menu_max_len(g_menu_c), temp_menu_sel, menux AS box_type

scrnw = 80 'Default screen size. -- Is overwritten if a Config file is found
scrnh = 25 'Smaller then 80x25 is not recommended or garentied to work

crlf$ = CHR$(13) + CHR$(10)
status$ = "Not Connected."

a$ = "QWERTYUIOP????ASDFGHJKL?????ZXCVBNM" 'Credit to Galleon for the ALT key code stuff.
DIM SHARED alt_codes$(LEN(a$) + 16)
FOR x = 1 TO LEN(a$)
  alt_codes$(x + 15) = MID$(a$, x, 1)
NEXT x

_TITLE "FTP Client -- QB64"

CONFIG = -1 'If 0, it won't check for a config file
CLI = 0 'Start Command Line only if -1.

'colors
main_gui_c1 = 15
main_gui_c2 = 1
main_gui_sel_c1 = 0
main_gui_sel_c2 = 7
file_non_sel = 10
status_c1 = 0
status_c2 = 3
menu_c1 = 0
menu_c2 = 7
menu_sel_c1 = 7
menu_sel_c2 = 0
menu_char_c = 15
box_c1 = 0
box_c2 = 7
box_sel_c1 = 7
box_sel_c2 = 0

IF INSTR(_OS$, "[LINUX]") OR INSTR(_OS$, "[MACOSX]") THEN
  opper$ = "NIX"
  'Local_dir$ = "/" 'Root
  sep$ = "/"
  temp_dir$ = "/tmp"
ELSE
  opper$ = "WIN"
  'Local_dir$ = "C:\" 'C Drive
  sep$ = "\"
  temp_dir$ = ENVIRON$("temp")
END IF

'setup command line help system
RESTORE commands
READ cmd_count
DIM SHARED commands$(cmd_count), help$(cmd_count, 10), helplen(cmd_count)
FOR x = 1 TO cmd_count
  READ cm$
  commands$(x) = cm$
  DO
    READ a$
    IF a$ > "" THEN helplen(x) = helplen(x) + 1: help$(x, helplen(x)) = a$
  LOOP UNTIL a$ = ""
NEXT x
y = cmd_count
DO
  flag = 0
  FOR x = 1 TO y - 1
    IF commands$(x) > commands$(x + 1) THEN
      SWAP commands$(x), commands$(x + 1)
      FOR m = 1 TO 6
        SWAP help$(x, m), help$(x + 1, m)
      NEXT m
      SWAP helplen(x), helplen(x + 1)
      flag = -1
    END IF
  NEXT x
  y = y - 1
LOOP UNTIL flag = 0

'check COMMAND$
IF COMMAND$ > "" THEN
  cmdarg$ = LCASE$(COMMAND$)
  IF INSTR(cmdarg$, "-cli") THEN
    CLI = -1
  END IF
  IF INSTR(cmdarg$, "-gui") THEN
    CLI = 0
  END IF
  IF INSTR(cmdarg$, "-config") THEN
    CONFIG = -1
  END IF
  IF INSTR(cmdarg$, "-noconfig") THEN
    CONFIG = 0
  END IF
  IF INSTR(cmdargs$, "-h") OR INSTR(cmdargs$, "--help") THEN

  END IF
END IF

IF CLI THEN
  command_line
  SYSTEM
ELSE
  _CONSOLE OFF
  _SCREENSHOW
END IF

'Load config file
IF CONFIG THEN
  read_config_file
END IF

LOCATE , , 0
butflag = 0

setup_main_GUI
dim s as string_type
allocate_array boxes(1).multi_line, 120, LEN(s)
allocate_array boxes(2).multi_line, 120, LEN(s)

'setup menu
'$include:'./menu/menu_gui.bas'

RANDOMIZE TIMER
status$ = "Not Connected."
main
free_gui_array boxes()
END

error_flag: 'rudimentary error checking. Simple but effective and only requires one error trap
err_flag = -1
RESUME NEXT

'$include:'./help/help_data.bas'

'$include:'./gui/draw_gui.bas'

'$include:'./main_gui/setup_gui_code.bas'

'$include:'./main_gui/main_gui.bas'

'$include:'./menu/menu_parser.bas'

'$include:'./gui/gui_dialogs.bas'

'$include:'./settings/settings_save_load.bas'

'$include:'./cli_code/cli_mode.bas'

'$include:'./ftp_code/ftp_handle.bas'

'$include:'./ftp_code/ftp_parser.bas'

'$include:'./gui/mem_handling.bas'


SUB main () 'main loop for GUI, etc.

get_new_dir
refresh_Local_files
update = -1
selected_box = 1
print_files boxes(1), Local_files()
print_files boxes(2), Remote_files()
DO
  _LIMIT 200
  m = mouse_range(boxes(), BOXES) 'check key presses
  IF m > 0 THEN
    IF selected_box <> 3 THEN
      selected_box = m
      update = -1
    ELSEIF m <> 3 THEN
      IF my < menux.row1 OR my > menux.row2 OR mx < menux.col1 OR mx > menux.col2 THEN
        selected_box = m
        update = -1
      END IF
    END IF
  END IF
  IF but AND TIMER - mtimer > .1 THEN
    mtimer = TIMER
    'Click
    IF m > 0 AND m < 3 AND selected_box < 3 THEN
      IF mx > boxes(m).col1 AND mx < boxes(m).col2 THEN
        IF my > boxes(m).row1 AND my < boxes(m).row2 THEN
          selec = boxes(m).offset + my - boxes(m).row1
          IF boxes(m).selected = selec THEN
            IF Local_files(boxes(1).selected).flag_cwd AND m = 1 THEN
              CHDIR RTRIM$(get_str$(Local_files(boxes(1).selected).nam))
              get_new_dir
              refresh_Local_files
              update = -1
            ELSEIF m = 2 THEN
              IF is_connected THEN
                IF Remote_files(boxes(2).selected).flag_cwd THEN
                  change_remote_dir RTRIM$(get_str$(Remote_files(boxes(2).selected).nam))
                  Update_Remote_Files
                  update = -1
                END IF
              END IF
            END IF
          ELSE
            boxes(m).selected = selec
          END IF
        END IF
      ELSEIF mx = boxes(m).col2 THEN 'Scroll bar
        IF my = boxes(m).row1 + 1 THEN
          IF boxes(m).selected > 1 THEN
            update = -1
            boxes(m).selected = boxes(m).selected - 1
            IF boxes(m).offset + 1 > boxes(m).selected THEN
              boxes(m).offset = boxes(m).offset - 1
            END IF
          END IF
        ELSEIF my = boxes(m).row2 - 1 THEN
          IF boxes(m).selected < boxes(m).length THEN
            update = -1
            boxes(m).selected = boxes(m).selected + 1
            IF boxes(m).offset + (boxes(m).row2 - boxes(m).row1 - 1) < boxes(m).selected THEN
              boxes(m).offset = boxes(m).offset + 1
            END IF
          END IF
        ELSEIF my > boxes(m).scroll_loc THEN
          IF boxes(m).selected < boxes(m).length THEN
            update = -1
            boxes(m).selected = boxes(m).selected + (boxes(m).row2 - boxes(m).row1)
            IF boxes(m).selected > boxes(m).length THEN boxes(m).selected = boxes(m).length

            IF boxes(m).offset + (boxes(m).row2 - boxes(m).row1 - 1) < boxes(m).selected THEN
              boxes(m).offset = (boxes(m).selected - (boxes(m).row2 - boxes(m).row1 - 1))
            END IF
          END IF
        ELSEIF my < boxes(m).scroll_loc THEN
          IF boxes(m).selected > 1 THEN
            update = 1
            boxes(m).selected = boxes(m).selected - (boxes(m).row2 - boxes(m).row1)
            IF boxes(m).selected <= 0 THEN boxes(m).selected = 1
            IF boxes(m).offset + 1 > boxes(m).selected THEN
              boxes(m).offset = boxes(m).selected - 1
            END IF
          END IF
        END IF
      END IF
      IF boxes(selected_box).selected > boxes(selected_box).length THEN boxes(selected_box).selected = boxes(selected_box).length
    ELSEIF m = 3 THEN
      temp_menu_sel = 0
      k = 2
      FOR x = 1 TO g_menu_c
        IF mx > k AND mx <= k + menu_len(Global_Menu$(x)) THEN
          global_menu_sel = x
          menu_sel = 1
          menux.text_box = -1
          'menux.nam = ""
          put_str menux.nam, ""
          menux.row1 = 2
          menux.row2 = 3 + Menun(global_menu_sel)
          menux.col1 = k
          menux.col2 = menux.col1 + menu_max_len(global_menu_sel)
          menux.c1 = menu_c1
          menux.c2 = menu_c2
          menux.shadow = -1
          update = -1
          EXIT FOR
        END IF
        k = k + menu_len(Global_Menu$(x))
      NEXT x
    END IF
    IF selected_box = 3 THEN
      IF my > menux.row1 AND my < menux.row2 THEN
        IF mx > menux.col1 AND mx < menux.col2 THEN
          sel = my - menux.row1
          IF sel = menu_sel THEN
            menu_clicked = -1
          END IF
          IF Menu$(global_menu_sel, sel) <> "-" THEN
            menu_sel = sel
            update = -1
            menu_clicked = -1
          END IF
        END IF
      END IF
    END IF

  END IF
  IF mscroll <> 0 THEN 'scroll wheel
    boxes(selected_box).offset = boxes(selected_box).offset + mscroll
    IF boxes(selected_box).offset > (boxes(selected_box).length - (boxes(selected_box).row2 - boxes(selected_box).row1 - 1)) THEN boxes(selected_box).offset = boxes(selected_box).length - (boxes(selected_box).row2 - boxes(selected_box).row1 - 1)
    IF boxes(selected_box).offset < 0 THEN boxes(selected_box).offset = 0
    update = -1
  END IF
  a$ = INKEY$
  SELECT CASE a$
    CASE CHR$(0) + CHR$(72)
      IF selected_box < 3 THEN
        IF boxes(selected_box).selected > 1 THEN update = -1: boxes(selected_box).selected = boxes(selected_box).selected - 1: IF boxes(selected_box).offset + 1 > boxes(selected_box).selected THEN boxes(selected_box).offset = boxes(selected_box).offset - 1
      ELSE
        menu_sel = menu_sel - 1
        IF menu_sel < 1 THEN menu_sel = Menun(global_menu_sel)
        IF Menu$(global_menu_sel, menu_sel) = "-" THEN menu_sel = menu_sel - 1
        update = -1
      END IF

    CASE CHR$(0) + CHR$(80)
      IF selected_box < 3 THEN
        IF boxes(selected_box).selected < boxes(selected_box).length THEN update = -1: boxes(selected_box).selected = boxes(selected_box).selected + 1: IF boxes(selected_box).offset + (boxes(selected_box).row2 - boxes(selected_box).row1 - 1) < boxes(selected_box).selected THEN boxes(selected_box).offset = boxes(selected_box).offset + 1
      ELSEIF temp_menu_sel = 0 AND global_menu_sel > 0 THEN
        menu_sel = (menu_sel MOD Menun(global_menu_sel)) + 1
        IF Menu$(global_menu_sel, menu_sel) = "-" THEN menu_sel = menu_sel + 1
        update = -1
      ELSEIF temp_menu_sel > 0 THEN
        global_menu_sel = temp_menu_sel
        temp_menu_sel = 0
        menu_sel = 1
        menux.text_box = -1
        '        menux.nam = ""
        put_str menux.nam, ""
        menux.row1 = 2
        menux.row2 = 3 + Menun(global_menu_sel)
        menux.col1 = 2
        FOR x = 1 TO global_menu_sel - 1
          menux.col1 = menux.col1 + menu_len(Global_Menu$(x))
        NEXT x

        menux.col2 = menux.col1 + menu_max_len(global_menu_sel)
        menux.c1 = menu_c1
        menux.c2 = menu_c2
        menux.shadow = -1
        update = -1

      END IF
    CASE CHR$(0) + CHR$(75)
      IF selected_box < 3 THEN
        selected_box = (selected_box MOD 2) + 1
        update = -1
      ELSEIF selected_box = 3 AND temp_menu_sel = 0 AND global_menu_sel > 0 THEN
        global_menu_sel = global_menu_sel - 1
        IF global_menu_sel = 0 THEN global_menu_sel = g_menu_c
        menu_sel = 1
        menux.text_box = -1
        '                menux.nam = ""
        put_str menux.nam, ""
        menux.row1 = 2
        menux.row2 = 3 + Menun(global_menu_sel)
        menux.col1 = 2
        FOR x = 1 TO global_menu_sel - 1
          menux.col1 = menux.col1 + menu_len(Global_Menu$(x))
        NEXT x
        menux.col2 = menux.col1 + menu_max_len(global_menu_sel)
        menux.c1 = menu_c1
        menux.c2 = menu_c2
        menux.shadow = -1

        update = -1
      ELSEIF temp_menu_sel > 0 THEN
        temp_menu_sel = temp_menu_sel - 1
        IF temp_menu_sel = 0 THEN temp_menu_sel = g_menu_c
        update = -1
      END IF
    CASE CHR$(0) + CHR$(77)
      IF selected_box < 3 THEN
        selected_box = (selected_box MOD 2) + 1
        update = -1
      ELSEIF selected_box = 3 AND temp_menu_sel = 0 AND global_menu_sel > 0 THEN
        global_menu_sel = (global_menu_sel MOD g_menu_c) + 1
        menu_sel = 1
        menux.text_box = -1
        '        menux.nam = ""
        put_str menux.nam, ""
        menux.row1 = 2
        menux.row2 = 3 + Menun(global_menu_sel)
        menux.col1 = 2
        FOR x = 1 TO global_menu_sel - 1
          menux.col1 = menux.col1 + menu_len(Global_Menu$(x))
        NEXT x

        menux.col2 = menux.col1 + menu_max_len(global_menu_sel)
        menux.c1 = menu_c1
        menux.c2 = menu_c2
        menux.shadow = -1
        update = -1
      ELSEIF temp_menu_sel > 0 THEN
        temp_menu_sel = temp_menu_sel + 1
        IF temp_menu_sel = g_menu_c + 1 THEN temp_menu_sel = 1
        update = -1
      END IF
    CASE CHR$(13) 'ENTER
      IF selected_box = 1 THEN 'Local File system
        IF Local_files(boxes(1).selected).flag_cwd THEN
          CHDIR RTRIM$(get_str$(Local_files(boxes(1).selected).nam))
          get_new_dir
          refresh_Local_files
          update = -1
        END IF
      ELSEIF selected_box = 2 THEN
        IF is_connected THEN
          IF Remote_files(boxes(2).selected).flag_cwd THEN
            change_remote_dir RTRIM$(get_str$(Remote_files(boxes(2).selected).nam))
            Update_Remote_Files
            update = -1
          END IF
        END IF
      ELSEIF selected_box = 3 THEN
        menu_clicked = -1
      END IF

    CASE CHR$(0) + CHR$(16) TO CHR$(0) + CHR$(50)
      k$ = alt_codes$(ASC(a$, 2))
      IF selected_box = 3 AND alt_flag_1 THEN
        IF temp_menu_sel > 0 THEN
          FOR x = 1 TO g_menu_c
            IF UCASE$(k$) = UCASE$(menu_char$(Global_Menu$(x))) THEN
              global_menu_sel = x
              temp_menu_sel = 0
              menu_sel = 1
              menux.text_box = -1
              '  menux.nam = ""
              put_str menux.nam, ""
              menux.row1 = 2
              menux.row2 = 3 + Menun(global_menu_sel)
              menux.col1 = 2
              FOR x = 1 TO global_menu_sel - 1
                menux.col1 = menux.col1 + menu_len(Global_Menu$(x))
              NEXT x
              menux.col2 = menux.col1 + menu_max_len(global_menu_sel)
              menux.c1 = menu_c1
              menux.c2 = menu_c2
              menux.shadow = -1
              update = -1
            END IF
          NEXT x
        ELSE
          men = 0
          FOR x = 1 TO Menun(global_menu_sel)
            IF UCASE$(k$) = UCASE$(menu_char$(Menu$(global_menu_sel, x))) THEN
              menu_sel = x
              temp_menu_sel = 0
              menu_clicked = -1
              update = -1
              men = -1
            END IF
          NEXT x
          IF men = 0 THEN
            FOR x = 1 TO g_menu_c
              IF UCASE$(k$) = UCASE$(menu_char$(Global_Menu$(x))) THEN
                global_menu_sel = x
                temp_menu_sel = 0
                menu_sel = 1
                menux.text_box = -1
                '                menux.nam = ""
                put_str menux.nam, ""
                menux.row1 = 2
                menux.row2 = 3 + Menun(global_menu_sel)
                menux.col1 = 2
                FOR x = 1 TO global_menu_sel - 1
                  menux.col1 = menux.col1 + menu_len(Global_Menu$(x))
                NEXT x
                menux.col2 = menux.col1 + menu_max_len(global_menu_sel)
                menux.c1 = menu_c1
                menux.c2 = menu_c2
                menux.shadow = -1
                update = -1
              END IF
            NEXT x
          END IF
        END IF

      END IF

    CASE CHR$(9) 'TAB
      selected_box = (selected_box MOD (BOXES - 1)) + 1
      update = -1
      'boxes(selected_box).selected = boxes(selected_box).offset + 1
    CASE CHR$(18)
      global_menu_sel = 2
      menu_sel = 1
      menu_clicked = -1

    CASE CHR$(0) + CHR$(73) ' PAGE UP
      IF sselected_box < 3 THEN
        IF boxes(selected_box).selected > 1 THEN
          update = 1
          boxes(selected_box).selected = boxes(selected_box).selected - (boxes(selected_box).row2 - boxes(selected_box).row1)
          IF boxes(selected_box).selected <= 0 THEN boxes(selected_box).selected = 1
          IF boxes(selected_box).offset + 1 > boxes(selected_box).selected THEN
            boxes(selected_box).offset = boxes(selected_box).selected - 1
          END IF
        END IF
      END IF
    CASE CHR$(0) + CHR$(81) ' PAGE DOWN
      IF selected_box < 3 THEN
        IF boxes(selected_box).selected < boxes(selected_box).length THEN
          update = -1
          boxes(selected_box).selected = boxes(selected_box).selected + (boxes(selected_box).row2 - boxes(selected_box).row1)
          IF boxes(selected_box).selected > boxes(selected_box).length THEN boxes(selected_box).selected = boxes(selected_box).length

          IF boxes(selected_box).offset + (boxes(selected_box).row2 - boxes(selected_box).row1 - 1) < boxes(selected_box).selected THEN
            boxes(selected_box).offset = (boxes(selected_box).selected - (boxes(selected_box).row2 - boxes(selected_box).row1 - 1))
          END IF
        END IF
      END IF
  END SELECT
  'Credit to Galleon for ALT key stuff
  keys = _KEYDOWN(100307) + _KEYDOWN(100308) * 2
  IF (keys) AND alt_flag_1 = 0 THEN 'Hold
    IF selected_box <> 3 THEN
      selected_box = 3
      temp_menu_sel = g_menu_c + 1
      update = -1
      'else
      '       selected_box = 4
      '           temp_menu_sel = 0
      '           update = -1
    END IF
    alt_flag_1 = -1
  END IF
  IF keys = 0 AND alt_flag_1 THEN 'Release
    IF temp_menu_sel > 0 AND temp_menu_sel < g_menu_c + 1 THEN
      selected_box = 4
      temp_menu_sel = 0
      update = -1
    ELSEIF temp_menu_sel = g_menu_c + 1 THEN
      temp_menu_sel = 1
      update = -1
    END IF
    alt_flag_1 = 0
  END IF
  IF menu_clicked THEN
    menu_clicked = 0
    selected_box = 4
    temp_menu_sel = 0
    update_scrn
    _DISPLAY 'So menu updates
    IF global_menu_sel = 1 THEN 'this is where we decide what to do based on what is selected in the menu
      IF menu_sel = 1 THEN
        Connect_To_FTP
        update = -1
      ELSEIF menu_sel = 2 THEN
        is_connected = 0
        CLOSE command_connect&
        status$ = "Not Connected."
        boxes(2).length = 0
        update = -1
      ELSEIF menu_sel = 3 THEN
        command_line
        update_scrn
        IF is_connected THEN Update_Remote_Files
        refresh_Local_files
        update = -1
      ELSEIF menu_sel = 5 THEN
        SYSTEM
      END IF
    ELSEIF global_menu_sel = 2 THEN
      IF menu_sel = 1 THEN
        refresh_Local_files
        update = -1
      ELSEIF menu_sel = 3 THEN
        rename_file_GUI 1 '1 = local rename file
        refresh_Local_files
        update = -1
      ELSEIF menu_sel = 4 THEN
        delete_file_GUI 1 '1 = local delete file
        update = -1
      ELSEIF menu_sel = 5 THEN
        show_hidden_local = NOT show_hidden_local
        refresh_Local_files
        update = -1
      END IF
    ELSEIF global_menu_sel = 3 THEN
      IF menu_sel = 1 THEN
        Update_Remote_Files
        update = -1
      ELSEIF menu_sel = 3 THEN
        rename_file_GUI 0 '0 = remote delete file
        Update_Remote_Files
        update = -1
      ELSEIF menu_sel = 4 THEN
        delete_file_GUI 0 '0 = remote delete file
        Update_Remote_Files
        update = -1
      ELSEIF menu_sel = 5 THEN
        show_hidden_remote = NOT show_hidden_remote
        Update_Remote_Files
        update = -1
      END IF
    ELSEIF global_menu_sel = 4 THEN
      IF menu_sel = 1 THEN
        send_file get_str$(Local_files(boxes(2).selected).nam)
        refresh_Local_files
        update = -1
      ELSEIF menu_sel = 2 THEN
        get_file get_str$(Remote_files(boxes(2).selected).nam)
        refresh_Local_files
        update = -1
      END IF
    ELSEIF global_menu_sel = 5 THEN
      IF menu_sel = 1 THEN 'help
        test = prompt_dialog("Test", 10, OK_BUTTON OR CANCEL_BUTTON OR NO_BUTTON OR CLOSE_BUTTON OR YES_BUTTON, 10)
      ELSEIF menu_sel = 2 THEN
        settings_dialog
        IF is_connected THEN Update_Remote_Files
        refresh_Local_files
        update = -1
      ELSE
        IF menu_sel = 4 THEN 'about
          about_dialog
          update = -1
        END IF
      END IF
    END IF
    menu_clicked = 0
    selected_box = 4
    temp_menu_sel = 0
    global_menu_sel = 0
    menu_sel = 0
    update = -1
  END IF
  IF update THEN
    'draw_menu
    update_scrn

    _DISPLAY
    update = 0
  END IF
LOOP 'UNTIL a$ = CHR$(27)
END SUB
