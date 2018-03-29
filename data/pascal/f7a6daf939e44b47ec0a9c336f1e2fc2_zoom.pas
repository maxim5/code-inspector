{* --------------------------------------------------------------- *}
{*                                                                 *}
{* Interface to Indexdata's zoom functions compiled in YAZ.DLL.    *}
{*                                                                 *}
{* Comments, improvements to:  Giannis Kosmas (kosmas@lib.uoc.gr)  *}
{*                                                                 *}
{* Use at your own risk! :-)                                       *}
{*                                                                 *}
{* --------------------------------------------------------------- *}

unit zoom;

interface

uses Classes,Windows,Variants, Qt;

const

ZOOM_ERROR_NONE=0;
ZOOM_ERROR_CONNECT=10000;
ZOOM_ERROR_MEMORY=10001;
ZOOM_ERROR_ENCODE=10002;
ZOOM_ERROR_DECODE=10003;
ZOOM_ERROR_CONNECTION_LOST=10004;
ZOOM_ERROR_INIT=10005;
ZOOM_ERROR_INTERNAL=10006;
ZOOM_ERROR_TIMEOUT=10007;
ZOOM_ERROR_UNSUPPORTED_PROTOCOL=10008;
ZOOM_ERROR_UNSUPPORTED_QUERY=10009;
ZOOM_ERROR_INVALID_QUERY=10010;
ZOOM_ERROR_CQL_PARSE=10011;
ZOOM_ERROR_CQL_TRANSFORM=10012;
ZOOM_ERROR_CCL_CONFIG=10013;
ZOOM_ERROR_CCL_PARSE=10014;

ZOOM_EVENT_NONE=0;
ZOOM_EVENT_CONNECT=1;
ZOOM_EVENT_SEND_DATA=2;
ZOOM_EVENT_RECV_DATA=3;
ZOOM_EVENT_TIMEOUT=4;
ZOOM_EVENT_UNKNOWN=5;
ZOOM_EVENT_SEND_APDU=6;
ZOOM_EVENT_RECV_APDU=7;
ZOOM_EVENT_RECV_RECORD=8;
ZOOM_EVENT_RECV_SEARCH=9;
ZOOM_EVENT_END=10;

ZOOM_SELECT_READ=1;
ZOOM_SELECT_WRITE=2;
ZOOM_SELECT_EXCEPT=4;

function InitZoom:integer;
procedure ZoomTerminate;

var HYaz : Thandle;

// Connections

    ZOOM_connection_new : function (const host : Pchar; portnum : Word) : PVariant; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_connection_create : function (ZOOM_options : Pvariant) : PVariant; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_connection_connect : procedure (ZOOM_connection : Pvariant; const host : Pchar; portnum : Word); {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_connection_destroy : procedure (c: PVariant); {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_connection_option_get : function(ZOOM_connection: Pvariant; const key: PChar) : PChar; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_connection_option_getl : function(ZOOM_connection: Pvariant; const key: PChar; const val : PChar; len : Word) : Pchar; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_connection_option_set : procedure(ZOOM_connection: Pvariant; const key: PChar; const val : PChar); {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_connection_option_setl : procedure(ZOOM_connection: Pvariant; const key: PChar; const val : PChar; len : Word); {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_connection_errcode : function (ZOOM_Connection:PVariant) : Integer; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_connection_errmsg : function (ZOOM_Connection:PVariant) : PChar; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_connection_error : function (ZOOM_Connection:PVariant; cp, addinfo :Pointer) : Integer; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_connection_error_x : function (ZOOM_Connection:PVariant; cp, addinfo, diagset :Pointer) : Integer; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_connection_addinfo : function (ZOOM_Connection:PVariant) : PChar; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_connection_diagset : function (ZOOM_Connection:PVariant) : PChar; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_diag_str : function (error : Word) : PChar; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_connection_last_event : function (ZOOM_Connection:PVariant) : integer; {$IFDEF WIN32} stdcall; {$ENDIF}

// Results sets

    ZOOM_connection_search : function (ZOOM_connection : Pvariant; q : Pvariant) : PVariant; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_connection_search_pqf : function (ZOOM_connection : Pvariant; const q : PChar) : PVariant; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_resultset_destroy : procedure (ZOOM_resultset:PVariant); {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_resultset_option_get : function(ZOOM_resultset: Pvariant; const key: PChar) : PChar; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_resultset_option_set : procedure(ZOOM_resultset: Pvariant; const key: PChar; const val : PChar); {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_resultset_size : function (ZOOM_resultset:PVariant) : LongWord; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_resultset_records : procedure (ZOOM_resultset : Pvariant; ZOOM_records : Pointer; start,count : Integer); {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_resultset_record : function (ZOOM_resultset: PVariant; pos : Integer):PVariant; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_resultset_record_immediate : function (ZOOM_resultset: PVariant; pos : Integer):PVariant; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_resultset_cache_reset : procedure (ZOOM_resultset:PVariant); {$IFDEF WIN32} stdcall; {$ENDIF}

// Records

    ZOOM_record_get : function (ZOOM_record :PVariant; const type_spec : PChar; len : PInteger) : PChar; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_record_destroy : procedure (ZOOM_record :PVariant); {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_record_clone : function (ZOOM_record :PVariant) : Pvariant; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_record_error : function (ZOOM_record:PVariant; msg, addinfo, diagset :Pointer) : Integer; {$IFDEF WIN32} stdcall; {$ENDIF}

// Queries
    ZOOM_query_create : function () : PVariant; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_query_destroy : procedure (ZOOM_query :PVariant); {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_query_cql : function (ZOOM_query :PVariant; const str : Pchar) : integer; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_query_cql2rpn : function (ZOOM_query :PVariant; const str : Pchar; ZOOM_connection : Pvariant) : integer; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_query_ccl2rpn : function (ZOOM_query :PVariant; const query_str : Pchar; const config: Pchar; ccl_error : Pointer; error_string : Pointer; error_pos: Pointer) : integer; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_query_prefix : function (ZOOM_query :PVariant; const str : Pchar) : integer; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_query_sortby : function (ZOOM_query :PVariant; const criteria : Pchar) : integer; {$IFDEF WIN32} stdcall; {$ENDIF}

// Scan
    ZOOM_connection_scan : function (ZOOM_connection : Pvariant; const scanterm : Pchar) : PVariant; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_connection_scan1 : function (ZOOM_connection : Pvariant; const scanterm : PVariant) : PVariant; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_scanset_term : function (ZOOM_scanset : Pvariant; pos : Integer; occ, len : Pinteger) : Pchar; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_scanset_display_term : function (ZOOM_scanset : Pvariant; pos : Integer; occ, len : Pinteger) : Pchar; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_scanset_size : function (ZOOM_scanset : Pvariant) : integer; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_scanset_destroy : procedure (ZOOM_scanset : Pvariant); {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_scanset_option_get : function(ZOOM_scanset: Pvariant; const key: PChar) : PChar; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_scanset_option_set : procedure(ZOOM_scanset: Pvariant; const key: PChar; const val : PChar); {$IFDEF WIN32} stdcall; {$ENDIF}

// Extended Services Package

    ZOOM_connection_package : function (ZOOM_connection : Pvariant; ZOOM_options : Pvariant) : Pvariant;  {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_package_destroy : procedure (ZOOM_package : Pvariant);  {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_package_send : procedure (ZOOM_package : Pvariant; const estype : Pchar);  {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_package_option_get : function (ZOOM_package : Pvariant; const key : Pchar) : Pchar;  {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_package_option_set : procedure (ZOOM_package : Pvariant; const key,val : Pchar);  {$IFDEF WIN32} stdcall; {$ENDIF}

// Sort

    ZOOM_resultset_sort : procedure (ZOOM_resultset : Pvariant; const sort_type : Pchar; const sort_spec : Pchar); {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_resultset_sort1 : procedure (ZOOM_resultset : Pvariant; const sort_type : Pchar; const sort_spec : Pchar); {$IFDEF WIN32} stdcall; {$ENDIF}

// Options

//...
    ZOOM_options_create : function ():Pvariant;  {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_options_create_with_parent : function (parent : Pvariant):Pvariant;  {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_options_create_with_parent2 : function (parent1, parent2 : Pvariant):Pvariant;  {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_options_get : function (ZOOM_options :Pvariant; const name:PChar):PChar;  {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_options_getl : function (ZOOM_options :Pvariant; const name:PChar; len:Word):PChar;  {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_options_set : procedure (ZOOM_options :Pvariant; const name:PChar; const value:PChar);  {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_options_setl : procedure (ZOOM_options :Pvariant; const name:PChar; const value:PChar; len : Word);  {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_options_destroy : procedure (ZOOM_options :Pvariant);  {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_options_get_bool : function (ZOOM_options :Pvariant; const name:PChar; defa : Word):integer;  {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_options_get_int : function (ZOOM_options :Pvariant; const name:PChar; defa : Word):integer;  {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_options_set_int : procedure (ZOOM_options :Pvariant; const name:PChar; value:Word);  {$IFDEF WIN32} stdcall; {$ENDIF}

// Events

    ZOOM_event : function (no :Word; cs :Pvariant) : Integer;  {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_connection_is_idle : function (ZOOM_Connection:PVariant) : integer; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_event_nonblock : function (no :Word; cs :Pvariant) : Integer;  {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_connection_process : function (ZOOM_Connection:PVariant) : integer; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_connection_get_socket : function (ZOOM_Connection:PVariant) : integer; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_connection_get_mask : function (ZOOM_Connection:PVariant) : integer; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_connection_set_mask : function (ZOOM_Connection:PVariant; mask : Word) : integer; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_connection_get_timeout : function (ZOOM_Connection:PVariant) : integer; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_connection_fire_event_timeout : function (ZOOM_Connection:PVariant) : integer; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_connection_fire_event_socket : function (ZOOM_Connection:PVariant; mask : Word) : integer; {$IFDEF WIN32} stdcall; {$ENDIF}
    ZOOM_connection_peek_event : function (ZOOM_Connection:PVariant) : integer; {$IFDEF WIN32} stdcall; {$ENDIF}


implementation

procedure ZoomTerminate;
begin
  FreeLibrary(HYaz);
end;

//
// Initializes YAZ Zoom.
// Returns :   0   if it fails to load yaz.dll
//             3   if it loads yaz 3
//             2   if it loads yaz 2
function InitZoom():integer;
begin
  Result:=3;
  HYaz := LoadLibrary('yaz3.DLL');
  if HYaz < 32 then { failure }
  begin
   Result:=2;
   HYaz := LoadLibrary('Yaz.DLL');
  end;
  if HYaz >= 32 then { success }
  begin
    ZOOM_connection_new := GetProcAddress(HYaz, '_ZOOM_connection_new@8');
    ZOOM_connection_create := GetProcAddress(HYaz, '_ZOOM_connection_create@4');
    ZOOM_connection_connect := GetProcAddress(HYaz, '_ZOOM_connection_connect@12');
    ZOOM_connection_search_pqf := GetProcAddress(HYaz, '_ZOOM_connection_search_pqf@8');
    ZOOM_connection_search := GetProcAddress(HYaz, '_ZOOM_connection_search@8');
    ZOOM_connection_errcode := GetProcAddress(HYaz, '_ZOOM_connection_errcode@4');
    ZOOM_connection_errmsg := GetProcAddress(HYaz, '_ZOOM_connection_errmsg@4');
    ZOOM_connection_error := GetProcAddress(HYaz, '_ZOOM_connection_error@12');
    ZOOM_connection_error_x := GetProcAddress(HYaz, '_ZOOM_connection_error_x@16');
    ZOOM_connection_addinfo := GetProcAddress(HYaz, '_ZOOM_connection_addinfo@4');
    ZOOM_connection_diagset := GetProcAddress(HYaz, '_ZOOM_connection_diagset@4');
    ZOOM_connection_last_event := GetProcAddress(HYaz, '_ZOOM_connection_last_event@4');
    ZOOM_diag_str := GetProcAddress(HYaz, '_ZOOM_diag_str@4');
    ZOOM_connection_destroy := GetProcAddress(HYaz, '_ZOOM_connection_destroy@4');
    ZOOM_connection_option_get := GetProcAddress(HYaz, '_ZOOM_connection_option_get@8');
    ZOOM_connection_option_getl := GetProcAddress(HYaz, '_ZOOM_connection_option_getl@12');
    ZOOM_connection_option_set := GetProcAddress(HYaz, '_ZOOM_connection_option_set@12');
    ZOOM_connection_option_setl := GetProcAddress(HYaz, '_ZOOM_connection_option_setl@16');

    ZOOM_resultset_size := GetProcAddress(HYaz, '_ZOOM_resultset_size@4');
    ZOOM_resultset_destroy := GetProcAddress(HYaz, '_ZOOM_resultset_destroy@4');
    ZOOM_resultset_cache_reset := GetProcAddress(HYaz, '_ZOOM_resultset_cache_reset@4');
    ZOOM_resultset_option_get := GetProcAddress(HYaz, '_ZOOM_resultset_option_get@8');
    ZOOM_resultset_option_set := GetProcAddress(HYaz, '_ZOOM_resultset_option_set@12');
    ZOOM_resultset_records := GetProcAddress(HYaz, '_ZOOM_resultset_records@16');
    ZOOM_resultset_record := GetProcAddress(HYaz, '_ZOOM_resultset_record@8');
    ZOOM_resultset_record_immediate := GetProcAddress(HYaz, '_ZOOM_resultset_record_immediate@8');

//... missing zoom_options_set_callback ...
    ZOOM_options_create := GetProcAddress(HYaz, '_ZOOM_options_create@0');
    ZOOM_options_create_with_parent := GetProcAddress(HYaz, '_ZOOM_options_create_with_parent@4');
    ZOOM_options_create_with_parent2 := GetProcAddress(HYaz, '_ZOOM_options_create_with_parent2@8');
    ZOOM_options_destroy := GetProcAddress(HYaz, '_ZOOM_options_destroy@4');
    ZOOM_options_get := GetProcAddress(HYaz, '_ZOOM_options_get@8');
    ZOOM_options_getl := GetProcAddress(HYaz, '_ZOOM_options_getl@12');
    ZOOM_options_set := GetProcAddress(HYaz, '_ZOOM_options_set@12');
    ZOOM_options_setl := GetProcAddress(HYaz, '_ZOOM_options_setl@16');
    ZOOM_options_set_int := GetProcAddress(HYaz, '_ZOOM_options_set_int@12');
    ZOOM_options_get_bool := GetProcAddress(HYaz, '_ZOOM_options_get_bool@12');
    ZOOM_options_get_int := GetProcAddress(HYaz, '_ZOOM_options_get_int@12');

    ZOOM_connection_scan := GetProcAddress(HYaz, '_ZOOM_connection_scan@8');
    ZOOM_connection_scan1 := GetProcAddress(HYaz, '_ZOOM_connection_scan1@8');
    ZOOM_scanset_term := GetProcAddress(HYaz, '_ZOOM_scanset_term@16');
    ZOOM_scanset_display_term  := GetProcAddress(HYaz, '_ZOOM_scanset_display_term@16');
    ZOOM_scanset_size := GetProcAddress(HYaz, '_ZOOM_scanset_size@4');
    ZOOM_scanset_destroy := GetProcAddress(HYaz, '_ZOOM_scanset_destroy@4');
    ZOOM_scanset_option_get := GetProcAddress(HYaz, '_ZOOM_scanset_option_get@8');
    ZOOM_scanset_option_set := GetProcAddress(HYaz, '_ZOOM_scanset_option_set@12');

    ZOOM_query_create := GetProcAddress(HYaz, '_ZOOM_query_create@0');
    ZOOM_query_destroy := GetProcAddress(HYaz, '_ZOOM_query_destroy@4');
    ZOOM_query_cql := GetProcAddress(HYaz, '_ZOOM_query_cql@8');
    ZOOM_query_cql2rpn := GetProcAddress(HYaz, '_ZOOM_query_cql2rpn@12');
    ZOOM_query_ccl2rpn := GetProcAddress(HYaz, '_ZOOM_query_ccl2rpn@24');
    ZOOM_query_prefix := GetProcAddress(HYaz, '_ZOOM_query_prefix@8');
    ZOOM_query_sortby := GetProcAddress(HYaz, '_ZOOM_query_sortby@8');

    ZOOM_connection_package := GetProcAddress(HYaz, '_ZOOM_connection_package@8');
    ZOOM_package_destroy := GetProcAddress(HYaz, '_ZOOM_package_destroy@4');
    ZOOM_package_send := GetProcAddress(HYaz, '_ZOOM_package_send@8');
    ZOOM_package_option_get := GetProcAddress(HYaz, '_ZOOM_package_option_get@8');
    ZOOM_package_option_set := GetProcAddress(HYaz, '_ZOOM_package_option_set@12');

    ZOOM_record_get := GetProcAddress(HYaz, '_ZOOM_record_get@12');
    ZOOM_record_destroy := GetProcAddress(HYaz, '_ZOOM_record_destroy@4');
    ZOOM_record_clone := GetProcAddress(HYaz, '_ZOOM_record_clone@4');
    ZOOM_record_error := GetProcAddress(HYaz, '_ZOOM_record_error@16');

    ZOOM_resultset_sort := GetProcAddress(HYaz, '_ZOOM_resultset_sort@12');
    ZOOM_resultset_sort1 := GetProcAddress(HYaz, '_ZOOM_resultset_sort1@12');

    ZOOM_event := GetProcAddress(HYaz, '_ZOOM_event@8');
    ZOOM_connection_is_idle := GetProcAddress(HYaz, '_ZOOM_connection_is_idle@4');
    ZOOM_event_nonblock := GetProcAddress(HYaz, '_ZOOM_event_nonblock@8');
    ZOOM_connection_process := GetProcAddress(HYaz, '_ZOOM_connection_process@4');
    ZOOM_connection_get_socket := GetProcAddress(HYaz, '_ZOOM_connection_get_socket@4');
    ZOOM_connection_get_mask := GetProcAddress(HYaz, '_ZOOM_connection_get_mask@4');
    ZOOM_connection_set_mask := GetProcAddress(HYaz, '_ZOOM_connection_set_mask@8');
    ZOOM_connection_get_timeout := GetProcAddress(HYaz, '_ZOOM_connection_get_timeout@4');
    ZOOM_connection_fire_event_timeout := GetProcAddress(HYaz, '_ZOOM_connection_fire_event_timeout@4');
    ZOOM_connection_fire_event_socket := GetProcAddress(HYaz, '_ZOOM_connection_fire_event_socket@8');
    ZOOM_connection_peek_event := GetProcAddress(HYaz, '_ZOOM_connection_peek_event@4');
  end
  else Result:=0;
end;

end.
