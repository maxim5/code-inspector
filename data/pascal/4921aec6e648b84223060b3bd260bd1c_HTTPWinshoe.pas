{

  Implementation of the HTTP 1.1 protcol as specified in RFC 2068.
  (See NOTE below for details of what is exactly implemented)

  Author: Hadi Hariri (hadihariri@hotmail.com or hadi@urusoft.com)
  Copyright: (c) Chad Z. Hower and The Winshoes Working Group.

NOTE:
  Initially only GET and POST will be supported. As time goes on more will
  be added. For other developers, please add the date and what you have done
  below.

Initials: Hadi Hariri - HH

Details of implementation
-------------------------

13-JAN-2000: MTL Moved to new Palette Scheme (Winshoes Clients)
10-DEC-1999: HH
  Not much. Just checked the Create method, basic
  properties for the component, etc.
13-DEC-1999: HH
  Added HEAD method.
  Added GET method.
14-DEC-1999: HH
  Started POS
08-JAN-2000: ML
  Cleaned up a few compiler hints during 7.038 build



Additional notes:
-----------------

- Currently does not allow relative paths. Full host has to be specified.

}

unit HTTPWinshoe;

interface

uses
  Classes,
  Winshoes,
  SysUtils;

Type
  THeaderInfo = class(TPersistent)
  private
    fComponent: TComponent;
    fsAccept,
    fsAcceptCharSet,
    fsAcceptEncoding,
    fsAcceptLanguage,
    fsFrom,
    fsHost,
    fsReferer,
    fsLocation,
    fsServer,
    fsContentVersion,
    fsWWWAuthenticate,
    fsContentEncoding,
    fsContentLanguage,
    fsContentType:string;
    fiContentLength:integer;
    fsUserAgent:string;
    fDate,
    fLastModified,
    fExpires:TDateTime;
    fsUsername,
    fsPassword:string;


    procedure SetHeaders(var slstHeaders:TStrings);
    procedure GetHeaders(slstHeaders:TStrings);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Component: TComponent); virtual;
  published
    property Date:TDateTime read fDate write fDate;
    property Accept: string read fsAccept write fsAccept;
    property AcceptCharSet: string read fsAcceptCharset write fsAcceptCharset;
    property AcceptEncoding: string read fsAcceptEncoding write fsAcceptEncoding;
    property AcceptLanguage: string read fsAcceptLanguage write fsAcceptLanguage;
    property From: string read fsFrom write fsFrom ;
    property Host: string read fsHost write fsHost;
    property Referer: string read fsReferer write fsReferer;
    property UserAgent: string read fsUserAgent write fsUserAgent;
    property Location: string read fsLocation write fsLocation;
    property Server: string read fsServer write fsServer;
    property WWWAuthenticate: string read fsWWWAuthenticate write fsWWWAuthenticate;
    property ContentEncoding: string read fsContentEncoding write fsContentEncoding;
    property ContentLanguage: string read fsContentLanguage write fsContentLanguage;
    property ContentType: string read fsContentType write fsContentType;
    property ContentLength: integer read fiContentLength write fiContentLength;
    property ContentVersion: string read fsContentVersion write fsContentVersion;
    property LastModified:TDateTime read fLastModified write fLastModified;
    property Expires:TDateTime read fExpires write fExpires;
    property Username:string read fsUsername write fsPassword;
    property Password:string read fsPassword write fsPassword;

  end ;

  EWinshoeHTTPException = class(Exception);

  TWinshoeHTTPClient = class(TWinshoeClient)
  protected
    FslstHeaders,fslstOptionalHeaders:TStrings;
    FHeaders:THeaderInfo;

    fiResponseCode:integer;
    fsResponseText:string;

    fslstPostData:TStrings;

    procedure SetHost(var sURL:string);
    procedure HandleResponsecode(const iCode:integer);
    procedure SetOptionalHeaders(const Value: TStrings);
    procedure SetPostData(const Value:TStrings);
    procedure Request(const sCommand:string; sURL:string);
    procedure GetResponse;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Get(sURL:string; const strmDest:TStream);
    procedure Head(sURL:string);
    procedure Post(sURL:string; const strmDest:TStream);
    property ResponseCode:integer read fiResponseCode;
    property ResponseText:string read fsResponseText;
  published
    property HeaderInfo: THeaderInfo read fHeaders write fHeaders;
    property OptionalHeaderInfo: TStrings read fslstOptionalHeaders write SetOptionalHeaders;
    property PostData:TStrings read fslstPostData write SetPostData;
  end;

// Procs
procedure Register;

implementation

uses WinsockIntf, GlobalWinshoe, encodewinshoe;

procedure Register;
begin
  RegisterComponents('Winshoes Clients', [TWinshoeHTTPClient]);
end;





{ TGeneralHeaderInfo }

procedure THeaderInfo.AssignTo(Dest: TPersistent);
begin
  if Dest is THeaderInfo then
  begin
    with Dest do
    begin
      fsAccept := Self.fsAccept;
      fsAcceptCharSet := Self.fsAcceptCharset;
      fsAcceptEncoding := Self.fsAcceptEncoding;
      fsAcceptLanguage := Self.fsAcceptLanguage;
      fsFrom := Self.fsFrom;
      fsHost := Self.fsHost;
      fsReferer := Self.fsReferer;
      fsLocation := Self.fsLocation;
      fsServer := Self.fsServer;
      fsContentVersion := Self.fsContentVersion;
      fsWWWAuthenticate := Self.fsWWWAuthenticate;
      fsContentEncoding := Self.fsContentEncoding;
      fsContentLanguage := Self.fsContentLanguage;
      fsContentType := Self.fsContentType;
      fiContentLength := Self.fiContentLength;
      fsUserAgent := Self.fsUserAgent;
      fDate := Self.fDate;
      fLastModified := Self.fLastModified;
      fExpires := Self.fExpireS;
      fsUsername := Self.fsUsername;
      fsPassword := Self.fsPassword;
    end;
  end
  else
  inherited AssignTo(Dest);
end;

constructor THeaderInfo.Create(Component: TComponent);
begin
  inherited Create;
  FComponent := Component;
  fsAccept := 'text/html, */*';
  fsAcceptCharSet := 'iso-8859-5';
  fsUserAgent := 'Winshoes Library';
end;

procedure THeaderInfo.GetHeaders(slstHeaders: TStrings);
begin
  // Set and Delete so that later we copy remaining to optional headers
  with slstHeaders do
  begin
    fsLocation := Values['Location'];
    Values['Location'] := ''; // Emtpy so I reamin with left-overs
    fsServer := Values['Server'];
    Values['Server'] := '';
    fsContentVersion := Values['Content-Version'];
    Values['Content-Version'] := '';
    fsWWWAuthenticate := Values['WWWAuthenticate'];
    Values['WWWAuthenticate'] := '';
    fsContentEncoding := Values['Content-Encoding'];
    Values['Content-Encoding'] := '';
    fsContentLanguage := Values['Content-Language'];
    Values['Content-Language'] := '';
    fsContentType := Values['Content-Type'];
    Values['Content-Type'] := '';
    fiContentLength := StrTOIntDef(Values['Content-Length'],0);
    Values['Content-Length'] := '';
    fDate := UnixDateStrToDateTime(slstHeaders.Values['Date']);
    Values['Date'] := '';
    fLastModified := UnixDateStrToDateTime(slstHeaders.Values['Last-Modified']);
    Values['Last-Modified'] := '';
    fExpires := UnixDateStrToDateTime(slstHeaders.Values['Expires']);
    Values['Expires'] := '';
  end ;
end;

procedure THeaderInfo.SetHeaders(var slstHeaders: TStrings);
var
  Encoder:TWinshoeEncoder;
begin
  with slstHeaders do
  begin
      // First add the general headers
    if fDate = 0 then
      Add('Date: ' + FormatDateTime('ddd, dd mmm yyyy hh:mm:ss ',Now)+
        DateTimeToGmtOffSetStr(OffSetFromUTC,False) + ' GMT');
    // Next add the request headers
    Add('Accept:'+fsAccept);
    if fsAcceptCharset <> '' then
      Add('Accept-Charset: '+fsAcceptCharSet);
    if fsAcceptEncoding <> '' then
      Add('Accept-Encoding: '+fsAcceptEncoding);
    if fsAcceptLanguage <> '' then
      Add('Accept-Language: '+fsAcceptLanguage);
    if fsFrom <> '' then
      Add('From: '+fsFrom);
    if fsReferer <> '' then
      Add('Referer: '+fsReferer);
    Add('User-Agent: '+fsUserAgent);
    if fsContentVersion <> '' then
      Add('Content-Version: ' + fsContentVersion);
    if fsContentEncoding <> '' then
      Add('Content-Encoding: ' + fsContentEncoding);
    if fsContentLanguage <> '' then
      Add('Content-Language: ' + fsContentLanguage);
    if fsContentType <> '' then
      Add('Content-Type: ' + fsContentType);
    if fiContentLength <> 0 then
      Add('Content-Length: ' + IntToStr(fiContentLength));
    if fsUsername <> '' then
    begin
      Encoder := TWinshoeEncoder.Create; // Encode base64
      Add('Authorization: Basic ' + Encoder.EncodeLine(fsUsername)+':'+Encoder.EncodeLine(fsPassword));
      Encoder.Free;
    end ;
  end;
end;


{ TWinshoeHTTPClient }

// Handle the response code
procedure TWinshoeHTTPClient.HandleResponsecode(const iCode: integer);
begin
  case iCode of
    100: fsResponseText := 'Continue';
    101: fsResponseText := 'Switching protocols';
    // 2XX: Successful
    200: fsResponseText := 'OK';
    201: fsResponseText := 'Created';
    202: fsResponseText := 'Accepted';
    203: fsResponseText := 'Non-authoritative Information';
    204: fsResponseText := 'No Content';
    205: fsResponseText := 'Reset Content';
    206: fsResponseText := 'Partial Content';
    // 3XX: Redirections
    301: fsResponseText := 'Moved Permanently';
    302: fsResponseText := 'Moved Temporarily';
    303: fsResponseText := 'See Other';
    304: fsResponseText := 'Not Modified';
    305: fsResponseText := 'Use Proxy';
    // 4XX Client Errors
    400: fsResponseText := 'Bad Request';
    401: fsResponseText := 'Unauthorized';
    403: fsResponseText := 'Forbidden';
    404: fsResponseText := 'Not Found';
    405: fsResponseText := 'Methode not allowed';
    406: fsResponseText := 'Not Acceptable';
    407: fsResponseText := 'Proxy Authentication Required';
    408: fsResponseText := 'Request Timeout';
    409: fsResponseText := 'Conflict';
    410: fsResponseText := 'Gone';
    411: fsResponseText := 'Length Required';
    412: fsResponseText := 'Precondition Failed';
    413: fsResponseText := 'Request Entity To Long';
    414: fsResponseText := 'Request-URI Too Long'; // max 256 chars
    415: fsResponseText := 'Unsupported Media Type';
    // 5XX Server errors
    500: fsResponseText := 'Internal Server Error';
    501: fsResponseText := 'Not Implemented';
    502: fsResponseText := 'Bad Gateway';
    503: fsResponseText := 'Service Unavailable';
    504: fsResponseText := 'Gateway timeout';
    505: fsResponseText := 'HTTP version not supported';
    else
      fsResponseText := 'Unknown Response Code';
  end;
  fiResponseCode := iCode ;
end;

procedure TWinshoeHTTPClient.SetHost(var sURL: string);
var
  sPrefix:string;
begin
  // Extracts the HOST from the URL
  if Pos('HTTP://',UpperCase(sURL))<>0 then
  begin
    sURL := Copy(sURL,8,Length(sURL)-7);
    sPrefix := 'http://';
  end
  else
    if Pos('HTTPS://',UpperCase(sURL))<>0 then
    begin
      sURL := Copy(sURL,9,Length(sURL)-8);
      sPrefix := 'https://';
    end;
  if Pos('/',sURL)<>0 then
  begin
    Host := Copy(sURL,1, Pos('/',sURL)-1);
    sURL := sPrefix + sURL;
  end
  else
  begin // If no file is specified, we need the last '/'
    Host := sURL ;
    sURL := sPrefix + sURL + '/';
  end ;
end;

constructor TWinshoeHTTPClient.Create;
begin
  inherited;
  Port := WSPORT_HTTP;
  // Create the headers
  FHeaders := THeaderInfo.Create(self);
  // Set the local host
  FHeaders.fsHost := LocalAddress ;
  // Create stringlist to hold internal headers
  fslstHeaders := TStringList.Create;
  fslstOptionalHeaders := TStringList.Create;

  // POst data
  fslstPostData := TStringList.Create;
end;

destructor TWinshoeHTTPClient.Destroy;
begin
  FHeaders.Free;

  fslstHeaders.Free;
  fslstOptionalHeaders.Free;
  fslstPostData.Free;
  inherited Destroy;
end;

// GET
procedure TWinshoeHTTPClient.Get(sURL: string; const strmDest:TStream);
begin
  SetHost(sURL);
  if not Connected then
    Connect ;
  Request('GET',sURL);
  if (ResponseCode div 100) <> 2 then
    raise EWinshoeHTTPException.CreateFmt('%d: %s',[Responsecode,ResponseText]);
  GetResponse;
  // Write to stream
  ReadToStream(strmDest,HeaderInfo.ContentLength);
  Disconnect;
end;


procedure TWinshoeHTTPClient.SetOptionalHeaders(const Value: TStrings);
begin
  FslstOptionalHeaders.Assign(Value);
end;

// HEAD
procedure TWinshoeHTTPClient.Head(sURL: string);
begin
  SetHost(sURL);
  if not Connected then
    Connect ;
  Request('HEAD',sURL);
  if (ResponseCode div 100) <> 2 then
    raise EWinshoeHTTPException.CreateFmt('%d: %s',[ResponseCode,ResponseText]);
  GetResponse;
  Disconnect;
end;

procedure TWinshoeHTTPClient.Request(const sCommand:string; sURL:string);
var
  i,iResponse:integer;
  s:string;
begin
  // Clear headers
  fslstHeaders.Clear;
  // Add headers
  FHeaders.SetHeaders(fslstHeaders);

  WriteLn(sCommand + ' '+ sURL + ' HTTP/1.1');
  WriteLn('Host: '+Host);
  // write the headers
  for i := 0 to Pred ( fslstHeaders.Count ) do
    WriteLn(fslstHeaders.Strings[i]);
  // write optional headers
  if fslstOptionalHeaders.Count > 0 then
    for i := 0 to Pred ( fslstOptionalHeaders.Count ) do
      WriteLn(fslstOptionalHeaders.Strings[i]);
  WriteLn('');
  s := ReadLn;
  // Examine the response code
  s := Copy(s, Pos(' ',s)+1, Length(s)-Pos(' ',s)+1);
  iResponse := StrToInt(Copy(s,1,3));
  HandleResponseCode(iResponse);
end;

procedure TWinshoeHTTPClient.SetPostData(const Value: TStrings);
begin
  FslstPostData.Assign(Value);
end;


procedure TWinshoeHTTPClient.Post(sURL: string; const strmDest: TStream);
var
  i:integer;
begin
  SetHost(sURL);
  if not Connected then
    Connect ;
  // Set additional headers for post
  FHeaders.ContentLength := Length(fslstPostData.Text);
  Request('POST',sURL);
  if ((ResponseCode div 100) <> 1) and ((ResponseCode div 100) <> 2) then
    raise EWinshoeHTTPException.CreateFmt('%d: %s',[ResponseCode,ResponseText]);
  // Now add the contents of the post
  for i := 0 to PostData.Count - 1 do
    WriteLn(PostData.Strings[i]);
  WriteLn('');
  GetResponse;
  // Write to stream
  ReadToStream(strmDest,HeaderInfo.ContentLength);
  Disconnect;
end;

procedure TWinshoeHTTPClient.GetResponse;
var
  s:string;
  i:integer;
begin
  // Set the response headers
  // Clear headers
  fslstHeaders.Clear ;
  s := ReadLn ;
  while s<>'' do
  begin
    fslstHeaders.Add(StringReplace(s,':','=',[]));
    s := ReadLn;
  end;
  FHeaders.GetHeaders(fslstHeaders);
  for i := 0 to Pred ( fslstHeaders.Count ) do
    fslstOptionalHeaders.Add(StringReplace(fslstHeaders.Strings[i],':','=',[]));
end;

end.
