unit ICore.Stream.Net;

interface
uses WinSock, ICore.Stream, ICore.Threads, ICore.Log, SysUtils;

const
  ERROR_CONNECT = 'can''t connect to host';
  ERROR_HOST    = 'can''t get host address';
  ERROR_RECEIVE = 'receive error';
  NET_BUF_SIZE  = 4096;

type

  TProxyType = (ptNone, ptUnknows, ptHTTP, ptSOCKS4);
  TNetStatus = (nsConnect, nsDisconnect, nsError, nsErrorProxy);

  TStreamNet = class;

  TOnNetStatus = procedure (StreamNet: TStreamNet; NetStatus: TNetStatus; const Msg: string = '');
  TOnNetEvent = procedure (StreamNet: TStreamNet);

  TLongArray = array [Word] of LongWord;
  PLongArray = ^TLongArray;

{$REGION 'TStreamNet = class(TStream)'}
  TStreamNet = class(TStream)
    constructor Create;
    destructor Destroy; override;
  private
    FSocket: TSocket;
    FThread: TThread;
    FConnected: Boolean;
    FBuffer: array [0..(NET_BUF_SIZE - 1)] of Byte;
    FCS: TCriticalSection;
    function ProxyConnect: Boolean;
    function DetectProxy(var Addr: TSockAddrIn): Boolean;
  public
    Host: AnsiString;
    Port: Word;
    Data: Pointer;
    onStatus: TOnNetStatus;
    onReceive: TOnNetEvent;
    function Read(out Buf; Count: LongInt): LongWord; override;
    function Write(const Buf; Count: LongInt): LongWord; override;
    procedure Connect;
    procedure Disconnect;
    property Connected: Boolean read FConnected;
  end;
{$ENDREGION}

procedure SocketsInit;
procedure SocketsFree;

var
  Proxy: record
    Type_: TProxyType;
    Host: AnsiString;
    Port: Word;
    Pass: AnsiString;
  end;

implementation

{$REGION 'Sockets'}
procedure SocketsInit;
var
  tmpData: TWSAData;
begin
  WSAStartup(257, tmpData);
end;

procedure SocketsFree;
begin
  WSACleanup;
end;

procedure SocketThread(StreamNet: TStreamNet); stdcall;
label alarm;
var
  Count: LongInt;
  PtrHostEnt: PHostEnt;
  Addr: TSockAddrIn;
begin
  with StreamNet do begin
    FSocket := socket(AF_INET, SOCK_STREAM, 0);

    FillChar(Addr, SizeOf(Addr), 0);
    Addr.sin_family := AF_INET;

    if (Proxy.Type_ = ptNone) then begin
      Addr.sin_port := htons(Port);
      PtrHostEnt := gethostbyname(PAnsiChar(Host));
      if (PtrHostEnt <> nil) then
       Addr.sin_addr.S_addr := PInAddr(PtrHostEnt^.h_addr_list^)^.S_addr
      else
       onStatus(StreamNet, nsError, ERROR_HOST);
    end else begin
      Addr.sin_port := htons(Proxy.Port);
      PtrHostEnt := gethostbyname(PAnsiChar(Proxy.Host));
      if (PtrHostEnt <> nil) then
       Addr.sin_addr.S_addr := PInAddr(PtrHostEnt.h_addr_list^)^.S_addr
      else
       onStatus(StreamNet, nsErrorProxy);
    end;

    if (PtrHostEnt = nil) then
     goto alarm;

    Count := WinSock.connect(FSocket, Addr, SizeOf(Addr));
    if (@onStatus <> nil) then
     if (Count <> SOCKET_ERROR) then begin
      FConnected := True;
      if (Proxy.Type_ = ptNone) then
       onStatus(StreamNet, nsConnect)
      else
       if (not DetectProxy(Addr)) or (not ProxyConnect) then
        onStatus(StreamNet, nsErrorProxy);
     end else
      if (Proxy.Type_ = ptNone) then
       onStatus(StreamNet, nsError, ERROR_CONNECT)
      else
       onStatus(StreamNet, nsErrorProxy);

    while (Connected) do begin
      Count := recv(FSocket, FBuffer, SizeOf(FBuffer), 0);
      if (Count > 0) then begin
        FPos := 0;
        FSize := Count;
        {$IFDEF DEBUG}
        Log.Info('r: ' + IntToStr(Count));
        {$ENDIF}
        if (@onReceive <> nil) then
         onReceive(StreamNet);
      end else
       if (Count <= 0) then
        Break;
    end;
  end;
alarm:
  StreamNet.FThread.Free;
  StreamNet.Free;
end;
{$ENDREGION}

{$REGION 'TStreamNet'}
constructor TStreamNet.Create;
begin
  inherited;
  FCS.Init;
end;

destructor TStreamNet.Destroy;
begin
  inherited;
  if (FThread <> nil) then begin
    FThread.Wait;
    FThread.Free;
    FThread := nil;
    FCS.Free;
  end;
end;

function TStreamNet.ProxyConnect: Boolean;
var
  Count: LongInt;
  Msg: AnsiString;
  Hello: AnsiString;
  PtrHostEnt: PHostEnt;
begin
  Result := False;
  Hello := '';
  case Proxy.Type_ of
    ptHTTP: Hello := 'CONNECT ' + AnsiString(Host) + ':' + AnsiString(IntToStr(Port)) + ' HTTP/1.x' + CRLF + CRLF;
    ptSOCKS4: begin
      PtrHostEnt := gethostbyname(PAnsiChar(Host));
      if (PtrHostEnt = nil) then begin
        onStatus(Self, nsErrorProxy, ERROR_HOST);
        Disconnect;
        Exit;
      end;
      SetLength(Hello, 9);
      PWordArray(@Hello[1])[0] := $0104;
      PWordArray(@Hello[1])[1] := htons(Port);
      PLongArray(@Hello[1])[1] := PInAddr(PtrHostEnt.h_addr_list^)^.S_addr;
      PByteArray(@Hello[1])[9] := 0;
    end;
  end;
  Write(Hello[1], Length(Hello));

  Msg := '';
  while Connected do begin
    Count := recv(FSocket, FBuffer, SizeOf(FBuffer), 0);
    if (Count > 0) then begin
      case Proxy.Type_ of
        ptHTTP: begin
            Msg := Msg + Copy(PAnsiChar(@FBuffer), 1, Count);
            if (Pos(AnsiString(CRLF + CRLF), Msg) > 0) then begin
              {$IFDEF DEBUG}
                Log.Info(Msg);
              {$ENDIF}
              case StrToIntDef(string(Copy(Msg, 10, 3)), 0) of
                200: begin
                    onStatus(Self, nsConnect);
                    Result := True;
                    Break;
                  end;
//                407:;//proxy-authorization
                else
                 Disconnect;
              end;
            end;
          end;
        ptSOCKS4:
          case FBuffer[1] of
           $5A: begin
             onStatus(Self, nsConnect);
             Result := True;
             Break;
           end;
           else
            {$IFDEF DEBUG}
             Log.Error('SOCKS4');
            {$ENDIF}
           Disconnect;
          end;
      end;
    end else
     Disconnect;
  end;
end;

function TStreamNet.DetectProxy(var Addr: sockaddr_in): Boolean;
var
  Socket: TSocket;
  Hello: AnsiString;
  Count: LongInt;
  Buf: array [0..3] of AnsiChar;
begin
  Result := False;
  if (Proxy.Type_ = ptUnknows) then begin
    Socket := WinSock.socket(AF_INET, SOCK_STREAM, 0);
    if (WinSock.connect(Socket, Addr, SizeOf(Addr)) <> SOCKET_ERROR) then begin
      Hello := 'GET / HTTP/1.x' + CRLF + CRLF;
      send(Socket, Hello[1], Length(Hello), 0);
      Buf := '    ';
      Count := recv(Socket, Buf[0], 4, 0);
      while (Count < 4) and (Count > 0) do
       Count := Count + recv(Socket, Buf[Count], 4 - Count, 0);
      if (Buf = 'HTTP') then
       Proxy.Type_ := ptHTTP
      else
       Proxy.Type_ := ptSOCKS4;
      Result := True;
      closesocket(Socket);
    end;
  end else
   Result := True;
end;

function TStreamNet.Read(out Buf; Count: Integer): LongWord;
begin
  if (Size > (Self.Size - Position)) then
   Result := Self.Size - Position
  else
   Result := Size;
  Move(FBuffer[Position], Data, Result);
  Position := Position + Result;
end;

function TStreamNet.Write(const Buf; Count: Integer): LongWord;
var
  i: LongInt;
begin
  Result := 0;
  if FConnected then begin
    FCS.Enter;
    i := 0;
    while (Result < Size) and (i <> SOCKET_ERROR) do begin
      i := WinSock.send(FSocket, PByteArray(@Data)[Result], Size - Result, 0);
      Inc(Result, i);
    end;
    if (i = SOCKET_ERROR) then
     Disconnect;
    FCS.Leave;
    {$IFDEF DEBUG}
    Log.Info('s: ' + IntToStr(Size));
    {$ENDIF}
  end;
end;

procedure TStreamNet.Connect;
begin
  FThread.Create(@SocketThread, nil, True);
end;

procedure TStreamNet.Disconnect;
begin
  if FConnected then begin
    if (@onStatus <> nil) then
     onStatus(Self, nsDisconnect);
    FConnected := False;
    closesocket(FSocket);
    FSocket := INVALID_SOCKET;
  end;
end;
{$ENDREGION}

end.
