unit Stream;

interface

uses Classes;

type
  TTextStream = class(TObject)
      private
        FHost: TStream;
        FOffset,FSize: Integer;
        FBuffer: array[0..1048575] of Char;
        FEOF: Boolean;
        function FillBuffer: Boolean;
      protected
        property Host: TStream read FHost;
      public
        constructor Create(AHost: TStream);
        function ReadLn: string; overload;
        function ReadLn(out Data: string): Boolean; overload;
        property EOF: Boolean read FEOF;
        property HostStream: TStream read FHost;
        property Offset: Integer read FOffset write FOffset;
      end;

implementation

    constructor TTextStream.Create(AHost: TStream);
    begin
      FHost := AHost;
      FillBuffer;
    end;

    function TTextStream.FillBuffer: Boolean;
    begin
      FOffset := 0;
      FSize := FHost.Read(FBuffer, SizeOf(FBuffer));
      Result := FSize > 0;
      FEOF := Result;
    end;

    function TTextStream.ReadLn(out Data: string): Boolean;
    var
      Len, Start: Integer;
      EOLChar: Char;
    begin
      Data := '';
      Result := False;
      repeat
        if FOffset >= FSize then
          if not FillBuffer then
            Exit; // no more data to read from stream -> exit
        Result := True;
        Start := FOffset;
        while (FOffset < FSize) and (not (FBuffer[FOffset] in [#13, #10])) do
          Inc(FOffset);
        Len := FOffset - Start;
        if Len > 0 then begin
          SetLength(Data, Length(Data) + Len);
          Move(FBuffer[Start], Data[Succ(Length(Data) - Len)], Len);
        end else
          Data := '';
      until FOffset <> FSize; // EOL char found
      EOLChar := FBuffer[FOffset];
      Inc(FOffset);
      if (FOffset = FSize) then
        if not FillBuffer then
          Exit;
      if FBuffer[FOffset] in ([#13, #10] - [EOLChar]) then begin
        Inc(FOffset);
        if (FOffset = FSize) then
          FillBuffer;
      end;
    end;

    function TTextStream.ReadLn: string;
    begin
      ReadLn(Result);
    end;

    end.
