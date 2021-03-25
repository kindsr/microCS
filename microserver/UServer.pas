// *****************************************************************************
//   File    : UServer.pas
//   Project : MicroServer.dpr
//             Easy example of TCP Server with indy component : TidTCPSever
//
//   see indy doc: http://www.indyproject.org/sockets/docs/index.en.aspx
//
//
// *****************************************************************************
unit UServer;

{.$DEFINE UDP_ACTIVE}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IdContext, IdComponent,
  Vcl.StdCtrls, IdUDPServer, IdSocketHandle, IdBaseComponent, IdCustomTCPServer,
  IdThreadSafe, IdTCPConnection, IdYarn, IdTCPServer, Vcl.ExtCtrls, IdGlobal,
  IdUDPBase, uPacket, System.Zlib, System.IOUtils, Soap.EncdDecd, uCompression;

type
  TFServer = class(TForm)
    Title: TLabel;
    btn_start: TButton;
    btn_stop: TButton;
    btn_clear: TButton;
    clients_connected: TLabel;
    Label1: TLabel;
    Panel1: TPanel;
    messagesLog: TMemo;
    btn_send: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btn_startClick(Sender: TObject);
    procedure btn_stopClick(Sender: TObject);
    procedure btn_clearClick(Sender: TObject);
    procedure IdTCPServerConnect(AContext: TIdContext);
    procedure IdTCPServerDisconnect(AContext: TIdContext);
    procedure IdTCPServerExecute(AContext: TIdContext);
    procedure IdTCPServerStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
    procedure IdUDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);
    procedure ShowNumberOfClients(p_disconnected: Boolean = False);
    procedure BroadcastMessage(p_message: string);
    procedure Display(p_sender, p_message: string);
    function GetNow(): string;
    procedure btn_sendClick(Sender: TObject);
  private
    function LoadFileToStr(const FileName: TFileName): String;
    procedure StrToFile(const FileName, SourceString: string);
    function Unzip(const zipped: string): string;
    function ByteArrayToStream(const aContent: TArray<Byte>): TMemoryStream;
    function StreamToByteArray(aStream: TMemoryStream): TArray<Byte>;
            { Private declarations }

  public
            { Public declarations }

  end;
    // ...


    // ... listening port
//    const GUEST_CLIENT_PORT = 20010;

const
  HOST_IP = '127.0.0.1';
  GUEST_CLIENT_PORT = 13102;
//  GUEST_CLIENT_PORT = 50001;
  {$IFDEF UDP_ACTIVE}
  GUEST_UDPCLIENT_PORT = 61001;
  {$ENDIF}

const
  TMP_FILE_NAME = '35.070000_128.830000_31.380000_121.600000.rtz';
//  TMP_FILE_NAME = '35.003333_128.791667_33.691667_-118.173333.rtz';

var
  FServer: TFServer;

  // ... Id TCP Server
  IdTCPServer: TIdTCPServer;
  {$IFDEF UDP_ACTIVE}
  IdUDPServer: TIdUDPServer;
  {$ENDIF}
  PacketHeader: TReqPacketHeader;
  FS, FSZIP: TFileStream;
  SendContext: TIdContext;

implementation

{$R *.dfm}

// *****************************************************************************
//   EVENT : onCreate()
//           ON FORM CREATE
// *****************************************************************************
procedure TFServer.FormCreate(Sender: TObject);
begin

    // ... create idTCPServer
  IdTCPServer := TIdTCPServer.Create(self);
  IdTCPServer.Active := False;

    // ... set properties
  IdTCPServer.MaxConnections := 20;

    // ... etc..

    // ... assign a new context class (if you need)
    // IdTCPServer.ContextClass    := TYourContext;

    // ... add some callback functions
  IdTCPServer.OnConnect := IdTCPServerConnect;
  IdTCPServer.OnDisconnect := IdTCPServerDisconnect;
  IdTCPServer.OnExecute := IdTCPServerExecute;
  IdTCPServer.OnStatus := IdTCPServerStatus;
    // ... etc..

    // ... add UDP functions
  {$IFDEF UDP_ACTIVE}
    // ... create idUDPServer
  IdUDPServer := TIdUDPServer.Create(self);
  IdUDPServer.Active := False;

//    IdUDPServer.OnUDPException  := IdUDPException;
  IdUDPServer.OnUDPRead := IdUDPRead;
  {$ENDIF}
end;
// .............................................................................


// *****************************************************************************
//   EVENT : onShow()
//           ON FORM SHOW
// *****************************************************************************

procedure TFServer.FormShow(Sender: TObject);
begin
    // ... INITIALIZE:

    // ... clear message log
  messagesLog.Lines.Clear;

    // ... zero to clients connected
  clients_connected.Caption := inttostr(0);

    // ... set buttons
  btn_start.enabled := True;
  btn_stop.enabled := False;
end;
// .............................................................................


// *****************************************************************************
//   EVENT : btn_startClick()
//           CLICK ON START BUTTON
// *****************************************************************************

procedure TFServer.btn_startClick(Sender: TObject);
begin
    // ... START SERVER:

    // ... clear the Bindings property ( ... Socket Handles )
  IdTCPServer.Bindings.Clear;
    // ... Bindings is a property of class: TIdSocketHandles;

    // ... add listening ports:

    // ... add a port for connections from guest clients.
  IdTCPServer.Bindings.Add.Port := GUEST_CLIENT_PORT;
    // ... etc..

    // ... ok, Active the Server!
  IdTCPServer.Active := True;

    // ... disable start button
  btn_start.enabled := False;

    // ... enable stop button
  btn_stop.enabled := True;

    // ... message log
  Display('SERVER', 'STARTED!');

  {$IFDEF UDP_ACTIVE}
  IdUDPServer.Bindings.Clear;
  IdUDPServer.Bindings.Add.Port := GUEST_UDPCLIENT_PORT;
  IdUDPServer.Active := True;

  Display('UDPSERVER', 'STARTED!');
  {$ENDIF}

end;
// .............................................................................


// *****************************************************************************
//   EVENT : btn_stopClick()
//           CLICK ON STOP BUTTON
// *****************************************************************************

procedure TFServer.btn_stopClick(Sender: TObject);
begin

    // ... before stopping the server ... send 'good bye' to all clients connected
  BroadcastMessage('Goodbye Client ');

    // ... stop server!
  IdTCPServer.Active := False;

    // ... hide stop button
  btn_stop.enabled := False;

    // ... show start button
  btn_start.enabled := True;

    // ... message log
  Display('SERVER', 'STOPPED!');

    // ... stop UDP server!
  {$IFDEF UDP_ACTIVE}
  IdUDPServer.Active := False;
  {$ENDIF}

end;
// .............................................................................


// *****************************************************************************
//   EVENT : btn_clearClick()
//           CLICK ON CLEAR BUTTON
// *****************************************************************************

procedure TFServer.btn_clearClick(Sender: TObject);
begin
    //... clear messages log
  MessagesLog.Lines.Clear;
end;
// .............................................................................

// .............................................................................
// .............................................................................
// .............................................................................

// *****************************************************************************
//   EVENT : onConnect()
//           OCCURS ANY TIME A CLIENT IS CONNECTED
// *****************************************************************************

procedure TFServer.IdTCPServerConnect(AContext: TIdContext);
var
  ip: string;
  port: Integer;
  peerIP: string;
  peerPort: Integer;
  nClients: Integer;
  msgToClient: string;
  typeClient: string;
begin
    // ... OnConnect is a TIdServerThreadEvent property that represents the event
    //     handler signalled when a new client connection is connected to the server.

    // ... Use OnConnect to perform actions for the client after it is connected
    //     and prior to execution in the OnExecute event handler.

    // ... see indy doc:
    //     http://www.indyproject.org/sockets/docs/index.en.aspx

    // ... getting IP address and Port of Client that connected
  ip := AContext.Binding.IP;
  port := AContext.Binding.Port;
  peerIP := AContext.Binding.PeerIP;
  peerPort := AContext.Binding.PeerPort;

    // ... message log
  Display('SERVER', 'Client Connected!');
  Display('SERVER', 'Port=' + IntToStr(port) + ' ' + '(PeerIP=' + peerIP + ' - ' + 'PeerPort=' + IntToStr(peerPort) + ')');

    // ... display the number of clients connected
  ShowNumberOfClients();

    // ... CLIENT CONNECTED:
  case port of
    GUEST_CLIENT_PORT:
      begin
                            // ... GUEST CLIENTS
        typeClient := 'GUEST';
      end;
                          // ...
  end;

    // ... send the Welcome message to Client connected
  msgToClient := 'Welcome ' + typeClient + ' ' + 'Client :)';
  AContext.Connection.IOHandler.WriteLn(msgToClient);

end;
// .............................................................................

// *****************************************************************************
//   EVENT : onDisconnect()
//           OCCURS ANY TIME A CLIENT IS DISCONNECTED
// *****************************************************************************

procedure TFServer.IdTCPServerDisconnect(AContext: TIdContext);
var
  ip: string;
  port: Integer;
  peerIP: string;
  peerPort: Integer;
  nClients: Integer;
begin

    // ... getting IP address and Port of Client that connected
  ip := AContext.Binding.IP;
  port := AContext.Binding.Port;
  peerIP := AContext.Binding.PeerIP;
  peerPort := AContext.Binding.PeerPort;

    // ... message log
  Display('SERVER', 'Client Disconnected! Peer=' + peerIP + ':' + IntToStr(peerPort));

    // ... display the number of clients connected
  ShowNumberOfClients(true);
end;
// .............................................................................


// *****************************************************************************
//   EVENT : onExecute()
//           ON EXECUTE THREAD CLIENT
// *****************************************************************************

procedure TFServer.IdTCPServerExecute(AContext: TIdContext);
var
  Port: Integer;
  PeerPort: Integer;
  PeerIP: string;
  msgFromClient: string;
  msgToClient: string;
  byteMsgFromClient: TIdBytes;
  byteBodyFromServer: TIdBytes;
  tmpBytes: TIdBytes;
  FileStream: TMemoryStream;
  LZip: TZCompressionStream;
  zipString, zipString2, zipString3: string;
  ansiStr: AnsiString;
  ZipStream, UnZipStream: TStream;
  size: Int64;
  len: Integer;
  tmpHeader: TReqPacketHeader;
begin

  // ... OnExecute is a TIdServerThreadEvents event handler used to execute
  //     the task for a client connection to the server.

  // ... here you can check connection status and buffering before reading
  //     messages from client

  // ... see doc:
  // ... AContext.Connection.IOHandler.InputBufferIsEmpty
  // ... AContext.Connection.IOHandler.CheckForDataOnSource(<milliseconds>);
  //     (milliseconds to wait for the connection to become readable)
  // ... AContext.Connection.IOHandler.CheckForDisconnect;

  // ... received a message from the client

  // ... get message from client
  msgFromClient := AContext.Connection.IOHandler.ReadLn;

  // ... getting IP address, Port and PeerPort from Client that connected
  PeerIP := AContext.Binding.PeerIP;
  PeerPort := AContext.Binding.PeerPort;

  // ... message log
  Display('CLIENT', '(Peer=' + PeerIP + ':' + IntToStr(PeerPort) + ') ' + msgFromClient);
  // ...

  // ... process message from Client
  byteMsgFromClient := IndyTextEncoding_ASCII.GetBytes(msgFromClient);



  if (byteMsgFromClient[0] = PACKET_DELIMITER_1) and (byteMsgFromClient[1] = PACKET_DELIMITER_2) then
  begin
    SetPacketHeader(byteMsgFromClient, PacketHeader);

    case PacketHeader.MsgType of
      PACKET_TYPE_REQ:
        begin
          PacketHeader.MsgType := 3;

          // Read BinaryFile
          FileStream := TMemoryStream.Create;

          try
            FileStream.LoadFromFile('3_1011.txt');
            FileStream.Position := 0;
            SetLength(tmpBytes, FileStream.Size);
            FileStream.Read(tmpBytes[0], FileStream.Size);
          finally
            FileStream.Free;
          end;
          FillChar(tmpHeader, SizeOf(TReqPacketHeader), 0);
          SetPacketHeader(tmpBytes, tmpHeader);

          len := tmpHeader.BodySize;
          PacketHeader.BodySize := len;
          byteMsgFromClient := GetPacketHeaderBytes(PacketHeader);

          SetLength(byteBodyFromServer, len);
//          FillChar(byteBodyFromServer, len, #0);
          Move(tmpBytes[SizeOf(TReqPacketHeader)], byteBodyFromServer[0], len);
//          if FileExists('temp1.txt') then
//            DeleteFile('temp1.txt');
//          Stream1 := TIdFileCreateStream.Create('temp1.txt');
//          try
//            Stream1.WriteBuffer(Pointer(byteBodyFromServer)^, Length(byteBodyFromServer));
//          finally
//            Stream1.Free;
//          end;
          AContext.Connection.IOHandler.Write(byteMsgFromClient);
          AContext.Connection.IOHandler.Write(byteBodyFromServer);
        end;

      PACKET_TYPE_NOTI: ;
      PACKET_TYPE_RES:
        begin
          len := PacketHeader.BodySize;
          SetLength(byteBodyFromServer, len);
//          FillChar(byteBodyFromServer, len, #0);
          Move(byteMsgFromClient[SizeOf(TReqPacketHeader)], byteBodyFromServer[0], len);

          AContext.Connection.IOHandler.Write(byteMsgFromClient);
          AContext.Connection.IOHandler.Write(byteBodyFromServer);


        end;
    end;
  end;

  // ...

  // ... send response to Client

//  FS := TFileStream.Create(TMP_FILE_NAME+'.gz', fmOpenRead or fmShareDenyWrite);
//  Decompress(TMP_FILE_NAME+'.gz', TMP_FILE_NAME+'.gz'+'.un');

//  AContext.Connection.IOHandler.WriteLn('... message sent from server :)');

end;

procedure TFServer.btn_sendClick(Sender: TObject);
var
  byteMsgFromServer: TIdBytes;
  byteBodyFromServer: TIdBytes;
  FileStream: TMemoryStream;
  Stream1: TStream;
  len: Integer;
begin
  // Read BinaryFile
  FileStream := TMemoryStream.Create;

  try
    FileStream.LoadFromFile('3_1011.txt');
    FileStream.Position := 0;
    SetLength(byteMsgFromServer, FileStream.Size);
    FileStream.Read(byteMsgFromServer[0], FileStream.Size);
  finally
    FileStream.Free;
  end;

  // Response ¹ÞÀ½

  if (byteMsgFromServer[0] = PACKET_DELIMITER_1) and (byteMsgFromServer[1] = PACKET_DELIMITER_2) then
  begin
    SetPacketHeader(byteMsgFromServer, PacketHeader);

    case PacketHeader.MsgType of
      PACKET_TYPE_REQ: ;
      PACKET_TYPE_NOTI: ;
      PACKET_TYPE_RES:
        begin
          len := PacketHeader.BodySize;
          SetLength(byteBodyFromServer, len);
//          FillChar(byteBodyFromServer, len, #0);
          Move(byteMsgFromServer[SizeOf(TReqPacketHeader)], byteBodyFromServer[0], len);

          SendContext.Connection.IOHandler.Write(byteMsgFromServer);
          SendContext.Connection.IOHandler.Write(byteBodyFromServer);

          if FileExists('temp1.txt') then
            DeleteFile('temp1.txt');
          Stream1 := TIdFileCreateStream.Create('temp1.txt');
          try
            Stream1.WriteBuffer(Pointer(byteBodyFromServer)^, Length(byteBodyFromServer));
          finally
            Stream1.Free;
          end;
        end;
    end;
  end;
end;

// .............................................................................


// *****************************************************************************
//   EVENT : onStatus()
//           ON STATUS CONNECTION
// *****************************************************************************

procedure TFServer.IdTCPServerStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
begin
    // ... OnStatus is a TIdStatusEvent property that represents the event handler
    //     triggered when the current connection state is changed...

    // ... message log
  Display('SERVER', AStatusText);
end;
// .............................................................................

// .............................................................................
// .............................................................................
// .............................................................................

// *****************************************************************************
//   EVENT : IdUDPRead()
//           OCCURS ANY TIME A CLIENT SEND MESSAGE
// *****************************************************************************
procedure TFServer.IdUDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);
var
//  DataStringStream: TStringStream;
  s: string;
  Port: Integer;
  PeerPort: Integer;
  PeerIP: string;
  msgFromClient: string;
begin
//  DataStringStream := TStringStream.Create('');
//  try
  try
      // ... get message from client
//      DataStringStream.CopyFrom(AData, AData.Size);
//      msgFromClient := DataStringStream.DataString;
    msgFromClient := BytesToString(AData);

    Display('UDPCLIENT', '(Peer=' + PeerIP + ':' + IntToStr(PeerPort) + ') ' + msgFromClient);
//      Memo1.Text := s;
  except
    on E: Exception do
    begin
    end;
  end;
//  finally
//    FreeAndNil(DataStringStream);
//  end;
end;

// .............................................................................

// *****************************************************************************
//   FUNCTION : getNow()
//              GET MOW DATE TIME
// *****************************************************************************
function TFServer.getNow(): string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ': ';
end;
// .............................................................................


// *****************************************************************************
//   PROCEDURE : broadcastMessage()
//               BROADCAST A MESSAGE TO ALL CLIENTS CONNECTED
// *****************************************************************************

procedure TFServer.broadcastMessage(p_message: string);
var
  tmpList: TList;
  contexClient: TidContext;
  nClients: Integer;
  i: integer;
begin

    // ... send a message to all clients connected

    // ... get context Locklist
  tmpList := IdTCPServer.Contexts.LockList;

  try
    i := 0;
    while (i < tmpList.Count) do
    begin
            // ... get context (thread of i-client)
      contexClient := tmpList[i];

            // ... send message to client
      contexClient.Connection.IOHandler.WriteLn(p_message);
      i := i + 1;
    end;

  finally
        // ... unlock list of clients!
    IdTCPServer.Contexts.UnlockList;
  end;
end;
// .............................................................................

// *****************************************************************************
//   PROCEDURE : Display()
//               DISPLAY MESSAGE UPON SYSOUT
// *****************************************************************************

procedure TFServer.Display(p_sender: string; p_message: string);
begin
    // ... DISPLAY MESSAGE
  TThread.Queue(nil,
    procedure
    begin
      MessagesLog.Lines.Add('[' + p_sender + '] - ' + getNow() + ': ' + p_message);
    end);

    // ... see doc..
    // ... TThread.Queue() causes the call specified by AMethod to
    //     be asynchronously executed using the main thread, thereby avoiding
    //     multi-thread conflicts.
end;
// .............................................................................

// *****************************************************************************
//   PROCEDURE : ShowNumberOfClients()
//               NUMBER OF CLIENTS CONNECTD
// *****************************************************************************

procedure TFServer.ShowNumberOfClients(p_disconnected: Boolean = False);
var
  nClients: integer;
begin

  try
        // ... get number of clients connected
    nClients := IdTCPServer.Contexts.LockList.Count;
  finally
    IdTCPServer.Contexts.UnlockList;
  end;

    // ... client disconnected?
  if p_disconnected then
    dec(nClients);

    // ... display
  TThread.Queue(nil,
    procedure
    begin
      clients_connected.Caption := IntToStr(nClients);
    end);
end;
// .............................................................................


function TFServer.LoadFileToStr(const FileName: TFileName): String;
var
  FileStream : TFileStream;
  Bytes: TBytes;
begin
  Result:= '';
  FileStream:= TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    if FileStream.Size>0 then begin
      SetLength(Bytes, FileStream.Size);
      FileStream.Read(Bytes[0], FileStream.Size);
    end;
    Result:= TEncoding.ASCII.GetString(Bytes);
  finally
    FileStream.Free;
  end;
end;

procedure TFServer.StrToFile(const FileName, SourceString : string);
var
  Stream : TFileStream;
begin
  Stream:= TFileStream.Create(FileName, fmCreate);
  try
    Stream.WriteBuffer(Pointer(SourceString)^, Length(SourceString));
  finally
    Stream.Free;
  end;
end;

function TFServer.Unzip(const zipped: string): string;
var
  DecompressionStream: TDecompressionStream;
  Compressed: TBytesStream;
  Decompressed: TStringStream;
begin
  Compressed := TBytesStream.Create(DecodeBase64(AnsiString(zipped)));
  try
    // window bits set to 15 + 16 for gzip
    DecompressionStream := TDecompressionStream.Create(Compressed, 15 + 16);
    try
      Decompressed := TStringStream.Create('', TEncoding.UTF8);
      try
        Decompressed.LoadFromStream(DecompressionStream);
        Result := Decompressed.DataString;
      finally
        Decompressed.Free;
      end;
    finally
      DecompressionStream.Free;
    end;
  finally
    Compressed.Free;
  end;
end;

function TFServer.ByteArrayToStream(const aContent: TArray<Byte>): TMemoryStream;
begin
  Result := TMemoryStream.Create;
  try
    if Length(aContent) > 0 then
      Result.WriteBuffer(aContent[0], Length(aContent));
    Result.Position := 0;
  except
    Result.Free;
    raise;
  end;
end;

function TFServer.StreamToByteArray(aStream: TMemoryStream): TArray<Byte>;
begin
  if Assigned(aStream) then
  begin
    SetLength(Result, aStream.Size);
    if Length(Result) > 0 then
      Move(aStream.Memory^, Result[0], aStream.Size);
  end
  else
    SetLength(Result, 0);
end;

end.
