// *****************************************************************************
//   File    : UClient.pas
//   Project : MicroServer.dpr
//             Easy example of TCP Client with indy component : TidTCPSever
//
//   see indy doc: http://www.indyproject.org/sockets/docs/index.en.aspx
//
// *****************************************************************************
unit UClient;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, IdBaseComponent, uPacket,
    IdComponent, IdTCPConnection, IdTCPClient, IdThreadComponent, IdGlobal, XMLIntf,
    XMLDoc, uCompression;

type
    TFClient = class(TForm)

    Label1        : TLabel;
    Label2        : TLabel;

    messageToSend : TMemo;
    messagesLog   : TMemo;

    btn_connect   : TButton;
    btn_disconnect: TButton;
    btn_send      : TButton;
    btn_sendreq: TButton;
    btn_savelog: TButton;
    btn_weather: TButton;
    btn_route: TButton;
    btn_nmea: TButton;
    btn_sysinfo: TButton;
    btn_readfile: TButton;


    procedure FormShow(Sender: TObject);

    procedure btn_connectClick(Sender: TObject);
    procedure btn_disconnectClick(Sender: TObject);
    procedure btn_sendClick(Sender: TObject);

    procedure IdTCPClientConnected(Sender: TObject);
    procedure IdTCPClientDisconnected(Sender: TObject);

    procedure IdThreadComponentRun(Sender: TIdThreadComponent);

    procedure Display(p_sender: String; p_message: string);
    function  GetNow():String;
    procedure FormCreate(Sender: TObject);
    procedure btn_sendreqClick(Sender: TObject);
    procedure btn_savelogClick(Sender: TObject);
    procedure btn_weatherClick(Sender: TObject);
    procedure btn_routeClick(Sender: TObject);
    procedure btn_nmeaClick(Sender: TObject);
    procedure btn_readfileClick(Sender: TObject);

    private
    procedure FormatXMLFile(const XmlFile: string);
        { Private declarations }
    public
        { Public declarations }
    end;

    // ... listening port : GUEST CLIENT
//    const PORT = 20010;
    const PORT = 13102;
          HOST = '192.168.0.240';
//          HOST = '127.0.0.1';

    var
        FClient             : TFClient;

        // ... TIdTCPClient
        idTCPClient         : TIdTCPClient;

        // ... TIdThreadComponent
        idThreadComponent   : TIdThreadComponent;
        PacketHeader: TReqPacketHeader;
        byteBodyFromServer: TIdBytes;
        BodyLength: Integer;

implementation

{$R *.dfm}

// *****************************************************************************
//   EVENT : onCreate()
//           ON CREATE FORM
// *****************************************************************************
procedure TFClient.FormCreate(Sender: TObject);
begin
    // ... create TIdTCPClient
    idTCPClient                 := TIdTCPClient.Create();

    // ... set properties
    idTCPClient.Host            := HOST;
    idTCPClient.Port            := PORT;
    // ... etc..

    // ... callback functions
    idTCPClient.OnConnected     := IdTCPClientConnected;
    idTCPClient.OnDisconnected  := IdTCPClientDisconnected;
    // ... etc..

    // ... create TIdThreadComponent
    idThreadComponent           := TIdThreadComponent.Create();

    // ... callback functions
    idThreadComponent.OnRun     := IdThreadComponentRun;
    // ... etc..

end;
// .............................................................................


// *****************************************************************************
//   EVENT : onShow()
//           ON SHOW FORM
// *****************************************************************************
procedure TFClient.FormShow(Sender: TObject);
begin

    // ... INITAILIZE

    // ... message to send
    messageToSend.Clear;
    messageToSend.Enabled     := False;

    // ... clear log
    messagesLog.Clear;

    // ... buttons
    btn_connect.Enabled       := True;
    btn_disconnect.Enabled    := False;
    btn_send.Enabled          := False;

end;
// .............................................................................


// *****************************************************************************
//   EVENT : btn_connectClick()
//           CLICK ON CONNECT BUTTON
// *****************************************************************************
procedure TFClient.btn_connectClick(Sender: TObject);
begin

  // ... disable connect button
  btn_connect.Enabled := False;

  // ... try to connect to Server
  try
    if not IdTCPClient.Connected then
      IdTCPClient.Connect;
  except
      on E: Exception do begin
          Display('CLIENT', 'CONNECTION ERROR! ' + E.Message);
          btn_connect.Enabled := True;
      end;
  end;
end;
// .............................................................................


// *****************************************************************************
//   EVENT : btn_disconnectClick()
//           CLICK ON DISCONNECT BUTTON
// *****************************************************************************
procedure TFClient.btn_disconnectClick(Sender: TObject);
begin
  // ... is connected?
  if IdTCPClient.Connected then
  begin
    // ... disconnect from Server
    IdTCPClient.Disconnect;

    // ... set buttons
    btn_connect.Enabled       := True;
    btn_disconnect.Enabled    := False;
    btn_send.Enabled          := False;
    messageToSend.Enabled     := False;
  end;
end;
// .............................................................................


// *****************************************************************************
//   EVENT : onConnected()
//           OCCURS WHEN A CLIENT IS CONNETED
// *****************************************************************************
procedure TFClient.IdTCPClientConnected(Sender: TObject);
begin

    // ... messages log
    Display('CLIENT', 'CONNECTED!');

    // ... after connection is ok, run the Thread ... waiting messages from server
    IdThreadComponent.Active  := True;

    // ... set buttons
    btn_connect.Enabled       := False;
    btn_disconnect.Enabled    := True;
    btn_send.Enabled          := True;

    // ... enable message to send
    messageToSend.Enabled     := True;

end;
// .............................................................................


// *****************************************************************************
//   EVENT : onDisconnected()
//           OCCURS WHEN CLIENT IS DISCONNECTED
// *****************************************************************************
procedure TFClient.IdTCPClientDisconnected(Sender: TObject);
begin
    // ... message log
    Display('CLIENT', 'DISCONNECTED!');
end;
// .............................................................................


// *****************************************************************************
//   EVENT : btn_sendClick()
//           CLICK ON SEND BUTTON
// *****************************************************************************
procedure TFClient.btn_savelogClick(Sender: TObject);
begin
  messagesLog.Lines.SaveToFile('log.txt');
end;

procedure TFClient.btn_sendClick(Sender: TObject);
begin
    // ... send message to Server
    IdTCPClient.IOHandler.WriteLn(messageToSend.Text);
end;

procedure TFClient.btn_sendreqClick(Sender: TObject);
const
  // Route
  Header: array[0..78] of Integer = (122, 123, 53, 102, 51, 51, 48, 55, 100, 98, 45, 97, 101, 56, 50, 45, 53, 54, 50, 57, 45, 102, 100, 100, 49, 45, 97, 52, 55, 56, 52, 99, 49, 102, 102, 51, 99, 55, 1, 41, 70, 15, 0, 54, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  Body: array[0..51] of Integer = (102, 14, 13, 91, 77, 43, 63, 63, 34, 2, 63, 15, 100, 68, 63, 63, 0, 63, 63, 109, 72, 63, 52, 63, 63, 63, 63, 63, 63, 32, 68, 64, 50, 48, 49, 57, 48, 53, 48, 50, 49, 48, 50, 48, 49, 57, 48, 53, 48, 56, 49, 50);
  // Weather
//  Header: array[0..78] of Integer = (122, 123, 52, 101, 50, 50, 102, 53, 99, 57, 45, 57, 100, 55, 48, 45, 52, 52, 49, 56, 45, 101, 98, 98, 102, 45, 57, 51, 54, 54, 51, 97, 48, 101, 101, 49, 98, 53, 1, 40, 70, 15, 0, 60, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
//  Body: array[0..59] of Integer = (119, 101, 97, 116, 104, 101, 114, 100, 97, 116, 97, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 115, 115, 112, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
var
  ReqByte: TIdBytes;
  i, count: Integer;
//  ResByte: TIdBytes;
begin
  SetLength(ReqByte, Length(Header)+Length(Body));
  for i := Low(Header) to High(Header) do
  begin
    ReqByte[i] := Header[i];
  end;

  for i := Length(Header) to Length(Header)+Length(Body)-1 do
  begin
    ReqByte[i] := Body[i-Length(Header)];
  end;

//  IdTCPClient.IOHandler.Write(ReqByte, SizeOf(ReqByte));

  // ... send message to Server
  IdTCPClient.IOHandler.WriteLn(IndyTextEncoding_ASCII.GetString(ReqByte));
end;

procedure TFClient.btn_weatherClick(Sender: TObject);
const
  // Weather
  Header: array[0..78] of Integer = (122, 123, 52, 101, 50, 50, 102, 53, 99, 57, 45, 57, 100, 55, 48, 45, 52, 52, 49, 56, 45, 101, 98, 98, 102, 45, 57, 51, 54, 54, 51, 97, 48, 101, 101, 49, 98, 53, 1, 40, 70, 15, 0, 60, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  Body: array[0..59] of Integer = (119, 101, 97, 116, 104, 101, 114, 100, 97, 116, 97, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 115, 115, 112, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
var
  ReqByte: TIdBytes;
  i, count: Integer;
  F: File;
begin
  SetLength(ReqByte, Length(Header)+Length(Body));
  for i := Low(Header) to High(Header) do
  begin
    ReqByte[i] := Header[i];
  end;

  for i := Length(Header) to Length(Header)+Length(Body)-1 do
  begin
    ReqByte[i] := Body[i-Length(Header)];
  end;

  AssignFile(F, 'SendReq_Weather.bin');
  Rewrite(F, 4);

  BlockWrite(F, ReqByte, 2);

  CloseFile(F);

  // ... send message to Server
  IdTCPClient.IOHandler.WriteLn(IndyTextEncoding_ASCII.GetString(ReqByte));
end;

procedure TFClient.btn_routeClick(Sender: TObject);
const
  // Route
  Header: array[0..78] of Integer = (122, 123, 53, 102, 51, 51, 48, 55, 100, 98, 45, 97, 101, 56, 50, 45, 53, 54, 50, 57, 45, 102, 100, 100, 49, 45, 97, 52, 55, 56, 52, 99, 49, 102, 102, 51, 99, 55, 1, 41, 70, 15, 0, 54, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  Body: array[0..51] of Integer = (102, 14, 13, 91, 77, 43, 63, 63, 34, 2, 63, 15, 100, 68, 63, 63, 0, 63, 63, 109, 72, 63, 52, 63, 63, 63, 63, 63, 63, 32, 68, 64, 50, 48, 49, 57, 48, 53, 48, 50, 49, 48, 50, 48, 49, 57, 48, 53, 48, 56, 49, 50);
var
  ReqByte: TIdBytes;
  i, count: Integer;
  F: File;
begin
  SetLength(ReqByte, Length(Header)+Length(Body));
  for i := Low(Header) to High(Header) do
  begin
    ReqByte[i] := Header[i];
  end;

  for i := Length(Header) to Length(Header)+Length(Body)-1 do
  begin
    ReqByte[i] := Body[i-Length(Header)];
  end;

  AssignFile(F, 'SendReq_Route.bin');
  Rewrite(F, 4);

  BlockWrite(F, ReqByte, 2);

  CloseFile(F);

  // ... send message to Server
  IdTCPClient.IOHandler.WriteLn(IndyTextEncoding_ASCII.GetString(ReqByte));
end;

procedure TFClient.btn_nmeaClick(Sender: TObject);
const
  // NMEA
//  Header: array[0..78] of Integer = (122, 123, 52, 101, 50, 50, 102, 53, 99, 57, 45, 57, 100, 55, 48, 45, 52, 52, 49, 56, 45, 101, 98, 98, 102, 45, 57, 51, 54, 54, 51, 97, 48, 101, 101, 49, 98, 53, 3, 105, 136, 30, 0, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
//  Body: array[0..16] of Integer = (36, 80, 51, 84, 72, 83, 44, 48, 46, 48, 44, 77, 42, 52, 70, 13, 10); //17
//  Body: array[0..16] of Integer = (36, 80, 51, 72, 68, 84, 44, 48, 46, 48, 44, 84, 42, 52, 49, 13, 10); //17
  Header: array[0..78] of Integer = (122, 123, 52, 101, 50, 50, 102, 53, 99, 57, 45, 57, 100, 55, 48, 45, 52, 52, 49, 56, 45, 101, 98, 98, 102, 45, 57, 51, 54, 54, 51, 97, 48, 101, 101, 49, 98, 53, 3, 105, 136, 30, 0, 31, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  Body: array[0..30] of Integer = (36, 80, 51, 86, 72, 87, 44, 48, 46, 48, 44, 84, 44, 44, 44, 48, 46, 48, 44, 78, 44, 48, 46, 48, 44, 75, 42, 53, 53, 13, 10);
var
  ReqByte: TIdBytes;
  i, count: Integer;
  F: File;
begin
  SetLength(ReqByte, Length(Header)+Length(Body));
  for i := Low(Header) to High(Header) do
  begin
    ReqByte[i] := Header[i];
  end;

  for i := Length(Header) to Length(Header)+Length(Body)-1 do
  begin
    ReqByte[i] := Body[i-Length(Header)];
  end;

  AssignFile(F, 'SendReq_NMEA.bin');
  Rewrite(F, 4);

  BlockWrite(F, ReqByte, 2);

  CloseFile(F);

  // ... send message to Server
  IdTCPClient.IOHandler.WriteLn(IndyTextEncoding_ASCII.GetString(ReqByte));
end;

procedure TFClient.btn_readfileClick(Sender: TObject);
var
  i, count: Integer;
  F: File;
  msgFromServer : string;
  byteMsgFromServer: TIdBytes;
  Stream: TStream;
  Stream1: TStream;
  FileStream: TMemoryStream;
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
  end
  else
  begin
    case PacketHeader.MsgType of
      PACKET_TYPE_REQ: ;
      PACKET_TYPE_NOTI: ;
      PACKET_TYPE_RES:
        begin
          len := Length(byteBodyFromServer);
          SetLength(byteBodyFromServer, Length(byteBodyFromServer)+Length(byteMsgFromServer));
          Move(byteMsgFromServer[0], byteBodyFromServer[len], Length(byteMsgFromServer));

          Stream1 := TIdAppendFileStream.Create('temp1.txt');
          try
            Stream1.WriteBuffer(Pointer(byteMsgFromServer)^, Length(byteMsgFromServer));
          finally
            Stream1.Free;
          end;
        end;
    end;
  end;

end;

// .............................................................................


// *****************************************************************************
//   EVENT : onRun()
//           OCCURS WHEN THE SERVER SEND A MESSAGE TO CLIENT
// *****************************************************************************
procedure TFClient.IdThreadComponentRun(Sender: TIdThreadComponent);
var
  msgFromServer : string;
  byteMsgFromServer: TIdBytes;
  FS: TFileStream;
//  Stream: TIdFileCreateStream;
//  Stream: TIdMemoryBufferStream;
  Stream: TStream;
  Stream1: TStream;
  len: Integer;
begin
  // ... read message from server
//  msgFromServer := IdTCPClient.IOHandler.ReadLn();
//  byteMsgFromServer := IndyTextEncoding_ASCII.GetBytes(msgFromServer);

  if BodyLength > 0 then
    IdTCPClient.IOHandler.ReadBytes(byteMsgFromServer, BodyLength)
  else
    IdTCPClient.IOHandler.ReadBytes(byteMsgFromServer, SizeOf(TReqPacketHeader));

////////////////////////////////////////////////////////////////////////////////

//  if PacketHeader.BodySize = 0 then
//  begin
//    IdTCPClient.IOHandler.ReadBytes(byteMsgFromServer, SizeOf(TReqPacketHeader));
//
//  end
//  else
//    IdTCPClient.IOHandler.ReadBytes(byteMsgFromServer, PacketHeader.BodySize);

  Display('SERVER', IndyTextEncoding_ASCII.GetString(byteMsgFromServer));

  if (byteMsgFromServer[0] = PACKET_DELIMITER_1) and (byteMsgFromServer[1] = PACKET_DELIMITER_2) then
  begin
    SetPacketHeader(byteMsgFromServer, PacketHeader);
//    FillChar(byteBodyFromServer, PacketHeader.BodySize, #0);
    case PacketHeader.MsgType of
      PACKET_TYPE_REQ: ;
      PACKET_TYPE_NOTI: ;
      PACKET_TYPE_RES:
        begin
//          len := Length(msgFromServer)-SizeOf(TReqPacketHeader);
//          len := PacketHeader.BodySize;
          BodyLength := PacketHeader.BodySize;
//          SetLength(byteBodyFromServer, BodyLength);
//          Move(byteMsgFromServer[SizeOf(TReqPacketHeader)], byteBodyFromServer[0], len);
//          if FileExists('temp1.txt') then
//            DeleteFile('temp1.txt');
//          Stream1 := TIdFileCreateStream.Create('temp1.txt');
//          try
//            Stream1.WriteBuffer(Pointer(byteBodyFromServer)^, Length(byteBodyFromServer));
//          finally
//            Stream1.Free;
//          end;
        end;
    end;
  end
  else
  begin
    case PacketHeader.MsgType of
      PACKET_TYPE_REQ: ;
      PACKET_TYPE_NOTI: ;
      PACKET_TYPE_RES:
        begin
//          len := Length(byteBodyFromServer);
          SetLength(byteBodyFromServer, BodyLength);
          Move(byteMsgFromServer[0], byteBodyFromServer[0], Length(byteMsgFromServer));
          Display('BODY', IndyTextEncoding_ASCII.GetString(byteBodyFromServer));
          Display('LENGTH', IntToStr(Length(byteBodyFromServer)));
          BodyLength := 0;
//
//          Stream1 := TIdAppendFileStream.Create('temp1.txt');
//          try
//            Stream1.WriteBuffer(Pointer(byteMsgFromServer)^, Length(byteMsgFromServer));
//          finally
//            Stream1.Free;
//          end;
        end;
    end;
  end;
////////////////////////////////////////////////////////////////////////////////

  // ... messages log
//  Display('SERVER', msgFromServer);

//  if PacketHeader.MsgType <> 3 then Exit;

//  if Length(byteBodyFromServer) = 4225 then
////  if Length(byteBodyFromServer) = 533 then
//  begin
//    if FileExists('temp.txt') then
//      DeleteFile('temp.txt');
//
//    Stream := TIdFileCreateStream.Create('temp.txt');
//
//    try
//      Stream.WriteBuffer(Pointer(byteBodyFromServer)^, Length(byteBodyFromServer));
//    finally
////      FS.Free;
//      FillChar(PacketHeader, SizeOf(PacketHeader), 0);
//      Stream.Free;
////      FormatXMLFile('temp.txt');
//    end;
//
////    if FileExists('tempUnzip.txt') then DeleteFile('tempUnzip.txt');
//
////    Decompress('temp.txt','tempUnzip.txt');
//  end;
end;
// .............................................................................

// *****************************************************************************
//   PROCEDURE : Display()
//               DISPLAY MESSAGE UPON SYSOUT
// *****************************************************************************
procedure TFClient.Display(p_sender : String; p_message : string);
begin
  TThread.Queue(nil, procedure
                     begin
                         MessagesLog.Lines.Add('[' + p_sender + '] - '
                         + GetNow() + ': ' + p_message);
                     end
               );
end;
// .............................................................................

// *****************************************************************************
//   FUNCTION : getNow()
//              GET MOW DATE TIME
// *****************************************************************************
function TFClient.getNow() : String;
begin
    Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ': ';
end;
// .............................................................................


Procedure TFClient.FormatXMLFile(const XmlFile:string);
var
  oXml : IXMLDocument;
begin
  oXml := TXMLDocument.Create(nil);
  oXml.LoadFromFile(XmlFile);
  oXml.XML.Text:=xmlDoc.FormatXMLData(oXml.XML.Text);
  oXml.Active := true;
  oXml.SaveToFile(XmlFile);
end;

end.
