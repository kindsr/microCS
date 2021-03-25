unit uPacket;

interface

uses
  Windows, IdGlobal;

const
  // #[02] Packet Sender
  PACKET_DELIMITER_1 = $7A;
  PACKET_DELIMITER_2 = $7B;

  // PACKET TYPE DEFINITION
  PACKET_TYPE_REQ		 = 1;
  PACKET_TYPE_NOTI	 = 2;
  PACKET_TYPE_RES		 = 3;
  PACKET_TYPE_ACK		 = 4;
  PACKET_TYPE_NAK		 = 5;

  // NEW ECDIS REQUEST
  MSG_WEATHER				 = 1001000;
  MSG_ROUTE					 = 1001001;

  // Gateway REQUEST
  MSG_UPDATE_ENC		 = 2001000;
  MSG_NMEA           = 2001001; // NMEA message
  MSG_SYSTEM_DATA		 = 2001002; // CPU, Memory, Disk data
  MSG_ENC_REPORT		 = 2001003;

type
  TReqPacketHeader = packed record
    Delimiter: array[0..1] of Byte;
    Guid: array[0..35] of AnsiChar;
    MsgType: Byte;
    MsgCode: DWORD;
    BodySize: Integer;
    Reserve: array[0..31] of Byte;
  end;
  PReqPacketHeader = ^TReqPacketHeader;

type
  TReqWeather = packed record
    Topic: array[0..23] of AnsiChar;
    Contents: array[0..15] of AnsiChar;
    StartDate: array[0..9] of AnsiChar;
    EndDate: array[0..9] of AnsiChar;
  end;
  PReqWeather = ^TReqWeather;

type
  TDiameter = packed record
    Latitude: Double;
    Longitude: Double;
  end;
  PDiameter = ^TDiameter;

type
  TReqRoute = packed record
    Departure: TDiameter;
    Arrival: TDiameter;
    DepartureDate: array[0..9] of AnsiChar;
    ArrivalDate: array[0..9] of AnsiChar;
  end;
  PReqRoute = ^TReqRoute;

type
  TSystemResourceData = packed record
    CpuUsagePercents: Byte;
    MemoryUsagePercents: Byte;
    MemoryFreeMegabytes: DWORD;
    DiskUsagePercents: Byte;
    DiskFreeMegabytes: DWORD;
  end;
  PSystemResourceData = ^TSystemResourceData;

procedure SetPacketHeader(const ABytes: TIdBytes; var APacketHeader: TReqPacketHeader);

implementation

procedure SetPacketHeader(const ABytes: TIdBytes; var APacketHeader: TReqPacketHeader);
var
  len, idx: Integer;
  tmpByte: array[0..3] of Byte;
begin
  FillChar(APacketHeader, SizeOf(APacketHeader), 0);
  FillChar(tmpByte, SizeOf(tmpByte), 0);

  idx := 0;

  for len := Low(ABytes) to High(ABytes) do
  begin
    if len in [0, 2, 38, 39, 43, 47, 79] then idx := 0;

    with APacketHeader do
    begin
      case len of
        0..1  : Delimiter[len] := ABytes[len];
        2..37 : Guid[idx] := AnsiChar(Chr(ABytes[len]));
        38    : MsgType := ABytes[len];
        39..42:
          begin
            tmpByte[idx] := ABytes[len];
            if len = 42 then
              MsgCode := PDWORD(@tmpByte)^;
          end;
        43..46:
          begin
            tmpByte[idx] := ABytes[len];
            if len = 46 then
              BodySize := PINT(@tmpByte)^;
          end;
      end;
    end;
    Inc(idx);
  end;
end;

end.
