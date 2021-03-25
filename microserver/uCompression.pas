unit uCompression;

interface

uses
  System.SysUtils, Classes, System.ZLib;

procedure Compress(const ASrc, ADest: string);
procedure Decompress(const ASrc, ADest: string);
function DoCompression(aContent: TArray<Byte>): TArray<Byte>;
function DoDecompression(aContent: TArray<Byte>): TArray<Byte>;
function ZCompressString(aText: string; aCompressionLevel: TZCompressionLevel): string;
function ZDecompressString(aText: string): string;

implementation

procedure Compress(const ASrc, ADest: string);
var
  B: array[1..2048] of byte;
  R: Integer;
  vSrc: TStream;  // source file stream
  vDest: TStream;  // destination file stream
  vCompressor: TStream;  // compression stream
begin
  if not FileExists(ASrc) then
    raise Exception.Create('Source file does not exist');

  vDest := TFileStream.Create(ADest, fmCreate);
  try
    vCompressor := TCompressionStream.Create(clMax, vDest);
    try
      vSrc := TFileStream.Create(ASrc, fmOpenRead);
      try
        repeat
          R := vSrc.Read(B, SizeOf(B));
          if R > 0 then
            vCompressor.Write(B, R);
        until R < SizeOf(B);
//        C.CopyFrom(S, 0);
      finally
        vSrc.Free;
      end;
    finally
      vCompressor.Free;
    end;
  finally
    vDest.Free;
  end;
end;

procedure Decompress(const ASrc, ADest: string);
var
  B: array[1..2048] of byte;
  R: Integer;
  vSrc: TStream;  // source file stream
  vDest: TStream;  // destination file stream
  vDecompressor: TStream;  // compression stream
begin
  if not FileExists(ASrc) then
    raise Exception.Create('Source file does not exist');

  vSrc := TFileStream.Create(ASrc, fmOpenRead);
  try
    vDecompressor := TDecompressionStream.Create(vSrc);
    try
      vDest := TFileStream.Create(ADest, fmCreate);
      try
        repeat
          R := vDecompressor.Read(B, SizeOf(B));
          if R > 0 then
            vDest.Write(B, R);
        until R < SizeOf(B);
      finally
        vDest.Free;
      end;
    finally
      vDecompressor.Free;
    end;
  finally
    vSrc.Free;
  end;
end;

function DoCompression(aContent: TArray<Byte>): TArray<Byte>;
var
  LContentStream, LOutputStream: TBytesStream;
  LCompressedStream: TZCompressionStream;
begin
  LContentStream := TBytesStream.Create(aContent);
  try
    LOutputStream := TBytesStream.Create(nil);
    try
      LCompressedStream := TZCompressionStream.Create(LOutputStream, zcDefault, 15 + 16);
      try
        LCompressedStream.CopyFrom(LContentStream, 0);
      finally
        LCompressedStream.Free;
      end;
      Result := Copy(LOutputStream.Bytes, 0, LOutputStream.Size);
    finally
      LOutputStream.Free;
    end;
  finally
    LContentStream.Free;
  end;
end;

function DoDecompression(aContent: TArray<Byte>): TArray<Byte>;
var
  LContentStream, LOutputStream: TBytesStream;
  LDecompressedStream: TZDecompressionStream;
begin
  LContentStream := TBytesStream.Create(aContent);
  try
    LOutputStream := TBytesStream.Create(nil);
    try
      LDecompressedStream := TZDecompressionStream.Create(LContentStream, 15 + 16);
      try
        LOutputStream.CopyFrom(LDecompressedStream, 0);
      finally
        LDecompressedStream.Free;
      end;
      Result := Copy(LOutputStream.Bytes, 0, LOutputStream.Size);
    finally
      LOutputStream.Free;
    end;
  finally
    LContentStream.Free;
  end;
end;


function ZCompressString(aText: string; aCompressionLevel: TZCompressionLevel): string;
var
  strInput,
  strOutput: TStringStream;
  Zipper: TZCompressionStream;
begin
  Result:= '';
  strInput:= TStringStream.Create(aText);
  strOutput:= TStringStream.Create;
  try
    Zipper:= TZCompressionStream.Create(strOutput, zcDefault, 15 + 16);
    try
      Zipper.CopyFrom(strInput, strInput.Size);
    finally
      Zipper.Free;
    end;
    Result:= strOutput.DataString;
  finally
    strInput.Free;
    strOutput.Free;
  end;
end;

function ZDecompressString(aText: string): string;
var
  strInput,
  strOutput: TStringStream;
  Unzipper: TZDecompressionStream;
begin
  Result:= '';
  strInput:= TStringStream.Create(aText);
  strOutput:= TStringStream.Create;
  try
    Unzipper:= TZDecompressionStream.Create(strInput);
    try
      strOutput.CopyFrom(Unzipper, Unzipper.Size);
    finally
      Unzipper.Free;
    end;
    Result:= strOutput.DataString;
  finally
    strInput.Free;
    strOutput.Free;
  end;
end;

end.
