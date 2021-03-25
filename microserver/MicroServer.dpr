program MicroServer;

uses
  Vcl.Forms,
  UServer in 'UServer.pas' {FServer},
  ConvertHex in 'ConvertHex.pas',
  uCompression in 'uCompression.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFServer, FServer);
  Application.Run;
end.
