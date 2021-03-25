program MicroClient;

uses
  Vcl.Forms,
  UClient in 'UClient.pas' {FClient},
  uPacket in 'uPacket.pas',
  uCompression in 'uCompression.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFClient, FClient);
  Application.Run;
end.
