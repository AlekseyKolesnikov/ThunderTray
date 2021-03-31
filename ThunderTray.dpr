program ThunderTray;

uses
  Vcl.Forms,
  uThunderTray in 'uThunderTray.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
