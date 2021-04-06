unit uThunderTray;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls, System.ImageList, Vcl.ImgList;

type
  TfrmMain = class(TForm)
    Timer: TTimer;
    TrayIcon: TTrayIcon;
    ImageList: TImageList;
    procedure HideTimer(Sender: TObject);
    procedure MonitorTimer(Sender: TObject);
    procedure TrayIconClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    procedure RunApp;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

var
  AppThread, WinHandle: Cardinal;
  {$IFDEF DEBUG}
  slDebug: TStringList;
  {$ENDIF}

function AddQuotes(const S: string): string;
begin
  Result := Trim(S);
  if (AnsiPos(' ', Result) <> 0) and
    ((Result[1] <> '"') or (AnsiLastChar(Result)^ <> '"')) then
    Result := '"' + Result + '"';
end;

function InstExec(const Filename, Params: string; WorkingDir: string; const ShowCmd: Integer; out ErrorCode: Integer): Cardinal;
var
  CmdLine: string;
  WorkingDirP: PChar;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  cWaitIdle: Cardinal;
begin
  CmdLine := AddQuotes(Filename) + ' ' + Params + #0 {needs null terminator for Delphi 1};

  if WorkingDir = '' then
    WorkingDir := ExtractFileDir(Filename);

  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := ShowCmd;

  if WorkingDir <> '' then
    WorkingDirP := PChar(WorkingDir)
  else
    WorkingDirP := nil;

  if not CreateProcess(nil, PChar(CmdLine), nil, nil, False, 0, nil, WorkingDirP, StartupInfo, ProcessInfo) then
  begin
    Result := $FFFFFFFF;
    ErrorCode := GetLastError;
    Exit;
  end;

  with ProcessInfo do
  begin
    CloseHandle(hThread);

    WaitForInputIdle(hProcess, INFINITE);

    repeat
      cWaitIdle := WaitForInputIdle(hProcess, 50);
    until (cWaitIdle <> WAIT_TIMEOUT);

    Result := dwThreadId;
    CloseHandle(hProcess);
  end;
end;

procedure HideApp(wHandle: Cardinal);
var
  style: Integer;
begin
  style := GetWindowLong(wHandle, GWL_EXSTYLE);
  SetWindowLong(wHandle, GWL_EXSTYLE, style or WS_EX_TOOLWINDOW);
end;

procedure ShowApp(wHandle: Cardinal);
var
  style: Integer;
begin
  style := GetWindowLong(wHandle, GWL_EXSTYLE);
  SetWindowLong(wHandle, GWL_EXSTYLE, style and (not WS_EX_TOOLWINDOW));
  //Sleep(500);
  PostMessage(wHandle, WM_SYSCOMMAND, SC_RESTORE, 0);
end;

function FindPopupNotificationProc(wHandle: Cardinal; _: Cardinal): BOOL; stdcall;
var
  Title: array[0..255] of char;
  pwi: TWindowInfo;
begin
  Result := True;

  if (wHandle <> WinHandle) and IsWindowVisible(wHandle) then
  begin
    GetWindowText(wHandle, Title, 255);

    {$IFDEF DEBUG}
    slDebug.Add('[' + IntToStr(AppThread) + '] ' + IntToStr(wHandle) + ' "' + string(Title) + '"');
    {$ENDIF}

    if (Trim(string(Title)) = '') then
    begin
      GetWindowInfo(wHandle, pwi);

      {$IFDEF DEBUG}
      slDebug.Add(IntToStr(pwi.dwStyle) + ' ' + IntToStr(pwi.dwExStyle) + ' ' + IntToStr(pwi.atomWindowType));
      {$ENDIF}

      if (pwi.dwStyle and $FF000000 = $94000000) and (pwi.dwExStyle = $00080888) then
      begin
        frmMain.TrayIcon.IconIndex := 1;
        Result := False;
      end;
    end;
  end;
  {$IFDEF DEBUG}
  try
    slDebug.SaveToFile('D:\log.log');
  except
  end;
  {$ENDIF}
end;

function FindWindowHandleProc(wHandle: Cardinal; _: Cardinal): BOOL; stdcall;
var
  Title: array[0..255] of char;
begin
  if IsWindowVisible(wHandle) then
  begin
    GetWindowText(wHandle, Title, 255);

    {$IFDEF DEBUG}
    slDebug.Add('[' + IntToStr(AppThread) + '] ' + IntToStr(wHandle) + ' "' + string(Title) + '"');
    slDebug.SaveToFile('D:\log.log');
    {$ENDIF}

    if Pos('Mozilla Thunderbird', string(Title)) > 0 then
      WinHandle := wHandle;
  end;

  Result := WinHandle = 0;
end;

procedure TfrmMain.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle and not WS_EX_APPWINDOW;
  Params.WndParent := Application.Handle;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Screen.Cursor := crAppStart;
  {$IFDEF DEBUG}
  slDebug := TStringList.Create;
  {$ENDIF}

  WinHandle := 0;

  RunApp;

  if WinHandle = 0 then
  begin
    Application.Terminate;
    Exit;
  end;

  Self.Hide;
  Screen.Cursor := crDefault;

  Timer.OnTimer := HideTimer;
  Timer.Enabled := True;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  {$IFDEF DEBUG}
  slDebug.Free;
  {$ENDIF}
end;

procedure TfrmMain.HideTimer(Sender: TObject);
begin
  if not IsWindow(WinHandle) then
  begin
    Timer.Enabled := False;
    Application.Terminate;
  end;

  if IsIconic(WinHandle) then
  begin
    Timer.Enabled := False;
    HideApp(WinHandle);

    Timer.Interval := 2000;
    Timer.OnTimer := MonitorTimer;
    Timer.Enabled := True;
  end;
end;

procedure TfrmMain.MonitorTimer(Sender: TObject);
begin
  if not IsWindow(WinHandle) then
  begin
    Timer.Enabled := False;
    Application.Terminate;
  end;

  EnumThreadWindows(AppThread, @FindPopupNotificationProc, 0);
  if TrayIcon.IconIndex = 1 then Timer.Enabled := False;
end;

procedure TfrmMain.RunApp;
var
  ErrorCode: Integer;
  Path: string;
  i: Integer;
begin
  Path := ExtractFilePath(Application.ExeName) + 'thunderbird.exe';

  if not FileExists(Path) then
    Path := 'C:\Program Files\Mozilla Thunderbird\thunderbird.exe';

  if not FileExists(Path) then
    Exit;

  AppThread := InstExec(Path, '', '', SW_NORMAL, ErrorCode);

  for i := 0 to 20 do
  begin
    Sleep(500);
    EnumThreadWindows(AppThread, @FindWindowHandleProc, 0);
    if WinHandle > 0 then
      Break;
  end;
end;

procedure TfrmMain.TrayIconClick(Sender: TObject);
begin
  if not IsIconic(WinHandle) then
  begin
    SetForegroundWindow(WinHandle);
    Exit;
  end;

  Timer.Enabled := False;

  ShowApp(WinHandle);
  frmMain.TrayIcon.IconIndex := 0;

  Timer.Interval := 500;
  Timer.OnTimer := HideTimer;
  Timer.Enabled := True;
end;

end.
