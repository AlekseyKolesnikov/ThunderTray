unit uThunderTray;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.ImageList, Vcl.ImgList;

type
  TfrmMain = class(TForm)
    Timer: TTimer;
    TrayIcon: TTrayIcon;
    ImageList: TImageList;
    procedure HideTimer(Sender: TObject);
    procedure MonitorTimer(Sender: TObject);
    procedure RunTimer(Sender: TObject);
    procedure TrayIconClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
  AppThread, AppHandle, WinHandle: Cardinal;
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

    Result := dwProcessId;
    AppThread := dwThreadId;
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

function FindPopupNotificationProc(wHandle: Cardinal; AppHWND: Cardinal): BOOL; stdcall;
var
  Title{, ClassName}: array[0..255] of char;
  ProcID: Cardinal;
  pwi: TWindowInfo;
begin
  Result := True;

  GetWindowThreadProcessId(wHandle, ProcID);
  if (ProcID = AppHWND) and (wHandle <> WinHandle) and IsWindowVisible(wHandle) then
  begin
    GetWindowText(wHandle, Title, 255);
    {$IFDEF DEBUG}
    slDebug.Add(IntToStr(wHandle) + ' ' + string(Title));
    {$ENDIF}

    //GetClassName(wHandle, ClassName, 255);

    if (Trim(string(Title)) = '') then
    begin
      GetWindowInfo(wHandle, pwi);
      if pwi.dwStyle and $FF000000 = $94000000 then
      begin
        frmMain.TrayIcon.IconIndex := 1;
        Result := False;
      end;
    end;
  end;
end;

function FindWindowHandleProc(wHandle: Cardinal; AppHWND: Cardinal): BOOL; stdcall;
var
  ProcID: Cardinal;
begin
  if IsWindowVisible(wHandle) then
  begin
    GetWindowThreadProcessId(wHandle, ProcID);
    if (ProcID = AppHWND) then
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

  {$IFDEF DEBUG}
  slDebug := TStringList.Create;
  if FileExists('D:\log.log') then
    slDebug.LoadFromFile('D:\log.log');
  {$ENDIF}

  EnumThreadWindows(AppThread, @FindPopupNotificationProc, AppHandle);
  if TrayIcon.IconIndex = 1 then Timer.Enabled := False;

  {$IFDEF DEBUG}
  slDebug.SaveToFile('D:\log.log');
  slDebug.Free;
  {$ENDIF}
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

  AppHandle := InstExec(Path, '', '', SW_NORMAL, ErrorCode);

  for i := 0 to 20 do
  begin
    Sleep(500);
    EnumWindows(@FindWindowHandleProc, AppHandle);
    if WinHandle > 0 then
      Break;
  end;
end;

procedure TfrmMain.RunTimer(Sender: TObject);
begin
  WinHandle := 0;
  Timer.Enabled := False;

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
