unit uThunderTray;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls, Vcl.Graphics,
  System.ImageList, Vcl.ImgList, Vcl.Menus;

type
  TfrmMain = class(TForm)
    Timer: TTimer;
    TrayIcon: TTrayIcon;
    ImageList: TImageList;
    pmPopupMenu: TPopupMenu;
    miShowHide: TMenuItem;
    N1: TMenuItem;
    miExit: TMenuItem;
    procedure HideTimer(Sender: TObject);
    procedure MonitorTimer(Sender: TObject);
    procedure TrayIconClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miShowHideClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
  private
    { Private declarations }
    procedure InitSettings;
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

uses
  IniFiles, Winapi.ShellAPI;

const
  LAUNCH_ERROR = $FFFFFFFF;

var
  AppThread, WinHandle: Cardinal;
  Settings: record
    Safe, UsePopupMonitor, PopupStyleCheck, PopupExStyleCheck, PopupTitleEqual, MainTitleEqual, UpdateTitle: Boolean;
    RunApp, Params, Folder, PopupTitle, MainTitle, IconTitle, IconMain, IconPopup: String;
    StartTimeout, HideMonitorTimer, PopupMonitorTimer: Integer;
    PopupStyleMask, PopupStyleValue, PopupExStyleMask, PopupExStyleValue: Cardinal
  end;
  PopupDetected: Boolean;
  {$IFDEF DEBUG}
  slDebug: TStringList;
  {$ENDIF}

function AddPath(const S: string): string;
begin
  Result := S;
  if (Result <> '') and (ExtractFilePath(Result) = '') then
    Result := ExtractFilePath(Application.ExeName) + Result;
end;

function AddQuotes(const S: string): string;
begin
  Result := Trim(S);
  if (AnsiPos(' ', Result) <> 0) and
    ((Result[1] <> '"') or (AnsiLastChar(Result)^ <> '"')) then
    Result := '"' + Result + '"';
end;

function ExecuteApp(const Filename, Params: string; WorkingDir: string; const ShowCmd: Integer; out ErrorCode: Integer): Cardinal;
var
  CmdLine: string;
  WorkingDirP: PChar;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  cWaitIdle: Cardinal;
begin
  CmdLine := AddQuotes(Filename) + ' ' + Params + #0;

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
    Result := LAUNCH_ERROR;
    ErrorCode := GetLastError;
    Exit;
  end;

  CloseHandle(ProcessInfo.hThread);

  WaitForInputIdle(ProcessInfo.hProcess, INFINITE);

  repeat
    cWaitIdle := WaitForInputIdle(ProcessInfo.hProcess, 50);
  until (cWaitIdle <> WAIT_TIMEOUT);

  Result := ProcessInfo.dwThreadId;
  CloseHandle(ProcessInfo.hProcess);
end;

procedure InitDefaultSettings;
begin
  with Settings do
  begin
    StartTimeout := 10;
    HideMonitorTimer := 500;
    PopupMonitorTimer := 2000;
    Safe := False;
    Params := '';
    Folder := '';
    RunApp := '';
    IconTitle := '';

    UpdateTitle := False;
    MainTitle := '';
    MainTitleEqual := False;
    PopupTitle := '';
    PopupTitleEqual := False;

    UsePopupMonitor := False;

    PopupStyleMask := $FFFFFFFF;
    PopupStyleValue := 0;
    PopupStyleCheck := False;
    PopupExStyleMask := $FFFFFFFF;
    PopupExStyleValue := 0;
    PopupExStyleCheck := False;
  end;
end;

procedure HideApp(wHandle: Cardinal);
var
  style: Integer;
begin
  if Settings.Safe then
  begin
    style := GetWindowLong(wHandle, GWL_EXSTYLE);
    SetWindowLong(wHandle, GWL_EXSTYLE, style or WS_EX_TOOLWINDOW);
  end
  else
    ShowWindow(wHandle, SW_HIDE);
end;

procedure LoadSettings(sIniFile: String);
var
  ini: TIniFile;

  function ReadBool(Section, Key: String; Default: Boolean): Boolean;
  var
    S: String;
  begin
    Result := Default;
    S := AnsiLowerCase(Trim(ini.ReadString(Section, Key, '')));

    if S = '' then
      Exit;

    Result := (S = 'true') or (S = 'yes') or (S = '1');
  end;

  function ReadCardinal(Section, Key: String; Default: Cardinal): Cardinal;
  var
    S: String;
  begin
    Result := Default;
    S := AnsiLowerCase(Trim(ini.ReadString(Section, Key, '')));

    if S = '' then
      Exit;

    S := StringReplace(S, '0x', '$', []);
    try
      Result := StrToUInt(S);
    except
    end;
  end;

begin
  ini := TIniFile.Create(sIniFile);

  with Settings do
  begin
    RunApp := Trim(ini.ReadString('Run', 'Path', ''));
    if RunApp = '' then
    begin
      ini.Free;
      Exit;
    end;
    RunApp := AddPath(RunApp);

    Params := Trim(ini.ReadString('Run', 'Params', Params));
    Folder := Trim(ini.ReadString('Run', 'Folder', Folder));
    Safe := ReadBool('Run', 'Safe', Safe);

    StartTimeout := ini.ReadInteger('Timers', 'StartTimeout', StartTimeout);
    HideMonitorTimer := ini.ReadInteger('Timers', 'HideMonitorTimer', HideMonitorTimer);
    PopupMonitorTimer := ini.ReadInteger('Timers', 'PopupMonitorTimer', PopupMonitorTimer);

    MainTitleEqual := ReadBool('Main', 'MainTitleEqual', MainTitleEqual);
    MainTitle := ini.ReadString('Main', 'MainTitle', MainTitle);
    IconTitle := ini.ReadString('Main', 'IconTitle', IconTitle);
    UpdateTitle := ReadBool('Main', 'UpdateTitle', UpdateTitle);

    IconMain := AddPath(Trim(ini.ReadString('Main', 'IconMain', IconMain)));
    IconPopup := AddPath(Trim(ini.ReadString('Main', 'IconPopup', IconPopup)));

    UsePopupMonitor := ReadBool('Popup', 'UsePopupMonitor', UsePopupMonitor);
    PopupTitleEqual := ReadBool('Popup', 'PopupTitleEqual', PopupTitleEqual);
    PopupTitle := ini.ReadString('Popup', 'PopupTitle', PopupTitle);

    PopupStyleCheck := ReadBool('Popup', 'PopupStyleCheck', PopupStyleCheck);
    PopupStyleMask := ReadCardinal('Popup', 'PopupStyleMask', PopupStyleMask);
    PopupStyleValue := ReadCardinal('Popup', 'PopupStyleValue', PopupStyleValue);

    PopupExStyleCheck := ReadBool('Popup', 'PopupExStyleCheck', PopupExStyleCheck);
    PopupExStyleMask := ReadCardinal('Popup', 'PopupExStyleMask', PopupExStyleMask);
    PopupExStyleValue := ReadCardinal('Popup', 'PopupExStyleValue', PopupExStyleValue);
  end;

  ini.Free;
end;

{$IFDEF DEBUG}
procedure LogSettings;
begin
  slDebug.Add('RunApp ' + Settings.RunApp);
  slDebug.Add('Params ' + Settings.Params);
  slDebug.Add('Folder ' + Settings.Folder);
  slDebug.Add('Safe ' + BoolToStr(Settings.Safe, True));

  slDebug.Add('StartTimeout ' + IntToStr(Settings.StartTimeout));
  slDebug.Add('HideMonitorTimer ' + IntToStr(Settings.HideMonitorTimer));
  slDebug.Add('PopupMonitorTimer ' + IntToStr(Settings.PopupMonitorTimer));

  slDebug.Add('MainTitleEqual ' + BoolToStr(Settings.MainTitleEqual, True));
  slDebug.Add('MainTitle ' + Settings.MainTitle);
  slDebug.Add('IconTitle ' + Settings.IconTitle);
  slDebug.Add('UpdateTitle ' + BoolToStr(Settings.UpdateTitle, True));
  slDebug.Add('IconMain ' + Settings.IconMain);
  slDebug.Add('IconPopup ' + Settings.IconPopup);

  slDebug.Add('UsePopupMonitor ' + BoolToStr(Settings.UsePopupMonitor, True));
  slDebug.Add('PopupTitleEqual ' + BoolToStr(Settings.PopupTitleEqual, True));
  slDebug.Add('PopupTitle ' + Settings.PopupTitle);

  slDebug.Add('PopupStyleCheck ' + BoolToStr(Settings.PopupStyleCheck, True));
  slDebug.Add('PopupStyleMask ' + IntToHex(Settings.PopupStyleMask));
  slDebug.Add('PopupStyleValue ' + IntToHex(Settings.PopupStyleValue));

  slDebug.Add('PopupExStyleCheck ' + BoolToStr(Settings.PopupExStyleCheck, True));
  slDebug.Add('PopupExStyleMask ' + IntToHex(Settings.PopupExStyleMask));
  slDebug.Add('PopupExStyleValue ' + IntToHex(Settings.PopupExStyleValue));
end;
{$ENDIF}

procedure ShowApp(wHandle: Cardinal);
var
  style: Integer;
begin
  if Settings.Safe then
  begin
    style := GetWindowLong(wHandle, GWL_EXSTYLE);
    SetWindowLong(wHandle, GWL_EXSTYLE, style and (not WS_EX_TOOLWINDOW));
  end
  else
    ShowWindow(wHandle, SW_SHOW);

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

    if Settings.PopupTitleEqual and (Trim(string(Title)) = Settings.PopupTitle) or
      (not Settings.PopupTitleEqual) and ((Settings.PopupTitle = '') or (Pos(Settings.PopupTitle, Trim(string(Title))) > 0)) then
    begin
      GetWindowInfo(wHandle, pwi);

      {$IFDEF DEBUG}
      slDebug.Add(IntToStr(pwi.dwStyle) + ' ' + IntToStr(pwi.dwExStyle) + ' ' + IntToStr(pwi.atomWindowType));
      {$ENDIF}

      if ((not Settings.PopupStyleCheck) or (pwi.dwStyle and Settings.PopupStyleMask = Settings.PopupStyleValue)) and
        ((not Settings.PopupExStyleCheck) or (pwi.dwExStyle and Settings.PopupExStyleMask = Settings.PopupExStyleValue)) then
      begin
        PopupDetected := True;
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

    if Settings.MainTitleEqual and (Trim(string(Title)) = Settings.MainTitle) or
      (not Settings.MainTitleEqual) and ((Settings.MainTitle = '') or (Pos(Settings.MainTitle, Trim(string(Title))) > 0)) then
    begin
      WinHandle := wHandle;
      if Settings.IconTitle = '' then
        Settings.IconTitle := string(Title);
    end;
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
  WinHandle := 0;

  {$IFDEF DEBUG}
  slDebug := TStringList.Create;
  {$ENDIF}

  InitDefaultSettings;
  InitSettings;

  {$IFDEF DEBUG}
  LogSettings;
  {$ENDIF}

  if Settings.RunApp = '' then
  begin
    Application.Terminate;
    Exit;
  end;

  RunApp;

  if WinHandle = 0 then
  begin
    Application.Terminate;
    Exit;
  end;

  Self.Hide;
  Screen.Cursor := crDefault;
  TrayIcon.Hint := Settings.IconTitle;

  Timer.Interval := Settings.HideMonitorTimer;
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
var
  Title: array[0..255] of char;
  aTitle: string;
begin
  if not IsWindow(WinHandle) then
  begin
    Timer.Enabled := False;
    Application.Terminate;
  end;

  if Settings.UpdateTitle then
  begin
    GetWindowText(WinHandle, Title, 255);
    aTitle := string(Title);
    if TrayIcon.Hint <> aTitle then
      TrayIcon.Hint := aTitle;
  end;

  if IsIconic(WinHandle) then
  begin
    Timer.Enabled := False;
    HideApp(WinHandle);
    miShowHide.Caption := 'Show';

    if Settings.UsePopupMonitor then
    begin
      Timer.Interval := Settings.PopupMonitorTimer;
      Timer.OnTimer := MonitorTimer;
      Timer.Enabled := True;
    end;
  end;
end;

procedure TfrmMain.InitSettings;
var
  iIniFile: string;
  anIcon: TIcon;
  i, startIdx: Integer;
begin
  if AnsiLowerCase(ExtractFileExt(ParamStr(1))) = '.ini' then
    startIdx := 2
  else
    startIdx := 1;

  iIniFile := ChangeFileExt(Application.ExeName, '.ini');
  if startIdx > 1 then
    iIniFile := ParamStr(1);
  iIniFile := AddPath(iIniFile);

  if FileExists(iIniFile) then
    LoadSettings(iIniFile);

  if AnsiLowerCase(ParamStr(1)) = '/safe' then
  begin
    Settings.Safe := True;
    startIdx := 2;
  end;

  if (ParamCount >= startIdx) and FileExists(AddPath(ParamStr(startIdx))) then
  begin
    Settings.RunApp := AddPath(ParamStr(startIdx));
    Inc(startIdx);
  end;

  for i := startIdx to ParamCount do
    Settings.Params := Settings.Params + ' ' + ParamStr(i);
  Settings.Params := Trim(Settings.Params);

  if Settings.IconMain <> '' then
  begin
    ImageList.Clear;
    anIcon := TIcon.Create;
    anIcon.LoadFromFile(Settings.IconMain);
    ImageList.AddIcon(anIcon);

    if Settings.IconPopup <> '' then
    begin
      anIcon := TIcon.Create;
      anIcon.LoadFromFile(Settings.IconPopup);
      ImageList.AddIcon(anIcon);
    end;
  end
  else
  if (Settings.RunApp <> '') and FileExists(Settings.RunApp) then
  begin
    anIcon := TIcon.Create;
    anIcon.Handle := ExtractIcon(HInstance, PWideChar(Settings.RunApp), 0);
    if anIcon.Handle > 0 then
    begin
      ImageList.Delete(0);
      ImageList.InsertIcon(0, anIcon);
    end;
  end;

  TrayIcon.Icons := ImageList;
  TrayIcon.IconIndex := -1; // hack

  TrayIcon.Visible := True;
  TrayIcon.IconIndex := 0;
end;

procedure TfrmMain.miExitClick(Sender: TObject);
begin
  if IsIconic(WinHandle) then
    ShowApp(WinHandle);
  PostMessage(WinHandle, WM_CLOSE, 0, 0);
  Application.Terminate;
end;

procedure TfrmMain.miShowHideClick(Sender: TObject);
begin
  if IsIconic(WinHandle) then
    TrayIconClick(Sender)
  else
  begin
    PostMessage(WinHandle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
    HideTimer(Sender);
  end;
end;

procedure TfrmMain.MonitorTimer(Sender: TObject);
begin
  if not IsWindow(WinHandle) then
  begin
    Timer.Enabled := False;
    Application.Terminate;
  end;

  PopupDetected := False;
  EnumThreadWindows(AppThread, @FindPopupNotificationProc, 0);

  if PopupDetected then
  begin
    Timer.Enabled := False;
    TrayIcon.IconIndex := 1;
  end;
end;

procedure TfrmMain.RunApp;
var
  ErrorCode: Integer;
  i: Integer;
begin
  if not FileExists(Settings.RunApp) then
    Exit;

  AppThread := ExecuteApp(Settings.RunApp, Settings.Params, Settings.Folder, SW_NORMAL, ErrorCode);

  if AppThread = LAUNCH_ERROR then
  begin
    MessageBox(Handle, PWideChar('Error running ' + Settings.RunApp), PWideChar('Error'), MB_ICONERROR);
    Exit;
  end;

  for i := 0 to Settings.StartTimeout * 2 do
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
  TrayIcon.IconIndex := 0;
  miShowHide.Caption := 'Hide';

  Timer.Interval := Settings.HideMonitorTimer;
  Timer.OnTimer := HideTimer;
  Timer.Enabled := True;
end;

end.
