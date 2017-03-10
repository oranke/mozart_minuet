unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, MPlayer, ComCtrls, ExtCtrls, SelectFrameUnit, MidiFile2,
  MidiPlayer2, XPMan;

type
  TMainForm = class(TForm)
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    Label3: TLabel;
    lbl_Synth: TLabel;
    cbSynthesizer: TComboBox;
    tbVolume: TTrackBar;
    PositionBar: TProgressBar;
    btnPlay: TButton;
    btnStop: TButton;
    btnPause: TButton;
    Button1: TButton;
    ScrollBox: TScrollBox;
    MinuetLabel: TLabel;
    TrioLabel: TLabel;
    XPManifest1: TXPManifest;
    Button2: TButton;
    procedure ScrollBoxResize(Sender: TObject);
    procedure cbSynthesizerChange(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure tbVolumeChange(Sender: TObject);
  private
    { Private declarations }
    fMidiPlayer: TMidiPlayer2;
    fMidiFile: TMidiFile2;

    fMinuetSels : array [0..16-1] of TSelectFrame;
    fTrioSels : array [0..16-1] of TSelectFrame;

    fFocusedMode: TFrameMode;
    fFocusedIndex: Integer;

    procedure MidiPlayerPosUpdate(TimePos, TicksPos: Cardinal);
    procedure MidiPlayerPlayEnd(Sender: TObject);

    procedure FillMidiOutDevices;
    procedure Composite;

    procedure SetFocusFrame(aMode: TFrameMode; aIndex: Integer);
  public
    mPaused: Boolean;
  public
    { Public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetMidiFile2(aMidiFile: TMidiFile2);

  public
    property MidiPlayer: TMidiPlayer2 read fMidiPlayer;
    property MidiFile: TMidiFile2 read fMidiFile;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  DataUnit;

const
  CMaxVolume = 65535;
  CStepVolume = 3120;

constructor TMainForm.Create(aOwner: TComponent);
var
  i: Integer;
begin
  inherited;

  mPaused := false;

  fMidiPlayer:= TMidiPlayer2.Create(Self);
  fMidiFile := TMidiFile2.Create(Self);

  fMidiPlayer.OnPosUpdate := MidiPlayerPosUpdate;
  fMidiPlayer.OnPlayEnd := MidiPlayerPlayEnd;

  Randomize;

  fFocusedMode:= fmMinuet;
  fFocusedIndex:= -1;

  for i := Low(fMinuetSels) to High(fMinuetSels) do
  begin
    fMinuetSels[i] := TSelectFrame.Create(Self, fmMinuet, i);
    fMinuetSels[i].Name := 'Minuet'+IntToStr(i+1);
    fMinuetSels[i].Parent := ScrollBox;
  end;

  for i := Low(fMinuetSels) to High(fMinuetSels) do
  begin
    fTrioSels[i] := TSelectFrame.Create(Self, fmTrio, i);
    fTrioSels[i].Name := 'Trio'+IntToStr(i+1);
    fTrioSels[i].Parent := ScrollBox;
  end;

  tbVolume.Max := CMaxVolume div CStepVolume;
  FillMidiOutDevices;
  if not MidiPlayer.VolumeAdjustable then
    tbVolume.Enabled := false
  else
    tbVolume.Position := (MidiPlayer.Volume.Left) div CStepVolume; // take right volume


end;

destructor TMainForm.Destroy;
begin
  if MidiPlayer.Playing then
    MidiPlayer.StopPlaying;

  inherited;
end;

procedure TMainForm.FillMidiOutDevices;
var
  iDevice: Integer;
  I: Integer;
begin
  for iDevice := 0 to MidiPlayer.MidiOutDevices - 1 do
    cbSynthesizer.Items.Add(MidiPlayer.MidiOutProductName(iDevice));

  if MidiPlayer.OpenedMidiOut <> '' then
    for I := 0 to cbSynthesizer.Items.Count - 1 do
      if MidiPlayer.OpenedMidiOut = cbSynthesizer.Items[I] then
        cbSynthesizer.ItemIndex := I;
end;

procedure TMainForm.Composite;
var
  i: Integer;
  MidiTrack: TMidiTrack;
  MidiEvent: PMidiEvent;
{$IFDEF RES_MIDI}
  ResStream: TResourceStream;
{$ENDIF}  
begin
  // 첫 소절은 편의상 파일을 읽어 구성한다.
{$IFDEF RES_MIDI}
  ResStream:= TResourceStream.Create(hInstance, fMinuetSels[0].GetMidiFileName, RT_RCDATA);
  try
    MidiFile.ReadFromStream(ResStream);
  finally
    ResStream.Free;
  end;
{$ELSE}
  MidiFile.ReadFromFile(fMinuetSels[0].GetMidiFileName);
{$ENDIF}

  MidiTrack := MidiFile.GetTrack(0);
  if not Assigned(MidiTrack) then Exit;

  // 마지막 EOF 날려줌.
  MidiEvent := MidiTrack.GetEvent(MidiTrack.EventCount-1);
  if MidiEvent.Event = 255 then
  if MidiEvent.Data1 = 47 then
    MidiTrack.DeleteEvent(MidiTrack.EventCount-1);

  // 트랙 정보 1440만큼 당겨옴.
  for i := 0 to MidiTrack.EventCount - 1 do
  begin
    MidiEvent := MidiTrack.GetEvent(i);
    if MidiEvent.Positon > 0 then
      Dec(MidiEvent.Positon, 1440);
  end;

  // 재생시간정보 기본값 변경
  MidiFile.Duration  := 1800;
  MidiFile.PlayTicks := 1440;

  // 미뉴엣 부분 추가.
  for i := Low(fMinuetSels)+1 to High(fMinuetSels) do
    fMinuetSels[i].MergeToMainMidiFile;

  // 트리오 부분 추가.
  for i := Low(fTrioSels) to High(fTrioSels) do
    fTrioSels[i].MergeToMainMidiFile;

  // 미뉴엣 부분 반복
  for i := Low(fMinuetSels) to High(fMinuetSels) do
    fMinuetSels[i].MergeToMainMidiFile;




end;

procedure TMainForm.SetFocusFrame(aMode: TFrameMode; aIndex: Integer);
var
  SelFrame: TSelectFrame;
begin
  if (aMode = fFocusedMode) and
    (aIndex = fFocusedIndex) then Exit;

  SelFrame := nil;
  if fFocusedIndex >= 0 then
  case fFocusedMode of
    fmMinuet: SelFrame := fMinuetSels[fFocusedIndex];
    fmTrio  : SelFrame := fTrioSels[fFocusedIndex];
  end;

  if Assigned(SelFrame) then
    //SelFrame.ImagePanel.Color := clWhite;
    //SelFrame.ImagePanel.Caption := '';
    SelFrame.NoLabel.Font.Color := clWindowText;

  fFocusedMode  := aMode;
  fFocusedIndex := aIndex;

  SelFrame := nil;
  if fFocusedIndex >= 0 then
  case fFocusedMode of
    fmMinuet: SelFrame := fMinuetSels[fFocusedIndex];
    fmTrio  : SelFrame := fTrioSels[fFocusedIndex];
  end;

  if Assigned(SelFrame) then
    //SelFrame.ImagePanel.Color := clGray;
    //SelFrame.ImagePanel.Caption := '재생';
    SelFrame.NoLabel.Font.Color := clRed;

end;



procedure TMainForm.SetMidiFile2(aMidiFile: TMidiFile2);
begin
  btnStopClick(btnStop);

  MidiPlayer.MidiFile := nil;
  MidiPlayer.MidiFile := aMidiFile; 

  if Assigned(aMidiFile) then
  begin
    PositionBar.Max := round(aMidiFile.Duration / 100.0);
    PositionBar.Position := 0;
    mPaused := false;
  end else
  begin
    fMidiFile.Free;
    fMidiFile := TMidiFile2.Create(Self);
  end;
end;


procedure TMainForm.MidiPlayerPlayEnd(Sender: TObject);
begin
//
  SetFocusFrame(fmMinuet, -1);
end;

procedure TMainForm.MidiPlayerPosUpdate(TimePos, TicksPos: Cardinal);
begin
  PositionBar.Position := round(TimePos / 100.0);
  //PositionBar.Update;

  if MidiPlayer.MidiFile = MidiFile then
  begin
    // 한 마디의 틱은 1440.
    // 미뉴엣 = 1440 * 16 = 23040,
    // 트리오 = 23040,
    // 다시 미뉴엣이 반복되므로 전체 재생시간은 69123 이다. 

    // 현재 재생부분이 미뉴엣인지 트리오인지 체크.

    if (TicksPos < 23040) then
    begin
      //Caption := '미뉴엣';
      SetFocusFrame(fmMinuet, TicksPos div 1440);
    end else
    if (TicksPos >= 23040 * 2) then
    begin
      //Caption := '미뉴엣 - 반복';
      SetFocusFrame(fmMinuet, (TicksPos - (23040 * 2)) div 1440);
    end else
    begin
      //Caption := '트리오';
      SetFocusFrame(fmTrio, (TicksPos - 23040) div 1440);
    end;
  end;

end;

procedure TMainForm.ScrollBoxResize(Sender: TObject);
var
  YPos: Integer;
  Column: Integer;
  i: Integer;
  x, y: Integer;
begin
  Column := ScrollBox.ClientWidth div fMinuetSels[0].Width;
  //WriteLn(Column);
  if Column < 1 then Column := 1;


  YPos := MinuetLabel.Top + MinuetLabel.Height + 10;

  for i := Low(fMinuetSels) to High(fMinuetSels) do
  begin
    x := i mod Column;
    y := i div Column;

    fMinuetSels[i].Left := x * fMinuetSels[i].Width;
    fMinuetSels[i].Top := y * fMinuetSels[i].Height + YPos;
  end;

  TrioLabel.Top :=
    (High(fMinuetSels) div Column +1) *  fMinuetSels[0].Height + YPos + 20;

  YPos := TrioLabel.Top + TrioLabel.Height + 10;

  for i := Low(fTrioSels) to High(fTrioSels) do
  begin
    x := i mod Column;
    y := i div Column;

    fTrioSels[i].Left := x * fTrioSels[i].Width;
    fTrioSels[i].Top := y * fTrioSels[i].Height + YPos;
  end;
end;

procedure TMainForm.tbVolumeChange(Sender: TObject);
var
  Volume: TVolume;
begin
  Volume.Left := tbVolume.Position * CStepVolume;
  Volume.Right := tbVolume.Position * CStepVolume;
  MidiPlayer.Volume := Volume;
end;

procedure TMainForm.cbSynthesizerChange(Sender: TObject);
begin
  MidiPlayer.SelectMidiOut(cbSynthesizer.Text);
  if not MidiPlayer.VolumeAdjustable then
    tbVolume.Enabled := false
  else begin
    tbVolume.Enabled := true;
    tbVolume.Position := (MidiPlayer.Volume.Left) div CStepVolume; // take left volume
  end;
end;


procedure TMainForm.btnPlayClick(Sender: TObject);
begin
  MidiPlayer.MidiFile := MidiFile;
  if not MidiPlayer.ReadyToPlay then
  begin
    Composite;

    if not MidiPlayer.ReadyToPlay then
      exit;
  end;
  
  SetMidiFile2(MidiFile);

  if mPaused then
  begin
    MidiPlayer.ResumePlaying;
  end else
  begin
    MidiPlayer.StartPlaying;
  end;

  if MidiPlayer.Playing then
  begin
    mPaused := false;
  end;
//
end;


procedure TMainForm.btnPauseClick(Sender: TObject);
begin
  if not MidiPlayer.Playing then exit;

  MidiPlayer.StopPlaying;
  mPaused := true;
end;

procedure TMainForm.btnStopClick(Sender: TObject);
begin
  if not MidiPlayer.Playing then exit;

  SetFocusFrame(fmMinuet, -1);
  MidiPlayer.StopPlaying;
  while MidiPlayer.Playing do Sleep(1);
  
  MidiPlayer.MidiFile := nil; 
  PositionBar.Position := 0;
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  i: Integer;
begin
  for i := Low(fMinuetSels) to High(fMinuetSels) do
    fMinuetSels[i].RandomBtnClick(nil);

  for i := Low(fTrioSels) to High(fTrioSels) do
    fTrioSels[i].RandomBtnClick(nil);
end;

end.
