unit SelectFrameUnit;


interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, Buttons, ExtCtrls, MidiFile2;

type
  TFrameMode = (fmMinuet, fmTrio);

  TSelectFrame = class(TFrame)
    NoteCombo: TComboBox;
    NoLabel: TLabel;
    RandomBtn: TButton;
    PlayBtn: TButton;
    procedure RandomBtnClick(Sender: TObject);
    procedure NoteComboChange(Sender: TObject);
    procedure PlayBtnClick(Sender: TObject);
    procedure NoteComboClick(Sender: TObject);
  private
    { Private declarations }
    fMidiFile: TMidiFile2;

    fMode: TFrameMode;
    fOrderNo: Integer;
    //procedure SetMode(const Value: TFrameMode);
    function GetSelectNo: Integer;

    procedure PrepareMidiFile;
  public
    { Public declarations }
    constructor Create(aOwner: TComponent; aMode: TFrameMode; aOrderNo: Integer); reintroduce;
    destructor Destroy; override;

    function GetMidiFileName: String;
    procedure MergeToMainMidiFile;
  public
    property Mode: TFrameMode read fMode;// write SetMode;
    property OrderNo: Integer read fOrderNo;
    property SelectNo: Integer read GetSelectNo;
    
    property MidiFile: TMidiFile2 read fMidiFile;
  end;

implementation

{$R *.dfm}


uses
  DataUnit,
  
  MainUnit;

  
{ TSelectFrame }


constructor TSelectFrame.Create(aOwner: TComponent; aMode: TFrameMode; aOrderNo: Integer);
var
  i: Integer;
  Str: String;
begin
  inherited Create(aOwner);
  fMode := aMode;
  fOrderNo := aOrderNo;
  fMidiFile:= TMidiFile2.Create(Self);

  NoLabel.Caption := IntToStr(fOrderNo + 1);
  NoteCombo.Items.BeginUpdate;
  try
    NoteCombo.Clear;

    case fMode of
      fmMinuet:
      for i:= 2 to 12 do
      begin
        Str := Format('%d : %d', [i, MINUET_TABLE[aOrderNo, i-2]]);
        NoteCombo.Items.Add(Str);
      end;

      fmTrio:
      for i:= 1 to 6 do
      begin
        Str := Format('%d : %d', [i, TRIO_TABLE[aOrderNo, i-1]]);
        NoteCombo.Items.Add(Str);
      end;
    end;

    NoteCombo.ItemIndex := 0;
  finally
    NoteCombo.Items.EndUpdate;
  end;

  PrepareMidiFile();
end;

destructor TSelectFrame.Destroy;
begin

  inherited;
end;

function TSelectFrame.GetMidiFileName: String;
begin
  case fMode of
    fmMinuet:
    begin
      Result :=
        Format(
        {$IFDEF RES_MIDI}
          'M%d',
        {$ELSE}
          ExtractFilePath(ParamStr(0)) + 'Data\Minuet\M%d.mid',
        {$ENDIF}
          [MINUET_TABLE[OrderNo, SelectNo]]
        )
    end;

    fmTrio  :
    begin
      Result :=
        Format(
        {$IFDEF RES_MIDI}
          'T%d',
        {$ELSE}
          ExtractFilePath(ParamStr(0)) + 'Data\Trio\T%d.mid',
        {$ENDIF}
          [TRIO_TABLE[OrderNo, SelectNo]]
        )
    end
  else
    Exit;
  end;
end;

procedure TSelectFrame.PrepareMidiFile;
var
  FileName: String;

  i: Integer;
  MidiTrack: TMidiTrack;
  MidiEvent: PMidiEvent;

{$IFDEF RES_MIDI}
  ResStream: TResourceStream;
{$ENDIF}  
begin
  FileName := GetMidiFileName;

{$IFDEF RES_MIDI}
  ResStream:= TResourceStream.Create(hInstance, FileName, RT_RCDATA);
  try
    MidiFile.ReadFromStream(ResStream);
  finally
    ResStream.Free;
  end;
{$ELSE}
  if not FileExists(FileName) then Exit;
  MidiFile.ReadFromFile(FileName);
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

  // 재생시간정보 변경
  MidiFile.Duration  := 1800;
  MidiFile.PlayTicks := 1440;

end;

procedure TSelectFrame.MergeToMainMidiFile;
var
  MidiTrackDst: TMidiTrack;
  MidiTrackSrc: TMidiTrack;
  DstEvent, SrcEvent: PMidiEvent;
  i: Integer; 
begin
  MidiTrackDst := MainForm.MidiFile.GetTrack(0);
  if not Assigned(MidiTrackDst) then Exit;

  MidiTrackSrc:= MidiFile.GetTrack(0);
  if not Assigned(MidiTrackSrc) then Exit;

  for i:= 0 to MidiTrackSrc.EventCount-1 do
  begin
    SrcEvent := MidiTrackSrc.GetEvent(i);
    if (SrcEvent.Event <> 255) and
      (SrcEvent.Event <> 192) and
      (SrcEvent.Event <> 193) then
    begin
      // 정보 복사
      New(DstEvent);
      System.Move(SrcEvent^, DstEvent^, SizeOf(TMidiEvent));

      // 시간 이동 후 리스트업
      Inc(DstEvent.Positon, MainForm.MidiFile.PlayTicks);
      MidiTrackDst.AddEvent(DstEvent);

    end;
  end;

  // 시간 증가
  MainForm.MidiFile.Duration := MainForm.MidiFile.Duration + 1800;
  MainForm.MidiFile.PlayTicks := MainForm.MidiFile.PlayTicks + 1440;



end;

function TSelectFrame.GetSelectNo: Integer;
begin
  Result := NoteCombo.ItemIndex;
end;

procedure TSelectFrame.NoteComboChange(Sender: TObject);
begin
//
//  WriteLn('Change ', OrderNo);
  MainForm.btnStopClick(MainForm.btnStop);
  PrepareMidiFile();
  MainForm.SetMidiFile2(nil);
end;

procedure TSelectFrame.NoteComboClick(Sender: TObject);
begin
//  WriteLn('Click ', OrderNo);
//
end;

procedure TSelectFrame.RandomBtnClick(Sender: TObject);
begin
  NoteCombo.ItemIndex := Random(NoteCombo.Items.Count);

  MainForm.btnStopClick(MainForm.btnStop);
  PrepareMidiFile();
  MainForm.SetMidiFile2(nil);

//
end;

procedure TSelectFrame.PlayBtnClick(Sender: TObject);
begin
  MainForm.btnStopClick(MainForm.btnStop);

  MainForm.SetMidiFile2(MidiFile);

  if not MainForm.MidiPlayer.ReadyToPlay then exit;

  MainForm.MidiPlayer.StartPlaying;
  MainForm.mPaused := false;
end;


end.
