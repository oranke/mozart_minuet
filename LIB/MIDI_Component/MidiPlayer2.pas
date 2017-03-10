{ TMidiPlayer2

   The component which plays midi tracks events.

   This component is based on the TMidiPlayer written by ZiZii Wan.
   TMidiPlayer2 uses seperate thread for processing MIDI event, which helps application to
    avoid timing problems.

     Author : Silhwan Hyun  (hyunsh@hanafos.com)

   #### Copyright Notice ####
 Copyright 2011 Silhwan Hyun, All Rights Reserved
 This unit is free. It may be used both in commercial and non-commercial software either in
  original or in modified form.
 This unit can be freely distributed in any way and by any means provided this copyright
  notice is preserved.

   Revision History
 ------------------------------
   v0.9.5   pending
    - Changed procedure SelectMidiOut to function SelectMidiOut

   v0.9.4  (27 May 2012)
    - Added property Pitch which adjust the Note number(= pitch) of MIDI event before rendering
       to MIDI output device.
    - Added property VolumeAdjustable which shows whether MIDI output device supports volume control.
    - Added property OnGotLyrics which fires at getting a synchronised lyrics at playing
       MIDI files.
    - Modifications for Windows 7, 64 bit

   V0.9.3  (27 Aug 2011)
    - Bug fix : MIDI_MAPPER was exculded at accounting Midi Out Devices.
    - The priority class is automatically set to ABOVE_NORMAL_PRIORITY_CLASS at starting playing
       MIDI events if the priority class of non-playing state is BELOW_NORMAL_PRIORITY_CLASS,
       IDLE_PRIORITY_CLASS or NORMAL_PRIORITY_CLASS.

   V0.9.2  (16 Jul 2011)
    - Bug fix : Unsuitable response at WM_QueryEndSession message in the procedure
       Tmidioutput.midioutput of MidiOut.pas, which blocks normal system termination.
    - The characteristic of property TimerInterval is changed from Read only to Read & Write,
       so, you can set the time resolution of handling MIDI events. (max : 20ms)

   V0.9.1  (10 Jul 2011)
    - Added property Priority, which enables you to set the thread(the thread responsible for
       processing MIDI events per every timer interval) priority.  (default = TIME_CRITICAL)
    - Added property TimerInterval, which shows you the timer interval in millisecond at processing
       MIDI events.
    - Added procedure SkipExclusiveMsg(Value: Boolean);
      note) Set Value true if you want to skip System Exclusive Messages in MIDI file, else set false.
          * System Exclusive Messages are to be skipped at initial state.
    - Changed data type of property Speed : Integer -> Double
      note) Speed = 0.5(Half of normal speed) ~ 2.0(Double of Normal speed)
    - Provision for the MIDI files which do not have the end of track mark.
    - Changed the defintion of event type TOnPosUpdate
       From : TOnPosUpdate = procedure(TimePos: Integer; TicksPos: Double) of object;
       To : TOnPosUpdate = procedure(TimePos, TicksPos: Cardinal) of object;
    - Renamed event type TOnSpeedChange to TOnTempoChange and Changed the defintion of it.
       From : TOnSpeedChange = procedure(Value: Integer) of object;
       To : TOnTempoChange = procedure(Tempo: Cardinal) of object;
    - Changed data type of property CurrentPos : Double -> LongWord
    - Changed data type of property CurrentTime : Integer -> LongWord
    - Replaced PutMidiEvent in the procedure TMidiDriver.DoOnMidiTimer with PutShort and PutLong
       to eliminate the redundant routine to use an instance of TMyMidiEvent.

   V0.9    (01 Jul 2011)
    - Initial release
}


unit MidiPlayer2;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Dialogs, MMSystem,
  StdCtrls, ExtCtrls, MidiCommon, MidiFile2, MidiOut;

const
  MaxSpeed = 2.0;            // 200% of normal speed
  MinSpeed = 0.5;            // 50% of normal speed
  HigherPitchLimit = 12;     // +1 Octave
  LowerPitchLimit  = -12;    // -1 Octave
  MaxPolyphony = 128;

type
  TPriority = (BELOW_NORMAL, NORMAL, ABOVE_NORMAL, HIGHEST, TIME_CRITICAL);
  TTimerProc = procedure(uTimerID, uMsg: Integer; dwUser, dwParam1, dwParam2: LongWord); stdcall;

  TOnMidiEvent = procedure(Track: Integer; Event: PMidiEvent) of object;
  TOnEndOfTrack = procedure(Track: Integer) of object;
  TOnPosUpdate = procedure(TimePos, TicksPos: Cardinal) of object;
  TOnTempoChange = procedure(Tempo: Cardinal) of object;  // Tempo : microseconds per quarter note

  TNoteConvRec = record
    Valid: boolean;
    ChNo: byte;
    NoteNo: byte;
    OutNoteNo: byte;
  end;

  TVolume = record
    Left: Word;
    Right: Word;
  end;

  PMidiDriver = ^TMidiDriver;
  TMidiDriver = class(TObject)
  private
    FPlayerHandle: HWND;
    FTimerPeriod: LongWord;
    FStartTime: LongWord;
    FSpeed: Double;
    FPitch: integer;
    FCurrentPos: LongWord;
    FCurrentTime: LongWord;
    FStepMode: Boolean;
    LastOutputTime: LongWord;

    ThreadHandle: HWND;
    ThreadId: LongWord;
    MidiFile: TMidiFile2;
    MidiOut: TMidiOutput;
    Suspend: Boolean;
    NoExclusiveMsg: Boolean;

    NoteConvTbl : array[1..MaxPolyphony] of TNoteConvRec;
    LastEntryNo: integer;

  protected
    procedure SetMidiFile(const Midi_File: TMidiFile2);
    procedure SetSpeed(const Value: Double);
    procedure SetPitch(const Value: integer);
    procedure SetStepMode(const Value: Boolean);
    procedure AddConvRecord(Channel, OrgNote: byte; var OutNote: byte);
    procedure DeleteConvRecord(Channel, OrgNote: byte; var OutNote: byte);
    procedure ClearNoteConvTable;
    property  TimerPeriod: LongWord read FTimerPeriod write FTimerPeriod;
  public
    constructor Create(const Midi_Out: TMidiOutput; PlayerHandle: HWND);
    destructor  Destroy; override;
    procedure SetCurrentTime(Value: LongWord);
    procedure SkipExclusiveMsg(Value: Boolean);
    procedure ProcessEvent(iTrack: Integer; pEvent: PMidiEvent);
    procedure DoOnMidiTimer;
  end;

  TMidiPlayer2 = class(TComponent)
  private
    FMidiFile: TMidiFile2;
    FMidiOut: TMidiOutput;
    FMidiOutDevices: Integer;
    FMidiOutNames: array of string;
    FOpenedMidiOut: string;
    FVolumeAdjustable: boolean;  // True if MIDI output device supports volume control, else false.

    FPriority: TPriority;
    FProcHandle : THandle;
    FPriorityClass: LongWord;
    FTimerInterval: LongWord;
    FCurrentPos: LongWord;
    FCurrentTime: LongWord;
    FSpeed: Double;
    FPitch: integer;
    FPlaying: Boolean;
    FStepMode: Boolean;

    MidiDriver: TMidiDriver;
    MidiPlayerHandle: HWND;

    procedure SetMidiFile(Value: TMidiFile2);
    procedure SetDriverPriority(Value: TPriority);
    procedure SetTimerInterval(Value: LongWord);
    procedure SetCurrentPos(const Value: LongWord);
    procedure SetCurrentTime(const Value: LongWord);
    procedure SetSpeed(Value: Double);
    procedure SetPitch(Value: integer);
    function IsReadyToPlay: Boolean;
    function GetVolume: TVolume;
    procedure SetVolume(Value: TVolume);
  protected
    FOnMidiEvent: TOnMidiEvent;
    FOnGotLyrics: TOnMidiEvent;
    FOnTempoChange: TOnTempoChange;
    FOnPosUpdate: TOnPosUpdate;
    FOnEndOfTrack: TOnEndOfTrack;
    FOnPlayEnd: TNotifyEvent;
    procedure ProcMsg(var Msg: TMessage);
    procedure SetStepMode(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure StartPlaying;
    procedure StopPlaying;
    procedure ResumePlaying;
  //  procedure SelectMidiOut(Device: string);
    function SelectMidiOut(Device: string): Boolean; // Changed from procedure type at ver 0.9.5
    function MidiOutProductName(iDevice: Integer): string;
    procedure SkipExclusiveMsg(Value: Boolean);
    function OpenFile(const FileName: WideString): Boolean;
    property ReadyToPlay: Boolean read IsReadyToPlay;
    property Playing: Boolean read FPlaying;
    property VolumeAdjustable: boolean read FVolumeAdjustable; // ** Added at ver 0.9.4
    property Volume: TVolume read GetVolume write SetVolume;
    property CurrentPos: LongWord read FCurrentPos write SetCurrentPos;     // in ticks
    property CurrentTime: LongWord read FCurrentTime write SetCurrentTime;  // in milliseconds
    property Speed: Double read FSpeed write SetSpeed;
    property Pitch: integer read FPitch write SetPitch;   // ** Added at ver 0.9.4
    property StepMode: Boolean read FStepMode write SetStepMode;
  published
    property MidiFile: TMidiFile2 read FMidiFile write SetMidiFile;
    property MidiOutDevices: integer read FMidiOutDevices;
    property Priority: TPriority read FPriority write SetDriverPriority;    // thread priority
    property TimerInterval: LongWord read FTimerInterval write SetTimerInterval;  // in milliseconds
    property OpenedMidiOut: string read FOpenedMidiOut;
    property OnMidiEvent: TOnMidiEvent read FOnMidiEvent write FOnMidiEvent;
    property OnGotLyrics: TOnMidiEvent read FOnGotLyrics write FOnGotLyrics;  // ** Added at ver 0.9.4
    property OnTempoChange: TOnTempoChange read FOnTempoChange write FOnTempoChange;
    property OnPosUpdate: TOnPosUpdate read FOnPosUpdate write FOnPosUpdate;
    property OnEndOfTrack: TOnEndOfTrack read FOnEndOfTrack write FOnEndOfTrack;
    property OnPlayEnd: TNotifyEvent read FOnPlayEnd write FOnPlayEnd;
  end;

procedure Register;

implementation

const
  DEFAULT_TIMER_RESOLUTION = 5;  // process MIDI events per 5ms interval
  MAX_TIMER_RESOLUTION = 20;
  WM_MIDI_Event = WM_USER + 1;
  WM_MIDI_GotLyrics = WM_USER + 2;  // ** Added at ver 0.9.4
  WM_MIDI_PosUpdate = WM_USER + 3;
  WM_MIDI_EndOfTrack = WM_USER + 4;
//  WM_MIDI_PlayEnd = WM_USER + 5;
  WM_MIDI_TempoChange = WM_USER + 6;
  WM_TIMER_FAIL = WM_USER + 7;
  WM_MULTIMEDIA_TIMER = WM_USER + 127;

  ABOVE_NORMAL_PRIORITY_CLASS = $00008000;
  BELOW_NORMAL_PRIORITY_CLASS = $00000020;

procedure Register;
begin
  RegisterComponents('Synth', [TMidiPlayer2]);
end;


//-------------------------------------------------------------------------------------------

{ TMidiDriver }
//  TMidiDriver processes MIDI events in a seperate thread.

constructor TMidiDriver.Create(const Midi_Out: TMidiOutput; PlayerHandle: HWND);
begin
  MidiOut := Midi_Out;
  FPlayerHandle := PlayerHandle;
  NoExclusiveMsg := true;
  FSpeed := 1.0;
  LastEntryNo := 0;

  inherited Create;
end;

destructor TMidiDriver.Destroy;
begin
  inherited Destroy;
end;

procedure TMidiDriver.SetMidiFile(const Midi_File: TMidiFile2);
begin
  MidiFile := Midi_File;
  ClearNoteConvTable;
end;

procedure TMidiDriver.SetCurrentTime(Value: LongWord);
var
  TickCount: LongWord;
  i: integer;
begin
  if Value = 0 then   // Replay
    FCurrentPos := 0
  else
    FCurrentPos := MidiFile.Time2TickPos(Value);

  TickCount := GetTickCount;
  FStartTime := TickCount - Value;
  if Value = 0 then
    LastOutputTime := 0
  else
    LastOutputTime := TickCount;

  for i := 0 to MidiFile.TrackCount - 1 do
  begin
    with MidiFile.GetTrack(i) do
    begin
      PlayPos := 0;
      while (PlayPos < EventCount) and (Round(FCurrentPos) > GetEvent(PlayPos).Positon) do
      begin
        ProcessEvent(i, GetEvent(PlayPos));
        PlayPos := PlayPos + 1;
      end;
    end;
  end;
  FCurrentTime := Value;
end;

procedure TMidiDriver.SkipExclusiveMsg(Value: Boolean);
begin
  NoExclusiveMsg := Value;
end;

procedure TMidiDriver.ProcessEvent(iTrack: Integer; pEvent: PMidiEvent);
var
  b: boolean;
  i: integer;
  Tempo: LongWord;
begin
  if PEvent.Event = $FF then
  begin
    if pEvent^.Data1 = $2F then // End this track
    begin
      MidiFile.GetTrack(iTrack).EndOfTrack := True;

    // Check if all tracks are played to the end
      b := True;  // Assume all tracks are played to the end
      for i := 0 to MidiFile.TrackCount - 1 do
        if not MidiFile.GetTrack(i).EndOfTrack then
        begin
          b := False; // Not all tracks are played to the end
          break;
        end;

      if b then  // all tracks are played to the end
      begin
        Suspend := true;  // to prohibit calling DoOnMidiTimer in PlayThread
        PostMessage(FPlayerHandle, WM_MIDI_EndOfTrack, iTrack, 1)
      end else
        PostMessage(FPlayerHandle, WM_MIDI_EndOfTrack, iTrack, 0);
    end else
    if PEvent^.Data1 = $51 then
    begin
      Tempo := Integer(Byte(PEvent^.Msg[1])) shl 16 +
        Integer(Byte(PEvent^.Msg[2])) shl 8 +
        Integer(Byte(PEvent^.Msg[3]));
      PostMessage(FPlayerHandle, WM_MIDI_TempoChange, integer(Tempo), 0);
    end else
    if PEvent^.Data1 = $05 then   // Got Lyrics ?
    begin
      PostMessage(FPlayerHandle, WM_MIDI_GotLyrics, iTrack, integer(pEvent));
    end;
  end;
end;

// This procedure is called before output Note On events to MIDI device.
procedure TMidiDriver.AddConvRecord(Channel, OrgNote: byte; var OutNote: byte);
var
  I: integer;
  Processed: boolean;
begin
  Processed := false;

// If the channel & Note number is registered, then use the value of OutNoteNo.
  for I := 1 to LastEntryNo do
    if NoteConvTbl[I].Valid then
      if NoteConvTbl[I].ChNo = Channel then
        if NoteConvTbl[I].NoteNo = OrgNote then
        begin
          OutNote := NoteConvTbl[I].OutNoteNo;
          Processed := true;
          break;
        end;

  if not Processed then
    if LastEntryNo = 0 then
    begin
      NoteConvTbl[1].Valid := true;
      NoteConvTbl[1].ChNo := Channel;
      NoteConvTbl[1].NoteNo := OrgNote;
      NoteConvTbl[1].OutNoteNo := OutNote;
      LastEntryNo := 1;
    end else
    begin
      for I := 1 to LastEntryNo do
        if not NoteConvTbl[I].Valid then
        begin
          NoteConvTbl[I].Valid := true;
          NoteConvTbl[I].ChNo := Channel;
          NoteConvTbl[I].NoteNo := OrgNote;
          NoteConvTbl[I].OutNoteNo := OutNote;
          Processed := true;
          break;
        end;

      if not Processed then
        if LastEntryNo < MaxPolyphony then
        begin
          inc(LastEntryNo);
          NoteConvTbl[LastEntryNo].Valid := true;
          NoteConvTbl[LastEntryNo].ChNo := Channel;
          NoteConvTbl[LastEntryNo].NoteNo := OrgNote;
          NoteConvTbl[LastEntryNo].OutNoteNo := OutNote;
        end;
    end;
end;

// This procedure is called before output Note Off events to MIDI device.
// The OutNote gets the adjusted Note number same to that on Note On event.
procedure TMidiDriver.DeleteConvRecord(Channel, OrgNote: byte; var OutNote: byte);
var
  I: integer;
begin
  for I := 1 to LastEntryNo do
    if NoteConvTbl[I].Valid then
      if NoteConvTbl[I].ChNo = Channel then
        if NoteConvTbl[I].NoteNo = OrgNote then
        begin
          NoteConvTbl[I].Valid := false;
          OutNote := NoteConvTbl[I].OutNoteNo;
          if I = LastEntryNo then
            dec(LastEntryNo);
          break;
        end;
end;

// This procedure is called at just after loading new MIDI file.
procedure TMidiDriver.ClearNoteConvTable;
begin
  LastEntryNo := 0;

end;

procedure TMidiDriver.DoOnMidiTimer;
var
  i: Integer;
  TickTime: LongWord;
  DeltaTime: LongWord;
  AMidiTrack: TMidiTrack;
  pEvent: pMidiEvent;
  EventCode: byte;
  OutNote: byte;
  EventPositon: LongWord;
begin
  if not Assigned(MidiFile) then Exit;

  TickTime := GetTickCount;
  if LastOutputTime = 0 then
  begin
    DeltaTime := TickTime - FStartTime;
    FCurrentTime := round(DeltaTime * FSpeed);
  end else
  begin
    DeltaTime := TickTime - LastOutputTime;
    FCurrentTime := FCurrentTime + round(DeltaTime * FSpeed);
  end;
  LastOutputTime := TickTime;
  FCurrentPos := MidiFile.Time2TickPos(FCurrentTime);

// I have found a MIDI file which does not have the end of track mark.
// Following sentences are for the case to stop playing forcibly.
  if FCurrentTime > (MidiFile.Duration + 300) then
  begin
    Suspend := true;  // to prohibit calling DoOnMidiTimer in PlayThread
    if FStepMode then
      SendMessage(FPlayerHandle, WM_MIDI_EndOfTrack, 0, 1)
    else
      PostMessage(FPlayerHandle, WM_MIDI_EndOfTrack, 0, 1);
    exit;
  end;

  for i := 0 to MidiFile.TrackCount - 1 do
  begin
    AMidiTrack := MidiFile.GetTrack(i);
    if not AMidiTrack.EndOfTrack then
      with AMidiTrack do
      begin
        while (AMidiTrack.PlayPos < AMidiTrack.EventCount) do
        begin
          pEvent := GetEvent(PlayPos);
          EventPositon := pEvent^.Positon;
          if (Round(FCurrentPos) <= EventPositon) then
            break;
          if PEvent.Event = $FF then
            ProcessEvent(i, pEvent)
          else if AMidiTrack.Active then
          begin
            if pEvent^.Msg = '' then  // Not a System Exclusive Message ?
            begin
              EventCode := pEvent^.Event and $F0;
            // The note number for drum channel defines the different percussion instruments,
            // So, we should not change that.
              if (pEvent^.Event and $09) = $09 then    // Drum channel ?
                OutNote := pEvent^.Data1
            // Output the adjusted note number for the Note On, Note Off and Note Aftertouch Events
            // by FPitch value.
              else if (EventCode = $80) or (EventCode = $90) or (EventCode = $A0) then
              begin
                OutNote := pEvent^.Data1 + FPitch;
                if (EventCode = $80) or ((EventCode = $90) and (pEvent^.Data2 = 0)) then
                  DeleteConvRecord(pEvent^.Event and $0F, pEvent^.Data1, OutNote)
                else
                  AddConvRecord(pEvent^.Event and $0F, pEvent^.Data1, OutNote);
              end else
                OutNote := pEvent^.Data1;
              MidiOut.PutShort(pEvent^.Event, OutNote, pEvent^.Data2);
            end else if (not NoExclusiveMsg) then
              MidiOut.PutLong(pAnsiChar(pEvent^.Msg), Length(pEvent^.Msg));
          end;
          if FStepMode then
            SendMessage(FPlayerHandle, WM_MIDI_Event, i, integer(pEvent))
          else
            PostMessage(FPlayerHandle, WM_MIDI_Event, i, integer(pEvent));
          PlayPos := PlayPos + 1;
        end;
      end;
  end;

  if FStepMode then
    SendMessage(FPlayerHandle, WM_MIDI_PosUpdate, FCurrentTime, FCurrentPos)
  else
    PostMessage(FPlayerHandle, WM_MIDI_PosUpdate, FCurrentTime, FCurrentPos);
end;

procedure TMidiDriver.SetSpeed(const Value: Double);
begin
  FSpeed := Value;
end;

procedure TMidiDriver.SetPitch(const Value: integer);
begin
  FPitch := Value;
end;

procedure TMidiDriver.SetStepMode(const Value: Boolean);
begin
  FStepMode := Value;
end;

procedure TimerCallBackProc(uTimerID, uMsg: Integer; dwUser, dwParam1, dwParam2: LongWord); stdcall;
begin
  PostThreadMessage(HWND(dwUser), WM_MULTIMEDIA_TIMER, 0, 0);
end;

function PlayThread(lpParam : pointer) : DWORD; stdcall;
var
  Msg: TMsg;
  TimerProc: TTimerProc;
  TimerInterval: Integer;
  MIDITimerID: integer;
  MidiDriver: TMidiDriver;
  CloseRequested: boolean;
  MsgReturn: longbool;
begin
  CloseRequested := false;
  MidiDriver := PMidiDriver(lpParam)^;
  TimerProc := TimerCallBackProc;
  TimerInterval := MidiDriver.TimerPeriod;
  MIDITimerID := timeSetEvent(TimerInterval, TimerInterval, @TimerProc,
                               GetCurrentThreadId, TIME_PERIODIC);
  if MIDITimerID = 0 then
  begin
    timeEndPeriod(TimerInterval);
    PostMessage(MidiDriver.FPlayerHandle, WM_TIMER_FAIL, 0, 0);
    CloseHandle(MidiDriver.ThreadHandle);
    MidiDriver.ThreadHandle := 0;
    ExitThread(1);
  end;

  repeat
    MsgReturn := GetMessage(Msg, 0, 0, 0);
    if ((Msg.message = WM_QUIT) or (Msg.message = WM_CLOSE)) then
      CloseRequested := true
    else if Msg.message = WM_MULTIMEDIA_TIMER then
      if not MidiDriver.Suspend then
        MidiDriver.DoOnMidiTimer;

    TranslateMessage(Msg);
    DispatchMessage(Msg);
  until (integer(MsgReturn) <= 0) or CloseRequested;

  timeKillEvent(MIDITimerID);
  timeEndPeriod(TimerInterval);
  CloseHandle(MidiDriver.ThreadHandle);
  MidiDriver.ThreadHandle := 0;
  Result := 0;
  ExitThread(0);
end;

function GetThreadPriority(EnumPriority: TPriority): Integer;
begin
  case ord(EnumPriority) of
    0 : result := THREAD_PRIORITY_BELOW_NORMAL;
    1 : result := THREAD_PRIORITY_NORMAL;
    2 : result := THREAD_PRIORITY_ABOVE_NORMAL;
    3 : result := THREAD_PRIORITY_HIGHEST;
    4 : result := THREAD_PRIORITY_TIME_CRITICAL;
    else result := THREAD_PRIORITY_NORMAL;
  end;
end;

function Start_MIDI_Play(ThreadPriority: TPriority; var MidiDriver: TMidiDriver): Boolean;
var
  ThreadId: LongWord;
begin
  if MidiDriver.ThreadHandle <> 0 then
  begin
    MidiDriver.Suspend := false;
    SetThreadPriority(ThreadId, GetThreadPriority(ThreadPriority));
    result := true;
    exit;
  end;

  result := false;
  try
    MidiDriver.ThreadHandle := CreateThread(nil, 0, @PlayThread, @MidiDriver, 0, ThreadId);
    if MidiDriver.ThreadHandle = 0 then
      exit;

    MidiDriver.ThreadId := ThreadId;
    MidiDriver.Suspend := false;
    SetThreadPriority(ThreadId, GetThreadPriority(ThreadPriority));
    result := true;
  except

  end;
end;


//---------------------------------------------------------------------------------

{ TMidiPlayer2 }

constructor TMidiPlayer2.Create(AOwner: TComponent);
var
  iDevice: Integer;
  I: Integer;
begin
  inherited Create(AOWner);

  FPriority := TIME_CRITICAL;
  FSpeed := 1.0;
  FCurrentPos := 0;
  FCurrentTime := 0;

  if not (csDesigning in ComponentState) then
    MidiPlayerHandle := AllocateHWnd(ProcMsg);

  FMidiOut := TMidiOutput.Create(Self);
  FMidiOutDevices := FMidiOut.Numdevs + 1;  // + 1 : to include MIDI_MAPPER
  MidiDriver := TMidiDriver.Create(FMidiOut, MidiPlayerHandle);
  SetLength(FMidiOutNames, FMidiOutDevices);
  for iDevice := 0 to FMidiOutDevices - 1 do
  begin
    {$if CompilerVersion > 21} // Delphi XE or later
    if iDevice = 0 then
      FMidiOut.DeviceID := MIDI_MAPPER
    else
      FMidiOut.DeviceID := UInt_Ptr(iDevice - 1);
    {$else}
    FMidiOut.DeviceID := iDevice - 1;      // DeviceID = - 1 => specifies MIDI_MAPPER
    {$ifend}
    FMidiOutNames[iDevice] := FMidiOut.ProductName;
  end;
  if FMidiOutDevices > 0 then
    if FMidiOutDevices > 1 then
    begin
    // Repeat until selecting available MIDI device    (changed at ver 0.9.5)
      if not SelectMidiOut(FMidiOutNames[1]) then
        if not SelectMidiOut(FMidiOutNames[0]) then
          if FMidiOutDevices > 2 then
            for I := 2 to FMidiOutDevices - 1 do
            begin
              if SelectMidiOut(FMidiOutNames[I]) then
                break;
            end;
    end else
      SelectMidiOut(FMidiOutNames[0]);

  SetTimerInterval(DEFAULT_TIMER_RESOLUTION);

  FProcHandle := OpenProcess(PROCESS_QUERY_INFORMATION, False, GetCurrentProcessID);
  FPriorityClass := GetPriorityClass(FProcHandle);
end;

destructor TMidiPlayer2.Destroy;
begin
  if FPlaying then
  begin
    MidiDriver.Suspend := true;
  // Post WM_CLOSE message to stop MidiDriver thread
    PostThreadMessage(MidiDriver.ThreadId, WM_CLOSE, 0, 0);
  end;
  MidiDriver.Free;
  FMidiOut.Free;
  SetLength(FMidiOutNames, 0);
  if not (csDesigning in ComponentState) then
    DeallocateHWnd(MidiPlayerHandle);

  SetPriorityClass(FProcHandle, FPriorityClass);
  CloseHandle(FProcHandle);

  inherited;
end;

procedure TMidiPlayer2.SetMidiFile(Value: TMidiFile2);
begin
  FMidiFile := Value;
  MidiDriver.SetMidiFile(FMidiFile);
end;

procedure TMidiPlayer2.SetDriverPriority(Value: TPriority);
begin
  FPriority := Value;
  if FPlaying then
    SetThreadPriority(MidiDriver.ThreadId, GetThreadPriority(FPriority));
end;

procedure TMidiPlayer2.SetTimerInterval(Value: LongWord);
var
  TimeCaps: TTimeCaps;
begin
  FTimerInterval := Value;
  if FTimerInterval > MAX_TIMER_RESOLUTION then
    FTimerInterval := MAX_TIMER_RESOLUTION;
  timeGetDevCaps(@TimeCaps, SizeOf(TimeCaps));
  if FTimerInterval < TimeCaps.wPeriodMin then
    FTimerInterval := TimeCaps.wPeriodMin
  else if FTimerInterval > TimeCaps.wPeriodMax then
    FTimerInterval := TimeCaps.wPeriodMax;
  MidiDriver.TimerPeriod := FTimerInterval;
end;

function TMidiPlayer2.IsReadyToPlay: Boolean;
begin
  if not Assigned(FMidiFile) then
  begin
    result := false;
    exit;
  end;

  result := (FMidiOut.State = mosOpen) and FMidiFile.Valid;
end;

procedure TMidiPlayer2.SetCurrentTime(const Value: LongWord);
var
  Suspended: Boolean;
begin
  if not IsReadyToPlay then Exit;

  if FPlaying then
  begin
    Suspended := true;
    MidiDriver.Suspend := true;
    FMidiOut.SentAllNotesOff;
  end else
    Suspended := false;

  MidiDriver.SetCurrentTime(Value);
  if Suspended then
    MidiDriver.Suspend := false;
end;

procedure TMidiPlayer2.SetCurrentPos(const Value: LongWord);
begin
  if not IsReadyToPlay then Exit;

  SetCurrentTime(FMidiFile.Tick2TimePos(Value));
end;

procedure TMidiPlayer2.StartPlaying;
var
  I: integer;
begin
  if FPlaying then exit;
  if not IsReadyToPlay then Exit;

// Raise priority class for application (=> higher level than NORMAL_PRIORITY_CLASS).
  if (FPriorityClass = BELOW_NORMAL_PRIORITY_CLASS) or (FPriorityClass = IDLE_PRIORITY_CLASS)
     or (FPriorityClass = NORMAL_PRIORITY_CLASS) then
    SetPriorityClass(FProcHandle, ABOVE_NORMAL_PRIORITY_CLASS);

  SetCurrentTime(0);
  for I := 0 to FMidiFile.TrackCount - 1 do
    FMidiFile.GetTrack(I).EndOfTrack := false;

  if Start_MIDI_Play(FPriority, MidiDriver) then
    FPlaying := true;
end;

procedure TMidiPlayer2.ResumePlaying;
begin
  if FPlaying then exit;
  if not IsReadyToPlay then Exit;

// Raise priority class for application (=> higher level than NORMAL_PRIORITY_CLASS).
  if (FPriorityClass = BELOW_NORMAL_PRIORITY_CLASS) or (FPriorityClass = IDLE_PRIORITY_CLASS)
     or (FPriorityClass = NORMAL_PRIORITY_CLASS) then
    SetPriorityClass(FProcHandle, ABOVE_NORMAL_PRIORITY_CLASS);

  SetCurrentTime(FCurrentTime);
  if Start_MIDI_Play(FPriority, MidiDriver) then
    FPlaying := true;
end;

procedure TMidiPlayer2.StopPlaying;
begin
  if not FPlaying then exit;

  MidiDriver.Suspend := true;
  FMidiOut.SentAllNotesOff;
 // Post WM_CLOSE message to stop MidiDriver thread
  PostThreadMessage(MidiDriver.ThreadId, WM_CLOSE, 0, 0);

 // Restore priority class to initial level.
  SetPriorityClass(FProcHandle, FPriorityClass);

  FPlaying := false;
end;

procedure TMidiPlayer2.ProcMsg(var Msg: TMessage);
begin
  case Msg.Msg of
    WM_MIDI_Event: if Assigned(FOnMidiEvent) then FOnMidiEvent(Msg.WParam, PMidiEvent(Msg.LParam));
    WM_MIDI_GotLyrics: if Assigned(FOnGotLyrics) then FOnGotLyrics(Msg.WParam, PMidiEvent(Msg.LParam));
    WM_MIDI_PosUpdate: begin
                     FCurrentTime := Msg.WParam;
                     FCurrentPos := Msg.LParam;
                     if Assigned(FOnPosUpdate) then FOnPosUpdate(FCurrentTime, FCurrentPos);
                   end;
    WM_MIDI_EndOfTrack: begin
                     if Assigned(FOnEndOfTrack) then
                        FOnEndOfTrack(Msg.WParam);
                     if Msg.LParam = 1 then  // all tracks are reached to the end ?
                     begin
                       StopPlaying;
                       if Assigned(FOnPlayEnd) then FOnPlayEnd(Self);
                     end;
                   end;
    WM_MIDI_TempoChange: begin
                     if Assigned(FOnTempoChange) then FOnTempoChange(Msg.WParam);
                   end;
    WM_TIMER_FAIL: begin
                     FPlaying := false;
                     raise Exception.Create('Failed to create multimedia timer.');
                   end;
  else
    Msg.Result := DefWindowProc(MidiPlayerHandle, Msg.Msg, Msg.WParam, Msg.LParam);
  end;
end;

function TMidiPlayer2.GetVolume: TVolume;
begin
  if not FVolumeAdjustable then   // ** Added at 2012-05-20
  begin
    result.Left := 0;
    result.Right := 0;
    exit;
  end;

  if FMidiOut.State = mosOpen then
  begin
    result.Left := FMidiOut.GetVolume shr 16;
    result.Right := FMidiOut.GetVolume and $0000FFFF;
  end else
  begin
    result.Left := 0;
    result.Right := 0;
  end;
end;

procedure TMidiPlayer2.SetVolume(Value: TVolume);
begin
  if not FVolumeAdjustable then   // ** Added at 2012-05-20
    exit;

  if FMidiOut.State = mosOpen then
    FMidiOut.SetVolume(Value.Left, Value.Right);
end;

procedure TMidiPlayer2.SetSpeed(Value: Double);
begin
  if Value > MaxSpeed then
    Value := MaxSpeed;
  if Value < MinSpeed then
    Value := MinSpeed;
  MidiDriver.SetSpeed(Value);
  FSpeed := Value;
end;

procedure TMidiPlayer2.SetPitch(Value: Integer);  // ** Added at 2012-05-27
begin
  if Value > HigherPitchLimit then
    Value := HigherPitchLimit;
  if Value < LowerPitchLimit then
    Value := LowerPitchLimit;
  MidiDriver.SetPitch(Value);
  FPitch := Value;
end;

procedure TMidiPlayer2.SetStepMode(const Value: Boolean);
begin
  FStepMode := Value;
  MidiDriver.SetStepMode(Value);
end;

function TMidiPlayer2.SelectMidiOut(Device: string): Boolean;
var
  Suspended: Boolean;
begin
  result := false;

  if FMidiOut.State = mosOpen then
    if Device = FMidiOut.ProductName then
    begin
      result := true;
      exit;
    end;

  if FPlaying then
  begin
    Suspended := true;
    MidiDriver.Suspend := true;
  end else
    Suspended := false;

  if Device <> '' then
  begin
    if FMidiOut.State = mosOpen then
    begin
      FMidiOut.SentAllNotesOff;
      FMidiOut.Close;
      FOpenedMidiOut := '';
    end;
    FMidiOut.ProductName := Device;
    if FMidiOut.Open then
    begin
      FOpenedMidiOut := Device;
      if (FMidiOut.Support and MIDICAPS_VOLUME) = MIDICAPS_VOLUME then
        FVolumeAdjustable := true
      else
        FVolumeAdjustable := false;
      result := true;
    end;
  end;

  if result then
    if Suspended then
      MidiDriver.Suspend := false;
end;

function TMidiPlayer2.MidiOutProductName(iDevice: Integer): string;
begin
  if (iDevice < 0) or (iDevice >= FMidiOutDevices) then
  begin
    result := '';
    exit;
  end;

  result := FMidiOutNames[iDevice];
end;

procedure TMidiPlayer2.SkipExclusiveMsg(Value: Boolean);
begin
  MidiDriver.SkipExclusiveMsg(Value);
end;

function TMidiPlayer2.OpenFile(const FileName: WideString): Boolean;
begin
  if FPlaying then
  begin
    result := false;
    exit;
  end;

  if not Assigned(FMidiFile) then
  begin
    result := false;
    exit;
  end;

  result := FMidiFile.ReadFromFile(FileName);
  if Result then
    SetCurrentTime(0);
end;


end.
