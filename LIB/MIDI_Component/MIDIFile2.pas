{ TMidiFile2

 The component for MIDI File Reader.
 You can get various information of MIDI file such as format type, number of tracks, playback
  length and others, with this component.

  note) The source units of Tnt Delphi UNICODE Controls are needed if you compile this unit
        with Delphi 2007 or earlier version.

   #### Copyright Notice ####
 Copyright 2011 Silhwan Hyun, All Rights Reserved
 This unit is free. It may be used both in commercial and non-commercial software either in
  original or in modified form.
 This unit can be freely distributed in any way and by any means provided this copyright
  notice is preserved.

 Author : Silhwan Hyun   (e-mail addr : hyunsh@hanafos.com)

 Author's comment : Please let me know if you have any idea to improve or to debug this unit.


  Revision History
 --------------------------
  Ver 0.9.4    07 Apr 2012
   - Added property LyricsTrack
   - Added property SyncLyrics

  Ver 0.9.3    27 Aug 2011
   - Fixed bug : incorrect result of function Time2TickPos
   - Added property Lyrics ( = whole lyrics in plain text)
     note) You also can get the lyrics with time information from property RawLyrics of TMidiTrack.

  Ver 0.9.2    09 Jul 2011
   - Fixed incorrect result of function Time2TickPos
   - Added function Tick2TimePos

  Ver 0.9.1    26 Jun 2011
   - Replaced data interface function "ReadMidiFile" with component "TMidiFile".

  Ver 0.9.0    05 Jun 2011
   - Initial release      (function "ReadMidiFile" is used for data interface)
'----------------------------------------------------------------------------}

unit MidiFile2;

interface

uses Classes, SysUtils, Types, Windows {$IFNDEF UNICODE}, TntClasses{$ENDIF};

type
  {$IFNDEF UNICODE}
  UnicodeString = WideString;
  {$ENDIF}

  TMidiEvent = record
    Event: Byte;
    Data1: Byte;
    Data2: Byte;
    Msg: AnsiString;
    Positon: LongWord;
  end;
  PMidiEvent = ^TMidiEvent;

  TTempoData = record
    TickPos: LongWord;
    TimePos: LongWord;
    Tempo: LongWord;
  end;
  PTempoData = ^TTempoData;

  TRawLyric = record
     Position: Integer;
     Lyric: AnsiString;
  end;
  TRawLyrics = array of TRawLyric;

  TMidiTrack = class(TObject)
  private
    FEventList: TList;
    FRawLyrics: TRawLyrics;
    FActive: Boolean;
    FEndOfTrack: Boolean;  // True : playing position reached the end of track
    FTrackName: AnsiString;
    FTrackKeyword: AnsiString;
    FCopyright: AnsiString;
    FInstrument: AnsiString;
    FPlayPos: Integer;
  protected
    function GetEventCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddEvent(Event: PMidiEvent);
    function GetEvent(Index: Integer): PMidiEvent;
   // function GetChannels(Index: Integer): Boolean;
  published
    property Active: Boolean read FActive write FActive;
    property RawLyrics: TRawLyrics read FRawLyrics;
    property EndOfTrack: Boolean read FEndOfTrack write FEndOfTrack;
    property PlayPos: Integer read FPlayPos write FPlayPos;
    property TrackKeyword: AnsiString read FTrackKeyword;
    property TrackName: AnsiString read FTrackName;
    property Copyright: AnsiString read FCopyright;
    property Instrument: AnsiString read FInstrument;
    property EventCount: Integer read GetEventCount;
  end;

  TMidiFileInfo = record
    FileSize: Int64;    { File size (bytes) }
    Format: Word;       { 0: single-track, 1: multiple tracks, synchronous, 2: multiple tracks, asynchronous }
    Tracks: Word;
    Tempos: Word;
    TrackList: TList;
    TempoList: TList;
    TickUnit: Word;     { 0: Ticks per quarter note, 1: Ticks per second }
    Ticks: Word;        { Ticks per quarter note or Ticks per second }
    PlayTime: LongWord;    { Play length in mili seconds }
    PlayTicks: LongWord;   { Play length in ticks }
  end;

  TMidiFile2 = class(TComponent)
  private
    FIsValid: Boolean;
    FFormat: Word;
    FTickUnit: Word;
    FTicksPerQuarter: Word;
    FTrackList: TList;
    FTempoList: TList;
    FTrackCount: word;
    FTempoCount: word;
    FDuration: LongWord;
    FPlayTicks: LongWord;
    FLyricsTrack: Integer; // ** Added at ver 0.9.4
    FLyrics: AnsiString;   // ** Added at ver 0.9.3

    procedure ClearList;
    function  GetSyncLyrics: TRawLyrics; // ** Added at ver 0.9.4
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ReadFromFile(const FileName: WideString): Boolean; { Load data }
    property Valid: Boolean read FIsValid;          { True if file valid }
    property Format: Word read FFormat;             { 0: single-track, 1: multiple tracks, synchronous, 2: multiple tracks, asynchronous }
    property TickUnit: Word read FTickUnit;         { 0: Ticks per quarter note, 1: Ticks per second }
    property TicksPerQuarter: Word read FTicksPerQuarter;
    property TrackCount: word read FTrackCount;     { Number of tracks }
    property TempoCount: word read FTempoCount;     { Number of tempos }
    function GetTrack(Index: Integer): TMidiTrack;  { Gets an item of TMidiTrack }
    function GetTempo(Index: Integer): PTempoData;  { Gets an item of tempo data }
    property Duration: LongWord read FDuration;        { Play length in milliseconds }
    property PlayTicks: LongWord read FPlayTicks;      { Play length in ticks }
    property LyricsTrack: Integer read FLyricsTrack;   { The track where lyrics is recorded }
    property Lyrics: AnsiString read FLyrics;          { The plain lyrics text }
    property SyncLyrics: TRawLyrics read GetSyncLyrics;   { The sync lyrics data }
    function Time2TickPos(TimeVal: LongWord): LongWord;   { Get position in ticks from position in time(milliseconds) }
    function Tick2TimePos(TickVal: LongWord): LongWord;   { Get position in time(milliseconds) from position in ticks }
  end;

// function ReadMidiFile(const FileName: UnicodeString; var Info: TMidiFile2Info): Boolean;

  procedure Register;

implementation

const
  MIDI_ID = 'MThd';
  TRACK_ID = 'MTrk';
 // End_Of_Track = $FF2F00;
  DefaultTempo = 500000;  // 500000 microseconds per quarter note = 0.5 second per quarter note.

type
  TFileID = array[1..4] of AnsiChar;

  TTempoMap = array of TTempoData;

  TTimeSig = packed record
    Numer: byte;  // numerator
    Denom: byte;  // denominator
    Metro: byte;  // metronome pulse
    Notes: byte;  // The number of 32nd notes per 24 MIDI clock signals
  end;

{ TMidiTrack }

constructor TMidiTrack.Create;
begin
  inherited Create;
  FEventList := TList.Create;
  FActive := true;
  FEndOfTrack := False;
  FTrackName := '';
  FTrackKeyword := '';
  FCopyright := '';
  FInstrument := '';
  FPlayPos := 0;
end;

destructor TMidiTrack.Destroy;
var
  i: integer;
begin
  for i := 0 to FEventList.Count - 1 do
    Dispose(PMidiEvent(FEventList[i]));
  FEventList.Free;
  SetLength(FRawLyrics, 0);  // **

  inherited;
end;

procedure TMidiTrack.AddEvent(Event: PMidiEvent);
var
  N: Integer;
begin
  if (Event^.Event = $FF) then
  begin
    case Event^.Data1 of
      $1: FTrackKeyword := FTrackKeyword + Event^.Msg;
      $2: FCopyright := FCopyright + Event^.Msg;
      $3: FTrackName := FTrackName + Event^.Msg;
      $4: FInstrument := FInstrument + Event^.Msg;
      $5: begin    // **
           N := High(FRawLyrics) + 1;  // N : the number of recored lyrics
           SetLength(FRawLyrics, N + 1);
           FRawLyrics[N].Position := Event^.Positon;
           FRawLyrics[N].Lyric := Event^.Msg;
         end;
    end;
  end else
  begin
   { case Event^.iEvent of
      $B0..$BF, $C0..$CF: // control change, program change
        FChannels[Event^.iEvent and $F] := True;
    end; }
  end;

  FEventList.Add(Event);
end;

function TMidiTrack.GetEvent(Index: Integer): PMidiEvent;
begin
  if (Index >= 0) and (Index < FEventList.Count) then
    Result := PMidiEvent(FEventList[Index]) else
    Result := nil;
end;

function TMidiTrack.GetEventCount: Integer;
begin
  Result := FEventList.Count;
end;


{function TMidiTrack.GetChannels(Index: Integer): Boolean;
begin
  Result := FChannels[Index];
end; }

function GetInfo(const FileName: UnicodeString; var Info: TMidiFileInfo): Boolean;
var
  {$IFNDEF UNICODE}
  SourceFile: TTntFileStream;
  {$ELSE}
  SourceFile: TFileStream;
  {$ENDIF}

  FileID: TFileID;
  TrackID: TFileID;
  buf: array[1..4] of byte;
  TrackDataSize: integer;
  I, N1: integer;
  DeltaTime: LongWord;
  Tempo: LongWord;
  Len: word;
  EndOfTrack: boolean;
  DataBytes: integer;

  Elapsed: LongWord;
  ElapsedTime: array of LongWord;
  MaxTime: LongWord;
  Play_Time: double;

  Status, RunningStatus: byte;
  SysExContinue: boolean;
  NextValue: byte;
  str: AnsiString;

  MidiTrack: TMidiTrack;
  TempoCounter: word;
  TempoMap: TTempoMap;
  PTempo: PTempoData;
  pEvent1: PMidiEvent;

 function GetDelta(var Len_: word): LongWord;
 var
   m: LongWord;
   n: integer;
   b: byte;
   LastByte: boolean;
 begin
   m := 0;
   Len_ := 0;
   LastByte := false;

   for n := 0 to 4 do
   begin
    if SourceFile.Position >= (SourceFile.Size - 1) then
      break;
    SourceFile.Read(b, 1);
   { if (b = $FF) then
      break; }
    inc(Len_);
    if (b and $80) = 0 then
      LastByte := true;

    m := (m shl 7) + (b and $7f);
    if LastByte then
      break;
   end;

  result := m;
 end;

 procedure AddDeltaTime(DeltaTime_: LongWord; Track_: byte; Adding: boolean; var Elapsed_: LongWord);
 begin
   if DeltaTime_ = 0 then
     exit;

   if Adding then
   begin  // Add
     if Info.Format = 0 then
       Elapsed_ := Elapsed_ + DeltaTime_
     else begin
       ElapsedTime[Track_] := ElapsedTime[Track_] + DeltaTime_;
       Elapsed_ := ElapsedTime[Track_];
     end;
   end else
   begin  // Substract
     if Info.Format = 0 then
       Elapsed_ := Elapsed_ - DeltaTime_
     else begin
       ElapsedTime[Track_] := ElapsedTime[Track_] - DeltaTime_;
       Elapsed_ := ElapsedTime[Track_];
     end;
   end;
 end;

 procedure SaveChannelEvents(Status_: byte; Elapsed_: LongWord);
 var
   MessageType: byte;
   ChanData: array[1..2] of byte;
   pEvent: PMidiEvent;
 begin
   MessageType := Status_ and $F0;
   New(pEvent); // New event
   pEvent^.Event := Status_;
   pEvent^.Positon := Elapsed_;

 // $80 : Note off,  $90 : Note on,  $A0 : Note After touch,  $B0 : Control change
 // $C0 : Program change,  $D0 : Channel after touch,  $E0 : Pitch wheel change
   case MessageType of
     $80, $90, $A0, $B0, $E0 : begin
             SourceFile.Read(ChanData, 2);
             pEvent^.Data1 := ChanData[1];
             pEvent^.Data2 := ChanData[2];
           end;

     $C0, $D0 : begin
             SourceFile.Read(ChanData, 1);
             pEvent^.Data1 := ChanData[1];
           end;
   end;

   MidiTrack.AddEvent(pEvent);
 end;

 procedure SaveMetaEvent(Status_: byte; Elapsed_: LongWord);
 var
   DataBytes_: word;
   DataType: byte;
   Len2: word;
   pEvent: PMidiEvent;
   str2: AnsiString;
 begin
   { DataType  Description
        0      Set track¡¯s sequence #
      $01      User Text
      $02      Copyright info.
      $03      Track name
      $04      Instrument Names
      $05      Lyric
      $06      Marker
      $07      Cue Point
      $08      Program Name
      $09      Device Name
      $20      MIDI channel prefix assignment
      $21      MIDI port
      $51      Set tempo (microseconds per quarter note)
      $54      SMPTE Offset
      $58      Time signature
      $59      Key signature
      $7f      Sequencer specific
     end; }

   SourceFile.Read(DataType, 1);  // Read type code of Meta event
   DataBytes_ := GetDelta(Len2);  // Read the length of data
   SetLength(str2, DataBytes_);
   SourceFile.Read(str2[1], DataBytes_); // Read text
   if DataType = $2f then        // End of Track ?
     EndOfTrack := true
   else if DataType = $51 then  // Set tempo
   begin
     Tempo := (byte(str2[1]) shl 16) + (byte(str2[2]) shl 8) + byte(str2[3]);
     if TempoCounter <> 0 then
       if TempoMap[TempoCounter-1].TickPos = Elapsed then
         exit;
     inc(TempoCounter);
     SetLength(TempoMap, TempoCounter);
     TempoMap[TempoCounter-1].TickPos := Elapsed;
     TempoMap[TempoCounter-1].Tempo := Tempo;
   end;

   New(pEvent);
   pEvent^.Event := Status_;
   pEvent^.Positon := Elapsed_;
   pEvent^.Data1 := DataType;
   pEvent^.Msg := str2;
   MidiTrack.AddEvent(pEvent);
 end;

 procedure Raise_Exception(S: string);
 begin
   MessageBox(0, pChar(S), 'Confirm', MB_OK);
   raise Exception.Create('');
 end;

begin
  { Get info from file }
  Result := false;
  SourceFile := nil;

  try
   {$IFNDEF UNICODE}
    SourceFile := TTntFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
   {$ELSE}
    SourceFile := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
   {$ENDIF}
    Info.FileSize := SourceFile.Size;
    SourceFile.Read(FileID, 4); // Read File ID. (should be 'MThd')
    if FileID <> MIDI_ID then
      Raise_Exception('File has invalid MIDI Header ID.'#13#10#13#10 + ' => ' + FileName);

    SourceFile.Read(buf, 4);    // Read Header Length (should be $00000006)
    if (buf[1] <> 0) or (buf[2] <> 0) or (buf[3] <> 0) or (buf[4] <> 6) then
      Raise_Exception('File has invalid MIDI Header length.'#13#10#13#10 + ' => ' + FileName);

    SourceFile.Read(buf, 2);    // Read Format Type ($00 ~ $02)
    Info.Format := buf[1] * 256 + buf[2];

    SourceFile.Read(buf, 2);    // Read number of tracks (1 ~ 65,535)
    Info.Tracks := buf[1] * 256 + buf[2];

    SourceFile.Read(buf, 2);    // Read Time Division
    Info.TickUnit := buf[1] shr 7;  // Take MSB of buf[1]
    if Info.TickUnit = 0 then   // 0: Ticks per quarter note
      Info.Ticks := buf[1] * 256 + buf[2]
    else                        // none 0: Ticks per frame
      Info.Ticks := (128 - (buf[1] and $7f)) * buf[2];

    N1 := Info.Tracks;
    Info.TrackList := TList.Create;
    SetLength(ElapsedTime, Info.Tracks);
    for I := 1 to Info.Tracks do
      ElapsedTime[I-1] := 0;
    Tempo:= DefaultTempo;
    TempoCounter := 0;

    for I := 0 to (N1 - 1) do
    begin
      SourceFile.Read(TrackID, 4); // Read Track ID. (should be 'MTrk')
      if TrackID <> TRACK_ID then
        Raise_Exception('File has invalid MIDI Track ID.'#13#10#13#10 + ' => ' + FileName);

      Elapsed := 0;
      MidiTrack := TMidiTrack.Create;
      Info.TrackList.Add(MidiTrack); // Add to track list

      SourceFile.Read(buf, 4);     // Read Track size
      TrackDataSize := (buf[1] shl 24) + (buf[2] shl 16) + (buf[3] shl 8) + buf[4];

    //  Info.TrackNames[I] := '';
      EndOfTrack := false;

      RunningStatus := 0;     // the running status byte
      SysExContinue := false; // whether we're in a multi-segment system exclusive message

      repeat
        DeltaTime := GetDelta(Len);
        SourceFile.Read(NextValue, 1);   // Read Status Byte
       // Are we continuing a sys ex?  If so, the next value should be $F7
        if (SysExContinue and (NextValue <> $F7)) then
          Raise_Exception(Format('Expected to find a system exclusive continue byte at,'#13#10#13#10 + '%d',
                                        [SourceFile.Position]));
       // Are we in running status?  Determine whether we're running and what the current status byte is.
        if ((NextValue and $80) = 0) then
        begin
          if (RunningStatus = 0) then
            Raise_Exception(Format('Status byte required for running status at,'#13#10#13#10 + '%d',
                                       [SourceFile.Position]));
       // Keep the last iteration's status byte, and now we're in running mode
          SourceFile.Position := SourceFile.Position - 1;   // backward to 1 byte.
          Status := RunningStatus;
        end else
        begin
        // Running status is only implemented for Voice Category messages (ie, Status is $80 to $EF).
          if (NextValue >= $80) and (NextValue <= $EF) then
            RunningStatus := NextValue
        // System Common Category messages (ie, Status of 0xF0 to 0xF7) cancel any running status.
        // note) RealTime Category messages (ie, Status of 0xF8 to 0xFF) do not effect running status in any way.
          else if (NextValue >= $F0) and (NextValue <= $F7) then
            RunningStatus := 0;
          Status := NextValue;
        end;

        AddDeltaTime(DeltaTime, I, true{Adding}, Elapsed);
        if (Status >= $80) and (Status <= $EF) then   // MIDI Channel Event ?
        begin
        // Handle channel events
          SaveChannelEvents(Status, Elapsed);
        end
        else if (Status = $FF) then                   // Meta Event ?
        begin
         // Handle meta events
          SaveMetaEvent(Status, Elapsed);
          if EndOfTrack then   // The DeltaTime at "End of Track" event should not be accounted.
            AddDeltaTime(DeltaTime, I, false{Adding}, Elapsed);
        end
        else if (Status >= $F8) and (Status <= $FE) then  // System Real-Time Message ?
        begin
          New(pEvent1);
          pEvent1^.Event := Status;
          pEvent1^.Positon := Elapsed;
          MidiTrack.AddEvent(pEvent1);
        end
        else if (Status >= $F1) and (Status <= $F6) then  // System Common Message ?
        begin
          New(pEvent1);
          pEvent1^.Event := Status;
          pEvent1^.Positon := Elapsed;
          if (Status = $F1) or (Status = $F3) then
          begin
            SourceFile.Read(buf, 1);
            pEvent1^.Data1 := buf[1];
          end else
          if (Status = $F2) then
          begin
            SourceFile.Read(buf, 2);
            pEvent1^.Data1 := buf[1];
            pEvent1^.Data2 := buf[2];
          end;
          MidiTrack.AddEvent(pEvent1);
        end
        else if (Status = $F0) or (Status = $F7) then   // System Exclusive Message ?
        begin
          DataBytes := GetDelta(Len);    // Read the length of data
          if DataBytes = 0 then    // ** Added for the case there is no data. (2012-04-16)
            Continue;
          SetLength(str, DataBytes);
          SourceFile.Read(str[1], DataBytes); // Read data
          if (Status = $F0) then
            if (ord(str[DataBytes]) <> $F7) then  // multi-segment Message ?
              SysExContinue := true;
          if (Status = $F7) then                  // Message continuations ?
            if (ord(str[DataBytes]) = $F7) then   // Found end marker ?
              SysExContinue := false;

          New(pEvent1);
          pEvent1^.Event := Status;
          pEvent1^.Positon := Elapsed;
          pEvent1^.Msg := str;
          MidiTrack.AddEvent(pEvent1);
        end;
      until EndOfTrack or (SourceFile.Position = (SourceFile.Size - 1));

      if SourceFile.Position = (SourceFile.Size - 1) then
        break;
    end;   // end of "for I := 1 to N do"

    if Info.Format = 0 then
      MaxTime := Elapsed
    else begin
      MaxTime := ElapsedTime[0];
      for I := 1 to (Info.TrackList.Count - 1) do
        if ElapsedTime[I] > MaxTime then
           MaxTime := ElapsedTime[I];
    end;

    Info.PlayTicks := MaxTime;
    if Info.TickUnit = 0 then     {0: Ticks per quarter note, 1: Ticks per second}
    begin
      if TempoCounter = 0 then
        Play_Time := (MaxTime / Info.Ticks) * (Tempo / 1000.0)
      else if TempoCounter = 1 then
        Play_Time := (MaxTime / Info.Ticks) * (TempoMap[0].Tempo / 1000.0)
      else begin
        if TempoMap[0].TickPos = 0 then
        begin
          Play_Time := 0;
          TempoMap[0].TimePos := 0
        end else begin
          Play_Time := ((TempoMap[0].TickPos) / Info.Ticks) * (DefaultTempo / 1000.0);
          TempoMap[0].TimePos := round(Play_Time);
        end;
        for I := 0 to TempoCounter - 2 do
        begin
          Play_Time := Play_Time
                     + ((TempoMap[I+1].TickPos - TempoMap[I].TickPos) / Info.Ticks) * (TempoMap[I].Tempo / 1000.0);
          TempoMap[I+1].TimePos := round(Play_Time);
        end;
        if MaxTime > TempoMap[TempoCounter - 1].TickPos then
          Play_Time := Play_Time
                     + ((MaxTime - TempoMap[TempoCounter - 1].TickPos) / Info.Ticks) * (TempoMap[TempoCounter - 1].Tempo / 1000.0);
      end;
      Info.PlayTime := round(Play_Time);
    end else
      Info.PlayTime := round((MaxTime / Info.Ticks) * 1000.0);

    Info.TempoList := TList.Create;
    if TempoCounter = 0 then
    begin
      Info.Tempos := 1;
      new(PTempo);
      PTempo^.TickPos := 0;
      PTempo^.TimePos := 0;
      PTempo^.Tempo := Tempo;
      Info.TempoList.Add(PTempo);
    end else
    begin
      Info.Tempos := TempoCounter;
      for I := 0 to (TempoCounter - 1) do
      begin
        new(PTempo);
        PTempo^.TickPos := TempoMap[I].TickPos;
        PTempo^.TimePos := TempoMap[I].TimePos;
        PTempo^.Tempo := TempoMap[I].Tempo;
        Info.TempoList.Add(PTempo);
      end;
    end;

    Result := true;
  finally
    SourceFile.Free;
    SetLength(ElapsedTime, 0);
    SetLength(str, 0);
    SetLength(TempoMap, 0);
  end;

end;


{ function ReadMidiFile(const FileName: UnicodeString; var Info: TMidiFile2Info): Boolean;
begin
  result := GetInfo(FileName, Info);
end; }


constructor TMidiFile2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FTrackList := TList.Create;
  FTempoList := TList.Create;
  FLyricsTrack := -1;
end;

destructor TMidiFile2.Destroy;

begin
  ClearList;
  FTrackList.Free;
  FTempoList.Free;

  inherited;
end;

procedure TMidiFile2.ClearList;
var
  i: Integer;
begin
  for i := 0 to FTrackCount - 1 do
    TMidiTrack(FTrackList[i]).Free;
  FTrackList.Clear;
  for i := 0 to FTempoCount - 1 do
    Dispose(PTempoData(FTempoList[i]));
  FTempoList.Clear;
end;

function TMidiFile2.ReadFromFile(const FileName: WideString): Boolean;
var
  FileInfo: TMidiFileInfo;
  I, N: integer;
begin
  FLyricsTrack := -1;
  FLyrics := '';  // ** Added at 2012-04-07

  try
    FileInfo.TrackList := nil;
    FileInfo.TempoList := nil;
    FIsValid := GetInfo(FileName, FileInfo);
  except

  end;

  if FIsValid then
  begin
    FFormat := FileInfo.Format;
    FTickUnit := FileInfo.TickUnit;
    FTicksPerQuarter := FileInfo.Ticks;
    ClearList;
    FTrackCount := FileInfo.Tracks;
    FTempoCount := FileInfo.Tempos;

    for I := 0 to FTrackCount - 1 do
    begin
      FTrackList.Add(FileInfo.TrackList[I]);
      if High(TMidiTrack(FileInfo.TrackList[I]).FRawLyrics) <> - 1 then
        FLyricsTrack := I;
    end;
    if FLyricsTrack <> -1 then
    begin
      N := High(TMidiTrack(FileInfo.TrackList[FLyricsTrack]).FRawLyrics);
      for I := 0 to N do
        FLyrics := FLyrics + TMidiTrack(FileInfo.TrackList[FLyricsTrack]).FRawLyrics[I].Lyric;
    end;

    for I := 0 to FTempoCount - 1 do
      FTempoList.Add(FileInfo.TempoList[I]);
    FDuration := FileInfo.PlayTime;
    FPlayTicks := FileInfo.PlayTicks;
    FIsValid := (FTrackCount > 0) and (FDuration > 0);
    result := true;
  end else
  begin
    FIsValid := false;
    result := false;
  end;

  if FileInfo.TrackList <> nil then
  begin
    FileInfo.TrackList.Clear;
    FileInfo.TrackList.Free;
  end;
  if FileInfo.TempoList <> nil then
  begin
    FileInfo.TempoList.Clear;
    FileInfo.TempoList.Free;
  end;
end;

function TMidiFile2.GetTrack(Index: Integer): TMidiTrack;
begin
  if (Index >= 0) and (Index < FTrackList.Count) then
    Result := TMidiTrack(FTrackList[Index]) else
    Result := nil;
end;

function TMidiFile2.GetTempo(Index: Integer): PTempoData;
begin
  if (Index >= 0) and (Index < FTempoList.Count) then
    Result := PTempoData(FTempoList[Index]) else
    Result := nil;
end;

function TMidiFile2.Time2TickPos(TimeVal: LongWord): LongWord;  // milliseconds -> ticks
var
  I, K: integer;
  Residue: LongWord;
  NumQuarter: double;
begin
  if not FIsValid then
  begin
    result := 0;
    exit;
  end;

  if TimeVal = 0 then
  begin
    result := 0;
    exit;
  end;

 { if Val > FDuration then
  begin
    result := FPlayTicks;
    exit;
  end; }

  if FTickUnit = 1 then     {0: Ticks per quarter note, 1: Ticks per second}
  begin
    result := round(TimeVal * FTicksPerQuarter / 1000.0);
    exit;
  end;

  if FTempoCount = 1 then
  begin
    if PTempoData(FTempoList[0])^.TimePos = 0 then
      result := round(TimeVal * FTicksPerQuarter / PTempoData(FTempoList[0])^.Tempo * 1000.0)
    else if TimeVal < PTempoData(FTempoList[0])^.TimePos then
      result := round(TimeVal * FTicksPerQuarter / DefaultTempo * 1000.0)
    else begin
      Residue := TimeVal - PTempoData(FTempoList[0])^.TimePos;
      NumQuarter := (Residue * 1000.0) / PTempoData(FTempoList[0])^.Tempo;
      result := round(PTempoData(FTempoList[0])^.TickPos + NumQuarter * FTicksPerQuarter);
    end;

    exit;
  end;

  K := (FTempoCount - 1);
  for I := 1 to (FTempoCount - 1) do
    if PTempoData(FTempoList[I])^.TimePos > TimeVal then
    begin
      K := I - 1;
      break;
    end;

  if (K = 0) and (TimeVal < PTempoData(FTempoList[K])^.TimePos) then  // ** Added
    NumQuarter := (TimeVal * 1000.0) / DefaultTempo
  else begin
    Residue := TimeVal - PTempoData(FTempoList[K])^.TimePos;
    NumQuarter := (Residue * 1000.0) / PTempoData(FTempoList[K])^.Tempo;
  end;
  result := round(PTempoData(FTempoList[K])^.TickPos + NumQuarter * FTicksPerQuarter);
end;

function TMidiFile2.Tick2TimePos(TickVal: LongWord): LongWord;  // ticks -> milliseconds
var
  I, K: integer;
begin
  if not FIsValid then
  begin
    result := 0;
    exit;
  end;

  if TickVal = 0 then
  begin
    result := 0;
    exit;
  end;

  if FTickUnit = 0 then     {0: Ticks per quarter note, 1: Ticks per second}
  begin
    K := FTempoCount - 1;
    for I := (FTempoCount - 1) downto 0 do
    begin
      if TickVal < PTempoData(FTempoList[I])^.TickPos then
        dec(K);
    end;

    if K < 0 then
      result := round((TickVal / FTicksPerQuarter) * (DefaultTempo / 1000.0))
    else
      result := round(PTempoData(FTempoList[K])^.TimePos
              + ((TickVal - PTempoData(FTempoList[K])^.TickPos) / FTicksPerQuarter) * (PTempoData(FTempoList[K])^.Tempo / 1000.0));

  end else
    result := round((TickVal / FTicksPerQuarter) * 1000.0);

end;

function TMidiFile2.GetSyncLyrics: TRawLyrics;
begin
  if FLyricsTrack <> -1 then
    result := TMidiTrack(FTrackList[FLyricsTrack]).FRawLyrics
  else
    SetLength(result, 0);
end;

procedure Register;
begin
  RegisterComponents('Synth', [TMidiFile2]);
end;

end.
