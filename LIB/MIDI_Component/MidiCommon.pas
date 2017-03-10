unit MidiCommon;

interface

uses Classes, Windows{WinTypes}, Messages, SysUtils, MMSystem;

const
  MIDI_ALLNOTESOFF = $78;
  MIDI_NOTEON = $90;
  MIDI_NOTEOFF = $80;
  MIDI_KEYAFTERTOUCH = $A0;
  MIDI_CONTROLCHANGE = $B0;
  MIDI_PROGRAMCHANGE = $C0;
  MIDI_CHANAFTERTOUCH = $D0;
  MIDI_PITCHBEND = $E0;
  MIDI_SYSTEMMESSAGE = $F0;
  MIDI_BEGINSYSEX = $F0;
  MIDI_MTCQUARTERFRAME = $F1;
  MIDI_SONGPOSPTR = $F2;
  MIDI_SONGSELECT = $F3;
  MIDI_ENDSYSEX = $F7;
  MIDI_TIMINGCLOCK = $F8;
  MIDI_START = $FA;
  MIDI_CONTINUE = $FB;
  MIDI_STOP = $FC;
  MIDI_ACTIVESENSING = $FE;
  MIDI_SYSTEMRESET = $FF;

  MIM_OVERFLOW = WM_USER; { Input buffer overflow }
  MOM_PLAYBACK_DONE = WM_USER + 1; { Timed playback complete }

type
  { MIDI input event }
  TMidiBufferItem = record
    timestamp: DWORD; { Timestamp in milliseconds after midiInStart }
    data: DWORD; { MIDI message received }
    sysex: PMidiHdr; { Pointer to sysex MIDIHDR, nil if not sysex }
  end;
  PMidiBufferItem = ^TMidiBufferItem;

  { MIDI input buffer }
  TCircularBuffer = record
    RecordHandle: HGLOBAL; { Windows memory handle for this record }
    BufferHandle: HGLOBAL; { Windows memory handle for the buffer }
    pStart: PMidiBufferItem; { ptr to start of buffer }
    pEnd: PMidiBufferItem; { ptr to end of buffer }
    pNextPut: PMidiBufferItem; { next location to fill }
    pNextGet: PMidiBufferItem; { next location to empty }
    Error: Word; { error code from MMSYSTEM functions }
    Capacity: Word; { buffer size (in TMidiBufferItems) }
    EventCount: Word; { Number of events in buffer }
  end;
  PCircularBuffer = ^TCircularBuffer;

  { This is the information about the control that must be accessed by
    the MIDI input callback function in the DLL at interrupt time }
  PMidiCtlInfo = ^TMidiCtlInfo;
  TMidiCtlInfo = record
    hMem: THandle; { Memory handle for this record }
    PBuffer: PCircularBuffer; { Pointer to the MIDI input data buffer }
    hWindow: HWnd; { Control's window handle }
    SysexOnly: Boolean; { Only process System Exclusive input }
  end;

  { Information for the output timer callback function, also required at
    interrupt time. }
  PMidiOutTimerInfo = ^TMidiOutTimerInfo;
  TMidiOutTimerInfo = record
    hMem: THandle; { Memory handle for this record }
    PBuffer: PCircularBuffer; { Pointer to MIDI output data buffer }
    hWindow: HWnd; { Control's window handle }
    TimeToNextEvent: DWORD; { Delay to next event after timer set }
    MIDIHandle: HMidiOut; { MIDI handle to send output to copy of component's FMidiHandle property }
    PeriodMin: Word; { Multimedia timer minimum period supported }
    PeriodMax: Word; { Multimedia timer maximum period supported }
    TimerId: Word; { Multimedia timer ID of current event }
  end;

  { A MIDI input/output event }
  TMyMidiEvent = class(TPersistent)
  public
    MidiMessage: Byte; { MIDI message status byte }
    Data1: Byte; { MIDI message data 1 byte }
    Data2: Byte; { MIDI message data 2 byte }
    Time: DWORD; { Time in ms since midiInOpen }
    SysexLength: Word; { Length of sysex data (0 if none) }
  // ** Changed for compatibility to Delphi 2009 or newer version
  //  Sysex: PChar; { Pointer to sysex data buffer }
    Sysex: PAnsiChar; { Pointer to sysex data buffer }
    destructor Destroy; override; { Frees sysex data buffer if nec. }
  end;
  PMyMidiEvent = ^TMyMidiEvent;

  { Encapsulates the MIDIHDR with its memory handle and sysex buffer }
  PMyMidiHdr = ^TMyMidiHdr;
  TMyMidiHdr = class(TObject)
  public
    hdrHandle: THandle;
    hdrPointer: PMIDIHDR;
    sysexHandle: THandle;
    sysexPointer: Pointer;
    constructor Create(BufferSize: Word);
    destructor Destroy; override;
  end;

{$IFDEF WIN32}
procedure midiHandler(
  hMidiIn: HMidiIn;
  wMsg: UINT;
  dwInstance: DWORD;
  dwParam1: DWORD;
  dwParam2: DWORD); stdcall export;
//function CircbufPutEvent(PBuffer: PCircularBuffer; PTheEvent: PMidiBufferItem): Boolean; stdcall; export;
{$ELSE}
procedure midiHandler(
  hMidiIn: HMidiIn;
  wMsg: Word;
  dwInstance: DWORD;
  dwParam1: DWORD;
  dwParam2: DWORD); export;
//function CircbufPutEvent(PBuffer: PCircularBuffer; PTheEvent: PMidiBufferItem): Boolean; export;
{$ENDIF}

function midiIOErrorString(IO: Boolean; WError: Word): string;

function GlobalSharedLockedAlloc(Capacity: Word; var hMem: HGLOBAL): Pointer;
procedure GlobalSharedLockedFree(hMem: HGLOBAL; ptr: Pointer);

function NoteToStr(note: integer; bzero: Boolean = False): string;
function MyTimeToStr(const Value: Integer): string;
function MyStrToTime(const Value: string): Integer;


implementation

function midiIOErrorString(IO: Boolean; WError: Word): string;
{ Convert the numeric return code from an MMSYSTEM function to a string }
var
  iError: Cardinal;
  errorDesc: PChar;
begin
  errorDesc := nil;
  try
    errorDesc := StrAlloc(MAXERRORLENGTH);
    if IO then
      iError := midiInGetErrorText(WError, errorDesc, MAXERRORLENGTH) else
      iError := midiOutGetErrorText(WError, errorDesc, MAXERRORLENGTH);
    if iError = 0 then
      result := StrPas(errorDesc)
    else
      result := 'Specified error number is out of range';
  finally
    if errorDesc <> nil then StrDispose(errorDesc);
  end;
end;

procedure midiHandler(hMidiIn: HMidiIn; wMsg: UINT; dwInstance: DWORD; dwParam1: DWORD; dwParam2: DWORD);
  function CircbufPutEvent(PBuffer: PCircularBuffer; PTheEvent: PMidiBufferItem): Boolean;
  begin
    if (PBuffer^.EventCount < PBuffer^.Capacity) then
    begin
      Inc(Pbuffer^.EventCount);
      { Todo: better way of copying this record }
      with PBuffer^.PNextput^ do
      begin
        Timestamp := PTheEvent^.Timestamp;
        Data := PTheEvent^.Data;
        Sysex := PTheEvent^.Sysex;
      end;
      { Move to next put location, with wrap }
      Inc(Pbuffer^.PNextPut);
      if (PBuffer^.PNextPut = PBuffer^.PEnd) then
        PBuffer^.PNextPut := PBuffer^.PStart;
      CircbufPutEvent := True;
    end else
      CircbufPutEvent := False;
  end;
var
  thisEvent: TMidiBufferItem;
  thisCtlInfo: PMidiCtlInfo;
  thisBuffer: PCircularBuffer;
begin
  case wMsg of
    mim_Open:
      { nothing};
    mim_Error:
      { TODO: handle (message to trigger exception?) };
    mim_Data, mim_Longdata, mim_Longerror:
      { Note: mim_Longerror included because there's a bug in the Maui
        input driver that sends MIM_LONGERROR for subsequent buffers when
        the input buffer is smaller than the sysex block being received }
      begin
        { TODO: Make filtered messages customisable, I'm sure someone wants
          to do something with MTC! }
        if (dwParam1 <> MIDI_ACTIVESENSING) and (dwParam1 <> MIDI_TIMINGCLOCK) then
        begin
          { The device driver passes us the instance data pointer we
            specified for midiInOpen. Use this to get the buffer address
            and window handle for the MIDI control }
          thisCtlInfo := PMidiCtlInfo(dwInstance);
          thisBuffer := thisCtlInfo^.PBuffer;
          { Screen out short messages if we've been asked to }
          if ((wMsg <> mim_Data) or (thisCtlInfo^.SysexOnly = False))
            and (thisCtlInfo <> nil) and (thisBuffer <> nil) then
          begin
            with thisEvent do
            begin
              Timestamp := dwParam2;
              if (wMsg = mim_Longdata) or (wMsg = mim_Longerror) then
              begin
                data := 0;
                sysex := PMidiHdr(dwParam1);
              end else
              begin
                data := dwParam1;
                sysex := nil;
              end;
            end;
            if CircbufPutEvent(thisBuffer, @thisEvent) then
              { Send a message to the control to say input's arrived }
              PostMessage(thisCtlInfo^.hWindow, mim_Data, 0, 0)
            else
              { Buffer overflow }
              PostMessage(thisCtlInfo^.hWindow, mim_Overflow, 0, 0);
          end;
        end;
      end;
    mom_Done:
      { Sysex output complete, dwParam1 is pointer to MIDIHDR }
      begin
        { Notify the control that its sysex output is finished.
          The control should call midiOutUnprepareHeader before freeing the buffer }
        PostMessage(PMidiCtlInfo(dwInstance)^.hWindow, mom_Done, 0, dwParam1);
      end;
  end; { End Case }
end;

destructor TMyMidiEvent.Destroy;
{ Free any sysex buffer associated with the event }
begin
  if (Sysex <> nil) then
    Freemem(Sysex, SysexLength);
  inherited Destroy;
end;

constructor TMyMidiHdr.Create(BufferSize: Word);
{ Allocate memory for the sysex header and buffer }
begin
  inherited Create;
  if BufferSize > 0 then
  begin
    hdrPointer := GlobalSharedLockedAlloc(sizeof(TMIDIHDR), hdrHandle);
    sysexPointer := GlobalSharedLockedAlloc(BufferSize, sysexHandle);
    hdrPointer^.lpData := sysexPointer;
    hdrPointer^.dwBufferLength := BufferSize;
  end;
end;

destructor TMyMidiHdr.Destroy;
begin
  GlobalSharedLockedFree(hdrHandle, hdrPointer);
  GlobalSharedLockedFree(sysexHandle, sysexPointer);
  inherited Destroy;
end;

function GlobalSharedLockedAlloc(Capacity: Word; var hMem: HGLOBAL): Pointer;
var
  ptr: Pointer;
begin
 { Allocate the buffer memory }
  hMem := GlobalAlloc(GMEM_SHARE or GMEM_MOVEABLE or GMEM_ZEROINIT, Capacity);
  if (hMem = 0) then
    ptr := nil else
  begin
    ptr := GlobalLock(hMem);
    if (ptr = nil) then
      GlobalFree(hMem);
  end;
{$IFNDEF WIN32}
  if (ptr <> nil) then
    GlobalPageLock(HIWORD(DWORD(ptr)));
{$ENDIF}
  GlobalSharedLockedAlloc := Ptr;
end;

procedure GlobalSharedLockedFree(hMem: HGLOBAL; ptr: Pointer);
begin
{$IFNDEF WIN32}
  if (ptr <> nil) then
    GlobalPageUnlock(HIWORD(DWORD(ptr)));
{$ENDIF}
  if (hMem <> 0) then
  begin
    GlobalUnlock(hMem);
    GlobalFree(hMem);
  end;
end;

function NoteToStr(note: integer; bzero: Boolean = False): string;
  function KeyToStr(key: integer): string;
  var
    n: integer;
    str: string;
  begin
    n := key mod 12;
    case n of
      0: str := 'C';
      1: str := 'C#';
      2: str := 'D';
      3: str := 'D#';
      4: str := 'E';
      5: str := 'F';
      6: str := 'F#';
      7: str := 'G';
      8: str := 'G#';
      9: str := 'A';
      10: str := 'A#';
      11: str := 'B';
    end;
    Result := str;
  end;
var
  n: integer;
  str: string;
begin
  n := note div 12;
  if bzero then // begin from C0 to C8
  begin
    str := KeyToStr(note);
    Result := str + IntToStr(note div 12);
  end else
  begin // begin from C2 to c5
    case n of
      0..1: str := UpperCase(KeyToStr(note)) + IntToStr(2 - n);
      2: str := UpperCase(KeyToStr(note));
      3: str := LowerCase(KeyToStr(note));
      4..8: str := LowerCase(KeyToStr(note)) + IntToStr(n - 3);
    end;
    Result := str;
  end;
end;

function MyStrToTime(const Value: string): Integer;
var
  hour: word;
  min: word;
  sec: word;
  msec: word;
begin //0:01:10.204
  try
    DecodeTime(StrToTime(Value), hour, min, sec, msec);
    Result := hour * 60 * 60 * 1000 + min * 60 * 1000 + sec * 1000 + msec;
  except
    Result := 0;
  end;
end;

function MyTimeToStr(const Value: Integer): string;
  function IntToLenStr(val: Integer; len: Integer): string;
  var
    str: string;
  begin
    str := IntToStr(val);
    while Length(str) < len do
      str := '0' + str;
    Result := str;
  end;
var
  hour: Integer;
  min: Integer;
  sec: Integer;
  msec: Integer;
begin
  msec := Value mod 1000;
  sec := Value div 1000;
  min := sec div 60;
  sec := sec mod 60;
  hour := min div 60;
  min := min mod 60;
  Result := IntToStr(hour) + ':' + IntToLenStr(min, 2) + ':' + IntToLenStr(sec, 2) + '.' + IntToLenStr(msec, 3);
end;

end.

