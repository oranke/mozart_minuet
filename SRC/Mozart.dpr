program Mozart;

uses
  Windows,
  Forms,
  MainUnit in 'MainUnit.pas' {MainForm},
  DataUnit in 'DataUnit.pas',
  SelectFrameUnit in 'SelectFrameUnit.pas' {SelectFrame: TFrame};

{$R *.res}

{$R Res_Minuet.RES}
{$R Res_Trio.RES}

begin
  ReportMemoryLeaksOnShutdown := true;

  //AllocConsole();
  
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
