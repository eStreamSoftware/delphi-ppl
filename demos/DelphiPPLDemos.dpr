program DelphiPPLDemos;

uses
  Vcl.Forms,
  System.ThreadingEx in '..\source\System.ThreadingEx.pas',
  DemoMainForm in 'DemoMainForm.pas' {Form1};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
