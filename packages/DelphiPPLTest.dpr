program DelphiPPLTest;

uses
  DUnitTestRunner,
  System.ThreadingEx.TestCase in '..\testcase\System.ThreadingEx.TestCase.pas',
  System.ThreadingEx in '..\source\System.ThreadingEx.pas',
  System.SyncObjs.RSP25999 in '..\source\System.SyncObjs.RSP25999.pas';

{$R *.RES}

begin
  ReportMemoryLeaksOnShutdown := True;
  TTestCase_PPL.ClassName;
  DUnitTestRunner.RunRegisteredTests;
end.
