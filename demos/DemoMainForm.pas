unit DemoMainForm;

interface

uses
  Winapi.Messages, Winapi.Windows, System.Classes, System.SysUtils,
  System.Variants, Vcl.Controls, Vcl.Dialogs, Vcl.Forms, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    procedure ShowValue(s: string);
    function GetValue: string;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses System.ThreadingEx;

function TForm1.GetValue: string;
begin
  Sleep(1000);
  Result := 'Hello World';
end;

procedure TForm1.ShowValue(s: string);
begin
  ShowMessage(s);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Future<string>(GetValue).BeginInvoke(ShowValue);
end;

end.
