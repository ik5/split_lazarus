unit unt_ik_ide_inf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation
uses MenuIntf, IDECommands, SrcEditorIntf;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var i : integer;
begin
  Memo1.Lines.BeginUpdate;
  Memo1.Lines.Clear;
  Memo1.Lines.Add('Menu name: ' + SourceTabMenuRoot.Name);
  for i := 0 to SourceTabMenuRoot.Count -1 do
    Memo1.Lines.Add(#9'First level menu item: ' +SourceTabMenuRoot.Items[i].Name);

  Memo1.Lines.Add('Current edited file name: '+ SourceEditorManagerIntf.ActiveEditor.FileName);
  Memo1.Lines.EndUpdate;
end;

{$R *.lfm}

end.

