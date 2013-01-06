unit unt_ik_ide_inf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmIDEInformation }

  TfrmIDEInformation = class(TForm)
    btnGetInfo: TButton;
    mmoInfo: TMemo;
    procedure btnGetInfoClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmIDEInformation: TfrmIDEInformation;

procedure Register;

implementation
uses MenuIntf, IDECommands, SrcEditorIntf;

procedure MenuSelect(Sender: TObject);
begin
  if Assigned(frmIDEInformation) then
   frmIDEInformation.Show
  else begin
    Application.CreateForm(TfrmIDEInformation, frmIDEInformation);
    frmIDEInformation.Show;
  end;
end;

procedure Register;
begin
  RegisterIDEMenuCommand(itmSecondaryTools, 'iktestinfosep', '-');

  RegisterIDEMenuCommand(itmSecondaryTools, 'iktestinfo',
    'IK IDE Testing information', nil, @MenuSelect);

end;

{ TfrmIDEInformation }

procedure TfrmIDEInformation.btnGetInfoClick(Sender: TObject);
var i : integer;
begin
  mmoInfo.Lines.BeginUpdate;
  mmoInfo.Lines.Clear;
  mmoInfo.Lines.Add('Menu name: ' + SourceTabMenuRoot.Name);
  for i := 0 to SourceTabMenuRoot.Count -1 do
    mmoInfo.Lines.Add(#9'First level menu item: ' +SourceTabMenuRoot.Items[i].Name);

  mmoInfo.Lines.Add('Current edited file name: '+ SourceEditorManagerIntf.ActiveEditor.FileName);
  mmoInfo.Lines.EndUpdate;
end;

{$R *.lfm}

finalization
  if Assigned(frmIDEInformation) then
    FreeAndNil(frmIDEInformation);
end.

