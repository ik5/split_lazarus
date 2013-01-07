unit unt_ik_ide_inf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls;

type

  { TfrmIDEInformation }

  TfrmIDEInformation = class(TForm)
    btnGetInfo: TButton;
    mmoInfo: TMemo;
    procedure btnGetInfoClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmIDEInformation: TfrmIDEInformation;

procedure Register;

implementation
uses MenuIntf, IDECommands, SrcEditorIntf, LCLType;

resourcestring
  txtCaption = 'Test IDE Info';

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
var
  Key, Key2 : TIDEShortCut;
  Cat       : TIDECommandCategory;
  Cmd       : TIDECommand;
begin
  RegisterIDEMenuCommand(itmSecondaryTools, 'iktestinfosep', '-');

  Key  := IDEShortCut(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Key2 := Key;
  Cat  := IDECommandList.FindCategoryByName(CommandCategoryToolMenuName);
  Cmd  := RegisterIDECommand(Cat, txtCaption,
          'A test tool for building plugins for Lazarus',
          Key, Key2, nil, @MenuSelect);

  RegisterIDEMenuCommand(itmSecondaryTools, 'iktestinfo',
    txtCaption, nil, @MenuSelect, Cmd);
end;

{ TfrmIDEInformation }

procedure TfrmIDEInformation.btnGetInfoClick(Sender: TObject);
var i : integer;
begin
  mmoInfo.Lines.BeginUpdate;
  mmoInfo.Lines.Clear;
  mmoInfo.Lines.Add('Menu name: ' + SrcEditMenuSectionPages.Name);
  for i := 0 to SrcEditMenuSectionPages.Count -1 do
    mmoInfo.Lines.Add(#9'First level menu item: ' +SrcEditMenuSectionPages.Items[i].Name);

  mmoInfo.Lines.Add('Current edited file name: '+ SourceEditorManagerIntf.ActiveEditor.FileName);
  mmoInfo.Lines.EndUpdate;
end;

procedure TfrmIDEInformation.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

{$R *.lfm}

finalization
  if Assigned(frmIDEInformation) then
    FreeAndNil(frmIDEInformation);
end.

