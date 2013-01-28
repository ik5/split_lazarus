unit untLazSplitView_Code;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SrcEditorIntf, SynEdit, ExtCtrls;

type
  TSplitType = (stNone, stVert, stHorz);

  TTabInfo = record
    ActiveEditor : TSourceEditorInterface;
    SplitType    : TSplitType;
    SplitEditor  : TSynEdit;
    Splitter     : TSplitter;
  end;



procedure register;

implementation
uses MenuIntf, IDECommands, LCLType;

resourcestring
  txtSplitViewPlugins           = 'Split View';
  txtToggleSplitView            = 'Toggle Split View';
  txtToggleSplitViewDescription = 'Allow the toggle between split view of the active editor';

procedure register;
var
  Key, Key2 : TIDEShortCut;
  Cat       : TIDECommandCategory;
  Cmd       : TIDECommand;
begin
  RegisterIDEMenuCommand(SrcEditMenuSectionPages, 'SplitViewSep', '-');

  Key  := IDEShortCut(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Key2 := Key;
  Cat  := IDECommandList.FindCategoryByName(txtSplitViewPlugins);
  Cmd  := RegisterIDECommand(Cat, txtToggleSplitView,
                             txtToggleSplitViewDescription,
                             Key, Key2, nil, nil{@SplitSelect});

  RegisterIDEMenuCommand(SrcEditMenuSectionPages, 'SplitViewToggle',
    txtToggleSplitView, nil, {@SplitSelect}nil, Cmd);
end;

end.

