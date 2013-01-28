unit untLazSplitView_Code;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SrcEditorIntf, SynEdit, ExtCtrls;

type
  TSplitType = (stNone, stVert, stHorz);

  PTabInfo = ^TTabInfo;
  TTabInfo = record
    ActiveEditor : TSourceEditorInterface;
    SplitType    : TSplitType;
    SplitEditor  : TSourceEditorInterface;
    Splitter     : TSplitter;
  end;

  { TSplitView }

  TSplitView = class(TObject)
  protected
    FTabList : TList;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure ToggleSplitView(Vert : Boolean = false); virtual;
  end;

var
  SplitView : TSplitView;

procedure register;

implementation
uses MenuIntf, IDECommands, LCLType;

resourcestring
  txtSplitViewPlugins        = 'Split View';
  txtToggleSplitViewHorz     = 'Toggle Split View Horizontal';
  txtToggleSplitViewVert     = 'Toggle Split View Vertical';
  txtToggleSplitViewHorzDesc = 'Toggle Split View Horizontal';
  txtToggleSplitViewVertDesc = 'Toggle Split View Vertical';

procedure SplitSelectHorz(Sender : TObject);
begin
  if not Assigned(SplitView) then
    SplitView := TSplitView.Create;

  SplitView.ToggleSplitView;
end;

procedure SplitSelectVert(Sender : TObject);
begin
  if not Assigned(SplitView) then
    SplitView := TSplitView.Create;

  SplitView.ToggleSplitView(true);
end;

procedure register;
var
  Key, Key2 : TIDEShortCut;
  Cat       : TIDECommandCategory;
  Cmd, Cmd2 : TIDECommand;
begin
  Key  := IDEShortCut(VK_UNKNOWN,[],VK_UNKNOWN,[]); // by default no shortcuts
  Key2 := Key;

  // Look for the shortcut category
  Cat  := IDECommandList.FindCategoryByName(txtSplitViewPlugins);

  // Check if it was found
  if Cat = nil then // if not, then register new category
    Cat := IDECommandList.CreateCategory(nil, 'SplitViewPlugins',
                                         txtSplitViewPlugins);

  // Register the shortcut
  Cmd  := RegisterIDECommand(Cat, txtToggleSplitViewHorz,
                             txtToggleSplitViewHorzDesc,
                             Key, Key2, nil, @SplitSelectHorz);

  Cmd2 := RegisterIDECommand(Cat, txtToggleSplitViewVert,
                             txtToggleSplitViewVertDesc,
                             Key, Key2, nil, @SplitSelectVert);

  // Register a sperator prior to registering the menu item
  RegisterIDEMenuCommand(SrcEditMenuSectionPages, 'SplitViewSep', '-');

  // Register a menu item
  RegisterIDEMenuCommand(SrcEditMenuSectionPages, 'SplitViewToggleHorz',
    txtToggleSplitViewHorz, nil, @SplitSelectHorz, Cmd);

  RegisterIDEMenuCommand(SrcEditMenuSectionPages, 'SplitViewToggleVert',
    txtToggleSplitViewVert, nil, @SplitSelectVert, Cmd2);
end;

{ TSplitView }

constructor TSplitView.Create;
begin
  FTabList := TList.Create;
end;

destructor TSplitView.Destroy;
var i   : integer;
    tab : TTabInfo;
begin
  for i := 0 to FTabList.Count -1 do
    begin
      tab := PTabInfo(FTabList.Items[i])^;

      if Assigned(tab.SplitEditor) then
        tab.SplitEditor.Free;

      if Assigned(tab.Splitter) then
        tab.Splitter.Free;
    end;

  FreeAndNil(FTabList);
  inherited Destroy;
end;

procedure TSplitView.ToggleSplitView(Vert : Boolean);
var ActiveEditor : TSourceEditorInterface;
    index        : integer;
    tab          : TTabInfo;
begin
  ActiveEditor := SourceEditorManagerIntf.ActiveEditor;
  index        := FTabList.IndexOf(ActiveEditor);
  //if index > -1 then
  //  FTabList.Items[index];
end;

finalization
 if Assigned(SplitView) then
   FreeAndNil(SplitView);
end.

