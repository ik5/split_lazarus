unit untLazSplitView_Code;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SrcEditorIntf, ExtCtrls;

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
    procedure FreeList; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure ToggleSplitView (Vert : Boolean = false);             virtual;
    procedure CreateSplitter  (Vert : Boolean; var Tab : TTabInfo); virtual;
    procedure CreateEditor    (Vert:  Boolean; var Tab : TTabInfo); virtual;
  end;

var
  SplitView : TSplitView;

procedure register;

implementation
uses MenuIntf, IDECommands, LCLType, Controls, SynEdit;

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

procedure TSplitView.FreeList;
var i   : integer;
    tab : TTabInfo;
begin
  for i := 0 to FTabList.Count -1 do
    begin
      tab := PTabInfo(FTabList.Items[i])^;

      if Assigned(tab.SplitEditor) then
        begin
          if Assigned(tab.SplitEditor.EditorControl) then
            tab.SplitEditor.EditorControl.Free;

          tab.SplitEditor.Free;
        end;

      if Assigned(tab.Splitter) then
        tab.Splitter.Free;
    end;
end;

constructor TSplitView.Create;
begin
  FTabList := TList.Create;
end;

destructor TSplitView.Destroy;
begin
  FreeList;
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

procedure TSplitView.CreateSplitter(Vert: Boolean; var Tab: TTabInfo);
var Splitter     : TSplitter;
    ActiveEditor : TWinControl;
begin
  Splitter     := Tab.Splitter;
  ActiveEditor := Tab.ActiveEditor.EditorControl;

  if not Assigned(Splitter) then
    Splitter := TSplitter.Create(ActiveEditor.Parent)
  else
    Splitter.Visible := False;

  Splitter.AutoSnap      := true;
  Splitter.ResizeControl := ActiveEditor;
  Splitter.Parent        := ActiveEditor.Parent;

  if Vert then
    begin
     Splitter.ResizeAnchor := akRight;
     Splitter.Align        := alRight;
     Splitter.AnchorVerticalCenterTo(ActiveEditor);
     Splitter.Width        := 5;
    end
  else
  begin
    Splitter.ResizeAnchor  := akBottom;
    Splitter.Align         := alBottom;
    Splitter.AnchorHorizontalCenterTo(ActiveEditor);
    Splitter.Height        := 5;
  end;

  Splitter.Visible := True;
end;

procedure TSplitView.CreateEditor(Vert: Boolean; var Tab: TTabInfo);
var Editor       : TWinControl;
    Parent       : TWinControl;
    ActiveEditor : TWinControl;
begin
  Editor       := Tab.SplitEditor.EditorControl;
  ActiveEditor := Tab.ActiveEditor.EditorControl;
  Parent       := ActiveEditor.Parent;

  if Assigned(Editor) then
    Editor.Visible := False
  else begin
   Editor := TSynEdit.Create(Parent);
  end;

  if Vert then
    Editor.Align := alRight
  else
    Editor.Align  := alBottom;

  Editor.Parent  := Parent;

  // magic of shared text buffer from
  TCustomSynEdit(Editor).ShareTextBufferFrom(TCustomSynEdit(ActiveEditor));

  Editor.Visible := True;
end;

finalization
 if Assigned(SplitView) then
   FreeAndNil(SplitView);
end.

