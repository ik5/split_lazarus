{

Copyright (c) 2012-2013 Ido Kanner idokan at@at gmail.com

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}
unit untLazSplitView_Code;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SrcEditorIntf, ExtCtrls, Controls;

type
  TSplitType = (stNone, stVert, stHorz);

  PTabInfo = ^TTabInfo;
  TTabInfo = record
    ActiveEditor : TSourceEditorInterface;
    SplitType    : TSplitType;
    SplitEditor  : TWinControl;
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
uses MenuIntf, IDECommands, LCLType, SynEdit;

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
          tab.SplitEditor.Free;

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

 procedure Hide;
 begin
   tab.SplitEditor.Visible := false;
   tab.SplitEditor.Free;
   tab.Splitter.Visible    := false;
   tab.Splitter.Free;
 end;

begin
  ActiveEditor := SourceEditorManagerIntf.ActiveEditor;
  index        := FTabList.IndexOf(ActiveEditor);
  if index > -1 then
    begin
      tab := PTabInfo(FTabList.Items[index])^;
      Hide;
		  case tab.SplitType of // nothing more to do if the item is already the
                            // same as it was. "Toggle" will close it only ...
		    stVert : if     Vert then exit;
        stHorz : if not Vert then exit;
      end;
    end
  else begin
    tab.ActiveEditor := ActiveEditor;
    tab.SplitEditor  := Nil;
    tab.Splitter     := Nil;
    if Vert then
      tab.SplitType  := stVert
    else
      tab.SplitType  := stHorz;

    index := FTabList.Add(@tab);
  end;

  CreateEditor   (Vert, tab);
  CreateSplitter (Vert, tab);
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
  Editor       := Tab.SplitEditor;
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

