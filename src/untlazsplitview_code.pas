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
  Classes, SysUtils, SrcEditorIntf, ExtCtrls, SynEdit;

type
  TSplitType = (stNone, stVert, stHorz);

  { TTabInfo }

  TTabInfo = class
  protected
    FActiveEditor : TSourceEditorInterface;
    FSplitEditor  : TCustomSynEdit;
    FSplitter     : TSplitter;
    FSplitType    : TSplitType;
  public
    constructor Create; Virtual;
    destructor Destroy; override;

    procedure FreeSplitter;
    procedure FreeEditor;
  published
    property ActiveEditor : TSourceEditorInterface read FActiveEditor write FActiveEditor;
    property SplitEditor  : TCustomSynEdit         read FSplitEditor  write FSplitEditor;
    property Splitter     : TSplitter              read FSplitter     write FSplitter;
    property SplitType    : TSplitType             read FSplitType    write FSplitType;
  end;

  { TSplitView }

  TSplitView = class(TObject)
  protected
    FEditorList : TList;
    FTabList    : TList;
    procedure FreeList; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure ToggleSplitView (Vert : Boolean = false);         virtual;
    procedure SetSplitter     (Vert : Boolean; Tab : TTabInfo); virtual;
    procedure SetEditor       (Vert:  Boolean; Tab : TTabInfo); virtual;
  end;

var
  SplitView : TSplitView;

procedure register;

implementation
uses MenuIntf, IDECommands, LCLType, LCLProc, Controls, SynEditHighlighter;

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

{ TTabInfo }

constructor TTabInfo.Create;
begin
  FSplitType    := stHorz;
  FSplitter     := nil;
  FSplitEditor  := nil;
  FActiveEditor := SourceEditorManagerIntf.ActiveEditor;
end;

destructor TTabInfo.Destroy;
begin
  FreeSplitter;
  FreeEditor;
  inherited Destroy;
end;

procedure TTabInfo.FreeSplitter;
begin
  if Assigned(FSplitter) then
    begin
      FSplitter.Visible := False;
      FreeAndNil(FSplitter);
    end;
end;

procedure TTabInfo.FreeEditor;
begin
  if Assigned(FSplitEditor) then
    begin
      DebugLn('TTabInfo.FreeEditor - Going to Hide the editor');
      FSplitEditor.Visible     := False;
      DebugLn('TTabInfo.FreeEditor - Going to make Highlighter nil');
      FSplitEditor.Highlighter := nil;
      DebugLn('TTabInfo.FreeEditor - Going to free the editor');
      FreeAndNil(FSplitEditor);
    end;
end;

{ TSplitView }

procedure TSplitView.FreeList;
var i   : integer;
    tab : TTabInfo;
begin
  for i := 0 to FTabList.Count -1 do
    begin
      tab := TTabInfo(FTabList.Items[i]);
      FreeAndNil(tab);
    end;
end;

constructor TSplitView.Create;
begin
  FTabList    := TList.Create;
  FEditorList := TList.Create;
end;

destructor TSplitView.Destroy;
begin
  FreeList;
  FreeAndNil(FTabList);
  FreeAndNil(FEditorList);
  inherited Destroy;
end;

procedure TSplitView.ToggleSplitView(Vert : Boolean);
var ActiveEditor : TSourceEditorInterface;
    index        : integer;
    tab          : TTabInfo;

 procedure CleanResources(tab: TTabInfo);
 begin
   DebugLn('TSplitView.ToggleSplitView -> CleanResources - Going to free SplitEditor (%P) and Splitter (%P)',
           [Pointer(tab.SplitEditor), Pointer(tab.Splitter)]);

   if Assigned(tab.Splitter) then
     begin
      DebugLn('TSplitView.ToggleSplitView -> CleanResources - Going to free tab.Splitter');
      tab.FreeSplitter;
      DebugLn('TSplitView.ToggleSplitView -> CleanResources - Splitter is free');
     end
   else begin
    DebugLn('TSplitView.ToggleSplitView -> CleanResources - tab.Splitter is not allocated');
   end;

   if Assigned(tab.SplitEditor) then
     begin
      DebugLn('TSplitView.ToggleSplitView -> CleanResources - Going to free tab.SplitEditor');
      tab.FreeEditor;
      DebugLn('TSplitView.ToggleSplitView -> CleanResources - SplitEditor is free');
     end
   else begin
     DebugLn('TSplitView.ToggleSplitView -> CleanResources - tab.SplitEditor is not allocated');
   end;

   DebugLn('TSplitView.ToggleSplitView -> CleanResources - Done freeing stuff');
 end;

begin
  ActiveEditor := SourceEditorManagerIntf.ActiveEditor;
  DebugLn('TSplitView.ToggleSplitView -> ActiveEditor (%P)',
          [Pointer(ActiveEditor)]);
  index        := FEditorList.IndexOf(ActiveEditor);
  DebugLn('TSplitView.ToggleSplitView -> Looked for an item index: %d', [index]);

  if index > -1 then
    begin
      DebugLn('TSplitView.ToggleSplitView -> Found the item');

      tab := TTabInfo(FTabList.Items[index]);

      DebugLn('TSplitView.ToggleSplitView -> FTabList.Items[index] (%P)',
      [FTabList.Items[index]]);

      DebugLn('TSplitView.ToggleSplitView -> tab (%P), SplitEditor(%P), Splitter (%P)',
              [Pointer(tab), Pointer(tab.SplitEditor), Pointer(tab.Splitter)]);

      CleanResources(tab);
      case tab.SplitType of // nothing more to do if the item is already the
                             // same as it was. "Toggle" will close it only ...
        stVert : if     Vert then
                  begin
                    Tab.SplitType := stNone;
                   exit;
                  end;
        stHorz : if not Vert then
                   begin
                    Tab.SplitType := stNone;
                    exit;
                   end;
      end;

      if Vert then
        Tab.SplitType := stVert
      else
        Tab.SplitType := stHorz;
    end
  else begin
    DebugLn('TSplitView.ToggleSplitView -> No Item was found');
    tab := TTabInfo.Create;
    tab.ActiveEditor := ActiveEditor;
    tab.SplitEditor  := TSynEdit.Create(tab.ActiveEditor.EditorControl.Parent);
    tab.Splitter     := TSplitter.Create(Tab.ActiveEditor.EditorControl.Parent);
    if Vert then
      tab.SplitType  := stVert
    else
      tab.SplitType  := stHorz;

    index := FEditorList.Add(ActiveEditor);

    FTabList.Insert(index, tab);
    DebugLn('TSplitView.ToggleSplitView -> Added new item index: %d', [index]);
    DebugLn('TSplitView.ToggleSplitView -> tab (%P), SplitEditor(%P), Splitter (%P)',
           [Pointer(tab), Pointer(tab.SplitEditor), Pointer(tab.Splitter)]);
  end;

  SetEditor   (Vert, tab);
  SetSplitter (Vert, tab);
  DebugLn('TSplitView.ToggleSplitView -> done execution');
end;

procedure TSplitView.SetSplitter(Vert: Boolean; Tab: TTabInfo);
begin
  DebugLn('TSplitView.SetSplitter -> Entering: Splitter (%P)',
  [Pointer(Tab.Splitter)]);

  if not Assigned(Tab.Splitter) then
    begin
      Tab.Splitter := TSplitter.Create(Tab.ActiveEditor.EditorControl.Parent);
      DebugLn('TSplitView.SetSplitter -> Created: Splitter (%P)',
             [Pointer(Tab.Splitter)]);
    end
  else begin
    Tab.Splitter.Visible := False;
  end;

  Tab.Splitter.AutoSnap      := true;
  Tab.Splitter.ResizeControl := Tab.ActiveEditor.EditorControl;
  Tab.Splitter.Parent        := Tab.ActiveEditor.EditorControl.Parent;

  if Vert then
    begin
     Tab.Splitter.ResizeAnchor := akRight;
     Tab.Splitter.Align        := alRight;
     Tab.Splitter.AnchorVerticalCenterTo(Tab.ActiveEditor.EditorControl);
     Tab.Splitter.Width        := 5;
    end
  else
  begin
    Tab.Splitter.ResizeAnchor  := akBottom;
    Tab.Splitter.Align         := alBottom;
    Tab.Splitter.AnchorHorizontalCenterTo(Tab.ActiveEditor.EditorControl);
    Tab.Splitter.Height        := 5;
  end;

  Tab.Splitter.Visible := True;
end;

procedure TSplitView.SetEditor(Vert: Boolean; Tab: TTabInfo);
begin
  DebugLn('TSplitView.SetEditor -> Enter: Editor (%P)',
          [Pointer(Tab.SplitEditor)]);

  if not Assigned(Tab.SplitEditor) then
    begin
     Tab.SplitEditor := TSynEdit.Create(Tab.ActiveEditor.EditorControl.Parent);
     DebugLn('TSplitView.SetEditor -> Created: Editor (%P)',
            [Pointer(Tab.SplitEditor)]);
    end
  else begin
    Tab.SplitEditor.Visible := false;
  end;

  if Vert then
    Tab.SplitEditor.Align := alRight
  else
    Tab.SplitEditor.Align  := alBottom;

  Tab.SplitEditor.Parent  := Tab.ActiveEditor.EditorControl.Parent;

  // magic of shared text buffer from
  Tab.SplitEditor.ShareTextBufferFrom(
                                TCustomSynEdit(Tab.ActiveEditor.EditorControl));

  with TSynEdit(Tab.ActiveEditor.EditorControl) do
   begin
     Tab.SplitEditor.Highlighter := Highlighter;
     Tab.SplitEditor.Font.Assign(Font);
   end;
  Tab.SplitEditor.Visible := True;
end;

finalization
 if Assigned(SplitView) then
   FreeAndNil(SplitView);
end.

