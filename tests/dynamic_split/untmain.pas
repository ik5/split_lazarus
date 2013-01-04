unit untMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons;

type

  { TfrmDynamicEditorSplittingTest }

  TfrmDynamicEditorSplittingTest = class(TForm)
    btnHorizontalSplit: TBitBtn;
    btnVerticalSplit: TBitBtn;
    pnlTop: TPanel;
    synedtMain: TSynEdit;
    procedure btnHorizontalSplitClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
  public
    { public declarations }
    Splitter : TSplitter;
    Editor   : TSynEdit;
    procedure CreateEditor(Vert : Boolean);
    procedure CreateSplitter(Vert : Boolean);
  end;

var
  frmDynamicEditorSplittingTest: TfrmDynamicEditorSplittingTest;

implementation

{$R *.lfm}

{ TfrmDynamicEditorSplittingTest }

procedure TfrmDynamicEditorSplittingTest.btnHorizontalSplitClick(Sender: TObject
  );

begin
 if Assigned(Splitter) then exit;
 if Assigned(Editor) then exit;

 CreateEditor(TComponent(Sender).Tag = 0);
 CreateSplitter(TComponent(Sender).Tag = 0);
 btnHorizontalSplit.Enabled := False;
 btnVerticalSplit.Enabled   := False;
end;

procedure TfrmDynamicEditorSplittingTest.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if Assigned(Splitter) then
    FreeAndNil(Splitter);

  if Assigned(Editor) then
    FreeAndNil(Splitter);
end;

procedure TfrmDynamicEditorSplittingTest.CreateEditor(Vert: Boolean);
begin
  if Assigned(Editor) then exit;

  Editor := TSynEdit.Create(self);
  if Vert then
    Editor.Align := alRight
  else
    Editor.Align  := alBottom;

  Editor.Parent  := self;
  Editor.ShareTextBufferFrom(synedtMain); // magic of shared text buffer from
  Editor.Visible := True;
end;

procedure TfrmDynamicEditorSplittingTest.CreateSplitter(Vert : Boolean);
begin
  if Assigned(Splitter) then exit;
  Splitter               := TSplitter.Create(self);
  Splitter.AutoSnap      := true;
  Splitter.ResizeControl := synedtMain;
  Splitter.Parent        := Self;
  if Vert then
    begin
     Splitter.ResizeAnchor := akRight;
     Splitter.Align        := alRight;
     Splitter.AnchorVerticalCenterTo(Editor);
     Splitter.Width        := 10;
    end
  else
  begin
    Splitter.ResizeAnchor  := akBottom;
    Splitter.Align         := alBottom;
    Splitter.AnchorHorizontalCenterTo(Editor);
    Splitter.Height        := 10;
  end;

  Splitter.Visible := True;
end;

end.

