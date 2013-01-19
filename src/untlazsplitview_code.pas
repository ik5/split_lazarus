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

procedure register;
begin

end;

end.

