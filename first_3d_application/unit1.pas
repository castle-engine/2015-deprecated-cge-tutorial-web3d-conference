unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  CastleControl;

type
  TForm1 = class(TForm)
    Button1: TButton;
    CastleControl1: TCastleControl;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses CastleFilesUtils, CastleScene, CastleViewport, CastleCameras;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage('Hooray!');
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Scene: TCastleScene;
  Viewport: TCastleViewport;
begin
  Scene := TCastleScene.Create(Application);
  Scene.Load(ApplicationData('medkit.x3d'));

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Viewport.AutoCamera := true;
  Viewport.InsertFront(TCastleExamineNavigation.Create(Application));
  Viewport.Items.Add(Scene);
  Viewport.Items.MainScene := Scene;

  CastleControl1.Controls.InsertFront(Viewport);
end;

end.

