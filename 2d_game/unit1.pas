unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  CastleControl, CastleViewport, CastleTransform, CastleVectors, CastleKeysMouse,
  CastleScene;

type
  TForm1 = class(TForm)
    CastleControl1: TCastleControl;
    procedure CastleControl1Press(Sender: TObject;
      const Event: TInputPressRelease);
    procedure CastleControl1Update(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Background: TCastleScene;
    Dragon: TCastleScene;
    DragonTransform: TCastleTransform;
    FlyingTarget: TVector2;
    Viewport: TCastleViewport;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses Math,
  CastleFilesUtils, CastleSceneCore, CastleUtils;

procedure TForm1.CastleControl1Update(Sender: TObject);

  function MoveTo(const Start, Target: Single): Single;
  var
    MoveDistance: Single;
  begin
    MoveDistance := 500 * CastleControl1.Fps.SecondsPassed;
    if Start < Target then
      Result := Min(Target, Start + MoveDistance) else
      Result := Max(Target, Start - MoveDistance);
  end;

var
  T: TVector3;
begin
  T := DragonTransform.Translation;
  // T.X := FlyingTarget.X;
  // T.Y := FlyingTarget.Y;
  T.X := MoveTo(T.X, FlyingTarget.X);
  T.Y := MoveTo(T.Y, FlyingTarget.Y);
  DragonTransform.Translation := T;

  if T.X < FlyingTarget.X then
    DragonTransform.Scale := Vector3(-0.2, 0.2, 0.2) else
  if T.X > FlyingTarget.X then
    DragonTransform.Scale := Vector3( 0.2, 0.2, 0.2);
end;

procedure TForm1.CastleControl1Press(Sender: TObject;
  const Event: TInputPressRelease);
begin
  if Event.IsMouseButton(buttonLeft) and
     (Background.PointingDeviceOverItem <> nil) then
    FlyingTarget := Vector2(
      Background.PointingDeviceOverPoint.X,
      Background.PointingDeviceOverPoint.Y);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  //OptimizeExtensiveTransformations := true; // TODO not reliable now in CGE

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Viewport.AutoCamera := true;
  Viewport.Setup2D;
  Viewport.Camera.Orthographic.Height := 700;
  CastleControl1.Controls.InsertFront(Viewport);

  Background := TCastleScene.Create(Application);
  Background.Load('castle-data:/background.x3dv');
  Background.Spatial := [ssRendering, ssDynamicCollisions];
  Background.ProcessEvents := true;
  Viewport.Items.Add(Background);
  Viewport.Items.MainScene := Background;

  DragonTransform := TCastleTransform.Create(Application);
  DragonTransform.Scale := Vector3(0.2, 0.2, 0.2);
  Viewport.Items.Add(DragonTransform);

  Dragon := TCastleScene.Create(Application);
  Dragon.Load('castle-data:/dragon/dragon.json');
  Dragon.ProcessEvents := true;
  Dragon.PlayAnimation('flying', true);
  DragonTransform.Add(Dragon);
end;

end.
