unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  CastleControl, Castle2DSceneManager, Castle3D, CastleVectors, CastleKeysMouse;

type
  TForm1 = class(TForm)
    Castle2DControl1: TCastle2DControl;
    procedure Castle2DControl1Press(Sender: TObject;
      const Event: TInputPressRelease);
    procedure Castle2DControl1Update(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Background: T2DScene;
    Dragon: T2DScene;
    DragonTransform: T3DTransform;
    FlyingTarget: TVector2Single;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses CastleFilesUtils, CastleSceneCore, CastleUtils;

procedure TForm1.Castle2DControl1Update(Sender: TObject);

  function MoveTo(const Start, Target: Single): Single;
  var
    MoveDistance: Single;
  begin
    MoveDistance := 500 * Castle2DControl1.Fps.UpdateSecondsPassed;
    if Start < Target then
      Result := Min(Target, Start + MoveDistance) else
      Result := Max(Target, Start - MoveDistance);
  end;

var
  T: TVector3Single;
begin
  T := DragonTransform.Translation;
  // T[0] := FlyingTarget[0];
  // T[1] := FlyingTarget[1];
  T[0] := MoveTo(T[0], FlyingTarget[0]);
  T[1] := MoveTo(T[1], FlyingTarget[1]);
  DragonTransform.Translation := T;

  if T[0] < FlyingTarget[0] then
    DragonTransform.Scale := Vector3Single(-0.2, 0.2, 0.2) else
  if T[0] > FlyingTarget[0] then
    DragonTransform.Scale := Vector3Single( 0.2, 0.2, 0.2);
end;

procedure TForm1.Castle2DControl1Press(Sender: TObject;
  const Event: TInputPressRelease);
begin
  if Event.IsMouseButton(mbLeft) and
     (Background.PointingDeviceOverItem <> nil) then
    FlyingTarget := Vector2Single(
      Background.PointingDeviceOverPoint[0],
      Background.PointingDeviceOverPoint[1]);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Background := T2DScene.Create(Application);
  Background.Load(ApplicationData('background.x3dv'));
  Background.Spatial := [ssRendering, ssDynamicCollisions];
  Background.ProcessEvents := true;

  Castle2DControl1.SceneManager.Items.Add(Background);
  Castle2DControl1.SceneManager.MainScene := Background;

  Castle2DControl1.SceneManager.ProjectionAutoSize := false;
  Castle2DControl1.SceneManager.ProjectionHeight := 721;

  DragonTransform := T3DTransform.Create(Application);
  DragonTransform.Scale := Vector3Single(0.2, 0.2, 0.2);
  Castle2DControl1.SceneManager.Items.Add(DragonTransform);

  Dragon := T2DScene.Create(Application);
  Dragon.Load(ApplicationData('dragon/dragon.json'));
  Dragon.ProcessEvents := true;
  Dragon.PlayAnimation('flying', paForceLooping);
  DragonTransform.Add(Dragon);
end;

end.

