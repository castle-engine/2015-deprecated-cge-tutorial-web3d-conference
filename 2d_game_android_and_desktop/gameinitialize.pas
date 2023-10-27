{$mode objfpc}{$H+}

{ Implements the game logic, independent from mobile / standalone. }
unit GameInitialize;

interface

uses CastleWindow;

var
  Window: TCastleWindow;

implementation

uses Classes, SysUtils, Math,
  CastleViewport, CastleTransform, CastleVectors, CastleKeysMouse,
  CastleFilesUtils, CastleSceneCore, CastleUtils, CastleScene;

var
  Viewport: TCastleViewport;
  Background: TCastleScene;
  Dragon: TCastleScene;
  DragonTransform: TCastleTransform;
  FlyingTarget: TVector2;

procedure WindowUpdate(Sender: TUIContainer);

  function MoveTo(const Start, Target: Single): Single;
  var
    MoveDistance: Single;
  begin
    MoveDistance := 500 * Window.Fps.SecondsPassed;
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

procedure WindowPress(Sender: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.IsMouseButton(buttonLeft) and
     (Background.PointingDeviceOverItem <> nil) then
    FlyingTarget := Vector2(
      Background.PointingDeviceOverPoint.X,
      Background.PointingDeviceOverPoint.Y);
end;

procedure ApplicationInitialize;
begin
  //OptimizeExtensiveTransformations := true; // TODO not reliable now in CGE

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Viewport.AutoCamera := true;
  Viewport.Setup2D;
  Viewport.Camera.Orthographic.Height := 700;
  Window.Controls.InsertFront(Viewport);

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

function MyGetApplicationName: string;
begin
  Result := 'example_2d_game';
end;

initialization
  { This sets SysUtils.ApplicationName.
    It is useful to make sure it is correct (as early as possible)
    as our log routines use it. }
  OnGetApplicationName := @MyGetApplicationName;

  { initialize Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { create Window and initialize Window callbacks }
  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window;
  Window.OnUpdate := @WindowUpdate;
  Window.OnPress := @WindowPress;
end.
