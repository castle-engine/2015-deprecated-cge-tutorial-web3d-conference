{$mode objfpc}{$H+}

{ Implements the game logic, independent from mobile / standalone. }
unit Game;

interface

uses CastleWindow;

var
  Window: TCastleWindowCustom;

implementation

uses Classes, SysUtils,
  Castle2DSceneManager, Castle3D, CastleVectors, CastleKeysMouse,
  CastleFilesUtils, CastleSceneCore, CastleUtils;

var
  SceneManager: T2DSceneManager;
  Background: T2DScene;
  Dragon: T2DScene;
  DragonTransform: T3DTransform;
  FlyingTarget: TVector2Single;

procedure WindowUpdate(Sender: TUIContainer);

  function MoveTo(const Start, Target: Single): Single;
  var
    MoveDistance: Single;
  begin
    MoveDistance := 500 * Window.Fps.UpdateSecondsPassed;
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

procedure WindowPress(Sender: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.IsMouseButton(mbLeft) and
     (Background.PointingDeviceOverItem <> nil) then
    FlyingTarget := Vector2Single(
      Background.PointingDeviceOverPoint[0],
      Background.PointingDeviceOverPoint[1]);
end;

procedure ApplicationInitialize;
begin
  { we use TCastleWindowCustom and create SceneManager outselves, to get T2DSceneManager class }
  SceneManager := T2DSceneManager.Create(Application);
  Window.Controls.Add(SceneManager);

  OptimizeExtensiveTransformations := true;

  Background := T2DScene.Create(Application);
  Background.Load(ApplicationData('background.x3dv'));
  Background.Spatial := [ssRendering, ssDynamicCollisions];
  Background.ProcessEvents := true;

  SceneManager.Items.Add(Background);
  SceneManager.MainScene := Background;

  SceneManager.ProjectionAutoSize := false;
  SceneManager.ProjectionHeight := 721;

  DragonTransform := T3DTransform.Create(Application);
  DragonTransform.Scale := Vector3Single(0.2, 0.2, 0.2);
  SceneManager.Items.Add(DragonTransform);

  Dragon := T2DScene.Create(Application);
  Dragon.Load(ApplicationData('dragon/dragon.json'));
  Dragon.ProcessEvents := true;
  Dragon.PlayAnimation('flying', paForceLooping);
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
  Window := TCastleWindowCustom.Create(Application);
  Application.MainWindow := Window;
  Window.OnUpdate := @WindowUpdate;
  Window.OnPress := @WindowPress;
end.
