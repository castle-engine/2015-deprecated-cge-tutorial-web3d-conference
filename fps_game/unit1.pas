unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Buttons, ComCtrls, ExtCtrls, CastleControl, CastlePlayer, X3DNodes;

type
  TForm1 = class(TForm)
    Player: TPlayer;
    CastleControl1: TCastleControl;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure CastleControl1Open(Sender: TObject);
    procedure CastleControl1Update(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    Root: TX3DRootNode;
    Sun: TSpotLightNode;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses CastleLevels, CastleResources, CastleUIControls,
  CastleVectors, CastleCreatures, Castle3D, CastleSceneCore, Math;

type
  TGame2DControls = class(TUIControl)
  public
    procedure Render; override;
  end;

procedure TGame2DControls.Render;
var
  Player: TPlayer;
  I, J: Integer;
  X: Integer;
begin
  Player := Form1.Player;
  X := 0;
  for I := 0 to Player.Inventory.Count - 1 do
    for J := 0 to Player.Inventory[I].Quantity - 1 do
    begin
      Player.Inventory[I].Resource.GLImage.Draw(X, 0);
      X += 100;
    end;
end;

procedure TForm1.CastleControl1Open(Sender: TObject);
var
  Game2DControls: TGame2DControls;
begin
  Player := TPlayer.Create(CastleControl1.SceneManager);
  CastleControl1.SceneManager.Items.Add(Player);
  CastleControl1.SceneManager.Player := Player;

  Resources.LoadFromFiles;
  Levels.LoadFromFiles;
  CastleControl1.SceneManager.LoadLevel('bridge');

  Root := CastleControl1.SceneManager.MainScene.RootNode;
  Sun := Root.FindNodeByName(TSpotLightNode, 'Sun', true) as TSpotLightNode;

  Game2DControls := TGame2DControls.Create(Application);
  CastleControl1.Controls.InsertFront(Game2DControls);
end;

procedure TForm1.CastleControl1Update(Sender: TObject);
var
  V: TVector3Single;
begin
  V := Sun.Location;
  V[2] := Sin(CastleControl1.SceneManager.MainScene.Time.Seconds) * 10;
  Sun.Location := V;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
var
  I: Integer;
  Hit: TRayCollision;
begin
  Hit := Player.Ray(Player.Middle, Player.Direction);
  if Hit <> nil then
  begin
    for I := 0 to Hit.Count - 1 do
      if Hit[I].Item is T3DAlive then
      begin
        (Hit[I].Item as T3DAlive).Hurt(100, Player.Direction, 1, Player);
        Break;
      end;
    FreeAndNil(Hit);
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Form1.Caption := ApplicationName + Format(' - FPS: %f', [CastleControl1.Fps.RealTime]);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  P: TVector3Single;
  Direction: TVector3Single;
  CreatureResource: TCreatureResource;
begin
  P := Player.Position + Player.Direction * 10;
  Direction := Player.Direction; { by default creature is facing back to player }
  CreatureResource := Resources.FindName('Knight') as TCreatureResource;
  { CreateCreature creates TCreature instance and adds it to SceneManager.Items }
  CreatureResource.CreateCreature(CastleControl1.SceneManager.Items, P, Direction);
end;

end.

