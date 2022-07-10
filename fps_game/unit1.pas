unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Buttons, ExtCtrls,
  CastleControl, CastlePlayer, X3DNodes, CastleLevels, CastleViewport,
  CastleTransformExtra;

type
  TForm1 = class(TForm)
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
    Viewport: TCastleViewport;
    Player: TPlayer;
    Level: TLevel;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses CastleResources, CastleUIControls,
  CastleVectors, CastleCreatures, CastleTransform;

type
  TGame2DControls = class(TCastleUserInterface)
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
      Player.Inventory[I].Resource.DrawableImage.Draw(X, 0);
      X += 100;
    end;
end;

procedure TForm1.CastleControl1Open(Sender: TObject);
var
  Game2DControls: TGame2DControls;
begin
  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  CastleControl1.Controls.InsertFront(Viewport);

  Resources.LoadFromFiles;
  Levels.LoadFromFiles;

  Player := TPlayer.Create(Application);

  Level := TLevel.Create(Application);
  Level.Viewport := Viewport;
  Level.Player := Player;
  Level.Load('bridge');

  Root := Viewport.Items.MainScene.RootNode;
  Sun := Root.FindNodeByName(TSpotLightNode, 'Sun', true) as TSpotLightNode;

  Game2DControls := TGame2DControls.Create(Application);
  CastleControl1.Controls.InsertFront(Game2DControls);
end;

procedure TForm1.CastleControl1Update(Sender: TObject);
var
  V: TVector3;
begin
  V := Sun.Location;
  V.Z := Sin(Viewport.Items.MainScene.Time) * 10;
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
      if Hit[I].Item is TCastleAlive then
      begin
        (Hit[I].Item as TCastleAlive).Hurt(100, Player.Direction, 1, Player);
        Break;
      end;
    FreeAndNil(Hit);
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Form1.Caption := ApplicationName + Format(' - FPS: %s', [CastleControl1.Fps.ToString]);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  P: TVector3;
  Direction: TVector3;
  CreatureResource: TCreatureResource;
begin
  P := Player.Translation + Player.Direction * 10;
  Direction := Player.Direction; { by default creature is facing back to player }
  CreatureResource := Resources.FindName('Knight') as TCreatureResource;
  { CreateCreature creates TCreature instance and adds it to SceneManager.Items }
  CreatureResource.CreateCreature(Level, P, Direction);
end;

end.

