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

uses CastleFilesUtils, CastleScene;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage('Hooray!');
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Scene: TCastleScene;
begin
  Scene := TCastleScene.Create(Application);
  Scene.Load(ApplicationData('medkit.x3d'));

  CastleControl1.SceneManager.Items.Add(Scene);
  CastleControl1.SceneManager.MainScene := Scene;
end;

end.

