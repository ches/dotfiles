/*  This file downloaded from Highend3d.com
''  
''  Highend3d.com File Information:
''  
''    Script Name: FractalNoise v2.5
''    Author: Valentin A. Koudriavtsev
''    Last Updated: October 8, 2002
''    Update/Change this file at:
''    http://www.highend3d.com/shake/macros/?section=image#1943
''  
''  Please do not alter any information above this line
''  it is generated dynamically by Highend3d.com and will
''  be changed automatically on any updates.
*/

//
//  FractalNoise v2.5
//  Writen by Valentin A. Koudriavtsev. Studio Klassika, Moscow,  8 Oct 2002.
//  This script make fractal noise images with convenient dynamics control without artifacts at big iteration values.
//  In v2 dramatically reduced rendering speed and added ability of making colored noise.
//  In v2.5 added Color Shift value in colored mode.

image FractalNoise(
int bytes=defaultBytes,
string Quality="Normal",
string Type="noise3d",
int Colored=0,
float red=1,
float green=1,
float blue=1,
float ColorShift=0.2,
float Width=defaultWidth,
float Height=defaultHeight,
float xSize=10,
float ySize=xSize,
float xIteration=3,
float yIteration=xIteration,
float Speed=1,
float Phase=time

)
{
int Div=5;
if (Quality == "Best")   {Div = 1;}
if (Quality == "High")   {Div = 2.5;}
if (Quality == "Normal") {Div = 5;}
if (Quality == "Low")    {Div = 10;}
if (Quality == "Poor")   {Div = 15;}
         if (Colored==0) {
	    Color1 = Color(Width/Div, Height/Div, bytes, 0, red, red, 1, 0);
		if (Type=="fnoise3d"){
		ColorX1 = ColorX(Color1, fnoise3d(x/xSize*Div+9999,y/ySize*Div+9999,Phase*Speed/10,xIteration,yIteration,yIteration), 0, 0, 0, z);}
		if (Type=="noise3d"){
		ColorX1 = ColorX(Color1, noise3d(x/xSize*Div+9999,y/ySize*Div+9999,Phase*Speed/10), 0, 0, 0, z);}
		if (Type=="turbulence3d"){
		ColorX1 = ColorX(Color1, turbulence3d(x/xSize*Div+9999,y/ySize*Div+9999,Phase*Speed/10,xIteration,yIteration,yIteration), 0, 0, 0, z);}
		Reorder1 = Reorder(ColorX1, "rrrr");
		         }
         if (Colored==1) {
	    Color1 = Color(Width/Div, Height/Div, bytes, 0, red, red, 1, 0);
		if (Type=="fnoise3d"){
		ColorX1 = ColorX(Color1, fnoise3d(x/xSize*Div+9999,y/ySize*Div+9999,Phase*Speed/10,xIteration,yIteration,yIteration), fnoise3d(x/xSize*Div+9999+ColorShift,y/ySize*Div+9999+ColorShift,Phase*Speed/10,xIteration,yIteration,yIteration), fnoise3d(x/xSize*Div+9999+ColorShift*2,y/ySize*Div+9999+ColorShift*2,Phase*Speed/10,xIteration,yIteration,yIteration), 0, z);}
		if (Type=="noise3d"){
		ColorX1 = ColorX(Color1, noise3d(x/xSize*Div+9999,y/ySize*Div+9999,Phase*Speed/10), noise3d(x/xSize*Div+9999+ColorShift,y/ySize*Div+9999+ColorShift,Phase*Speed/10), noise3d(x/xSize*Div+9999+ColorShift*2,y/ySize*Div+9999+ColorShift*2,Phase*Speed/10), 0, z);}
		if (Type=="turbulence3d"){
		ColorX1 = ColorX(Color1, turbulence3d(x/xSize*Div+9999,y/ySize*Div+9999,Phase*Speed/10,xIteration,yIteration,yIteration), turbulence3d(x/xSize*Div+9999+ColorShift,y/ySize*Div+9999+ColorShift,Phase*Speed/10,xIteration,yIteration,yIteration), turbulence3d(x/xSize*Div+9999+ColorShift*2,y/ySize*Div+9999+ColorShift*2,Phase*Speed/10,xIteration,yIteration,yIteration), 0, z);}
		Reorder1 = Reorder(ColorX1, "rgbl");
		         }                 
	Resize1 = Resize(Reorder1, Width, Height, "gauss", 0);
        Mult1 = Mult(Resize1, red, green, blue, 1, 1);
    return Mult1;
}