image SliceTool

(
image In1=0,
int colorChannel=0,
int backgroundType=0,
int backgroundColor=0,
int graphSoftness=20,
int graphPosition=0,
int sliceColor=0,
int lineThickness=2,
string point1Name="A",
string point2Name="B",
float point1X=0,
float point1Y=0,
float point2X=In1.width,
float point2Y=In1.height
)

{
Text1 = Text(720, 486, 1, point1Name, "Utopia Regular", round(min(In1.width, In1.height)/10), xFontScale, 1, 0, 0, 0, (point1X<borderL || (point1X>point2X && point1X<borderR))?1:3, (point1Y<borderB || (point1Y>point2Y && point1Y<borderT))?1:3, (sliceColor==2 || sliceColor==3)?0:1, (sliceColor==1 || sliceColor==3)?0:1, (sliceColor==1 || sliceColor==2)?0:1, 1, 0, 0, 0, 45, 0, 1, int borderL=round(xFontScale/1.5), int borderB=xFontScale, int borderR=In1.width-round(xFontScale/1.5), int borderT=In1.height-xFontScale);
Text2 = Text(720, 486, 1, point2Name, "Utopia Regular", Text1.xFontScale, xFontScale, 1, 0, 0, 0, (point2X>Text1.borderR || (point2X<point1X && point2X>Text1.borderL))?3:1, (point2Y>Text1.borderT || (point2Y<point1Y && point2Y>Text1.borderB))?3:1, Text1.red, Text1.green, Text1.blue, 1, 0, 0, 0, 45, 0, 1);
Pan1 = Pan(Text1, point1X, point1Y, 0, 0.5, 0);
Pan2 = Pan(Text2, point2X, point2Y, 0, 0.5, 0);
IAdd1 = IAdd(Pan1, Pan2, 0, 100);
Color = Color(distance(point1X,point1Y,point2X,point2Y), lineThickness, 1, (sliceColor==2 || sliceColor==3)?0:1, (sliceColor==1 || sliceColor==3)?0:1, (sliceColor==1 || sliceColor==2)?0:1, 1, 0);
Move2D1 = Move2D(Color, point1X, point1Y, atan2d(point2Y-point1Y, point2X-point1X), 1, 1, xScale, 0, 0, 0, 1, "default", "default", "trsx", 0, 0, 0.5, 0, 0, time);
IAdd2 = IAdd(IAdd1, Move2D1, 0, 100);
Xor = Xor(IAdd2, In1, 1, 0);
Over1 = Over(IAdd2, In1, 1, 100);
Select1 = Select(sliceColor==0?1:2, Xor, Over1, 0, 0);
Crop1 = Crop(Select1, 0, 0, width, height);
Viewport1 = Viewport(Crop1, 0, graphPosition==3?-1:0, graphPosition==0?In1.width*2:In1.width, graphPosition==1?In1.height*2:In1.height);
Reorder1 = Reorder(In1, colorChannel==0?"rgbn":colorChannel==1?"rrrn":colorChannel==2?"gggn":"bbbn");
Move2D2 = Move2D(Reorder1, -point1X, -point1Y, -atan2d(point2Y-point1Y, point2X-point1X), 1, 1, xScale, 0, 0, point1X, point1Y, "default", "default", "trsx", 0, 0, 0.5, 0, 0, time);
Viewport2 = Viewport(Move2D2, 1, 0, distance(point1X, point1Y, point2X, point2Y)-1, 1);
Blur = Blur (Viewport2, graphSoftness, xPixels, 0, "default", "default", "rgba");
PlotScanline = PlotScanline(Blur, In1.width, In1.height, 0);
Reorder2 = Reorder(PlotScanline, colorChannel==0?"rgbn":colorChannel==1?"rnnn":colorChannel==2?"ngnn":"nnbn");
LumaKey = LumaKey(Reorder2, 0, 0, 0, 0, 0);
ColorReplace = ColorReplace(LumaKey, 0, 1, rSource, rSource, (backgroundColor==1 && backgroundType!=0)?0:1, rReplace, rReplace, 0.1, 0.1, 0.1, 0.1, 1, 1);
DilateErode = DilateErode(ColorReplace, "rgba", lineThickness-1, xPixels, 0, 1, 0);
Resize = Resize(Blur, In1.width, graphPosition==3?1:In1.height, "default", 1);
Over2 = Over(DilateErode, Resize, 1, 0, 0);
Text3 = Text(In1.width, In1.height, 1, "0.9\n\n0.8\n\n0.7\n\n0.6\n\n0.5\n\n0.4\n\n0.3\n\n0.2\n\n0.1", "Utopia Regular", 0.05*height, xFontScale, 1, 0, 0.505*height, 0, 1, 2, 1, 1, 1, 1, 0, 0, 0, 45, 0, 1);
Pan3 = Pan(Text3, 1, -1, 0, 0.5, 0);
Brightness = Brightness(Pan3, 0.5);
IAdd3 = IAdd(Text3, Brightness, 1, 100);
Checker = Checker(In1.width, In1.height+lineThickness, 1, round(In1.height/10), xSize);
Pan4 = Pan(Checker, 0, lineThickness, 0, 0.5, 0);
ISubA = ISubA(Checker, Pan4, 1, 100);
Crop2 = Crop(ISubA, 0, lineThickness, width, height);
Over3 = Over(IAdd3, Crop2, 1, 0, 0);
Invert1 = Invert(Over3, backgroundColor==1?"rgba":"nnnn");
Over4 = Over(DilateErode, Invert1, 1, 0, 0);
Invert2 = Invert(Reorder2, (backgroundColor==1 && graphPosition!=2)?"rgba":"nnnn");
Over5 = Over(DilateErode, Invert2, 1, 0, 0);
Select2 = Select((backgroundType==0 && (graphPosition==0 || graphPosition==1))?1: (backgroundType==1 && (graphPosition==0 || graphPosition==1))?2: graphPosition==3?4: 3, Over2, Over4, Over5, Resize);
Crop3 = Crop(Select2, 0, 0, width, height);
Pan5 = Pan(Crop3, graphPosition==0?In1.width:0, graphPosition==1?In1.height:0, 0, 0.5, 0);
Over6 = Over(Pan5, Viewport1, 1, 0, 0);
Copy = Copy(Over6, In1, 0, "a");
IAdd4 = IAdd(Pan5, Viewport1, 1, 100);
return Copy;
}