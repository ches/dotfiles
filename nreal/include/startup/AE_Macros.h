//----------------------------------
// Collection of Shake Macros
// to mimic AE effects
//
// (c) Francois Sugny 2004
// Email : francois.sugny@wanadoo.fr
//----------------------------------

float sqr(float a)
{
	return a*a;
}

extern "nrfx.dll" image NGLRender(
int		width,
int		height,
int		bytes,
const char	*expr
);

// -- Void Noide
// Used by the AE Exporter Plugin
image 	AE_VoidNode(image Input)
{
	return Input;
}

// -- SuperFileIn
// Use by the AE Exporter Plugin
image	SuperFileIn(
	char * 	Filename,
	int	Premult,
	int	OutResX,
	int	OutResY,
	int	NbFrames,
	int	InPoint,
	int	OutPoint,
	float	Stretch)
{
	FileIn1	= SFileIn(	Filename,
				"auto",
				1, 0,
				"v1.0",
				"0");
	IRetime(FileIn1,0,InPoint,OutPoint,"Freeze","Freeze");
	FIBlend(FileIn1,NbFrames + Stretch*(time-InPoint),InPoint,OutPoint,2,1,1,2,0);
	ColorX1 = ColorX(FileIn1,r*a,g*a,b*a,a,z);
	Viewport1 = Viewport(ColorX1,0,OutResX,OutResY);
	return Viewport1;	
}

// Arc Macro from Emmanuel Montgennet
// included for compatibility
image AE_Arc(
int	width=		GetDefaultWidth(),
int	height=		GetDefaultHeight(),
float	red=		1.0f,
float	green=		1.0f,
float	blue=		1.0f,
float	angle=		0,
float	arc=		180,
float	radius=		min(width,height)/2-thickness,
float	thickness=	50,
float	xCenter=	width/2,
float	yCenter=	height/2,
int	nPoints=	100)
{
    return NGLRender(
	width,
	height,
	1,
	"
	    nglPushMatrix();
		nglTranslatef(-xCenter,-yCenter,0.0f);
		nglRotatef(angle,0.0f,0.0f,1.0f);
		nglTranslatef(xCenter,yCenter,0.0f);

		nglColor3f(red,green,blue);
		nglBegin(NGL_POLYGON);
		    for(int i=0;i<=nPoints;++i)
		    {
			float alpha= (arc*i)/nPoints;
			float cosAlpha= cosd(alpha);
			float sinAlpha= sind(alpha);
			nglVertex2f(
			    xCenter + radius*cosAlpha,
			    yCenter + radius*sinAlpha
			);
		    }
		    for(int j=nPoints;j>=0;--j)
		    {
			float alpha= (arc*j)/nPoints;
			float cosAlpha= cosd(alpha);
			float sinAlpha= sind(alpha);
			nglVertex2f(
			    xCenter + (radius+thickness)*cosAlpha,
			    yCenter + (radius+thickness)*sinAlpha
			);
		    }
		nglEnd();
	    nglPopMatrix();
	"
    );
}
image AE_Arc2(
int	width=		GetDefaultWidth(),
int	height=		GetDefaultHeight(),
float	red=		1.0f,
float	green=		1.0f,
float	blue=		1.0f,
float	angle=		0,
float	arc=		180,
float	radius=		min(width,height)/2-thickness,
float	thickness=	50,
float	xCenter=	width/2,
float	yCenter=	height/2,
int	nPoints=	100
)
{
    return NGLRender(
	width,
	height,
	1,
	"
	    nglPushMatrix();
		nglTranslatef(-xCenter,-yCenter,0.0f);
		nglRotatef(angle,0.0f,0.0f,1.0f);
		nglTranslatef(xCenter,yCenter,0.0f);

		nglColor3f(red,green,blue);
		nglBegin(NGL_POLYGON);
		    for(int i=0;i<=nPoints;++i)
		    {
			float alpha= (arc*i)/nPoints;
			float cosAlpha= cosd(alpha);
			float sinAlpha= sind(alpha);
			nglVertex2f(
			    xCenter + radius*cosAlpha,
			    yCenter + radius*sinAlpha
			);
		    }
		    for(int j=nPoints;j>=0;--j)
		    {
			float alpha= (arc*j)/nPoints;
			float cosAlpha= cosd(alpha);
			float sinAlpha= sind(alpha);
			nglVertex2f(
			    xCenter + (radius+thickness*j/nPoints)*cosAlpha,
			    yCenter + (radius+thickness*j/nPoints)*sinAlpha
			);
		    }
		nglEnd();
	    nglPopMatrix();
	"
    );
}

image AE_ForceRGB(
	image background)
{
    Reorder1 = Reorder(background, "rgab");
    Reorder2 = Reorder(Reorder1, "rgab");
    SwitchMatte1 = SwitchMatte(Reorder2, background, 1, "A", 0, 0);
    
    return SwitchMatte1;
}

//--
image AE_Mirror(
		image 	BackGround,
		int	CenterX,
		int	CenterY,
		float	Angle)
{
	ColorX1 = ColorX( 0,
			  (Angle%360)<=180?
			  	((y>tand(90-Angle)*x+CenterY-CenterX*tand(90-Angle))?1:0):
			  	((y<tand(90-Angle)*x+CenterY-CenterX*tand(90-Angle))?1:0),
			  0,0,0,
			  0);
	Reorder1 = Reorder(ColorX1,"rrrr");
	Blur1 = Blur(Reorder1,10,10);
	IMult1 = IMult(Blur1,BackGround);
	Move2D1 = Move2D(IMult1,0,0,-2*Angle,1,-1,1,0,0,CenterX,CenterY);
	Over1 = Over(IMult1,Move2D1);
	return Over1;
}

//--
image AE_CornerPin(
		image 	Background,
		float	x0,
		float	y0,
		float	x1,
		float	y1,
		float	x2,
		float	y2,
		float	x3,
		float	y3)
{
	C1 = CornerPin(Background,x0,y0,x1,y1,x2,y2,x3,y3);
	return C1;
}

image AE_Bulge(
		image	Background,
		float	xRadius,
		float	yRadius,
		float	PosX,
		float	PosY,
		float	lensStrength)
{
	Bytes1 = Bytes(0, 4);
	ColorX2 = ColorX(0,
				1.0-distance(x,y,PosX,PosY)/100.0,
				1.0-distance(x,y,PosX,PosY)/100.0,
				b, a, z);
	Scale1 = Scale(ColorX2,xRadius/100.0,yRadius/100.0,PosX,PosY);
	ColorX1 = ColorX(Bytes1,
				0.33*lensStrength*(x-PosX)*exp(-distance(x,y,PosX,PosY)*distance(x,y,PosX,PosY)/(width*width))/width,
    				0.33*lensStrength*(y-PosY)*exp(-distance(x,y,PosX,PosY)*distance(x,y,PosX,PosY)/(width*width))/height,
    				b, a, z);
	IMult1 = IMult(ColorX1, Scale1, 1, 100, 0);
	IDisplace1 = IDisplace(Background, IMult1, width, xScale, 0, xDOffset, "R", "G", 0, xDelta);
	
	return IDisplace1;
}
		
//--
image AE_Twirl(
		image 	Background,
		float	Angle,
		float	xCenter,
		float	yCenter,
		float	radius)
{
	RGrad1 = RGrad(width, height, 1, xCenter, yCenter, 1, radius, 
    			radius/5, 0.5, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0);
	Twirl1 = Twirl(Background, Angle, 0, 1, xCenter, yCenter, radius, radius, 1, 0);
	Mask(Twirl1, RGrad1, "A", 100, 0, 1);
	
	return Twirl1;
}

//--
image AE_Scroll(
		image	Background,
		float	xPan,
		float	yPan,
		float	percentOfOriginal)
{
	Scroll1 = Scroll(Background, xPan, yPan, 0, 0.5, 0);
	Mix2 = Mix(Scroll1, Background, 1, percentOfOriginal, "rgba");
	
	return Mix2;
}

//--
image AE_ProgressiveWave(
		image	Background,
		float	Amplitude,
		float	Period,
		float	axisAngle,
		float	phase,
		string	mode,
		float	speed)
{
	if (mode == "Sinus")
	{
		ColorX1 = ColorX(0, 0.5-0.5*sin(M_PI/2+phase+2*M_PI*(-time*speed+sind(axisAngle)*x+cosd(axisAngle)*y)/(2*Period)), 0, 0, 1, z);
		IDisplace1 = IDisplace(Background, ColorX1, Amplitude*cosd(axisAngle), Amplitude*sind(axisAngle), 0.5, 0.5, "R", "R", 0, 0);
	}
	else if (mode == "Square")
	{
		ColorX1 = ColorX(0, (sin(2*M_PI*(cosd(axisAngle)*x+sind(axisAngle)*y-time*speed)/(2*Period)))>0?1:0, 0, 0, a, z);
		IDisplace1 = IDisplace(Background, ColorX1, Amplitude*cosd(90-axisAngle), Amplitude*sind(90-axisAngle), 0.5, 0.5, "R", "R", 0, 0);
	}
	else if (mode == "Triangular")
	{
		ColorX1 = ColorX(0, (abs(-time*speed+cosd(axisAngle)*x+sind(axisAngle)*y))%(2*Period)>Period?((abs(-time*speed+cosd(axisAngle)*x+sind(axisAngle)*y))%(2*Period))/(2*Period):1-((abs(-time*speed+cosd(axisAngle)*x+sind(axisAngle)*y))%(2*Period))/(2*Period),  0, 0, a, z);
		IDisplace1 = IDisplace(Background, ColorX1, Amplitude*cosd(90-axisAngle), Amplitude*sind(90-axisAngle), 0.75, 0.75, "R", "R", 0, 0);
	}
	else if (mode == "Half-Triangular")
	{
		ColorX1 = ColorX(0, 1.0-((abs(-time*speed+cosd(axisAngle)*x+sind(axisAngle)*y))%(2*Period))/(2*Period),  0, 0, a, z);
		IDisplace1 = IDisplace(Background, ColorX1, Amplitude*cosd(90-axisAngle), Amplitude*sind(90-axisAngle), 0.75, 0.75, "R", "R", 0, 0);
	}
	else if (mode == "Circular")
	{
		IDisplace1 = Background;
	}
	else if (mode == "Half-Circular")
	{
		ColorX1 = ColorX(0, sqrt(1.0-sqr((cosd(axisAngle)*x+sind(axisAngle)*y)%(2*Period)-Period)/(4*Period*Period)), 0, 0, a, z);
		Blur1 = Blur(ColorX1,10,10);
		IDisplace1 = IDisplace(Background, Blur1, 10*Amplitude*cosd(90-axisAngle), 10*Amplitude*sind(90-axisAngle), 0.95, 0.95, "R", "R", 0, 0);
	}
	else if (mode == "AntiCircular")
	{
		IDisplace1 = Background;
	}
	else if (mode == "Noise")
	{
		ColorX1 = ColorX(0, fnoise((cosd(axisAngle)*(-time+x)+sind(axisAngle)*(-time+y)),Period), 0, 0, a, z);
		IDisplace1 = IDisplace(Background, ColorX1, Amplitude*cosd(90-axisAngle), Amplitude*sind(90-axisAngle), 0.5, 0.5, "R", "R", 0, xDelta);
	}
	else if (mode == "Aliased Noise")
	{
		ColorX1 = ColorX(0, fnoise((cosd(axisAngle)*(-time+x)+sind(axisAngle)*(-time+y)),Period), 0, 0, a, z);
		IDisplace1 = IDisplace(Background, ColorX1, Amplitude*cosd(90-axisAngle), Amplitude*sind(90-axisAngle), 0.5, 0.5, "R", "R", 0, xDelta);
	}
    	return IDisplace1;
}

image AE_Spherize(
		image 	Background,
		float	sphereRadius,
		float	xCenter,
		float	yCenter)
{
	controlBlur = 20;
	RGrad1 = RGrad(width, height, 1, xCenter, yCenter, 1, sphereRadius, sphereRadius, 0.75, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0);
	Promote_Bytes = Bytes(RGrad1, max(bytes,2));
	Blur_Control_Image = Blur(Promote_Bytes, controlBlur, controlBlur, 1, "gauss", "gauss", "rgba");
	Emboss_for_Horizontal = Emboss(Blur_Control_Image, 10, 0, 30);
	Emboss_for_Vertical = Emboss(Blur_Control_Image, 10, 90, 30);
	Copy1 = Copy(Emboss_for_Horizontal, Emboss_for_Vertical, 1, "g");
	Monochrome1 = Monochrome(Blur_Control_Image, 0.3, 0.59, 0.11);
	Restore_Bytes2 = Bytes(Monochrome1, RGrad1.bytes);
	Restore_Bytes1 = Bytes(Copy1, RGrad1.bytes);
	Do_the_Warp = IDisplace(Background, Restore_Bytes1, 50, 50, 0.5, 0.5, "R", "G", 0, 0);
	Mult_Color = Mult(Do_the_Warp, 1, 1, 1, 1, 1);
	Mask(Mult_Color, Restore_Bytes2, "R", 100, 0, 1);
	return Mult_Color;
}

//--
image AE_Cineon_Convert(
		image 	Background,
		string	conversionType,
		int	BlackPoint10bits,
		int	BlackPointInternal,
		int	WhitePoint10bits,
		int	WhitePointInternal,
		float	Gamma)
{
	LogLin1 = LogLin(Background, 
				(conversionType == "LogToLin")?0:1, 
				0, 0, 0, 
				BlackPoint10bits, rBlack, rBlack, 
				WhitePoint10bits, rWhite, rWhite, 
				1, rNGamma, rNGamma, 
				Gamma, rDGamma, rDGamma, 
				0, rSoftClip, rSoftClip);
    	return LogLin1;
}

//--
image AE_Color_Balance(
		image	Background,
		float	coef,
		float	luminosity,
		float	saturation)
{
	AdjustHSV1 = AdjustHSV(Background, 0, coef/360.0, 1, 0, 1.5, 0, sourceSat-sourceSat,1, 0, 1.5, 0, sourceVal-sourceVal, 1, 0, 1.5);
	Brightness1 = Brightness(AdjustHSV1, exp(luminosity*0.023));
	Saturation1 = Saturation(Brightness1, exp(saturation*0.027));
	
	return Saturation1;
}

//--
image AE_Gamma(
		image	Background,
		float	rGamma,
		float	rAdd,
		float	rGain,
		float	gGamma,
		float	gAdd,
		float	gGain,
		float	bGamma,
		float	bAdd,
		float	bGain)
{
	ColorCorrect1 = ColorCorrect(Background, "v1.0", 
		x, x, x, x, 					// Expr
		1, 1, 1, 1, 					// Contrast
		0.5, 0.5, 0.5, 0.5, 				// Center
		0, 0, 0, 0, 					// SoftClip
		rGamma, gGamma, bGamma, 1, 			// Gamma
		rGain, gGain, bGain, 1, 			// Gain
    		rAdd, gAdd, bAdd, 0 				// Add
		);
}

//--
image AE_Median(
	image 	Background,
	int	radius,
	int	useAlpha)
{
	if (useAlpha == 0)
		Channels = "rgb";
	else
		Channels = "rgba";
		
	Final = Background;
	
	for (count=1;count<pow(2,radius);count++)
	{
		Final = Median(Final, Channels, 1, 0);
	}
	return Final;
}

image AE_TimeCode(
	image	Background,
	string	mode="TimeCode",
	float	fontScale=50,
	float	xPos=50,
	float	yPos=50,
	int	DropFrame=0,
	const char *	font="Arial",
	const float	red=1,
	const float	green=1,
	const float	blue=1)
{
	if (mode == "FrameNumber")
        {
    		TheTimer = Text(width, height, 1, "%F", font, fontScale, fontScale, 1, xPos, yPos, 0, 1, 3, red, green, blue, 1, 0, 0, 0, 45);
		Color3 = Color(3*fontScale, 1.5*fontScale, 1, 0, 0, 0, 1, 0);
		Pan1 = Pan(Color3, xPos-0.4*fontScale, yPos-1.45*fontScale, 0, 0.5, 0);
		Timer = Over(TheTimer, Pan1, 1, 0, 0);
        }
        else if (mode == "TimeCode")
	{
		if (DropFrame == 0)
    			TheTimer = Text(width, height, 1, "%T", font, fontScale, fontScale, 1, xPos, yPos, 0, 1, 3, red, green, blue, 1, 0, 0, 0, 45);
    		else
    			TheTimer = Text(width, height, 1, "%TD", font, fontScale, fontScale, 1, xPos, yPos, 0, 1, 3, red, green, blue, 1, 0, 0, 0, 45);
		Color3 = Color(6*fontScale, 1.5*fontScale, 1, 0, 0, 0, 1, 0);
		Pan1 = Pan(Color3, xPos-0.4*fontScale, yPos-1.45*fontScale, 0, 0.5, 0);
		Timer = Over(TheTimer, Pan1, 1, 0, 0);
        }
        else if (mode == "Feet + Frames (35mm)")
        {
    		TheTimer = Text(width, height, 1, ":stringf(\"%4.4d\",abs(time/16))+\"+\"+stringf(\"%2.2d\",abs(time%16))", font, fontScale, fontScale, 1, xPos, yPos, 0, 1, 3, red, green, blue, 1, 0, 0, 0, 45);
		Color3 = Color(4.75*fontScale, 1.5*fontScale, 1, 0, 0, 0, 1, 0);
		Pan1 = Pan(Color3, xPos-0.4*fontScale, yPos-1.45*fontScale, 0, 0.5, 0);
		Timer = Over(TheTimer, Pan1, 1, 0, 0);
        }
        else if (mode == "Feet + Frames (16mm)")
        {
    		TheTimer = Text(width, height, 1, ":stringf(\"%4.4d\",abs(time/40))+\"+\"+stringf(\"%2.2d\",abs(time%40))", font, fontScale, fontScale, 1, xPos, yPos, 0, 1, 3, red, green, blue, 1, 0, 0, 0, 45);
		Color3 = Color(4.75*fontScale, 1.5*fontScale, 1, 0, 0, 0, 1, 0);
		Pan1 = Pan(Color3, xPos-0.4*fontScale, yPos-1.45*fontScale, 0, 0.5, 0);
		Timer = Over(TheTimer, Pan1, 1, 0, 0);
        }
    	IRetime(Timer, 0, -1000000, 1000000, "Freeze", "Freeze");
    	return Over(Timer,Background);
}

//--
image AE_TVColors(
	image 	Background,
	string	standard="NTSC",
	string  method="Luminance",
	int	maxAmplitude=110)
{
	VideoSafe1 = VideoSafe(Background, (standard == "NTSC")?0:1, (method == "Saturation")?1:0, 100, maxAmplitude, 2.2);
	return VideoSafe1;
}

//--
image AE_ReduceInterlaceFlicker(
	image	Background,
	float	strength)
{
	Blur1 = Blur(Background,0,3*strength);
	return Blur1;
}

image AE_Invert(
	image	Background,
	string	channel,
	int	mix)
{
	if (channel == "RGB")
	{
		Invert1 = Invert(Background, "rgb");
		Mix1 = Mix(Background, Invert1, 1, 100-mix, "rgba");
	}
	else if (channel == "R")
	{
		Invert1 = Invert(Background, "r");
		Mix1 = Mix(Background, Invert1, 1, 100-mix, "rgba");
	}
	else if (channel == "G")
	{
		Invert1 = Invert(Background, "r");
		Mix1 = Mix(Background, Invert1, 1, 100-mix, "rgba");
	}
	else if (channel == "B")
	{
		Invert1 = Invert(Background, "b");
		Mix1 = Mix(Background, Invert1, 1, 100-mix, "rgba");
	}
	else if (channel == "HLS")
	{
		ColorSpace1 = ColorSpace(Background,"rgb","hls");
		Invert1 = Invert(ColorSpace1, "rgb");
		ColorSpace2 = ColorSpace(Invert1,"hls","rgb");
		Mix1 = Mix(Background, ColorSpace2, 1, 100-mix, "rgba");
	}
	else if (channel == "H")
	{
		ColorSpace1 = ColorSpace(Background,"rgb","hls");
		Invert1 = Invert(ColorSpace1, "r");
		ColorSpace2 = ColorSpace(Invert1,"hls","rgb");
		Mix1 = Mix(Background, ColorSpace2, 1, 100-mix, "rgba");
	}
	else if (channel == "L")
	{
		ColorSpace1 = ColorSpace(Background,"rgb","hls");
		Invert1 = Invert(ColorSpace1, "g");
		ColorSpace2 = ColorSpace(Invert1,"hls","rgb");
		Mix1 = Mix(Background, ColorSpace2, 1, 100-mix, "rgba");
	}
	else if (channel == "S")
	{
		ColorSpace1 = ColorSpace(Background,"rgb","hls");
		Invert1 = Invert(ColorSpace1, "b");
		ColorSpace2 = ColorSpace(Invert1,"hls","rgb");
		Mix1 = Mix(Background, ColorSpace2, 1, 100-mix, "rgba");
	}
	else if (channel == "YUV")
	{
		ColorSpace1 = ColorSpace(Background,"rgb","yiq");
		Invert1 = Invert(ColorSpace1, "rgb");
		ColorSpace2 = ColorSpace(Invert1,"yiq","rgb");
		Mix1 = Mix(Background, ColorSpace2, 1, 100-mix, "rgba");
	}
	else if (channel == "Y")
	{
		ColorSpace1 = ColorSpace(Background,"rgb","yiq");
		Invert1 = Invert(ColorSpace1, "r");
		ColorSpace2 = ColorSpace(Invert1,"yiq","rgb");
		Mix1 = Mix(Background, ColorSpace2, 1, 100-mix, "rgba");
	}
	else if (channel == "U")
	{
		ColorSpace1 = ColorSpace(Background,"rgb","yiq");
		Invert1 = Invert(ColorSpace1, "g");
		ColorSpace2 = ColorSpace(Invert1,"yiq","rgb");
		Mix1 = Mix(Background, ColorSpace2, 1, 100-mix, "rgba");
	}
	else if (channel == "V")
	{
		ColorSpace1 = ColorSpace(Background,"rgb","yiq");
		Invert1 = Invert(ColorSpace1, "b");
		ColorSpace2 = ColorSpace(Invert1,"yiq","rgb");
		Mix1 = Mix(Background, ColorSpace2, 1, 100-mix, "rgba");
	}
	else if (channel == "A")
	{
		Invert1 = Invert(Background, "a");
		Mix1 = Mix(Background, Invert1, 1, 100-mix, "rgba");
	}
	return Mix1;
}

//--
image AE_Arithmetic(
	image	Background,
	string	operation,
	int	rValue,
	int	gValue,
	int	bValue)
{
	if (operation == "And")
	{
		colorX1 = ColorX(Background,
			(((int)(r*255))&rValue)/255.0,
			(((int)(g*255))&gValue)/255.0,
			(((int)(b*255))&bValue)/255.0,
			a,z);
	}
	else if (operation == "Or")
	{
		colorX1 = ColorX(Background,
			(((int)(r*255))|rValue)/255.0,
			(((int)(g*255))|gValue)/255.0,
			(((int)(b*255))|bValue)/255.0,
			a,z);
	}
	else if (operation == "Xor")
	{
		colorX1 = ColorX(Background,
			(((int)(r*255))^rValue)/255.0,
			(((int)(g*255))^gValue)/255.0,
			(((int)(b*255))^bValue)/255.0,
			a,z);
	}
	else if (operation == "Add")
	{
		colorX1 = ColorX(Background,
			r+rValue/255.0,
			g+gValue/255.0,
			b+bValue/255.0,
			a,z);
	}
	else if (operation == "Sub")
	{
		colorX1 = ColorX(Background,
			r-rValue/255.0,
			g-gValue/255.0,
			b-bValue/255.0,
			a,z);
	}
	else if (operation == "Difference")
	{
		colorX1 = ColorX(Background,
			rValue/255.0-r,
			gValue/255.0-g,
			bValue/255.0-b,
			a,z);
	}
	else if (operation == "Min")
	{
		colorX1 = ColorX(Background,
			((int)(r*255))<rValue?rValue/255.0:r,
			((int)(g*255))<gValue?gValue/255.0:g,
			((int)(b*255))<bValue?bValue/255.0:b,
			a,z);
	}
	else if (operation == "Max")
	{
		colorX1 = ColorX(Background,
			((int)(r*255))>rValue?rValue/255.0:r,
			((int)(g*255))>gValue?gValue/255.0:g,
			((int)(b*255))>bValue?bValue/255.0:b,
			a,z);
	}
	else if (operation == "Block Below")
	{
		colorX1 = ColorX(Background,
			r<=rValue/255.0?0:r,
			g<=gValue/255.0?0:g,
			b<=bValue/255.0?0:b,
			a,z);
	}
	else if (operation == "Block Above")
	{
		colorX1 = ColorX(Background,
			r>=rValue/255.0?0:r,
			g>=gValue/255.0?0:g,
			b>=bValue/255.0?0:b,
			a,z);
	}
	else if (operation == "Slice")
	{
		colorX1 = ColorX(Background,
			r<=rValue/255.0?0:1.0,
			g<=gValue/255.0?0:1.0,
			b<=bValue/255.0?0:1.0,
			a,z);
	}
	else if (operation == "Mult")
	{
		colorX1 = ColorX(Background,
			r*(rValue/255.0),
			g*(gValue/255.0),
			b*(bValue/255.0),
			a,z);
	}
	return colorX1;
}

//--
image AE_ShiftChannels(
	image	Background,
	string	rChannel,
	string	gChannel,
	string	bChannel,
	string	aChannel)
{
	const char *channels=stringf("%s%s%s%s",rChannel,gChannel,bChannel,aChannel);
	Reorder1 = Reorder(Background,channels);
	return Reorder1;
}

//--
image AE_BrightnessContrast(
	image	Background,
	int	luminosity,
	int	contrast)
{
	Brightness1 = Brightness(Background, exp(0.00693*luminosity));
	ContrastLum1 = ContrastLum(Brightness1, exp(0.023*contrast), 0.5, 0);
	return ContrastLum1;
}

//--
image AE_Threshold(
	image	Background,
	int	threshold)
{
	Monochrome1 = Monochrome(Background, 0.3, 0.59, 0.11);
	ColorX1 = ColorX(Monochrome1,
		r<=threshold/255.0?0.0:1.0,
		g<=threshold/255.0?0.0:1.0,
		b<=threshold/255.0?0.0:1.0,
		a,z);
	return ColorX1;
}

image AE_Text(
	image	Background,
	string	text,
	const char *	font="Arial",
	int	xPos=width/2,
	int	yPos=height/2,
	string	displayMode="Fill",
	float	red=1,
	float	green=1,
	float	blue=1,
	float	rOutline=1,
	float	gOutline=0,
	float	bOutline=0,
	float	outlineSize=3,
	int	size=50,
	int	kerning=0,
	int	composite=1,
	string	align="Center",
	string	direction="Horizontal")
{
	int textAlign = 0;
	if (align == "Left")
		textAlign = 1;
	else if (align == "Center")
		textAlign = 2;
	else if (align == "Right")
		textAlign = 3;
	
	float textAngle = 0.0;
	if (direction == "Horizontal")
		textAngle = 0.0;
	else if (direction == "Vertical")
		textAngle = 90.0f;
		
	// Text
	Text1 = Text(	width, height, 1, 			// Image width & height and Image depth
			text, 					// Text String
			font, size, size, 			// Font and font size
    			1.0, 					// Leading space
    			xPos, yPos, 0.0,			// Position
    			textAlign, 2, 				// X & Y Align
    			red, green, blue, 1, 			// Text Color
    			0, 0, textAngle,			// X & Y & Z rotations
    			45, 					// Field of View
    			10*kerning, 				// kerning between chars
    			1);					// Font Quality
    	
    	// Outline
	Text2 = Text(	width, height, 1, 			// Image width & height and Image depth
			text, 					// Text String
			font, size, size, 			// Font and font size
    			1.0, 					// Leading space
    			xPos, yPos, 0.0, 			// Position
    			textAlign, 2, 				// X & Y Align
    			rOutline, gOutline, bOutline, 1, 	// Text Color
    			0, 0, textAngle,			// X & Y & Z rotations
    			45, 					// Field of View
    			10*kerning, 				// kerning between chars
    			1);					// Font Quality

	DilateErode1 = DilateErode(Text2, "rgba", outlineSize, outlineSize, 0, 0, 0);
	LayerX1 = LayerX(Text1, DilateErode1, r2*(1-a), g2*(1-a), b2*(1-a), a2*(1.0-a), z);
	
	if (displayMode == "Fill Only")
	{
		Over1 = Over(Text1,0,1,0,0);
	}
	else if (displayMode == "Stroke Only")
	{
		Over1 = Over(LayerX1,0,1,0,0);
	}
	else if (displayMode == "Fill Over Stroke")
	{
		Over1 = Over(Text1, LayerX1, 1, 0, 0);
	}
	else if (displayMode == "Stroke Over Fill")
	{
		Over1 = Over(LayerX1,Text1, 1, 0, 0);
	}

	Over2 = Over(Over1, Background, 1, 0, 0);

	if (composite == 1)
		return Over2;
	else
		return Over1;
}

//ADBE Numbers2
image AE_Numbers(
	image	Background,
	string	Type,
	int	Random,
	float	Value,
	int	Decimals,
	int	TodayDate,
	float	PosX,
	float	PosY,
	string	LayoutMode,
	float	rText,
	float	gText,
	float	bText,
	float	rOutline,
	float	bOutline,
	float	gOutline,
	float	OutlineWidth,
	float	Size,
	int	Kerning,
	int	Composite)
{
	const char *thestring;
	if (Type == "Number")
	{
		if (Random == 1)
			thestring = stringf("%*.*f",0,Decimals,Value+10000*rnd(0)); 
		else
			thestring = stringf("%*.*f",0,Decimals,Value);

	}
	else if (Type == "Number (0s on left)")
	{
		if (Random == 1)
			thestring = stringf("%0*.*f",6+Decimals,Decimals,Value+rnd(0)); 
		else
			thestring = stringf("%0*.*f",6+Decimals,Decimals,Value);
	}	
	else if (Type == "Timecode (30)")
	{
		thestring = "{abs(time/(60*30))}'{fmod(abs(time/30),60)}''{fmod(time,30)}";
	}	
	else if (Type == "Timecode (25)")
	{
		thestring = "{abs(time/(60*25))}'{fmod(abs(time/25),60)}''{fmod(time,25)}";
	}	
	else if (Type == "Timecode (24)")
	{
 		thestring = "{abs(time/(60*24))}'{fmod(abs(time/24),60)}''{fmod(time,24)}";
	}	
	else if (Type == "Numeric Date")
	{
 		thestring = stringf("%%d/%%m/%%Y");
	}	
	else if (Type == "Short Date")
	{
 		thestring = stringf("%%D %%d %%M %%Y");
	}	
	else if (Type == "Long Date")
	{
 		thestring = stringf("%%E %%d %%N %%Y");
	}	
	else if (Type == "Hexadecimal")
	{
		if (Random == 1)
			thestring = stringf("0x%08x",(int)(Value+10000*rnd(0))); 
		else
			thestring = stringf("0x%08x",(int)(Value));
	}
	
	Text1 = Text (	Background.width,Background.height,1,thestring,"Arial",Size,Size,0,PosX,PosY,0,3,1,rText,gText,bText,1,0,0,0,45,10*Kerning,1);
	Text2 = Text (	Background.width,Background.height,1,thestring,"Arial",Size,Size,0,PosX,PosY,0,3,1,rOutline,gOutline,bOutline,1,0,0,0,45,10*Kerning,1);

	DilateErode1 = DilateErode(Text2, "rgba", OutlineWidth, OutlineWidth, 0, 0, 0);
	LayerX1 = LayerX(Text1, DilateErode1, r2*(1-a), g2*(1-a), b2*(1-a), a2*(1.0-a), z);
	
	if (LayoutMode == "Fill Only")
	{
		Over1 = Over(Text1,0,1,0,0);
	}
	else if (LayoutMode == "Stroke Only")
	{
		Over1 = Over(LayerX1,0,1,0,0);
	}
	else if (LayoutMode == "Fill Over Stroke")
	{
		Over1 = Over(Text1, LayerX1, 1, 0, 0);
	}
	else if (LayoutMode == "Stroke Over Fill")
	{
		Over1 = Over(LayerX1,Text1, 1, 0, 0);
	}

	if (Composite == 1)
		return Over(Over1,Background);
	else
		return Text1;
}

// ADBE Ramp
image AE_Ramp(
	image 	Background,
	int	width=720,
	int	height=486,
	float	Pos1X,
	float	Pos1Y,
	float	r1,
	float	g1,
	float	b1,
	float	Pos2X,
	float	Pos2Y,
	float	r2,
	float	g2,
	float	b2,
	string	type,
	float	mix)
{
	if (type == "Linear")
	{
		Crop1 = Crop(Reorder(NGLRender(
			width, height, 1,
			"
			curve float a = atan2d(Pos1Y-Pos2Y, Pos1X-Pos2X);
			curve float dist = distance(Pos2X,Pos2Y, Pos1X, Pos1Y);
			curve float maxwidth = distance(0,0,width,height)*2.5;
	      		nglPushMatrix();
			nglRotatef(a ,0.0f,0.0f,1.0f);
			nglTranslatef(Pos2X,Pos2Y,0.0f);		
	
			for(int i=-maxwidth/2 ;i<=maxwidth/2;i=i+width/10 )
			{
				nglBegin(NGL_POLYGON);
					nglColor4f(1,1,1,1);
					nglVertex2f(0,i);
					nglColor4f(0,0,0,0);
					nglVertex2f(dist,i);
					nglVertex2f(dist, i+width/10 );
					nglColor4f(1,1,1,1);
					nglVertex2f(0, i+width/10 );
				nglEnd();
				nglBegin(NGL_POLYGON);
					nglColor4f(0,0,0,0);
					nglVertex2f(dist,i+width/10 );
					nglVertex2f(maxwidth ,i+width/10 );
					nglVertex2f(maxwidth , i);
					nglVertex2f(dist, i);
				nglEnd();
				nglBegin(NGL_POLYGON);
					nglColor4f(1,1,1,1);
					nglVertex2f(-maxwidth ,i+width/10 );
					nglVertex2f(0,i+width/10 );
					nglVertex2f(0,i);
					nglVertex2f(-maxwidth , i);
				nglEnd();
			}
	      		nglPopMatrix();"),
	  		"aaaa"));
		Color1 = Color(width, height, 1, r1, g1, b1, 1, 0);
		Mult1 = Mult(Crop1, r2, g2, b2, 1, 1);
		Over1 = Over(Mult1, Color1, 1, 0, 0);
	}
	else if (type == "Radial")
	{
		Over1 = RGrad(	Background.width, Background.height, 1, 
				Pos1X, Pos1Y, 1, 
				1,distance(Pos1X,Pos1Y,Pos2X,Pos2Y), 0.5, 
				r1, g1, b1, 1, 0, 
				r2, g2, b2, 0, 0);
	}
	Mix1 = Mix(Over1, Background, 1, mix, "rgba");
  	return Mix1;
}

image AE_Ramp4Colors(
	image 	Background,
	float	Pos1X,	float	Pos1Y,
	float	r1,	float	g1,	float	b1,
	float	Pos2X,	float	Pos2Y,
	float	r2,	float	g2,	float	b2,
	float	Pos3X,	float	Pos3Y,
	float	r3,	float	g3,	float	b3,
	float	Pos4X,	float	Pos4Y,
	float	r4,	float	g4,	float	b4,
	string	blendMode,
	int	opacity)
{
	RGrad1 = RGrad(width, height, 1, Pos1X, height-Pos1Y, 1, 1, 3*width/4, 0.5, r1, g1, b1,1,0,0,0,0,1,0);
	RGrad2 = RGrad(width, height, 1, Pos2X, height-Pos2Y, 1, 1, 3*width/4, 0.5, r2, g2, b2,1,0,0,0,0,1,0);
	RGrad3 = RGrad(width, height, 1, Pos3X, height-Pos3Y, 1, 1, 3*width/4, 0.5, r3, g3, b3,1,0,0,0,0,1,0);
	RGrad4 = RGrad(width, height, 1, Pos4X, height-Pos4Y, 1, 1, 3*width/4, 0.5, r4, g4, b4,1,0,0,0,0,1,0);
	Screen1 = Screen(RGrad2, RGrad3, 1);
	Screen2 = Screen(Screen1, RGrad4, 1);
	Screen3 = Screen(Screen2, RGrad1, 1);
	ColorX1 = ColorX(Screen3, r*opacity/100.0,g*opacity/100.0,b*opacity/100.0,a*opacity/100.0);
	
	if (blendMode == "None")
		Final = ColorX1;
	else
		Final = AE_Layers(ColorX1,Background,0,blendMode);
	return Final;
}

// ADBE Set Matte2
image AE_SetMatte2(
	image 	Foreground, 
	image 	MatteSource,
	string	Channel,
	int	Invert,
	int	AdaptSize,
	int	Mix,
	int	Premult)
{
	if (Premult == 1)
		NewMatteSource = ColorX(MatteSource, r*a, g*a, b*a, a, z);
	else 
		NewMatteSource = MatteSource;
		
	if (Channel == "r")
		New_Matte = Reorder(NewMatteSource,"rgbr");
	else if (Channel == "g")
		New_Matte = Reorder(NewMatteSource,"rgbg");
	else if (Channel == "b")
		New_Matte = Reorder(NewMatteSource,"rgbb");
	else if (Channel == "a")
		New_Matte = NewMatteSource;
	else if (Channel == "h")
	{
		ColorSpace1 = ColorSpace(NewMatteSource, "rgb", "hls", 0.3, 0.59, 0.11);
		New_Matte = Reorder(ColorSpace1,"rgbr");
	}
	else if (Channel == "l")
	{
		ColorSpace1 = ColorSpace(NewMatteSource, "rgb", "hls", 0.3, 0.59, 0.11);
		New_Matte = Reorder(ColorSpace1,"rgbg");
	}
	else if (Channel == "s")
	{
		ColorSpace1 = ColorSpace(NewMatteSource, "rgb", "hls", 0.3, 0.59, 0.11);
		New_Matte = Reorder(ColorSpace1,"rgbb");
	}
	else if (Channel == "1")
		New_Matte = SetAlpha(NewMatteSource, 1);
	else if (Channel == "0")
		New_Matte = SetAlpha(NewMatteSource, 0);
	
	if (Invert == 1)
		New_Matte1 = Invert(New_Matte,"a");
	else 
		New_Matte1 = New_Matte;
	
	if (AdaptSize == 1)
		New_Matte2 = Resize(New_Matte1, Foreground.width, Foreground.height, "default", 0);
	else
		New_Matte2 = New_Matte1;
	
	LayerX1 = LayerX(Foreground,New_Matte2,r*a2,g*a2,b*a2,a2);
	
	return LayerX1;
}

//ADBE Set Channels
image AE_SetChannels(
	image	RedMatte,	string	rChannel,
	image	GreenMatte,	string	gChannel,
	image	BlueMatte,	string	bChannel,
	image	AlphaMatte,	string	aChannel)
{
	Reorder1 = Reorder(RedMatte,rChannel+"gba");
	Reorder2 = Reorder(GreenMatte,"r"+gChannel+"ba");
	Reorder3 = Reorder(BlueMatte,"rg"+bChannel+"a");
	Reorder4 = Reorder(AlphaMatte,"rgb"+aChannel);
	
	Copy1 = Copy(Reorder1,Reorder2,1,"g");
	Copy2 = Copy(Copy1,Reorder3,1,"b");
	Copy3 = Copy(Copy2,Reorder4,1,"a");
	
	ColorX1 = ColorX(Copy3, r*a, g*a, b*a, a, z);
	return ColorX1;
}

//ADBE Find Edges
image AE_EdgeDetect(
	image 	Foreground,
	int	Invert,
	float	Mix)
{
	EdgeDetect1 = EdgeDetect(Foreground);
	if (Invert == 0)
		Invert1 = Invert(EdgeDetect1, "rgba");
	else
		Invert1 = EdgeDetect1;
		
	Mix1 = Mix(Invert1, Foreground, 1, Mix, "rgba");
	return Mix1;
}

//ADBE Emboss
image AE_Emboss(
	image	Foreground,
	float	Angle,
	float	Contrast,
	float	Mix)
{
	Emboss1 = Emboss(Foreground, Contrast/100.0, Angle-90, 30);
	Mix1 = Mix(Emboss1, Foreground, 1, Mix, "rgba");	
	return Mix1;
}

//ADBE Scatter
image AE_Scatter(
	image	Foreground,
	float	Intensity,
	string	Grain,
	int	RandomizeTime)
{
	if (RandomizeTime == 1)
		Rand1 = Rand(Foreground.width, Foreground.height, 1, 1, time);
	else
		Rand1 = Rand(Foreground.width, Foreground.height, 1, 1, 0);
	
	if (Grain == "Horizontal")
		IDisplace1 = IDisplace(Foreground, Rand1, Intensity, 0, 0.5, xDOffset, "A", "A", 0, xDelta);
	else if (Grain == "Vertical")
		IDisplace1 = IDisplace(Foreground, Rand1, 0, Intensity, 0.5, xDOffset, "A", "A", 0, xDelta);
	else if (Grain == "Both")
		IDisplace1 = IDisplace(Foreground, Rand1, Intensity, xScale, 0.5, xDOffset, "A", "A", 0, xDelta);

	Over1 = Over(IDisplace1, Foreground, 1, 0, 0);
	return Over1;
}

//ADBE Mosaic
image AE_Mosaic(
	image	Foreground,
	int	XBox,	int	YBox)
{
	Pixelize1 = Pixelize(Foreground, Foreground.width/XBox, Foreground.height/YBox);
	return Pixelize1;	
}

//ADBE Texturize
image AE_Texturize(
	image	Background,
	image	Texture,
	float	LightAngle,
	float	Contrast,
	string	Disposition)
{
	Emboss1 = Emboss(	Texture, Contrast, LightAngle-90, 30);
	LayerX1 = LayerX(	Emboss1, Background, 
				r2+(r-0.5019)*Contrast, 
				g2+(g-0.5019)*Contrast, 
				b2+(b-0.5019)*Contrast, 
				a, z);
    	return LayerX1;
}

//ADBE Strobe
image AE_Strobe(
	image	Background,
	float	r,	float	g,	float	b,
	float	Mix,
	float	Length,
	float	Period,
	string	Mode,
	string	Operation)
{
	Color1 = Color(	Background.width,Background.height,1,r,g,b);
	if (Operation == "Copy")
		Strobe_Image = Color1;
	else if (Operation == "Add")
		Strobe_Image = IAdd(Background,Color1,1,Mix);
	else if (Operation == "Sub")
		Strobe_Image = ISubA(Background,Color1,1,Mix);
	else if (Operation == "Mult")
		Strobe_Image = IMult(Background,Color1,1,Mix,0);
	else if (Operation == "Difference")
		Strobe_Image = ISubA(Background,Color1,1,Mix);
	else if (Operation == "And")
		Strobe_Image = LayerX(Background, Color1, 
					(((int)(r*255))&((int)(r2*255)))/255.0, 
    					(((int)(g*255))&((int)(g2*255)))/255.0, 
    					(((int)(b*255))&((int)(b2*255)))/255.0, 
    					a, z);
	else if (Operation == "Or")
		Strobe_Image = LayerX(Background, Color1, 
					(((int)(r*255))|((int)(r2*255)))/255.0, 
    					(((int)(g*255))|((int)(g2*255)))/255.0, 
    					(((int)(b*255))|((int)(b2*255)))/255.0, 
    					a, z);
	else if (Operation == "Xor")
		Strobe_Image = LayerX(Background, Color1, 
		
					(((int)(r*255))^((int)(r2*255)))/255.0, 
    					(((int)(g*255))^((int)(g2*255)))/255.0, 
    					(((int)(b*255))^((int)(b2*255)))/255.0, 
    					a, z);
 	else if (Operation == "Lighten")
		Strobe_Image = Max(Background,Color1,1,Mix);
 	else if (Operation == "Darken")
		Strobe_Image = Min(Background,Color1,1,Mix);
 	else if (Operation == "Minimum")
		Strobe_Image = Min(Background,Color1,1,Mix);
 	else if (Operation == "Maximum")
		Strobe_Image = Max(Background,Color1,1,Mix);
 	else if (Operation == "Over")
		Strobe_Image = Over(Background,Color1);
	
	Select1 = Select(Step(2,1@1,2@(Period-Length),1@Period), Background, Strobe_Image, 0, 0);
	
	return Select1;
}

//ADBE Motion Blur
image AE_MotionBlur(
	image	Background,
	float	Angle,
	float	Distance)
{
    Rotate1 = Rotate(Background, Angle-90, 1, width/2, height/2, 0, 0.5, 0);
    Blur1 = Blur(Rotate1, Distance*4, 0, 1, "gauss", xFilter, "rgba");
    Rotate2 = Rotate(Blur1, -Angle+90, 1, width/2, height/2, 0, 0.5, 0);
    return Rotate2;
}

//ADBE Gaussian Blur
image AE_GaussianBlur(
	image 	Background,
	float	Amount,
	string	Type)
{
	float XBlur=0;
	float YBlur=0;
	if (Type == "Both")
	{
		XBlur = Amount*3;
		YBlur = Amount*3;
	}
	else if (Type == "Horizontal")
		XBlur = Amount*3;
	else if (Type == "Vertical")
		YBlur = Amount*3;
		
	Blur1 = Blur(Background,XBlur,YBlur);
	return Blur1;
}

//ADBE Channel Blur
image AE_ChannelBlur(
	image	Background,
	float	RBlur,	float	GBlur,	float	BBlur,	float	ABlur,
	string	Type)
{
	float XBlur=0;
	float YBlur=0;
	if (Type == "Both")
	{
		XBlur = RBlur*3;
		YBlur = RBlur*3;
	}
	else if (Type == "Horizontal")
		XBlur = RBlur*3;
	else if (Type == "Vertical")
		YBlur = RBlur*3;
	Blur1 = Blur(Background,XBlur,YBlur,1, "gauss", xFilter, "r");
	if (Type == "Both")
	{
		XBlur = GBlur*3;
		YBlur = GBlur*3;
	}
	else if (Type == "Horizontal")
		XBlur = GBlur*3;
	else if (Type == "Vertical")
		YBlur = GBlur*3;
	Blur2 = Blur(Blur1,XBlur,YBlur,1, "gauss", xFilter, "g");
	if (Type == "Both")
	{
		XBlur = BBlur*3;
		YBlur = BBlur*3;
	}
	else if (Type == "Horizontal")
		XBlur = BBlur*3;
	else if (Type == "Vertical")
		YBlur = BBlur*3;
	Blur3 = Blur(Blur2,XBlur,YBlur,1, "gauss", xFilter, "b");
	if (Type == "Both")
	{
		XBlur = ABlur*3;
		YBlur = ABlur*3;
	}
	else if (Type == "Horizontal")
		XBlur = ABlur*3;
	else if (Type == "Vertical")
		YBlur = ABlur*3;
	Blur4 = Blur(Blur3,XBlur,YBlur,1, "gauss", xFilter, "a");
	return Blur4;
}

image AE_RBlur(
	image 	Background,
	float	Value,
	float	PosX,	float	PosY,
	string	Mode,
	string	Quality)
{
	if (Mode == "Zoom")
	{
		if (Quality == "Low")
			RBlur1 = RBlur(Background, PosX, PosY, 1, 10, 1, 1, Value, 0, 0, 0);	
		else
			RBlur1 = RBlur(Background, PosX, PosY, 1, 10, 1, 1, Value, 1, 0, 0);
		FinalNode = RBlur1;	
	}
	else if (Mode == "Rotation")
	{
		if (Quality == "Low")
		{
			BlurQuality = 0.1;
		}
		else
		{
			BlurQuality = 1.0;
		}
		Move2D1 =  Move2D(	Background, 
		   			0, 0, AngleRot*Value*10.0, 
        				1, 1, xScale, 
        				0, 0, PosX, PosY, 
        				"default", xFilter, "trsx", 0, 
        				BlurQuality, 0.5, -0.25, 1, time, float AngleRot = Linear(1,0@1,2@2));
        	FinalNode = Move2D1;
	}
	return Over(FinalNode,Background);
}

//ADBE Sharpen
image AE_Sharpen(
	image	Background,
	float	Intensity)
{
	Sharpen1 = Sharpen(Background, 100, Intensity, xPixels, "rgba");
	return Sharpen1;
}

//ADBE Basic 3D
image AE_Basic3D(
	image 	Background,
	float	XRotate,	float	YRotate,
	float	Distance)
{
	Move3D1 = Move3D(	Background, 
				0, 0, -5*Distance, 
				YRotate, -XRotate, 0, 
				1, 1, xScale, 0, 
    				width/2, height/2, 0, 
    				45, "default", xFilter, 
    				"trs", 0, 0, 
    				0.5, 0, 0, time);	
}

//ADBE Bevel Alpha
image AE_BevelAlpha(
	image	Background,
	float	Thickness,
	float	Angle,
	float	r,	float	g,	float	b,
	float	Intensity)
{
	Ramp1 = Ramp(Thickness, Background.height, 1, 1, 0.5, 0, 0, 0, 0, 0, fabs(r*cosd(Angle+90)), fabs(g*cosd(Angle+90)), fabs(b*cosd(Angle+90)), fabs(cosd(Angle+90)), 0);
	Ramp2 = Ramp(Background.width, Thickness, 1, 0, 0.5, 0, 0, 0, 0, 0, fabs(r*sind(Angle+90)), fabs(g*sind(Angle+90)), fabs(b*sind(Angle+90)), fabs(sind(Angle+90)), 0);
	Invert1 = Invert(Ramp1, "rgba");
	Invert2 = Invert(Ramp2, "rgba");
	Move2D1 = Move2D(Ramp1, Background.width-Thickness, 0, 0, 1, 1, xScale, 0, 0, width/2, height/2, "default", xFilter, "trsx", 0, 0, 0.5, 0, 0, time);
	Move2D3 = Move2D(Ramp2, 0, Background.height-Thickness, 0, 1, 1, xScale, 0, 0, width/2, height/2, "default", xFilter, "trsx", 0, 0, 0.5, 0, 0, time);
	Layer1 = Over(Move2D1, Background);
	Move2D2 = Move2D(Invert1, 0, 0, 0, 1, -1, xScale, 0, 0, width/2, height/2, "default", xFilter, "trsx", 0, 0, 0.5, 0, 0, time);
	Move2D4 = Move2D(Invert2, 0, 0, 0, 1, 1, -1, 0, 0, width/2, height/2, "default", xFilter, "trsx", 0, 0, 0.5, 0, 0, time);
	IMult1 = IMult(Move2D2, Layer1, 1, 100, 0);
	Layer2 = Over(Move2D3, IMult1);
	IMult2 = IMult(Move2D4, Layer2, 1, 100, 0);
	return IMult2;
}

//ADBE Bevel Edges
image AE_BevelEdges(
	image	Background,
	float	Thickness,
	float	Angle,
	float	red,	float	green,	float	blue,
	float	Intensity)
{
	ColorX1 = ColorX(Background, 
			((y>=x) && (y<=height-x) && (x<=size))?
				clamp(r+red*Intensity*cosd(Angle+90),0,1):
		  		(
		  			((y>=width-x) && (y<=height-width+x) && (x>=width-size))?
		  				clamp(r+red*Intensity*cosd(Angle+270),0,1):
		  				(
		  					((y>=height-x) && (y>=height-width+x) && (y>=height/2) && (y>=height-size))?
		  						clamp(r+red*Intensity*sind(Angle+90),0,1):
		  						(
		  							(y<=size)?
		  								clamp(r+red*Intensity*sind(Angle+270),0,1):
		  								r
		  						)
		  				)
		  		),
			((y>=x) && (y<=height-x) && (x<=size))?
				clamp(g+green*Intensity*cosd(Angle+90),0,1):
		  		(
		  			((y>=width-x) && (y<=height-width+x) && (x>=width-size))?
		  				clamp(g+green*Intensity*cosd(Angle+270),0,1):
		  				(
		  					((y>=height-x) && (y>=height-width+x) && (y>=height/2) && (y>=height-size))?
		  						clamp(g+green*Intensity*sind(Angle+90),0,1):
		  						(
		  							(y<=size)?
		  								clamp(g+green*Intensity*sind(Angle+270),0,1):
		  								g
		  						)
		  				)
		  		),
			((y>=x) && (y<=height-x) && (x<=size))?
				clamp(b+blue*Intensity*cosd(Angle+90),0,1):
		  		(
		  			((y>=width-x) && (y<=height-width+x) && (x>=width-size))?
		  				clamp(b+blue*Intensity*cosd(Angle+270),0,1):
		  				(
		  					((y>=height-x) && (y>=height-width+x) && (y>=height/2) && (y>=height-size))?
		  						clamp(b+blue*Intensity*sind(Angle+90),0,1):
		  						(
		  							(y<=size)?
		  								clamp(b+blue*Intensity*sind(Angle+270),0,1):
		  								b
		  						)
		  				)
		  		),
			a, z,
			float size=Thickness*min(width,height));
	return ColorX1;
}

//ADBE Drop Shadow
image AE_DropShadow(
	image	Background,
	float	red,	float	green,	float	blue,
	float	Opacity,
	float	Angle,
	float	Distance,
	float	Fuzziness,
	int	ShadowOnly)
{
	if (ShadowOnly == 0)
	{
		// Background + Shadow	
		TheShadow = AddShadow(	Background, 
					cosd(90-Angle)*Distance, sind(90-Angle)*Distance, 
					2*Fuzziness, 
					red, green, blue, 
					Opacity/100.0);
		
	}
	else
	{
		// Shadow Only
		Shadow = DropShadow(	Background, 
					2*Fuzziness, 
					red, blue, green, 
					Opacity/100.0);	
		TheShadow = Move2D(	Shadow, cosd(90-Angle)*Distance, sind(90-Angle)*Distance, 0, 1, 1, xScale, 
    					0, 0, width/2, height/2, "default", xFilter, "trsx", 0, 0, 0.5,
    					0, 0, time);
	}
	return TheShadow;
}

//ADBE Fractal
image AE_Fractal(
	image	Background,
	string	Type)
{
	if (Type == "Mandelbrot")
	{
		TheFractal = Mandelbrot(    Background.width, Background.height, 1, 50, 0, 0, 0.333333343, 
					    0.2, 0.2, 0.2, 1, 0.08949517, 0.07994568, 0.645, 0.07707026, 
					    0.09555196, 1, 0.347873151, 0.00631994, 1, 0.560742259, 0.00430929661, 
					    1, 0.6565884, 0.00511306524, 1, 0.8153812, 0.07236862);
	}
	else if (Type == "Julia")
	{
		TheFractal = Julia(	Background.width, Background.height, 1, 50, 0, 0, 0.333333343, -0.7, 0, 0.2, 
					0.2, 0.2, 0.265, 0.0125609608, 0.00243595615, 0.405, 0.00402954966, 
					0.007897095, 0.6, 0.01733873, 0.00587879447, 0.78, 0.0459355079, 
					0.0310375281, 0.86, 0.0171561055, 0.0253685154, 1, 0.266087383, 
					0.0540612936, 1, 0.5303567, 0.04984212, 1, 0.899549246, 0.05596614);
	}
	return TheFractal;
}

//ADBE Block Dissolve
image AE_BlockDissolve(
	image 	Background,
	float	Percent,
	float	BlockWidth,	float	BlockHeight,
	float	BlurAmount)
{
	Rand1 = Rand((int)(Background.width/BlockWidth), (int)(Background.height/BlockHeight), 1, 1, 1);
	Add1 = Add(Rand1, 0, 0, 0, 0.01, 0);
	Monochrome1 = Monochrome(Add1, 0.3, 0.59, 0.11);
	Resize1 = Resize(Monochrome1, Background.width, Background.height, "dirac", 0);
	ColorX1 = ColorX(Resize1, r, g, b, a>Percent/100.0?1:0, z);
	Blur1 = Blur(ColorX1, BlurAmount, BlurAmount, 0, "gauss", xFilter, "a");
	Copy1 = Copy(Background, Blur1, 0, "a");
	ColorX2 = ColorX(Copy1, r*a, g*a, b*a, a, z);
	return ColorX2;
}

//ADBE Venetian Blinds
image AE_VenitianBlinds(
	image	Background,
	float	Percent,
	float	Angle,
	float	Thickness,
	float	BlurAmount)
{
	Color1 = Color(mask_size, mask_size, 1, 0, 0, 0, 0, 0,int mask_size = (int)(sqrt(Background.width*Background.width+Background.height*Background.height)));
	ColorX1 = ColorX(Color1,r,g,b,x%Thickness>Percent*Thickness/100?1:0);
	Move2D1 = Move2D(ColorX1, -(width-Background.width)/2, -(height-Background.height)/2, -Angle, 1, 1, xScale, 0, 0, width/2, height/2, 
    				"default", xFilter, "trsx", 0, 0, 0.5, 0, 0, time);
	Blur1 = Blur(Move2D1, BlurAmount, BlurAmount, 0, "gauss", xFilter, "a");
	Copy1 = Copy(Background, Blur1, 0, "a");
	ColorX2 = ColorX(Copy1, r*a, g*a, b*a, a, z);
	return ColorX2;
}

//ADBE Linear Wipe
image AE_LinearWipe(
	image	Background,
	float	Percent,
	float	Angle,
	float	BlurAmount)
{
	Color1 = Color(mask_size, mask_size, 1, 0, 0, 0, 0, 0,int mask_size = (int)(sqrt(Background.width*Background.width+Background.height*Background.height)));
	ColorX1 = ColorX(Color1,r,g,b,x>Percent*width/100?1:0);
	Move2D1 = Move2D(ColorX1, -(width-Background.width)/2, -(height-Background.height)/2, -Angle, 1, 1, xScale, 0, 0, width/2, height/2, 
    				"default", xFilter, "trsx", 0, 0, 0.5, 0, 0, time);
	Blur1 = Blur(Move2D1, BlurAmount, BlurAmount, 0, "gauss", xFilter, "a");
	Copy1 = Copy(Background, Blur1, 0, "a");
	ColorX2 = ColorX(Copy1, r*a, g*a, b*a, a, z);
	return ColorX2;
}

//ADBE Radial Wipe
image AE_RadialWipe(
	image	Background,
	float	Percent,
	float	InitialAngle,
	float	CenterX,	float	CenterY,
	string	Mode,
	float	BlurAmount)
{
	if (Mode == "Clockwise")
	{
    	    ColorWheel1 = ColorWheel(512, 512, 2, 0, 1, 1, 1);
    	    Crop1 = Crop(ColorWheel1,100,100,412,412);
    	    ColorSpace1 = ColorSpace(Crop1, "rgb", "hsv", 0.3, 0.59, 0.11);
    	    Rotate1 = Rotate(ColorSpace1, 90-InitialAngle, 1, width/2, height/2, 0, 0.5, 0);
    	    Invert2 = Invert(Rotate1, "rgba");
    	    Lookup1 = Lookup(Invert2, x>thresh?1:0, 
    	    				  HermiteV(x,0,[0,50,50]@0,[1,50,50]@1), 
    	    				  HermiteV(x,0,[0,50,50]@0,[1,50,50]@1), 
    	    				  HermiteV(x,0,[0,50,50]@0,[1,50,50]@1), 
    	    				  float thresh = Percent/100.0);
    	    Reorder1 = Reorder(Lookup1, "rrrr");
    	    Resize1 = Resize(Reorder1, Background.width, Background.height, "default", 0);
    	    FinalMask = Blur(Resize1, BlurAmount, xPixels, 0, "gauss", xFilter, "rgba");
    	 }
    	 else if (Mode == "CounterClockwise")
    	 {
    	    ColorWheel1 = ColorWheel(512, 512, 2, 0, 1, 1, 1);
    	    Crop1 = Crop(ColorWheel1,100,100,412,412);
    	    ColorSpace1 = ColorSpace(Crop1, "rgb", "hsv", 0.3, 0.59, 0.11);
    	    Rotate1 = Rotate(ColorSpace1, 90-InitialAngle, 1, width/2, height/2, 0, 0.5, 0);
    	    Invert2 = Invert(Rotate1, "rgba");
    	    Lookup1 = Lookup(Invert2, x>thresh?1:0, 
    	    				  HermiteV(x,0,[0,50,50]@0,[1,50,50]@1), 
    	    				  HermiteV(x,0,[0,50,50]@0,[1,50,50]@1), 
    	    				  HermiteV(x,0,[0,50,50]@0,[1,50,50]@1), 
    	    				  float thresh = Percent/100.0);
    	    Reorder1 = Reorder(Lookup1, "rrrr");
    	    Resize1 = Resize(Reorder1, Background.width, Background.height, "default", 0);
    	    Blur1 = Blur(Resize1, BlurAmount, xPixels, 0, "gauss", xFilter, "rgba");
    	    FinalMask = Scale(Blur1,-1,1);
	}
	else if (Mode == "FlipFlop")
	{
    	    ColorWheel1 = ColorWheel(512, 512, 2, 0, 1, 1, 1);
    	    Crop1 = Crop(ColorWheel1,100,100,412,412);
    	    ColorSpace1 = ColorSpace(Crop1, "rgb", "hsv", 0.3, 0.59, 0.11);
    	    Rotate1 = Rotate(ColorSpace1, 90-InitialAngle, 1, width/2, height/2, 0, 0.5, 0);
    	    Invert2 = Invert(Rotate1, "rgba");
    	    Lookup1 = Lookup(Invert2, ((x>thresh)||(x<1.0-thresh))?1:0, 
    	    				  HermiteV(x,0,[0,50,50]@0,[1,50,50]@1), 
    	    				  HermiteV(x,0,[0,50,50]@0,[1,50,50]@1), 
    	    				  HermiteV(x,0,[0,50,50]@0,[1,50,50]@1), 
    	    				  float thresh = Percent/200.0+0.5);
    	    Reorder1 = Reorder(Lookup1, "rrrr");
    	    Resize1 = Resize(Reorder1, Background.width, Background.height, "default", 0);
    	    Blur1 = Blur(Resize1, BlurAmount, xPixels, 0, "gauss", xFilter, "rgba");
    	    FinalMask = Scale(Blur1,1,-1);
	}
	Final = LayerX(Background,FinalMask,r*a2,g*a2,b*a2,a2,z);
	return Final;
}

//ADBE Grid
image AE_Grid(
	image	Background,
	float	AnchorX,	float	AnchorY,
	string	GridSize,
	float	CornerX,	float	CornerY,
	float	Width,		float	Height,
	float	LineWidth,
	int	InvertGrid,
	float	rGrid,		float	gGrid,	float	bGrid,
	float	Opacity,
	string	TransferMode)
{
	if (GridSize == "Corner")
	{
		ColorX1 = ColorX(0,
			((x%GridWidth<LineWidth/2)||(x%GridWidth>GridWidth-LineWidth/2)) || ((y%GridHeight<LineWidth/2)||(y%GridHeight>GridHeight-LineWidth/2))?rGrid:0,
			((x%GridWidth<LineWidth/2)||(x%GridWidth>GridWidth-LineWidth/2)) || ((y%GridHeight<LineWidth/2)||(y%GridHeight>GridHeight-LineWidth/2))?gGrid:0,
			((x%GridWidth<LineWidth/2)||(x%GridWidth>GridWidth-LineWidth/2)) || ((y%GridHeight<LineWidth/2)||(y%GridHeight>GridHeight-LineWidth/2))?bGrid:0,
			((x%GridWidth<LineWidth/2)||(x%GridWidth>GridWidth-LineWidth/2)) || ((y%GridHeight<LineWidth/2)||(y%GridHeight>GridHeight-LineWidth/2))?Opacity/100.0:0,
			float	GridWidth = abs(CornerX-AnchorX)+LineWidth,
			float	GridHeight = abs(CornerY-AnchorY)+LineWidth);
	}
	else if (GridSize == "Width")
	{
		ColorX1 = ColorX(0,
			((x%GridWidth<LineWidth/2)||(x%GridWidth>GridWidth-LineWidth/2)) || ((y%GridHeight<LineWidth/2)||(y%GridHeight>GridHeight-LineWidth/2))?rGrid:0,
			((x%GridWidth<LineWidth/2)||(x%GridWidth>GridWidth-LineWidth/2)) || ((y%GridHeight<LineWidth/2)||(y%GridHeight>GridHeight-LineWidth/2))?gGrid:0,
			((x%GridWidth<LineWidth/2)||(x%GridWidth>GridWidth-LineWidth/2)) || ((y%GridHeight<LineWidth/2)||(y%GridHeight>GridHeight-LineWidth/2))?bGrid:0,
			((x%GridWidth<LineWidth/2)||(x%GridWidth>GridWidth-LineWidth/2)) || ((y%GridHeight<LineWidth/2)||(y%GridHeight>GridHeight-LineWidth/2))?Opacity/100.0:0,
			float	GridWidth = Width+LineWidth,
			float	GridHeight = Width+LineWidth);
	}
	else if (GridSize == "Width And Height")
	{
		ColorX1 = ColorX(0,
			((x%GridWidth<LineWidth/2)||(x%GridWidth>GridWidth-LineWidth/2)) || ((y%GridHeight<LineWidth/2)||(y%GridHeight>GridHeight-LineWidth/2))?rGrid:0,
			((x%GridWidth<LineWidth/2)||(x%GridWidth>GridWidth-LineWidth/2)) || ((y%GridHeight<LineWidth/2)||(y%GridHeight>GridHeight-LineWidth/2))?gGrid:0,
			((x%GridWidth<LineWidth/2)||(x%GridWidth>GridWidth-LineWidth/2)) || ((y%GridHeight<LineWidth/2)||(y%GridHeight>GridHeight-LineWidth/2))?bGrid:0,
			((x%GridWidth<LineWidth/2)||(x%GridWidth>GridWidth-LineWidth/2)) || ((y%GridHeight<LineWidth/2)||(y%GridHeight>GridHeight-LineWidth/2))?Opacity/100.0:0,
			float	GridWidth = Width+LineWidth,
			float	GridHeight = Height+LineWidth);
	}
	if (InvertGrid == 1)
		Invert1 = Invert(ColorX1);
	else
		Invert1 = ColorX1;
		
	if (TransferMode == "None")
		Final = Invert1;
	else
		Final = AE_Layers(Invert1,Background,0,TransferMode);
		
	return Final;
}
	
//ADBE Fill
image AE_Fill(
	image	Background,
	image	FillMask,
	int	AllMasks,
	float	rFill,	float	gFill,	float	bFill,
	int	Invert,
	float	BlurX,	float	BlurY,
	float	Opacity)
{
	Color1 = Color(Background.width,Background.height,1,rFill,gFill,bFill);
	if (Invert == 1.0)
		FinalMask = Invert(FillMask);
	else
		FinalMask = FillMask;
	LayerX1 = LayerX(Color1,FinalMask,r*a2*Opacity/100.0,g*a2*Opacity/100.0,b*a2*Opacity/100.0,a2*Opacity/100.0);
	Blur1 = Blur(LayerX1,3*BlurX,3*BlurY);
	Over1 = Over(Blur1,Background,1,0,1);
	return Over1;
}

//APC Radio Waves
image AE_RadioWaves(
	image	Background,
	image	SecondSource,
	float	CenterX,	float	CenterY,
	int	RenderQuality,
	string	Shape,
	
	int	PolyNbPoints,
	int	PolyStar,
	float	StarDepth,
	
	float	SourceCenterX,	float	SourceCenterY,
	string	SourceChannel,
	int	InvertSource,
	float	SourceThreshold,
	float	PreBlur,
	float	Tolerance,
	float	BlurAmount,
	
	float	Frequency,
	float	Expansion,
	float	Orientation,
	float	Direction,
	float	Speed,
	float	Rotation,
	float	Length,
	int	Reflection,
	
	float	rColor,		float	gColor,	float	bColor,
	float	Opacity,
	float	FadeBefore,	float	FadeAfter,
	float	StartWidth,	float	EndWidth)
{
	if (Shape == "Polygon")
	{
		Final = Black(Background.width,Background.height,1);
		for (int count=0; count<Frequency*time/24; count++)
		{
			Final = Over(
				   Fade(
			           	Move2D(
					// Image
					AE_Arc(	Background.width,Background.height,
						rColor,gColor,bColor,
						0,360,
						100,
						Hermite(0,[StartWidth,0,0]@count*24/Frequency,[EndWidth,0,0]@(count/Frequency+Length)*24),
						CenterX,CenterY,
						PolyNbPoints),
					// xPan
					Hermite(0,[0,0,0]@count*24/Frequency,[Speed*cosd(Direction)*Length,0,0]@(count/Frequency+Length)*24),
					// yPan
					Hermite(0,[0,0,0]@count*24/Frequency,[Speed*sind(Direction)*Length,0,0]@(count/Frequency+Length)*24),
					// Angle
					Orientation+Hermite(0,[0,0,0]@count*24/Frequency,[Rotation*Length,0,0]@(count/Frequency+Length)*24),
					// Aspect Ratio
					1,
					// Scale
					Hermite(0,[0,0,0]@count*24/Frequency,[Expansion,0,0]@(count/Frequency+Length)*24),xScale,
					// Shear
					0,0,
					// Center
					CenterX,CenterY,
					// Filters
					"default",xFilter,
					// Order
					"trsx"),
					// Opacity
					Hermite(0,[0,0,0]@-1,[Opacity,0,0]@(count/Frequency+FadeBefore)*24,[Opacity,0,0]@(count/Frequency+Length-FadeBefore)*24,[0,0,0]@(count/Frequency+Length)*24+1)),
				   Final);
		}					
	}
	else if (Shape == "Source")
	{
		Final = Background;
	}
	else if (Shape == "Mask")
	{
		Final = Black(Background.width,Background.height,1);
		ColorX1 = ColorX(SecondSource, rColor*a, gColor*a, bColor*a, a, z);
		for (int count=0; count<Frequency*time/24; count++)
		{
			Final = Over(
				   Fade(
			           	Move2D(
					// Image
					EdgeDetect(ColorX1, 1,0, 0, 0, 0, 0, 0, xBlur, Hermite(0,[StartWidth/2,0,0]@count*24/Frequency,[EndWidth/2,0,0]@(count/Frequency+Length)*24), xDilateErode, 0, 0, "Sobel", 8, 1),
					// xPan
					Hermite(0,[0,0,0]@count*24/Frequency,[Speed*cosd(Direction)*Length,0,0]@(count/Frequency+Length)*24),
					// yPan
					Hermite(0,[0,0,0]@count*24/Frequency,[Speed*sind(Direction)*Length,0,0]@(count/Frequency+Length)*24),
					// Angle
					Orientation+Hermite(0,[0,0,0]@count*24/Frequency,[Rotation*Length,0,0]@(count/Frequency+Length)*24),
					// Aspect Ratio
					1,
					// Scale
					Hermite(0,[0,0,0]@count*24/Frequency,[Expansion,0,0]@(count/Frequency+Length)*24),xScale,
					// Shear
					0,0,
					// Center
					CenterX,CenterY,
					// Filters
					"default",xFilter,
					// Order
					"trsx"),
					// Opacity
					Hermite(0,[0,0,0]@-1,[Opacity,0,0]@(count/Frequency+FadeBefore)*24,[Opacity,0,0]@(count/Frequency+Length-FadeBefore)*24,[0,0,0]@(count/Frequency+Length)*24+1)),
				   Final);
		}					
	}
	return Final;
}

//ADBE Noise
image AE_Noise(
	image	Background,
	float	Amount,
	int	Color,
	int	Level)
{
	Rand1 = Rand(Background.width, Background.height, 1, Amount/100.0, 1);
	if (Color == 1.0)
		Monochrome1 = Rand1;
	else 
		Monochrome1 = Monochrome(Rand1, 0.3, 0.59, 0.11);
	Over1 = Over(Monochrome1, Background, 1, 0, 0);
	return Over1;
}

//ADBE Equalize
image AE_Equalize(
	image	Background,
	float	Factor)
{
	LumaKey1 = LumaKey(Background, 0, Factor/100.0, 0, 0, 1);
	return LumaKey1;
}

//ADBE Tint
image AE_Tint(
	image	Background,
	float	red1,	float	green1,	float	blue1,
	float	red2,	float	green2,	float	blue2,
	float	Factor)
{
	ColorSpace1 = ColorSpace(Background, "rgb", "hls", 0.3, 0.59, 0.11);
	LayerX1 = LayerX(	ColorSpace1,Background,
				(g*(red2-red1)+red1)*Factor/100.0 + (1.0-Factor/100.0)*r2,	
				(g*(green2-green1)+green1)*Factor/100.0 + (1.0-Factor/100.0)*g2,
				(g*(blue2-blue1)+blue1)*Factor/100.0 + (1.0-Factor/100.0)*b2,
				a,z);
	return LayerX1;
}

//ADBE Geometry2
image AE_Geometry2(
	image	Background,
	float	AnchorX,	float	AnchorY,
	float	xPan,		float	yPan,
	int	UniformScale,
	float	xScale,		float	yScale,
	float	ShearAmount,	float	ShearAngle,
	float	Angle,
	float	Opacity,
	float	UseShutter,
	float	ShutterAngle)
{
	return Fade(  Move2D(	Background,
				-AnchorX+xPan,AnchorY-yPan,
				-Angle,
				1,
				xScale/100.0,(UniformScale==1)?xScale:yScale/100.0,
				ShearAmount*cosd(ShearAngle-90)/20.0,ShearAmount*sind(ShearAngle-90)/20.0,
				AnchorX,AnchorY,
				"default", xFilter, "trsx",
				0,0,0,ShutterAngle/360.0,
				0,time),
		      Opacity/100.0);
}

//ADBE Displacement Map
image AE_DisplacementMap(
	image	Background,
	image	Map,
	string	HorizontalChannel,	float	HorizontalStrength,
	string	VerticalChannel,	float	VerticalStrength,
	string	Disposition,
	int	Loop)
{
	IDisplace1 = IDisplace(Background, Map, -2*HorizontalStrength, 2*VerticalStrength, 0.5, xDOffset, HorizontalChannel, VerticalChannel, 0, xDelta);
	if (Loop == 1.0)
	{
		Flop1 = Flop(Background);
		Final = Over(IDisplace1,Flop1);	
	}
	else
		Final = IDisplace1;
	return Final;
}
	
//ADBE Polar Coordinates
image AE_PolarCoordinates(
	image	Background,
	float	Interpolation,
	string	Type)
{
	if (Type == "RectToPolar")
	{
		Background1 = Flip(Background);
		Final1 = Viewport(WarpX(SetDOD(Background1,0,Background.width,0,Background.height),0,
			{{ float radius= sqrt( (x-width/2) *(x-width/2) +(y-height/2) *(y-height/2));
				theta=asind( (y-height/2) /radius);
				if( ((y-height/2)>0) && ((x-width/2)<0)){theta=180-theta} ;
				if( ((y-height/2)<0) && ((x-width/2)<0)){theta=asind((height/2-y)/radius)+180} ;
				if( ((y-height/2)<0) && ((x-width/2)>0)){theta=360-asind((height/2-y)/radius)}
				((((360+theta)%360)*width/360)*Interpolation/100.0 + (100.0-Interpolation)*x/100.0)}},
			{{ float radius= sqrt( (x-width/2) *(x-width/2) +(y-height/2) *(y-height/2)) ;
				((radius/(height/2)*height)*Interpolation/100.0 + (100.0-Interpolation)*y/100.0)}},width,height),
			0,0,Background.width,Background.height);
		Final = Rotate(Final1, 90, 1, width/2, height/2, 0, 0.5, 0);
	}
	else if (Type == "PolarToRect")
	{
		WarpX1 = WarpX(Background,0,
			0.01*(100.0-Interpolation)*x+0.01*Interpolation*((x>width/4)&&(x<3*width/4)?width/2+y*fabs(cosd(x*360.0/width)):width/2-y*fabs(cosd(x*360.0/width))),
			0.01*(100.0-Interpolation)*y+0.01*Interpolation*((x<width/2)?height/2+y*fabs(sind(x*360.0/width)):height/2-y*fabs(sind(x*360.0/width))));
		Final = Scale(WarpX1,1,-1);
	}	
	return Final;
}

//ADBE Blend
image AE_Blend(
	image	Background,
	image	Matte,
	string	Mode,
	float	Percent,
	string	Adjust)
{
	if (Mode == "Lighten")
	{
		Final = Max(Background, Matte, 0,100-Percent);
	}	
	else if (Mode == "Darken")
	{
		Final = Min(Background, Matte, 0,100-Percent);
	}	
	else if (Mode == "Fade")
	{
		Final = Mix(Background, Matte, 0,100-Percent,"rgba");
	}	
	else if (Mode == "Color Only")
	{
		ColorSpace1 = ColorSpace(Matte, "rgb", "hls", 0.3, 0.59, 0.11);
		ColorSpace2 = ColorSpace(Background, "rgb", "hls", 0.3, 0.59, 0.11);
		Copy1 = Copy(ColorSpace1, ColorSpace2, 1, "g");
		ColorSpace3 = ColorSpace(Copy1, "hls", "rgb", 0.3, 0.59, 0.11);
		Over1 = Over(ColorSpace3, Background, 1, 0, 0);
		Final = Mix(Background, Over1, 1, 100-Percent/2.0, "rgba");
	}	
	else if (Mode == "Hue Only")
	{
		Screen1 = Screen(Background,Matte);
		Final = Mix(Background, Screen1, 1, 100-Percent/2.0, "rgba");
	}
	return Final;
}

//ADBE Minimax
image AE_Minimax(
	image	Background,
	string	Operation,
	float	Radius,
	string	Channels,
	string	Direction)
{
	float xRadius;
	float yRadius;
	
	if (Direction == "Horizontal Only")
	{
		xRadius = Radius;
		yRadius = 0.0;
	}
	else if (Direction == "Vertical Only")
	{
		xRadius = 0.0;
		yRadius = Radius;
	}
	else if (Direction == "Horizontal and Vertical")
	{
		xRadius = Radius;
		yRadius = Radius;
	}
	if (Operation == "Maximum")
	{
		DilateErode1 = DilateErode(Background, Channels, xRadius, yRadius, 0, 0, 0);
	}
	else if (Operation == "Minimum")
	{
		DilateErode1 = DilateErode(Background, Channels, -xRadius, -yRadius, 0, 0, 0);
	}
	else if (Operation == "Minimum then Maximum")
	{
		DilateErode2 = DilateErode(Background, Channels, -xRadius, -yRadius, 0, 0, 0);
		DilateErode1 = DilateErode(DilateErode2, Channels, xRadius, yRadius, 0, 0, 0);
	}
	else if (Operation == "Maximum then Minimum")
	{
		DilateErode2 = DilateErode(Background, Channels, xRadius, yRadius, 0, 0, 0);
		DilateErode1 = DilateErode(DilateErode2, Channels, -xRadius, -yRadius, 0, 0, 0);
	}
	return DilateErode1;
}

//ADBE Alpha Levels2
image AE_AlphaLevels2(
	image	Background,
	float	BlackInput,	float	WhiteInput,
	float	Gamma,
	float	BlackOutput,	float	WhiteOutput)
{
	Expand1 = Expand(Background, 0, 0, 0, BlackInput/255.0, 1, 1, 1, WhiteInput/255.0);
	Gamma1 = Gamma(Expand1, 1, 1, 1, Gamma);
	Compress1 = Compress(Gamma1, 0, 0, 0, BlackOutput/255.0, 1, 1, 1, WhiteOutput/255.0);
	return Compress1;
}

//ADBE Compound Arithmetic
image AE_CompoundArithmetic(
	image	Background,
	image	SecondSource,
	string	Operation,
	string	Channels,
	string	OverflowMethod,
	int	Adapt,
	float	Percent)
{
	if (Adapt == 1.0)
		Source = Resize(SecondSource,Background.width,Background.height);
	else
		Source = SecondSource;
	if (Operation == "Copy")
	{
		Copy(Background,Source,1,Channels);	
	}
	else if (Operation == "Add")
	{
		if (Channels == "rgb")
			Final = LayerX(	Background, Source,r+r2,g+g2,b+b2,a,z);
		else if (Channels == "rgba")
			Final = LayerX(	Background, Source,r+r2,g+g2,b+b2,a+a2,z);
		else if (Channels == "a")
			Final = LayerX(	Background, Source,r,g,b,a+a2,z);
	}
	else if (Operation == "Sub")
	{
		if (Channels == "rgb")
			Final = LayerX(	Background, Source,r-r2,g-g2,b-b2,a,z);
		else if (Channels == "rgba")
			Final = LayerX(	Background, Source,r-r2,g-g2,b-b2,a-a2,z);
		else if (Channels == "a")
			Final = LayerX(	Background, Source,r,g,b,a-a2,z);
	}
	else if (Operation == "Mult")
	{
		if (Channels == "rgb")
			Final = LayerX(	Background, Source,r*r2,g*g2,b*b2,a,z);
		else if (Channels == "rgba")
			Final = LayerX(	Background, Source,r*r2,g*g2,b*b2,a*a2,z);
		else if (Channels == "a")
			Final = LayerX(	Background, Source,r,g,b,a*a2,z);
	}
	else if (Operation == "Difference")
	{
		if (Channels == "rgb")
			Final = LayerX(	Background, Source,fabs(r-r2),fabs(g-g2),fabs(b-b2),a,z);
		else if (Channels == "rgba")
			Final = LayerX(	Background, Source,fabs(r-r2),fabs(g-g2),fabs(b-b2),fabs(a-a2),z);
		else if (Channels == "a")
			Final = LayerX(	Background, Source,r,g,b,fabs(a-a2),z);
	}
	else if (Operation == "And")
	{
		if (Channels == "rgb")
			Final = LayerX(Background, Source, (((int)(r*255))&((int)(r2*255)))/255.0, (((int)(g*255))&((int)(g2*255)))/255.0, (((int)(b*255))&((int)(b2*255)))/255.0, a, z);
		else if (Channels == "rgba")
			Final = LayerX(Background, Source, (((int)(r*255))&((int)(r2*255)))/255.0, (((int)(g*255))&((int)(g2*255)))/255.0, (((int)(b*255))&((int)(b2*255)))/255.0, (((int)(a*255))&((int)(a2*255)))/255.0, z);	
		else if (Channels == "a")
			Final = LayerX(Background, Source, r,g,b, (((int)(a*255))&((int)(a2*255)))/255.0, z);	
	}
	else if (Operation == "Or")
	{
		if (Channels == "rgb")
			Final = LayerX(Background, Source, (((int)(r*255))|((int)(r2*255)))/255.0, (((int)(g*255))|((int)(g2*255)))/255.0, (((int)(b*255))|((int)(b2*255)))/255.0, a, z);
		else if (Channels == "rgba")
			Final = LayerX(Background, Source, (((int)(r*255))|((int)(r2*255)))/255.0, (((int)(g*255))|((int)(g2*255)))/255.0, (((int)(b*255))|((int)(b2*255)))/255.0, (((int)(a*255))|((int)(a2*255)))/255.0, z);	
		else if (Channels == "a")
			Final = LayerX(Background, Source, r,g,b, (((int)(a*255))|((int)(a2*255)))/255.0, z);	
	}
	else if (Operation == "Xor")
	{
		if (Channels == "rgb")
			Final = LayerX(Background, Source, (((int)(r*255))^((int)(r2*255)))/255.0, (((int)(g*255))^((int)(g2*255)))/255.0, (((int)(b*255))^((int)(b2*255)))/255.0, a, z);
		else if (Channels == "rgba")
			Final = LayerX(Background, Source, (((int)(r*255))^((int)(r2*255)))/255.0, (((int)(g*255))^((int)(g2*255)))/255.0, (((int)(b*255))^((int)(b2*255)))/255.0, (((int)(a*255))^((int)(a2*255)))/255.0, z);	
		else if (Channels == "a")
			Final = LayerX(Background, Source, r,g,b, (((int)(a*255))^((int)(a2*255)))/255.0, z);	
	}
	else if (Operation == "Lighten")
	{
		if (Channels == "rgb")
			Final = LayerX(	Background, Source,r>r2?r:r2,g>g2?g:g2,b>b2?b:b2,a,z);
		else if (Channels == "rgba")
			Final = LayerX(	Background, Source,r>r2?r:r2,g>g2?g:g2,b>b2?b:b2,a>a2?a:a2,z);
		else if (Channels == "a")
			Final = LayerX(	Background, Source,r,g,b,a>a2?a:a2,z);
	}
	else if (Operation == "Darken")
	{
		if (Channels == "rgb")
			Final = LayerX(	Background, Source,r<r2?r:r2,g<g2?g:g2,b<b2?b:b2,a,z);
		else if (Channels == "rgba")
			Final = LayerX(	Background, Source,r<r2?r:r2,g<g2?g:g2,b<b2?b:b2,a<a2?a:a2,z);
		else if (Channels == "a")
			Final = LayerX(	Background, Source,r,g,b,a<a2?a:a2,z);
	}
	else if (Operation == "Minimum")
	{
		if (Channels == "rgb")
			Final = LayerX(	Background, Source,r<r2?r:r2,g<g2?g:g2,b<b2?b:b2,a,z);
		else if (Channels == "rgba")
			Final = LayerX(	Background, Source,r<r2?r:r2,g<g2?g:g2,b<b2?b:b2,a<a2?a:a2,z);
		else if (Channels == "a")
			Final = LayerX(	Background, Source,r,g,b,a<a2?a:a2,z);
	}
	else if (Operation == "Maximum")
	{
		if (Channels == "rgb")
			Final = LayerX(	Background, Source,r>r2?r:r2,g>g2?g:g2,b>b2?b:b2,a,z);
		else if (Channels == "rgba")
			Final = LayerX(	Background, Source,r>r2?r:r2,g>g2?g:g2,b>b2?b:b2,a>a2?a:a2,z);
		else if (Channels == "a")
			Final = LayerX(	Background, Source,r,g,b,a>a2?a:a2,z);
	}
	else if (Operation == "Screen")
	{
		if (Channels == "rgb")
			Final = LayerX(	Background, Source,1.0-(1.0-r)*(1.0-r2),1.0-(1.0-g)*(1.0-g2),1.0-(1.0-b)*(1.0-b2),a,z);
		else if (Channels == "rgba")
			Final = LayerX(	Background, Source,1.0-(1.0-r)*(1.0-r2),1.0-(1.0-g)*(1.0-g2),1.0-(1.0-b)*(1.0-b2),1.0-(1.0-a)*(1.0-a2),z);
		else if (Channels == "a")
			Final = LayerX(	Background, Source,r,g,b,1.0-(1.0-a)*(1.0-a2),z);
	}
	else if (Operation == "Overlay")
	{
		if (Channels == "rgb")
			Final = LayerX(Background, Source, r2<0.5?2*r*r2:1.0-2.0*(1-r)*(1-r2), g2<0.5?2*g*g2:1.0-2.0*(1-g)*(1-g2), b2<0.5?2*b*b2:1.0-2.0*(1-b)*(1-b2), a, z);
		else if (Channels == "rgba")
			Final = LayerX(Background, Source, r2<0.5?2*r*r2:1.0-2.0*(1-r)*(1-r2), g2<0.5?2*g*g2:1.0-2.0*(1-g)*(1-g2), b2<0.5?2*b*b2:1.0-2.0*(1-b)*(1-b2), a2<0.5?2*a*a2:1.0-2.0*(1-a)*(1-a2), z);
		else if (Channels == "a")
			Final = LayerX(Background, Source, r,g,b, a2<0.5?2*a*a2:1.0-2.0*(1-a)*(1-a2), z);
	}
	else if (Operation == "HardLight")
	{
		if (Channels == "rgb")
			Final = LayerX(Background, Source, r<0.5?2*r*r2:1.0-2.0*(1-r)*(1-r2), g<0.5?2*g*g2:1.0-2.0*(1-g)*(1-g2), b<0.5?2*b*b2:1.0-2.0*(1-b)*(1-b2), a, z);	
		else if (Channels == "rgba")
			Final = LayerX(Background, Source, r<0.5?2*r*r2:1.0-2.0*(1-r)*(1-r2), g<0.5?2*g*g2:1.0-2.0*(1-g)*(1-g2), b<0.5?2*b*b2:1.0-2.0*(1-b)*(1-b2), a<0.5?2*a*a2:1.0-2.0*(1-a)*(1-a2), z);	
		else if (Channels == "a")
			Final = LayerX(Background, Source, r,g,b, a<0.5?2*a*a2:1.0-2.0*(1-a)*(1-a2), z);	
	}
	Mix1 = Mix(Final,Background, 1, Percent, "rgba");
	return Mix1;
}

//ADBE Color Emboss
image AE_ColorEmboss(
	image	Background,
	float	Angle,
	float	Height,
	float	Contrast,
	float	Percent)
{
	Emboss1 = Emboss(Background, Contrast/100.0, Angle-90, 30);	
	ISub1 = ISub(Background, Emboss1, 1, 100);
	Screen1 = Screen(Background, ISub1, 1);
	Mix1 = Mix(Screen1,Background, 1, Percent, "rgba");
	return Mix1;
}

//ADBE Tile
image AE_Tile(
	image	Background,
	float	xCenter,	float	yCenter,
	float	JuxtaposeWidth,	float	JuxtaposeHeight,
	float	OutputWidth,	float	OutputHeight,
	int	Mirror,
	float	Phase,
	int	HorizontalPhase)
{
	float	NbHoriz = 100.0/JuxtaposeWidth;
	float	NbVert  = 100.0/JuxtaposeHeight;
	Final = Black(Background.width,Background.height,1);
	Final2 = Black(Background.width,Background.height,1);
	Scale1 = Scale(Background,0.01*JuxtaposeWidth,0.01*JuxtaposeHeight,xCenter,yCenter,0,0.5,0);

	if (HorizontalPhase == 1.0)
	{
		for (int count=-NbHoriz/2-2; count<NbHoriz/2+2; count++)
		{
			Final = Over(Pan(Scale1,count*0.01*JuxtaposeWidth*width, 0, 0, 0.5, 0),Final);
		}
		FinalHoriz = Final;
		
		for (int count2=-NbVert/2-2; count2<NbVert/2+2; count2++)
		{
			Final2 = Over(Pan(FinalHoriz,(count2&1)*Phase/360.0 * Background.width*0.01*JuxtaposeWidth,count2*0.01*JuxtaposeHeight*height,0,0.5,0),Final2);
		}
	}
	else
	{
		for (int count2=-NbVert/2-2; count2<NbVert/2+2; count2++)
		{
			Final = Over(Pan(Scale1,0,count2*0.01*JuxtaposeHeight*height,0,0.5,0),Final);
		}

		FinalVert = Final;

		for (int count=-NbHoriz/2-2; count<NbHoriz/2+2; count++)
		{
			Final2 = Over(Pan(FinalVert,count*0.01*JuxtaposeWidth*width, (count&1)*Phase/360.0 * Background.height*0.01*JuxtaposeHeight, 0, 0.5, 0),Final2);
		}
	}
	
	Crop1 = Crop(Final2, (width-OutputResX)/2, (height-OutputResY)/2, width-(width-OutputResX)/2, height-(height-OutputResY)/2,
			float	OutputResX = Background.width*OutputWidth/100.0,
			float	OutputResY = Background.height*OutputHeight/100.0);
	return Crop1;
}

//ADBE Roughen Edges
image AE_RoughenEdges(
	image	Background,
	string	EdgeType,
	float	rEdge,	float	gEdge,	float	bEdge,
	float	EdgeSize,
	float	Bluriness,
	float	FractalInfluence,
	float	Scale,
	float	IncreaseWidthHeight,
	float	xTurbulenceOffset,	float	yTurbulenceOffset,
	int	Complexity,
	float	Evolution,
	int	CycleEvolution,
	int	Cycle,
	int	Random)
{
	if (EdgeType == "Roughen")
	{
		ColorX1 = ColorX(Background, r, g, b, fnoise2d(x,y,Scale/10.0,Scale/10.0), z);
		Blur3 = Blur(ColorX1,(2.0-Bluriness)*10.0,xPixels, 0, "gauss", xFilter, "rgba");
		Reorder1 = Reorder(Blur3, "aaaa");
		Color1 = Color(width-2*EdgeSize/4, height-2*EdgeSize/4, 1, 1, 1, 1, 1, 0);
		Pan1 = Pan(Color1, EdgeSize/4, EdgeSize/4, 0, 0.5, 0);
		Window1 = Window(Pan1, 0, 0, 720, 486);
		Blur2 = Blur(Window1, EdgeSize/4, xPixels, 0, "gauss", xFilter, "rgba");
		IAdd1 = IAdd(Blur2, Reorder1, 1, 100);
		ContrastLum1 = ContrastLum(IAdd1, 10, 0.567, 0);
		Expand1 = Expand(ContrastLum1, 0, rLo, rLo, 0.33, 1, rHi, rHi, 1);
		Reorder2 = Reorder(Expand1, "rrrr");
		Copy1 = Copy(Background, Reorder2, 1, "a");
		ColorX2 = ColorX(Copy1, r*a, g*a, b*a, a, z);
	}
	else if (EdgeType == "Roughen Color")
	{
		ColorX1 = ColorX(Background, r, g, b, fnoise2d(x,y,Scale/10.0,Scale/10.0), z);
		Blur3 = Blur(ColorX1,(2.0-Bluriness)*10.0,xPixels, 0, "gauss", xFilter, "rgba");
		Reorder1 = Reorder(Blur3, "aaaa");
		Color1 = Color(width-2*EdgeSize/4, height-2*EdgeSize/4, 1, 1, 1, 1, 1, 0);
		Pan1 = Pan(Color1, EdgeSize/4, EdgeSize/4, 0, 0.5, 0);
		Window1 = Window(Pan1, 0, 0, 720, 486);
		Blur2 = Blur(Window1, EdgeSize/4, xPixels, 0, "gauss", xFilter, "rgba");
		IAdd1 = IAdd(Blur2, Reorder1, 1, 100);
		ContrastLum1 = ContrastLum(IAdd1, 10, 0.567, 0);
		Expand1 = Expand(ContrastLum1, 0, rLo, rLo, 0.33, 1, rHi, rHi, 1);
		Reorder2 = Reorder(Expand1, "rrrr");
		Copy1 = Copy(Background, Reorder2, 1, "a");
		ColorX2 = ColorX(Copy1, a==1?r*a:rEdge*a, a==1?g*a:gEdge*a, a==1?b*a:bEdge*a, a, z);
	}
	return ColorX2;	
}

//ADBE ELLIPSE
image AE_Ellipse(
	image	Background,
	float	xCenter,	float	yCenter,
	float	EllipseWidth,	float	EllipseHeight,
	float	Thickness,
	float	Bluriness,
	float	rInternal,	float	gInternal,	float	bInternal,
	float	rExternal,	float	gExternal,	float	bExternal,
	int	Composite)
{
	RGrad1 = RGrad(width, height, 1, xCenter, yCenter, EllipseHeight/EllipseWidth, EllipseWidth/2, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0);
	RGrad2 = RGrad(width, height, 1, xCenter, yCenter, EllipseHeight/EllipseWidth, (EllipseWidth-Thickness)/2, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0);
	ISubA1 = ISubA(RGrad1, RGrad2, 1, 100);
    	ColorX1 = ColorX(ISubA1,a*rInternal,a*gInternal,a*bInternal,a,z);
    	if (Composite == 1.0)
    		Final = Over(ColorX1,Background);
    	else
    		Final = ColorX1;
    	return Final;	
}
	
//ADBE Lens Flare
image AE_LensFlare(
	image	Background,
	float	LightX,		float	LightY,
	float	Intensity,
	string	LensType,
	float	Percent)
{
	if (LensType == "50-300mm (zoom)")
	{
		RGrad1 = RGrad(width, height, 1, LightX, LightY, 1, 0, 50, 0.5, 1, 1, 1, 1, 0, 0.815, 0.14670001, 0.14670001, 0, 0);
		RGrad2 = RGrad(width, height, 1, LightX, LightY, 1, 60, 60, 0.5, 1, 1, 1, 0.2, 0, 0, 0, 0, 0, 0);
		AE_Ellipse1 = AE_Ellipse(0, LightX, LightY, 125, 125, 10, 1, 0.685, 0.07519609, 0.0882798955, 1, 0, 0, 1);
		ColorX1 = ColorX(RGrad1, r*a, g*a, b*a, a, z);
		ColorX2 = ColorX(0, r, g, b, fnoise(x,10), z);
		AE_PolarCoordinates1 = AE_PolarCoordinates(ColorX2, 100, "RectToPolar");
		Scale1 = Scale(AE_PolarCoordinates1, yScale, width/height, width/2, height/2, 0, 0.5, 0);
		Pan1 = Pan(Scale1, LightX-width/2, LightY-height/2, 0, 0.5, 0);
		Blur1 = Blur(AE_Ellipse1, 20, xPixels, 0, "gauss", xFilter, "rgba");
		LayerX1 = LayerX(Pan1, RGrad2, a*a2, a*a2, a*a2, a*a2, z);
		IAdd1 = IAdd(ColorX1, LayerX1, 1, 100);
		Screen2 = Screen(IAdd1, Blur1, 1);
		Screen1 = Screen(Background, Screen2, 1);

		RGrad3 = RGrad(width, height, 1, width/2, height/2, 1, 15, 35, 0.4, 0, 0, 0, 1, 0, 0.517878234, 0.65, 0.2610305, 0, 0);
		RGrad4 = RGrad(width, height, 1, width/2, height/2, 1, 50, 50, 0, 1, 1, 1, 0.5, 0, 0, 0, 0, 0, 0);
		Invert1 = Invert(RGrad3, "a");
		LayerX2 = LayerX(Invert1, RGrad4, r*a2*a, g*a2*a, b*a2*a, a2*a, z);
		Blur2 = Blur(LayerX2, 20, xPixels, 0, "gauss", xFilter, "rgba");
		Pan2 = Pan(Blur2,width/2-LightX,height/2-LightY,0,0.5,0);
		Screen3 = Screen(Screen1,Pan2,1);

		RGrad5 = RGrad(width, height, 1, width/2, height/2, 1, 25, 0, 0.5, 0.284388363, 0.320453674, 1, 0.1, 0, 0, 0, 0, 0.1, 0);
		ColorX3 = ColorX(RGrad5, r*a, g*a, b*a, a, z);
		Pan3 = Pan(ColorX3,(LightX-width/2)/2,(LightY-height/2)/2,0,0.5,0);
		Screen4 = Screen(Screen3,Pan3,1);

		RGrad6 = RGrad(width, height, 1, width/2, height/2, 1, 15, 0, 0.5, 0.865, 0.6179, 0.2283, 0.1, 0, 0, 0, 0, 0.1, 0);
		ColorX4 = ColorX(RGrad6, r*a, g*a, b*a, a, z);
		Pan4 = Pan(ColorX4,(LightX-width/2)/4,(LightY-height/2)/4,0,0.5,0);
		Screen5 = Screen(Screen4,Pan4,1);
	
		RGrad7 = RGrad(width, height, 1, width/2, height/2, 1, 10, 0, 0.5, 0.284388363, 0.320453674, 1, 0.04, 0, 0, 0, 0, 0.1, 0);
		ColorX5 = ColorX(RGrad7, r*a, g*a, b*a, a, z);
		Pan5 = Pan(ColorX5,(LightX-width/2)/1.7,(LightY-height/2)/1.7,0,0.5,0);
		Screen6 = Screen(Screen5,Pan5,1);
	
		RGrad8 = RGrad(width, height, 1, width/2, height/2, 1, 30, 0, 0.5, 0.284388363, 0.320453674, 1, 0.01, 0, 0, 0, 0, 0.1, 0);
		ColorX6 = ColorX(RGrad8, r*a, g*a, b*a, a, z);
		Pan6 = Pan(ColorX6,(LightX-width/2)/1.85,(LightY-height/2)/1.85,0,0.5,0);
		Screen7 = Screen(Screen6,Pan6,1);
	
		RGrad9 = RGrad(width, height, 1, width/2, height/2, 1, 1, 15, 0.5, 0.7337295, 0.8478403, 1, 1, 0, 0, 0, 0, 0, 0);
		ColorX7 = ColorX(RGrad9, r*a, g*a, b*a, a, z);
		Pan7 = Pan(ColorX7,-(LightX-width/2)/1.7,-(LightY-height/2)/1.7,0,0.5,0);
		Screen8 = Screen(Screen7,Pan7,1);
	
		RGrad10 = RGrad(width, height, 1, width/2, height/2, 1, 15, 0, 1, 0.7960392, 0.9615536, 1, 0.3, 0, 0, 0, 0, 0, 0);
		ColorX8 = ColorX(RGrad10, r*a, g*a, b*a, a, z);
		Pan8 = Pan(ColorX8,-(LightX-width/2)/1.3,-(LightY-height/2)/1.3,0,0.5,0);
		Screen9 = Screen(Screen8,Pan8,1);
	
		RGrad11 = RGrad(width, height, 1, width/2, height/2, 1, 30, 0, 0.5, 1, 0.7967354, 0.4033426, 0.338, 0, 0, 0, 0, 0, 0);
		ColorX9 = ColorX(RGrad11, r*a, g*a, b*a, a, z);
		Pan9 = Pan(ColorX9,-(LightX-width/2)/1.5,-(LightY-height/2)/1.5,0,0.5,0);
		Screen10 = Screen(Screen9, Pan9, 1);
	
		RGrad12 = RGrad(720, 486, 1, 371.3708, 243, 1, 200, 5, 0.5, 1, 1, 1, 0.1, 0, 0, 0, 0, 0, 0);
		ColorX10 = ColorX(RGrad12, r*a, g*a, b*a, a, z);
		Blur10 = Blur(ColorX10, 40, xPixels, 0, "gauss", xFilter, "rgba");
		Pan10 = Pan(Blur10,(LightX-width/2)/0.9,(LightY-height/2)/0.9,0,0.5,0);
		Screen11 = Screen(Screen10, Pan10, 1);
		
		Pan11 = Pan(Blur2,(LightX-width/2)/0.7,(LightY-height/2)/0.7,0,0.5,0);
		Fade1 = Fade(Pan11,0.3);
		Screen12 = Screen(Screen11,Fade1,1);
	
		Mix1 = Mix(Screen12,Background, 1, Percent, "rgba");
	}
	else if (LensType == "35 mm normal")
	{
		RGrad1 = RGrad(width, height, 1, width/2, height/2, 1, min(width,height)/4, min(width,height)/4, 0.5, 1, 1, 1, 1, 0, 0.88, 0.105154157, 0.0967506, 0, 0);
		AE_MotionBlur1 = AE_MotionBlur(RGrad2, 45, 20);
		ColorX1 = ColorX(0, r, g, b, ((int)(x)%(width/12))==1?1:0, z);
		Scale1 = Scale(RGrad1, 0.2, 0.2, width/2, height/2, 0, 0.5, 0);
		Blur1 = Blur(ColorX1, 14.4, xPixels, 0, "gauss", xFilter, "rgba");
		ColorX2 = ColorX(Scale1, r*a, g*a, b*a, a, z);
		AE_PolarCoordinates1 = AE_PolarCoordinates(Blur1, 100, "RectToPolar");
		LayerX1 = LayerX(RGrad1, AE_PolarCoordinates1, r*a2*a, g*a2*a,b*a2*a, a*a2, z);
		IAdd2 = IAdd(ColorX2, LayerX1, 1, 100);
		AE_Ellipse1 = AE_Ellipse(IAdd2, width/2, height/2, 341, 325, 20, 0, 0.345, 0.0910363048, 0.06794545, 1, 0, 0, 0);
		Blur2 = Blur(AE_Ellipse1, 34.6, xPixels, 0, "gauss", xFilter, "rgba");
		IAdd3 = IAdd(IAdd2, Blur2, 1, 100);
		Scale2 = Scale(IAdd3, 0.5, xScale, width/2, height/2, 0, 0.5, 0);
		Pan1 = Pan(Scale2,LightX-width/2,LightY-height/2,0,0.5,0);
		IAdd1 = IAdd(Background, Pan1, 1, 100);

		RGrad2 = RGrad(width, height, 1, 360, 243, 1, 20, 0, 0, 0.3022581, 0.347000569, 0.66, 1, 0, 0, 0, 0, 0, 0);
		Pan2 = Pan(RGrad2,(LightX-width/2)/0.6,(LightY-height/2)/0.6,0,0.5,0);
		AE_MotionBlur1 = AE_MotionBlur(Pan2, atan2d(width/2-LightX,LightY-height/2), distance(LightX,LightY,width/2,height/2)/10);
		IAdd2 = IAdd(AE_MotionBlur1, IAdd1, 1, 100);

		RGrad3 = RGrad(width, height, 1, 360, 243, 1, 7, 0, 0, 0.3022581, 0.347000569, 0.66, 1, 0, 0, 0, 0, 0, 0);
		Pan3 = Pan(RGrad3,(LightX-width/2)/0.7,(LightY-height/2)/0.7,0,0.5,0);
		AE_MotionBlur2 = AE_MotionBlur(Pan3, atan2d(width/2-LightX,LightY-height/2), distance(LightX,LightY,width/2,height/2)/20);
		IAdd3 = IAdd(AE_MotionBlur2, IAdd2, 1, 100);

		RGrad4 = RGrad(width, height, 1, 360, 243, 1, 30, 0, 0, 0.67, 0.37, 1, 0.3, 0, 0, 0, 0, 0, 0);
		Pan4 = Pan(RGrad4,(LightX-width/2)/2,(LightY-height/2)/2,0,0.5,0);
		IAdd4 = IAdd(IAdd3,Pan4, 1, 10);
		
		RGrad5 = RGrad(width, height, 1, width/2, height/2, 1, 100, 25, 0.5, 0.7719806, 1, 0.74, 0.2, 0, 0, 0, 0, 0, 0);
		ColorX5 = ColorX(RGrad5, r*a, g*a, b*a, a, z);
		Pan5 = Pan(ColorX5, -(LightX-width/2), -(LightY-height/2), 0, 0.5, 0);
		Screen5 = Screen(IAdd4, Pan5, 1);
		
		Mix1 = Mix(Screen5,Background, 1, Percent, "rgba");
	}
	else if (LensType == "105 mm normal")
	{
		Mix1 = Mix(0,Background, 1, Percent, "rgba");
	}
	return Mix1;
}

//AE_EASY_LEVELS
image AE_EasyLevels(
	image	Background,
	float	RGBBlackInput,	float	RGBWhiteInput,	float	RGBGamma,	float	RGBBlackOutput,	float	RGBWhiteOutput,
	float	RBlackInput,	float	RWhiteInput,	float	RGamma,		float	RBlackOutput,	float	RWhiteOutput,
	float	GBlackInput,	float	GWhiteInput,	float	GGamma,		float	GBlackOutput,	float	GWhiteOutput,
	float	BBlackInput,	float	BWhiteInput,	float	BGamma,		float	BBlackOutput,	float	BWhiteOutput,
	float	ABlackInput,	float	AWhiteInput,	float	AGamma,		float	ABlackOutput,	float	AWhiteOutput)
{
	// RGB
	Expand1 = Expand(Background, RGBBlackInput/255.0, RGBBlackInput/255.0, RGBBlackInput/255.0, RGBBlackInput/255.0, RGBWhiteInput/255., RGBWhiteInput/255., RGBWhiteInput/255., RGBWhiteInput/255.0);
	Gamma1 = Gamma(Expand1, RGBGamma, RGBGamma, RGBGamma, RGBGamma);
	Compress1 = Compress(Gamma1, RGBBlackOutput/255., RGBBlackOutput/255., RGBBlackOutput/255., RGBBlackOutput/255.0, RGBWhiteOutput/255.0, RGBWhiteOutput/255.0, RGBWhiteOutput/255.0, RGBWhiteOutput/255.0);
	// R
	Expand2 = Expand(Compress1, RBlackInput/255.0, 0, 0, 0, RWhiteInput/255.0, 1, 1, 1);
	Gamma2 = Gamma(Expand2, RGamma, 1, 1, 1);
	Compress2 = Compress(Gamma2, RBlackOutput/255.0, 0, 0, 0, RWhiteOutput/255.0, 1, 1, 1);
	// G
	Expand3 = Expand(Compress2, 0, GBlackInput/255.0, 0, 0, 1, GWhiteInput/255.0, 1, 1);
	Gamma3 = Gamma(Expand3, 1, GGamma, 1, 1);
	Compress3 = Compress(Gamma3, 0, GBlackOutput/255.0, 0, 0, 1, GWhiteOutput/255.0, 1, 1);
	// B
	Expand4 = Expand(Compress3, 0, 0, BBlackInput/255.0, 0, 1, 1, BWhiteInput/255.0, 1);
	Gamma4 = Gamma(Expand4, 1, 1, BGamma, 1);
	Compress4 = Compress(Gamma4, 0, 0, BBlackOutput/255.0, 0, 1, 1, BWhiteOutput/255.0, 1);
	// A
	Expand5 = Expand(Compress4, 0, 0, 0, ABlackInput/255.0, 1, 1, 1, AWhiteInput/255.0);
	Gamma5 = Gamma(Expand5, 1, 1, 1, AGamma);
	Compress5 = Compress(Gamma5, 0, 0, 0, ABlackOutput/255.0, 1, 1, 1, AWhiteOutput/255.0);
	return Compress5;
}
	
//ADBE Posterize
image AE_Posterize(
	image	Background,
	int	Levels)
{
	ColorX1 = ColorX(Background,
				((int)(r*Levels))/((float)(Levels*1.0)),
				((int)(g*Levels))/((float)(Levels*1.0)),
				((int)(b*Levels))/((float)(Levels*1.0)),
				a,z);
	return ColorX1;
}

//ADBE Gradient Wipe
image AE_GradientWipe(
	image	Background,
	image	SourceMask,
	float	Percent,
	float	BlurAmount,
	string	Disposition,
	int	Invert)
{
	ColorSpace1 = ColorSpace(SourceMask, "rgb", "hls", 0.3, 0.59, 0.11);
	ColorX1 = ColorX(ColorSpace1, r, g>=Percent/100.0?1:0, b, a, z);
	if (Invert == 1)
		Invert1 = Invert(ColorX1,"rgba");
	else
		Invert1 = ColorX1;
	Blur1 = Blur(Invert1,BlurAmount,xPixels);
	LayerX1 = LayerX(Background,Blur1,r*g2,g*g2,b*g2,g2,z);
	return LayerX1;
}

//ADBE IRIS_WIPE
image AE_IrisWipe(
	image	Background,
	float	PosX,	float	PosY,
	int	NbPoints,
	float	ExternalRadius,
	int	UseInternalRadius,
	float	InternalRadius,
	float	Rotation,
	float	BlurAmount)
{
	IrisMask = NGLRender(
			Background.width, Background.height, 1,
			"
			nglPushMatrix();
			nglColor4f(1,1,1,1);
			float NumberOfPoints = NbPoints & 0xFFFFFFFE;
			float Internal_Radius;
			if (UseInternalRadius == 1.0)
				Internal_Radius = InternalRadius;
			else
				Internal_Radius = ExternalRadius;
			for (int count=0;count<NumberOfPoints;count++)
			{
				if ((count&1) == 0)
				{
					nglBegin(NGL_POLYGON);
					nglVertex2f(PosX,PosY);
					nglVertex2f(PosX+ExternalRadius*cosd(count*360.0/NumberOfPoints+Rotation),PosY+ExternalRadius*sind(count*360.0/NumberOfPoints+Rotation));
					nglVertex2f(PosX+Internal_Radius*cosd((count+1)*360.0/NumberOfPoints+Rotation),PosY+Internal_Radius*sind((count+1)*360.0/NumberOfPoints+Rotation));
					nglEnd();
				}
				else
				{
					nglBegin(NGL_POLYGON);
					nglVertex2f(PosX,PosY);
					nglVertex2f(PosX+Internal_Radius*cosd(count*360.0/NumberOfPoints+Rotation),PosY+Internal_Radius*sind(count*360.0/NumberOfPoints+Rotation));
					nglVertex2f(PosX+ExternalRadius*cosd((count+1)*360.0/NumberOfPoints+Rotation),PosY+ExternalRadius*sind((count+1)*360.0/NumberOfPoints+Rotation));
					nglEnd();
				}
			}
			nglPopMatrix();
			");
	Invert1 = Invert(IrisMask,"rgba");
	Blur1 = Blur(Invert1,BlurAmount,xPixels);
	LayerX1 = LayerX(Background,Blur1,r*a2,g*a2,b*a2,a2);
	return LayerX1;
}	
	
	
//ADBE Luma Key
image AE_LumaKey(
	image	Background,
	string	PixelMask,
	int	Threshold,
	int	Tolerance,
	int	Edges,
	int	BlurAmount)
{
	ColorSpace1 = ColorSpace(Background, "rgb", "hls", 0.3, 0.59, 0.11);

	if (PixelMask == "Bright Pixels")
	{
		ColorX1 = ColorX(ColorSpace1,0,0,0,g>(Threshold-Tolerance)/255.0?0:a);
	}	
	else if (PixelMask == "Dark Pixels")
	{
		ColorX1 = ColorX(ColorSpace1,0,0,0,g<(Threshold+Tolerance)/255.0?0:a);
	}	
	else if (PixelMask == "Same Pixels")
	{
		ColorX1 = ColorX(ColorSpace1,0,0,0,fabs(g-Threshold/255.0)<=Tolerance/255.0?0:a);
	}	
	else if (PixelMask == "Different Pixels")
	{
		ColorX1 = ColorX(ColorSpace1,0,0,0,fabs(g-Threshold/255.0)>=Tolerance/255.0?0:a);
	}
	DilateErode1 = DilateErode(ColorX1, "a", Edges, xPixels, 0, 0, 0);
	Blur1 = Blur(DilateErode1,BlurAmount,xPixels);
	LayerX1 = LayerX(Background,Blur1,r*a2,g*a2,b*a2,a2);
	return LayerX1;
}

//ADBE Extract
image AE_Extract(
	image	Background,
	string	Channel,
	int	BlackPoint,		int	WhitePoint,
	int	BlackTransparency,	int	WhiteTransparency,
	int	Invert)
{
	if (Channel == "Luminance")
	{
		ColorSpace1 = ColorSpace(Background, "rgb", "hls", 0.3, 0.59, 0.11);
		ColorX1 = ColorX(ColorSpace1,r,g,b,
				(g<=(BlackPoint-BlackTransparency)/255.0)?0:
				(g<=BlackPoint/255.0)&&(g>=(BlackPoint-BlackTransparency)/255.0)?(g*255.0-(BlackPoint-BlackTransparency))/BlackTransparency:
				(g>=BlackPoint/255.0)&&(g<=WhitePoint/255.0)?1:
				(g>=WhitePoint/255.0)&&(g<=(WhitePoint+WhiteTransparency)/255.0)?1.0-(g*255.0-WhitePoint)/WhiteTransparency:0);
	}
	else if (Channel == "Red")
	{
		ColorX1 = ColorX(Background,r,g,b,
				(r<=BlackPoint/255.0-BlackTransparency/255.0)?0:
				(r<=BlackPoint/255.0)&&(r>=BlackPoint/255.0-BlackTransparency/255.0)?(r*255.0-BlackPoint+BlackTransparency)/BlackTransparency:
				(r>=BlackPoint/255.0)&&(r<=WhitePoint/255.0)?1:
				(r>=WhitePoint/255.0)&&(r<=WhitePoint/255.0+WhiteTransparency/255.0)?1.0-(r*255.0-WhitePoint)/WhiteTransparency:0);
	}
	else if (Channel == "Green")
	{
		ColorX1 = ColorX(Background,r,g,b,
				(g<=BlackPoint/255.0-BlackTransparency/255.0)?0:
				(g<=BlackPoint/255.0)&&(g>=BlackPoint/255.0-BlackTransparency/255.0)?(g*255.0-BlackPoint+BlackTransparency)/BlackTransparency:
				(g>=BlackPoint/255.0)&&(g<=WhitePoint/255.0)?1:
				(g>=WhitePoint/255.0)&&(g<=WhitePoint/255.0+WhiteTransparency/255.0)?1.0-(g*255.0-WhitePoint)/WhiteTransparency:0);
	}
	else if (Channel == "Blue")
	{
		ColorX1 = ColorX(Background,r,g,b,
				(b<=BlackPoint/255.0-BlackTransparency/255.0)?0:
				(b<=BlackPoint/255.0)&&(b>=BlackPoint/255.0-BlackTransparency/255.0)?(b*255.0-BlackPoint+BlackTransparency)/BlackTransparency:
				(b>=BlackPoint/255.0)&&(b<=WhitePoint/255.0)?1:
				(b>=WhitePoint/255.0)&&(b<=WhitePoint/255.0+WhiteTransparency/255.0)?1.0-(b*255.0-WhitePoint)/WhiteTransparency:0);
	}
	else if (Channel == "Alpha")
	{
		ColorX1 = ColorX(Background,r,g,b,
				(a<=BlackPoint/255.0-BlackTransparency/255.0)?0:
				(a<=BlackPoint/255.0)&&(a>=BlackPoint/255.0-BlackTransparency/255.0)?(a*255.0-BlackPoint+BlackTransparency)/BlackTransparency:
				(a>=BlackPoint/255.0)&&(a<=WhitePoint/255.0)?1:
				(a>=WhitePoint/255.0)&&(a<=WhitePoint/255.0+WhiteTransparency/255.0)?1.0-(a*255.0-WhitePoint)/WhiteTransparency:0);
	}
	
	if (Invert == 1)
		Invert1 = Invert(ColorX1);
	else
		Invert1 = ColorX1;
	
	LayerX1 = LayerX(Background,Invert1,r*a2,g*a2,b*a2,a2,z);
		
	return LayerX1;
}

//ADBE Color Key
image AE_ColorKey(
	image	Background,
	float	rKey,	float	gKey,	float	bKey,
	int	Tolerance,
	int	Edges,
	int	BlurAmount)
{
	ColorX1 = ColorX(Background,fabs(r-rKey)<=Tolerance,(fabs(g-gKey)<=Tolerance/255.0),(fabs(b-bKey)<=Tolerance/255.0),(fabs(r-rKey)<=Tolerance/255.0)&&(fabs(g-gKey)<=Tolerance/255.0)&&(fabs(b-bKey)<=Tolerance/255.0)?0:a,z);
	DilateErode1 = DilateErode(ColorX1, "a", Edges, xPixels, 0, 0, 0);
	Blur1 = Blur(DilateErode1,BlurAmount,xPixels);
	LayerX1 = LayerX(Background,Blur1,r*a2,g*a2,b*a2,a2,z);
	return LayerX1;
}

//ADBE Color Range
image AE_ColorRange(
	image	Background,
	int	Tolerance,
	string	Color_Space,
	int	MinLYR,		int	MaxLYR,
	int	MinaUG,		int	MaxaUG,
	int	MinbVB,		int	MaxbVB)
{
	if (Color_Space == "Lab")
	{
		// RGB -> XYZ
		ColorX1 = ColorX(Background,
					0.5893*r + 0.1789*g + 0.1831*b,
					0.2904*r + 0.6051*g + 0.1045*b,
					0.0000*r + 0.0684*g + 1.0202*b,
					a,z);
		float	Xn = 95.13;
		float	Yn = 100.0;
		float	Zn = 108.86;
		
		Final = ColorX(Background,
					g/Yn>=0.008856?
						116.0*pow(g/Yn,1/3)-16:
						903.3*g/Yn,
					500.0*((r/Xn>=0.008856?pow(r/Xn,1/3):7.87*r/Xn+16/116)-(g/Yn>=0.008856?pow(g/Yn,1/3):7.87*g/Yn+16/116)),
					200.0*((g/Yn>=0.008856?pow(g/Yn,1/3):7.87*g/Yn+16/116)-(b/Zn>=0.008856?pow(b/Zn,1/3):7.87*b/Zn+16/116)),
					a,z);
	}
	else if (Color_Space == "YUV")
	{
		ColorSpace1 = ColorSpace(Background, "rgb", "yuv", 0.3, 0.59, 0.11);
		ColorX1 = ColorX(ColorSpace1,r,g,b,(r>=MinLYR/255.0)&&(r<=MaxLYR/255.0)&&(g>=MinaUG/255.0)&&(g<=MaxaUG/255.0)&&(b>=MinbVB/255.0)&&(b<=MaxbVB/255.0)?0:a,z);
		Final = LayerX(Background,ColorX1,r*a2,g*a2,b*a2,a2);
	}
	else if (Color_Space == "RGB")
	{
		ColorX1 = ColorX(Background,r,g,b,(r>=MinLYR/255.0)&&(r<=MaxLYR/255.0)&&(g>=MinaUG/255.0)&&(g<=MaxaUG/255.0)&&(b>=MinbVB/255.0)&&(b<=MaxbVB/255.0)?0:a,z);
		Final = LayerX(Background,ColorX1,r*a2,g*a2,b*a2,a2);
	}
	return Final;					
}

//ADBE Difference Matte2
image AE_DifferenceMatte(
	image	Background,
	image	MatteReference,
	string	Adjust,
	float	Tolerance,
	float	BlurAmount,
	float	PreBlur)
{
	Blur1 = Blur(Background,PreBlur,xPixels);
	Blur2 = Blur(MatteReference,PreBlur,xPixels);
	ISubA1 = ISubA(Blur1, Blur2, 1, 100);
	Monochrome1 = Monochrome(ISubA1, 0, 0, 0);
	ColorX1 = ColorX(Monochrome1,r>=2.5*Tolerance/255.0?1:0,r>=2.5*Tolerance/255.0?1:0,r>=2.5*Tolerance/255.0?1:0,r>=2.5*Tolerance/255.0?1:0,z);
	Blur3 = Blur(ColorX1,BlurAmount,xPixels);
	LayerX1 = LayerX(Background,Blur3,r*a2,g*a2,b*a2,a2);
	return LayerX1;
}

//ADBE Linear Color Key2
image AE_LinearColorKey(
	image	Background,
	float	rKey,	float	gKey,	float	bKey,
	string	CompareSpace,
	float	Tolerance,
	float	BlurAmount,
	string	Operation)
{
	Blur1 = Blur(Background,BlurAmount,xPixels);
	if (CompareSpace == "RGB")
	{
		ColorX1 = ColorX(Blur1,r,g,b,(fabs(r-rKey)<=3.125*Tolerance/255.0)&&(fabs(g-gKey)<=3.125*Tolerance/255.0)&&(fabs(b-bKey)<=3.125*Tolerance/255.0)?0:1,z);
	}
	else if (CompareSpace == "Hue")
	{
		// Compute H Key Value
		float H;
		float max_v=max3(rKey,gKey,bKey);
		float min_v=min3(rKey,gKey,bKey);
		float rdist=(max_v-rKey)/(max_v-min_v);
		float gdist=(max_v-gKey)/(max_v-min_v);
		float bdist=(max_v-bKey)/(max_v-min_v);
		if (rKey == max_v)
			H = 60.0*(bdist - gdist);
		else if (gKey == max_v)
			H = 60.0*(2 + rdist - bdist);
		else 
			H = 60.0*(4 + gdist - rdist);
		if (H<0)
			H = H + 360.0;
		ColorSpace1 = ColorSpace(Blur1, "rgb", "hsv", 0.3, 0.59, 0.11);
		ColorX1 = ColorX(ColorSpace1,r,g,b,(fabs(r-H/360.0)<=Tolerance/360.0)?0:1,z);
	}
	else if (CompareSpace == "Chrominance")
	{
		ColorX1 = ColorX(Blur1,r,g,b,(fabs((1.0-r)-(1.0-rKey))<=3.125*Tolerance/255.0)?0:1,z);
	}
	if (Operation == "Mask")
		LayerX1 = LayerX(Background,ColorX1,r*a2,g*a2,b*a2,a2);
	else
		LayerX1 = Background;
	return LayerX1;
}

//ADBE Compound Blur 
image AE_CompoundBlur(
	image	Background,
	image	CtrlImg,
	int	Maximum,
	int	Adapt,
	int	Inversion)
{
	if (Adapt == 1)
		Resize1 = Resize(CtrlImg, Background.width, Background.height, "default", 0);
	else
		Resize1 = CtrlImg;
	ColorSpace1 = ColorSpace(Resize1, "rgb", "hls", 0.3, 0.59, 0.11);
	Reorder1 = Reorder(ColorSpace1, "gggg");
	if (Inversion == 1)
		Invert1 = Invert(Reorder1);
	else
		Invert1 = Reorder1;
	IBlur1 = IBlur(Background, Invert1, 3*Maximum, xPixels, 0, "gauss", xFilter, 5, 1, "A", "rgba", 0);
	return IBlur1;
}

//ADBE Fast Blur
image AE_FastBlur(
	image	Background,
	float	Intensity,
	string	Direction)
{
	float	XBlur = Intensity;
	float	YBlur = Intensity;
	if (Direction == "Vertical")
		XBlur = 0;
	else if (Direction == "Horizontal")
		YBlur = 0;
	Blur1 = Blur (Background,XBlur,YBlur);
	return Blur1;			
}

//ADBE Unsharp Mask
image AE_UnsharpMask(
	image	Background,
	float	Amount,
	float	Radius,
	int	Threshold)
{
	Sharpen1 = Sharpen(Background, Amount, 4*Radius, 4*xPixels, "rgba");
	return Sharpen1;
}

//ADBE Simple Choker
image AE_SimpleChoker(
	image	Background,
	string	Output,
	float	ChokeMatte)
{
	DilateErode1 = DilateErode(Background, "a", -ChokeMatte/2, -ChokeMatte/2, 0, 0, 0);
	if (Output == "Final Output")
		ColorX1 = ColorX(DilateErode1,r*a,g*a,b*a,a,z);
	else
		ColorX1 = ColorX(DilateErode1,a,a,a,a,z);
	return ColorX1;
}

//ADBE Matte Choker
image AE_MatteChoker(
	image	Background,
	float	GeomSoftness1,	int	Choke1,	float	GrayLevelSoftness1,	
	float	GeomSoftness2,	int	Choke2,	float	GrayLevelSoftness2,
	int	Iterations)
{
	DilateErode1 = Background;
	DilateErode2 = Background;
	Blur1 = Background;
	Blur2 = Background;
	for (int count=0 ; count<Iterations ; count++)
	{
		DilateErode1 = DilateErode(Blur2,"a",-2*GeomSoftness1*Choke1/255.0,-2*GeomSoftness1*Choke1/255.0,0,0,0);
		Blur1 = Blur(DilateErode1,10*GrayLevelSoftness2/100.0*fabs(Choke1)/255.0,xPixels, 0, "gauss", xFilter, "a");
		DilateErode2 = DilateErode(Blur1,"a",-2*GeomSoftness2*Choke2/255.0,-2*GeomSoftness2*Choke2/255.0,0,0,0);
		Blur2 = Blur(DilateErode2,10*GrayLevelSoftness2/100.0*fabs(Choke2)/255.0,xPixels, 0, "gauss", xFilter, "a");
	}
	ColorX1 = ColorX(Blur2,r*a,g*a,b*a,a,z);
	return ColorX1;
}	

//ADBE CHANNEL MIXER
image AE_ChannelMixer(
	image	Background,
	int	RedRed,		int RedGreen,	int RedBlue,	int RedConst,
	int	GreenRed,	int GreenGreen, int GreenBlue,	int GreenConst,
	int	BlueRed,	int BlueGreen,	int BlueBlue,	int BlueConst,
	int	Monochrome)
{
	if (Monochrome == 1)
	{
		ColorX1 = ColorX(Background,
				r*RedRed/100.0+g*RedGreen/100.0+b*RedBlue/100.0+RedConst/100.0,	
				r*RedRed/100.0+g*RedGreen/100.0+b*RedBlue/100.0+RedConst/100.0,	
				r*RedRed/100.0+g*RedGreen/100.0+b*RedBlue/100.0+RedConst/100.0,	
				a,z);
	}
	else
	{
		ColorX1 = ColorX(Background,
				r*RedRed/100.0+g*RedGreen/100.0+b*RedBlue/100.0+RedConst/100.0,	
				r*GreenRed/100.0+g*GreenGreen/100.0+b*GreenBlue/100.0+GreenConst/100.0,	
				r*BlueRed/100.0+g*BlueGreen/100.0+b*BlueBlue/100.0+BlueConst/100.0,	
				a,z);
	}
	return ColorX1;
}

//AE_HUE_SATURATION
image AE_HueSaturation(
	image	Background,
	string	ChannelControl,
	int	MasterHue,	int	MasterSaturation,	int	MasterLightness,
	int	Colorize,
	int	ColorizeHue,	int	ColorizeSaturation,	int	ColorizeLightness)
{
	ColorSpace1 = ColorSpace(Background, "rgb", "hls", 0.3, 0.59, 0.11);
	ColorX1 = ColorX(ColorSpace1, r+MasterHue/360.0, g+MasterLightness/100.0, b*exp(MasterSaturation/30.0), a, z);
	ColorSpace2 = ColorSpace(ColorX1, "hls", "rgb", 0.3, 0.59, 0.11);
}


//ADBE Ripple
image AE_Ripple(
	image	Background,
	float	Radius,
	float	PosX,	float	PosY,
	string	Conversion,
	float	WaveSpeed,	float	WaveWidth,	float	WaveHeight,
	float	RipplePhase)
{
	float TheRadius = 7.0*Radius;
	if (Conversion == "Asymmetric")
	{
		ColorX1 = ColorX(Background,
				distance(x,y,PosX,PosY)>=TheRadius?0.5:
				0.5+0.5*(1.0-distance(x,y,PosX,PosY)/TheRadius)*sin(M_PI*RipplePhase/180.0+2*M_PI*(-WaveSpeed*time+distance(x,y,PosX,PosY))/(3*WaveWidth)),
				g, 
				b, 
				a, 
				z);
		IDisplace1 = IDisplace(Background, ColorX1, WaveHeight, xScale, 0.5, xDOffset, "R", "R", 0, xDelta);
	}
	else
	{
		ColorX1 = ColorX(Background,
				distance(x,y,PosX,PosY)>=TheRadius?0.5:
				0.5+sind(atan2d(x-PosX,y-PosY))*0.5*(1.0-distance(x,y,PosX,PosY)/TheRadius)*sin(M_PI*RipplePhase/180.0+2*M_PI*(-WaveSpeed*time+distance(x,y,PosX,PosY))/(3*WaveWidth)),
				distance(x,y,PosX,PosY)>=TheRadius?0.5:
				0.5+cosd(atan2d(x-PosX,y-PosY))*0.5*(1.0-distance(x,y,PosX,PosY)/TheRadius)*sin(M_PI*RipplePhase/180.0+2*M_PI*(-WaveSpeed*time+distance(x,y,PosX,PosY))/(3*WaveWidth)),
				b, 
				a, 
				z);
		IDisplace1 = IDisplace(Background, ColorX1, WaveHeight, xScale, 0.5, xDOffset, "R", "G", 0, xDelta);
	}
	IDisplace2 = Crop(IDisplace1);
	Over1 = Over(IDisplace2, Background, 1, 1, 0);
	return Over1;
}


image AE_DrawParticle(
	int	thewidth,	int	theheight,
	float	PosX,		float	PosY,
	float	rParticle,	float	gParticle,	float	bParticle,
	float	Radius)
{
	Particle = NGLRender(
			thewidth, theheight, 1,
			"
			nglPushMatrix();
			nglColor4f(rParticle,gParticle,bParticle,1);
			nglBegin(NGL_POLYGON);
				nglVertex2f(PosX-Radius/2,PosY-Radius/2);
				nglVertex2f(PosX+Radius/2,PosY-Radius/2);
				nglVertex2f(PosX+Radius/2,PosY+Radius/2);
				nglVertex2f(PosX-Radius/2,PosY+Radius/2);
			nglEnd();
			nglPopMatrix();
			");
	return Particle;		
}

image AE_ParticlePlayground(
	image	Background,
	float	PosX,		float	PosY,
	float	BarrelRadius,
	float	ParticlesPerSecond,
	float	Direction,	float	DirectionRandom,
	float	Velocity,	float	VelocityRandom,
	float	rParticle,	float	gParticle,		float	bParticle,
	float	ParticleRadius,
	image	LayerMap,
	string	TimeOffsetType,	float	TimeOffset,
	string	LayerMapAffect,
	float	GravityForce,	float	GravityForceSpread,	float	GravityDirection)
{
	if (LayerMapAffect == "None")
	{
		Final = NGLRender(
			Background.width,Background.height, 1,
			"
			nglPushMatrix();
			nglColor4f(rParticle,gParticle,bParticle,1);
			for (int count=1; count<=time*ParticlesPerSecond/framesPerSecond; count++)
			{
				float ThePosX = PosX+(-0.5+rnd(count))*BarrelRadius + 0.1*(Velocity+(-0.5+rnd(2*count))*VelocityRandom) * cosd(Direction+(-0.5+rnd(3*count))*2*DirectionRandom) * (time-count/ParticlesPerSecond*framesPerSecond) + 0.5*(GravityForce+(-0.5+rnd(4*count))*GravityForceSpread)*sind(GravityDirection)*sqr(time-count/ParticlesPerSecond*framesPerSecond)/100.0;
				float ThePosY = PosY + 0.1*(Velocity+(-0.5+rnd(2*count))*VelocityRandom) * sind(Direction+(-0.5+rnd(3*count))*2*DirectionRandom)*(time-count/ParticlesPerSecond*framesPerSecond) + 0.5*(GravityForce+(-0.5+rnd(count))*GravityForceSpread)*cosd(GravityDirection)*sqr(time-count/ParticlesPerSecond*framesPerSecond)/100.0;
				if ((ThePosX>-ParticleRadius) && (ThePosX<=Background.width+ParticleRadius)
				 && (ThePosY>-ParticleRadius) && (ThePosY<=Background.height+ParticleRadius))
				{
					nglBegin(NGL_POLYGON);
					nglVertex2f(ThePosX-ParticleRadius,ThePosY-ParticleRadius);
					nglVertex2f(ThePosX+ParticleRadius,ThePosY-ParticleRadius);
					nglVertex2f(ThePosX+ParticleRadius,ThePosY+ParticleRadius);
					nglVertex2f(ThePosX-ParticleRadius,ThePosY+ParticleRadius);
					nglEnd();
				}
			}
			nglPopMatrix();
			");
	}
	else
	{		
		Final = Black(Background.width,Background.height,1);
		for (int count=1; count<=time*ParticlesPerSecond/framesPerSecond; count++)
		{
			Final = IAdd(		Move2D(	LayerMap,
							      //------------X0--------------------- + -----------------V0---------------------------- * ------------------Cos(Theta)----------------------- * ----------time--------------------------------- + --------------------------------------0.5*g*time^2-------------------------------------------------------------------------------------
								PosX+(-0.5+rnd(count))*BarrelRadius + 0.1*(Velocity+(-0.5+rnd(2*count))*VelocityRandom) * cosd(Direction+(-0.5+rnd(3*count))*2*DirectionRandom) * (time-count/ParticlesPerSecond*framesPerSecond) + 0.5*(GravityForce+(-0.5+rnd(4*count))*GravityForceSpread)*sind(GravityDirection)*sqr(time-count/ParticlesPerSecond*framesPerSecond)/100.0,
							      //-Y0- + -----------------V0---------------------------- * ------------------Sin(Theta)---------------------- * ----------time-------------------------------- + --------------------------------------0.5*g*time^2-------------------------------------------------------------------------------------
								PosY + 0.1*(Velocity+(-0.5+rnd(2*count))*VelocityRandom) * sind(Direction+(-0.5+rnd(3*count))*2*DirectionRandom)*(time-count/ParticlesPerSecond*framesPerSecond) + 0.5*(GravityForce+(-0.5+rnd(count))*GravityForceSpread)*cosd(GravityDirection)*sqr(time-count/ParticlesPerSecond*framesPerSecond)/100.0,
								0,1,1,1,0,0,
								width/2,height/2,
								"default",xFilter,
								"trsx",0,
								0,0.5,0,
								0,time),
						Final);
		}
	}
	return Final;
}

//ADBE Stroke
image AE_Stroke(
	image	Background,
	image	Path,
	float	rColor,	float	gColor,	float	bColor,
	float	BrushSize,
	float	BrushHardness,
	float	Opacity,
	string	PaintStyle)
{
	DilateErode1 = DilateErode(Path, "rgba", -BrushSize/2, xPixels, 0, 0, 0);
	DilateErode2 = DilateErode(Path, "rgba", BrushSize/2, xPixels, 0, 0, 0);
	ISub1 = ISub(DilateErode2, DilateErode1, 1, 100);
	ColorX1 = ColorX(ISub1,r*rColor*a*Opacity/100.0,g*gColor*a*Opacity/100.0,b*bColor*a*Opacity/100.0,a*Opacity/100.0,z);
	Blur1 = Blur(ColorX1, (100.0-BrushHardness)/100.0*BrushSize, xPixels, 0, "gauss", xFilter, "rgba");
	if (PaintStyle == "On Original Image")
		Final = Over(Blur1,Background);
	else if (PaintStyle == "On Transparent")
		Final = Blur1;
	else
		Final = LayerX(Background,Blur1,r*a2,g*a2,b*a2,a2,z);			
	return Final;
}

//ADBE Optics Compensation
image AE_OpticsCompensation(
	image	Background,
	float	FOV,
	int	ReverseDistorsion,
	string	FOVOrientation,
	float	PosX,	float	PosY,
	int	OptimalPixels,
	string	Resize)
{
	NullImage = Color(Background.width,Background.height,4);
	XYR = ColorX(NullImage,
				(2*x-width)/width,
				(2*y-height)/height,
				sqr((2*x-width)/width)+sqr((2*y-height)/height),0,z);
	if (ReverseDistorsion == 0)
	{
		X2Y2 = ColorX(XYR,
					r/(1-alphax *( sqr(r/(1-alphax*b)) + sqr(g/(1-alphay*b)) ) ),
					g/(1-alphay *( sqr(r/(1-alphax*b)) + sqr(g/(1-alphay*b)) ) ),
					b,a,z,
					float alphax = -0.0008*(90-FOV) + 0.048,
					float alphay = -0.0032*(90-FOV) + 0.187);
		DisplaceMap = ColorX(X2Y2,(r+1)*width/2,(g+1)*height/2,b,a,z);
	}
	else
	{
		X2Y2 = ColorX(XYR,
					r*(1 - alphax * b),
					g*(1 - alphay * b ),
					b,a,z,
					float alphax = -0.0008*(90-FOV) + 0.048,
					float alphay = -0.0032*(90-FOV) + 0.187);
		DisplaceMap = ColorX(X2Y2,(r+1)*width/2,(g+1)*height/2,b,a,z);
	}
	DisplaceX1 = DisplaceX(Background, DisplaceMap, 1, r, g, 0, xDelta);
	return DisplaceX1;
}

//APC Colorama
image AE_Colorama(
	image	Background,
	string	InputPhaseChannel,
	int	UseSecondPhase,
	image	SecondPhaseImage,
	string	AddPhaseFrom,
	string	AddMode,
	int	PhaseShift,
	string	ModifyChannels,
	int	ModifyAlpha,
	int	ChangeEmptyPixels,
	float	rMatchingColor,	float	gMatchingColor,	float	bMatchingColor,
	float	MatchingTolerance,
	float	MatchingSoftness,
	string	MatchingMode,
	image	MaskLayer,
	string	MaskingMode,
	int	Mix,
	int	Percent)
{
	if (InputPhaseChannel == "Intensity")
	{
		ColorSpace1 = ColorSpace(Background, "rgb", "hls", 0.3, 0.59, 0.11);
		FirstPhase = Reorder(ColorSpace1,"gggg");
	}
	else if (InputPhaseChannel == "Red")
		FirstPhase = Reorder(Background,"rrrr");
	else if (InputPhaseChannel == "Green")
		FirstPhase = Reorder(Background,"gggg");
	else if (InputPhaseChannel == "Blue")
		FirstPhase = Reorder(Background,"bbbb");
	else if (InputPhaseChannel == "Hue")
	{
		ColorSpace1 = ColorSpace(Background, "rgb", "hls", 0.3, 0.59, 0.11);
		FirstPhase = Reorder(ColorSpace1,"rrrr");
	}
	else if (InputPhaseChannel == "Lightness")
	{
		ColorSpace1 = ColorSpace(Background, "rgb", "hls", 0.3, 0.59, 0.11);
		FirstPhase = Reorder(ColorSpace1,"gggg");
	}
	else if (InputPhaseChannel == "Saturation")
	{
		ColorSpace1 = ColorSpace(Background, "rgb", "hls", 0.3, 0.59, 0.11);
		FirstPhase = Reorder(ColorSpace1,"bbbb");
	}
	else if (InputPhaseChannel == "Alpha")
	{
		FirstPhase = Reorder(Background,"aaaa");
	}
	else if (InputPhaseChannel == "Zero")
	{
		FirstPhase = ColorX(Background,0,0,0);
	}
	
	if (UseSecondPhase == 0)
	{
		ColorX1 = ColorX(FirstPhase, 
				0.5+0.5*cos(2*M_PI*r+M_PI*PhaseShift/180.0), 
		    		0.5+0.5*cos(2*M_PI*(r+0.66)+M_PI*PhaseShift/180.0),
		    		0.5+0.5*cos(2*M_PI*(r+1.33)+M_PI*PhaseShift/180.0), 
		    		a, z);
	}
	else 
	{
		if (AddPhaseFrom == "Intensity")
		{
			ColorSpace2 = ColorSpace(SecondPhaseImage, "rgb", "hls", 0.3, 0.59, 0.11);
			SecondPhase = Reorder(ColorSpace2,"gggg");
		}
		else if (AddPhaseFrom == "Red")
			SecondPhase = Reorder(SecondPhaseImage,"rrrr");
		else if (AddPhaseFrom == "Green")
			SecondPhase = Reorder(SecondPhaseImage,"gggg");
		else if (AddPhaseFrom == "Blue")
			SecondPhase = Reorder(SecondPhaseImage,"bbbb");
		else if (AddPhaseFrom == "Hue")
		{
			ColorSpace2 = ColorSpace(SecondPhaseImage, "rgb", "hls", 0.3, 0.59, 0.11);
			SecondPhase = Reorder(ColorSpace2,"rrrr");
		}
		else if (AddPhaseFrom == "Lightness")
		{
			ColorSpace2 = ColorSpace(SecondPhaseImage, "rgb", "hls", 0.3, 0.59, 0.11);
			SecondPhase = Reorder(ColorSpace2,"gggg");
		}
		else if (AddPhaseFrom == "Saturation")
		{
			ColorSpace2 = ColorSpace(SecondPhaseImage, "rgb", "hls", 0.3, 0.59, 0.11);
			SecondPhase = Reorder(ColorSpace2,"bbbb");
		}
		else if (AddPhaseFrom == "Alpha")
		{
			SecondPhase = Reorder(SecondPhaseImage,"aaaa");
		}
		else if (AddPhaseFrom == "Zero")
		{
			SecondPhase = ColorX(SecondPhaseImage,0,0,0);
		}
		
		if (AddMode == "Wrap")
		{
			LayerX1 = LayerX(FirstPhase,SecondPhase,r+r2>1.0?r+r2-1.0:r+r2,g,b,a,z);	
		}
		else if (AddMode == "Clamp")
		{
			LayerX1 = LayerX(FirstPhase,SecondPhase,r+r2>1.0?1.0:r+r2,g,b,a,z);	
		}
		else if (AddMode == "Average")
		{
			LayerX1 = LayerX(FirstPhase,SecondPhase,(r+r2)/2,g,b,a,z);	
		}
		else if (AddMode == "Screen")
		{
			LayerX1 = LayerX(FirstPhase,SecondPhase,1.0-(1.0-r)*(1.0-r2),g,b,a,z);	
		}
		
		ColorX1 = ColorX(LayerX1, 
				0.5+0.5*cos(2*M_PI*r+M_PI*PhaseShift/180.0), 
		    		0.5+0.5*cos(2*M_PI*(r+0.66)+M_PI*PhaseShift/180.0),
		    		0.5+0.5*cos(2*M_PI*(r+1.33)+M_PI*PhaseShift/180.0), 
		    		a, z);
	}
	
	if (ModifyChannels == "All")
		ModifiedChannels = ColorX1;
	else if (ModifyChannels == "Red")
		ModifiedChannels = Copy(ColorX1, Background, 1, "gba");
	else if (ModifyChannels == "Green")
		ModifiedChannels = Copy(ColorX1, Background, 1, "rba");
	else if (ModifyChannels == "Blue")
		ModifiedChannels = Copy(ColorX1, Background, 1, "rga");
	else if (ModifyChannels == "RG")
		ModifiedChannels = Copy(ColorX1, Background, 1, "ba");
	else if (ModifyChannels == "GB")
		ModifiedChannels = Copy(ColorX1, Background, 1, "ra");
	else if (ModifyChannels == "RB")
		ModifiedChannels = Copy(ColorX1, Background, 1, "ga");
	else if (ModifyChannels == "RG")
		ModifiedChannels = Copy(ColorX1, Background, 1, "ba");
	else if (ModifyChannels == "None")
		ModifiedChannels = Reorder(ColorX1,"0000");
	if (ModifyAlpha == 0)
		FinalChannels = Copy(ColorX1, Background, 1, "a");
	else
		FinalChannels = ModifiedChannels;

	if (MatchingMode != "Off")
	{
		MatchingColor = ColorX(Background,r,g,b,(fabs(r-rMatchingColor)<=MatchingTolerance)&&(fabs(g-gMatchingColor)<=MatchingTolerance)&&(fabs(b-bMatchingColor)<=MatchingTolerance)?1:0,z);
		MatchingColorMask = LayerX(FinalChannels,MatchingColor,r*a2,g*a2,b*a2,a*a2);
		PixelSelected = Blur(MatchingColorMask,10*MatchingSoftness,xPixels);
	}
	else
		PixelSelected = FinalChannels;

	if (MaskingMode != "Off")
	{
		if (MaskingMode == "Intensity")
		{
			ColorSpace3 = ColorSpace(MaskLayer, "rgb", "hls", 0.3, 0.59, 0.11);
			MaskedChannels = LayerX(PixelSelected,ColorSpace3,r*g2,g*g2,b*g2,a*g2);
		}	
		else if (MaskingMode == "Alpha")
		{
			MaskedChannels = LayerX(PixelSelected,MaskLayer,r*a2,g*a2,b*a2,a*a2);
		}	
		else if (MaskingMode == "Inverted Intensity")
		{
			MaskedChannels = LayerX(PixelSelected,MaskLayer,r*(1.0-g2),g*(1.0-g2),b*(1.0-g2),a*(1.0-g2));
		}	
		else if (MaskingMode == "Inverted Alpha")
		{
			MaskedChannels = LayerX(PixelSelected,MaskLayer,r*(1.0-a2),g*(1.0-a2),b*(1.0-a2),a*(1.0-a2));
		}
	}
	else
		MaskedChannels = PixelSelected;

	if (Mix == 1)
		FinalNode = Over(MaskedChannels,Background);
	else
		FinalNode = MaskedChannels;

	return Mix(FinalNode,Background,1,Percent+1,"rgba");
}

//ADBE Fractal Noise
image AE_FractalNoise(
	image 	Background,
	string	FractalType,
	string	NoiseType,
	int	Invert,
	float	Contrast,
	float	Brightness,
	string	Overflow,
	float	Complexity,
	int	RandomSeed,
	float	Opacity,
	string	TransferMode)
{
	if (FractalType == "Basic")
	{
		ColorX1 = ColorX(Background,
				turbulence2d(x+RandomSeed,y+RandomSeed,(11-Complexity)*50.0,(11-Complexity)*50.0), 
				turbulence2d(x+RandomSeed,y+RandomSeed,(11-Complexity)*50.0,(11-Complexity)*50.0), 
				turbulence2d(x+RandomSeed,y+RandomSeed,(11-Complexity)*50.0,(11-Complexity)*50.0), 
				a, z);	
	}
	
	Bytes1 = Bytes(ColorX1, 4);
	
	if (Invert == 1)
		Invert1 = Invert(Bytes1);
	else
		Invert1 = Bytes1;
	
	ContrastLum1 = ContrastLum(Invert1, Contrast/133.0, 0.5, 0);
	Brightness1 = Brightness(ContrastLum1, exp(Brightness/50.0));
	ColorX2 = ColorX(Brightness1,r*Opacity/100.0,g*Opacity/100.0,b*Opacity/100.0,a*Opacity/100.0);
	
	if (TransferMode == "None")
		FinalNode = Background;
	else 
		FinalNode = AE_Layers(ColorX2,Background,0,TransferMode);

	return FinalNode;
}

//ADBE Laser
image AE_Laser(
	image	Background,
	float	StartX,	float	StartY,
	float	EndX,	float	EndY,
	float	Length,
	float	TimePct,
	float	StartThickness,	float	EndThickness,
	float	rInside,float	gInside,float	bInside,
	int	Composite)
{
	Laser = NGLRender(
			width,height,1,
			"
				float	Vx = EndX-StartX;
				float	Vy = EndY-StartY;
				
				float	SideAngle = 90-atan2d(Vy,Vx);

				float	CenterX = StartX + Vx*TimePct/100.0 + (-1.0+2*TimePct/100.0)*Vx*Length/200.0;
				float	CenterY = StartY + Vy*TimePct/100.0 + (-1.0+2*TimePct/100.0)*Vy*Length/200.0;

				float	P1X = CenterX-Length/200.0*Vx;
				if (P1X<min(StartX,EndX)) 	P1X = min(StartX,EndX);
				else if (P1X>max(StartX,EndX))	P1X = max(StartX,EndX);
				float	P1Y = CenterY-Length/200.0*Vy;
				if (P1Y<min(StartY,EndY))	P1Y = min(StartY,EndY);
				else if (P1Y>max(StartY,EndY))	P1Y = max(StartY,EndY);
				float	P2X = CenterX+Length/200.0*Vx;
				if (P2X<min(StartX,EndX))	P2X = min(StartX,EndX);
				else if (P2X>max(StartX,EndX))	P2X = max(StartX,EndX);
				float	P2Y = CenterY+Length/200.0*Vy;
				if (P2Y<min(StartY,EndY))	P2Y = min(StartY,EndY);
				else if (P2Y>max(StartY,EndY))	P2Y = max(StartY,EndY);
				
				float	CurrentStartThickness = (TimePct/100.0-Length/200.0)*(EndThickness-StartThickness) + StartThickness;
				float	CurrentEndThickness   = (TimePct/100.0+Length/200.0)*(EndThickness-StartThickness) + StartThickness;
				nglPushMatrix();
				nglColor3f(rInside,gInside,bInside);
				nglBegin(NGL_POLYGON);
					nglVertex2f(P1X-CurrentStartThickness*cosd(SideAngle) 	, P1Y+CurrentStartThickness*sind(SideAngle));
					nglVertex2f(P1X+CurrentStartThickness*cosd(SideAngle) 	, P1Y-CurrentStartThickness*sind(SideAngle));
					nglVertex2f(P2X+CurrentEndThickness*cosd(SideAngle) 	, P2Y-CurrentEndThickness*sind(SideAngle));
					nglVertex2f(P2X-CurrentEndThickness*cosd(SideAngle) 	, P2Y+CurrentEndThickness*sind(SideAngle));
				nglEnd();
				nglPopMatrix();
			");
	if (Composite ==1)
		FinalNode = Over(Laser,Background);
	else
		FinalNode = Laser;

	return FinalNode;
}

//ADBE BEZMESH
image AE_BezierMesh(
	image	Background,
	float	TopLeftVertexX,	float TopLeftVertexY,float TopLeftTangentX,float TopLeftTangentY,float	TopRightTangentX,float TopRightTangentY,
	float	RightTopVertexX,float RightTopVertexY,float RightTopTangentX,float RightTopTangentY,float RightBottomTangentX,float RightBottomTangentY,
	float	BottomRightVertexX,float BottomRightVertexY,float BottomRightTangentX,float BottomRightTangentY,float BottomLeftTangentX,float BottomLeftTangentY,
	float	LeftBottomVertexX,float	LeftBottomVertexY,float	LeftBottomTangentX,float LeftBottomTangentY,float LeftTopTangentX,float	LeftTopTangentY)
{
	WrapX1 = WarpX(	Background,
			1,
			width * (x - (LeftCurve@@y))   / ((RightCurve@@y) - (LeftCurve@@y)),
			height* (y - (BottomCurve@@x)) / ((TopCurve@@x) - (BottomCurve@@x)),
			0,
			0,
			float TopCurve 	  = Hermite(0,
					[TopLeftVertexY    ,0,1.5*atan2d(TopLeftTangentY-TopLeftVertexY,TopLeftTangentX-TopLeftVertexX)]@0,
					[RightTopVertexY   ,1.5*atan2d(RightTopVertexY-TopRightTangentY,RightTopVertexX-TopRightTangentX),0]@width),
			float BottomCurve = Hermite(0,
					[LeftBottomVertexY ,0,1.5*atan2d(BottomLeftTangentY-LeftBottomVertexY,BottomLeftTangentX-LeftBottomVertexX)]@0,
					[BottomRightVertexY,1.5*atan2d(BottomRightVertexY-BottomRightTangentY,BottomRightVertexX-BottomRightTangentX),0]@width),
			float LeftCurve   = Hermite(0,
					[LeftBottomVertexX ,0,1.5*atan2d(LeftBottomTangentX-LeftBottomVertexX,LeftBottomTangentY-LeftBottomVertexY)]@0, 	
					[TopLeftVertexX    ,1.5*atan2d(TopLeftVertexX-LeftTopTangentX,TopLeftVertexY-LeftTopTangentY),0]@height),
			float RightCurve  = Hermite(0,
					[BottomRightVertexX,0,1.5*atan2d(RightBottomTangentX-BottomRightVertexX,RightBottomTangentY-BottomRightVertexY)]@0,
					[RightTopVertexX   ,1.5*atan2d(RightTopVertexX-RightTopTangentX,RightTopVertexY-RightTopTangentY),0]@height)
		      );
	
	return WrapX1;
}
	
//ADBE Glo2
image AE_Glow(
	image	Background,
	string	Base,
	float	Threshold,
	float	Radius,
	float	Intensity,
	string	CompositeMode,
	string	Operation,
	string	Dimensions)
{
	curve ExpandValue = JSpline(0,1.0@0,0.7@1,0.65@2,0.6@3,0.55@4);
	curve ExpandValue2 = JSpline(0,0.0@0,0.5@1,0.5@4);
	ColorX1 = ColorX(Background, r, (r+g+b)/3.0, b, a, z);
	
	if (Base == "Color Channels")
		ColorX2 = ColorX(ColorX1, r, g, b, (g>Threshold/100.0-0.1)?1:0, z);
	else
		ColorX2 = ColorX(ColorX1, r, g, b, (a>Threshold/100.0-0.1)?1:0, z);
	
	if (Intensity>1.0)
		LayerX1 = LayerX(Background, ColorX2, r*a2, g*a2, b*a2, a, z);
	else
		LayerX1 = LayerX(Background, ColorX2, r*a2*Intensity, g*a2*Intensity, b*a2*Intensity, a, z);
	
	Expand1 = Expand(LayerX1, ExpandValue2@@Intensity, ExpandValue2@@Intensity, ExpandValue2@@Intensity, 0, ExpandValue@@Intensity, ExpandValue@@Intensity, ExpandValue@@Intensity, 1);
	
	float X_Blur,Y_Blur;
	if (Dimensions == "Horizontal And Vertical")
	{
		X_Blur = Radius;
		Y_Blur = Radius;
	}
	else if (Dimensions == "Horizontal")
	{
		X_Blur = Radius;
		Y_Blur = 0;
	}
	if (Dimensions == "Vertical")
	{
		X_Blur = 0;
		Y_Blur = Radius;
	}
	Blur1 = Blur(Expand1, X_Blur, Y_Blur, 0, "gauss", xFilter, "rgba");

	if (CompositeMode == "On Top")
	{
		Composite1 = Blur1;
		Composite2 = Background;	
	}
	if (CompositeMode == "Behind")
	{
		Composite1 = Background;
		Composite2 = Blur1;	
	}
	else
	{
		Composite1 = Blur1;
		Composite2 = 0;
	}
	
	if (Operation == "None")
		FinalNode = Blur1;
	else
		FinalNode = AE_Layers(Composite1,Composite2,0,Operation);

	return FinalNode;
}

//ADBE Write-on
image AE_WriteOn(
	image	Background,
	float	PosX,		float	PosY,
	float	rColor,		float	gColor,		float	bColor,
	float	BrushSize,	float	BrushHardness,	float	BrushOpacity,
	float	StrokeLength,
	float	BrushSpacing,
	string	PaintStyle)
{
	int	NbBrushes = (int)(StrokeLength/BrushSpacing) + 1;
	
	TheBrush = RGrad(Background.width, Background.height, 
			 1, width/2, height/2, 1, 
			 BrushSize, BrushSize*BrushHardness/200.0, 0.5, 
			 rColor, gColor, bColor, BrushOpacity/100.0, 0, 
			 0, 0, 0, 0, 0);

	Final = Black(Background.width, Background.height,1);
	
	for (int count=0; count < NbBrushes; count++)
	{
		float ptime = time-framesPerSecond*count*BrushSpacing;
		if (ptime < 1)
			ptime = 1;
		Final = Over(	Move2D(	TheBrush,
					(PosX@@ptime)-width/2,
					(PosY@@ptime)-height/2,
					0,1,
					1,1,
					0,0,
					width/2,height/2,
					"default",xFilter,
					"trsx",
					0,0,0.5,0,0,time),
				Final);
	}
	
	if (PaintStyle == "On Original Image")
		FinalImage = Over(Final,Background);
	else if (PaintStyle == "On Transparent")
		FinalImage = Final;
	else if (PaintStyle == "Reveal Original Image")
		FinalImage = LayerX(Background,Final,r*a2,g*a2,b*a2,a2);
	return FinalImage;
}

//ADBE Path Text
image 	AE_PathText(
	image 	Background,
	string	TheText,
	string	ShapeType,
	float	Tangent1X,	float	Tangent1Y,
	float	Vertex1X,	float	Vertex1Y,
	float	Tangent2X,	float	Tangent2Y,
	float	Vertex2X,	float	Vertex2Y,
	int	ReversePath,
	string	TextOptions,
	float	rFill,		float	gFill,		float	bFill,
	float	rOutline,	float	gOutline,	float	bOutline,
	float	OutlineWidth,
	float	CharSize,	float	CharTracking,
	int	Composite)
{
	Final = Color(Background.width,Background.height,1);
	Outline = Final;
	Filled = Final;

	if (ShapeType == "Bezier")
	{
		int	StringLength = strlen(TheText);
		curve float 	XCurve = Hermite(0,	[Vertex1X   ,0,0.75*atan2d(Tangent1Y-Vertex1Y,Tangent1X-Vertex1X)]@0,
							[Vertex2X   ,0.75*atan2d(Vertex2Y-Tangent2Y,Vertex2X-Tangent2X),0]@(StringLength-1));
		curve float 	YCurve = Hermite(0,	[Vertex1Y   ,0,0.75*atan2d(Tangent1Y-Vertex1Y,Tangent1X-Vertex1X)]@0,
							[Vertex2Y   ,0.75*atan2d(Vertex2Y-Tangent2Y,Vertex2X-Tangent2X),0]@(StringLength-1));
	
		for (int count=0; count<StringLength; count++)
		{
			Filled = Over(Text(
					width,height,1,
					strsub(TheText,count,1), "Utopia Regular",
					CharSize, xFontScale,
					1,
					XCurve@@count,YCurve@@count,0,
					2,2,
					rFill,gFill,bFill,1,
					0,0,0,
					45,0,1),
					Filled);
		}
		for (count=0; count<StringLength; count++)
		{
			Outline = Over(DilateErode(Text(
					width,height,1,
					strsub(TheText,count,1), "Utopia Regular",
					CharSize, xFontScale,
					1,
					XCurve@@count,YCurve@@count,0,
					2,2,
					rOutline,gOutline,bOutline,1,
					0,0,0,
					45,0,1),"rgba",OutlineWidth,OutlineWidth,0,0,0),
					Outline);
		}
	}
	else if (ShapeType == "Line")
	{
		Filled = Text(	width,height,1,
				TheText, "Utopia Regular",
				CharSize,xFontScale,
				1,
				Vertex1X,Vertex1Y,0,
				1,1,
				rFill,gFill,bFill,1,
				0,0,atan2d(Vertex2Y-Vertex1Y,Vertex2X-Vertex1X),
				45,0,1);

		Outline = DilateErode(Text(	width,height,1,
				TheText, "Utopia Regular",
				CharSize,xFontScale,
				1,
				Vertex1X,Vertex1Y,0,
				1,1,
				rOutline,gOutline,bOutline,1,
				0,0,atan2d(Vertex2Y-Vertex1Y,Vertex2X-Vertex1X),
				45,0,1),"rgba", OutlineWidth, OutlineWidth, 0, 0, 0);
	}
	/*
	else if (ShapeType == "Circle")
	{
		int	StringLength = strlen(TheText);
		float	CurrentAngle = atan2d(Vertex2Y-Vertex1Y,Vertex2X-Vertex1X);
		for (int count=0; count<StringLength; count++)
		{
			Filled = Over(Move2D(	Text(	width,height,1,
						strsub(TheText,count,1), "Utopia Regular",
						CharSize, xFontScale,
						1,
						width/2,height/2,0,
						1,1,
						rFill,gFill,bFill,1,
						0,0,0,
						45,0,1),
					Vertex1X-width/2 +distance(Vertex1X,Vertex1Y,Vertex2X,Vertex2Y)*cosd(CurrentAngle),
					Vertex1Y-height/2+distance(Vertex1X,Vertex1Y,Vertex2X,Vertex2Y)*sind(CurrentAngle),
					-90+(CurrentAngle),
					1,1,1,0,0,width/2,height/2,
					"default",xFilter,
					"trsx",0,0,0.5,0,0,1),
					Filled);
			Outline = Over(Move2D(	DilateErode(Text(width,height,1,
						strsub(TheText,count,1), "Utopia Regular",
						CharSize, xFontScale,
						1,
						width/2,height/2,0,
						1,1,
						rOutline,gOutline,bOutline,1,
						0,0,0,
						45,0,1),"rgba", OutlineWidth, OutlineWidth, 0, 0, 0),
					Vertex1X-width/2 +distance(Vertex1X,Vertex1Y,Vertex2X,Vertex2Y)*cosd(CurrentAngle),
					Vertex1Y-height/2+distance(Vertex1X,Vertex1Y,Vertex2X,Vertex2Y)*sind(CurrentAngle),
					-90+(CurrentAngle),
					1,1,1,0,0,width/2,height/2,
					"default",xFilter,
					"trsx",0,0,0.5,0,0,1),
				Outline);
			CurrentAngle -= atand(0.5*CharSize/distance(Vertex1X,Vertex1Y,Vertex2X,Vertex2Y));
		}
	}
	*/
	if (TextOptions == "Fill Only")
		Final = Filled;
	else if (TextOptions == "Stroke Only")
		Final = LayerX(Filled,Outline,r2*(1-a),g2*(1-a),b2*(1-a),a2*(1-a),z);
	else if (TextOptions == "Fill Over Stroke")
		Final = Over(Filled,Outline);
	else if (TextOptions == "Stroke Over Fill")
		Final = Over(Outline,Filled);

	if (Composite == 1)
		FinalNode = Over(Final,Background);
	else
		FinalNode = Final;
	return FinalNode;
}


//-- ADBE 3D Glasses
image AE_3DGlasses(
	image	leftview,
	image	rightview,
	int	ConvergenceOffset,
	int	SwapLeftRight,
	string	View3D,
	float	Balance,
	string	ReferenceSize)
{
	image 	Left;
	image	Right;
	
	if (ReferenceSize == "Left View")
	{
		Input1 	= leftview;
		Input2 	= Resize(rightview,leftview.width,leftview.height);
	}
	else
	{
		Input1 	= Resize(leftview,rightview.width,rightview.height);
		Input2	= rightview;
	}
		
	if (SwapLeftRight == 1.0)
	{
		Left 	= Input2;
		Right	= Input1;
	}
	else
	{
		Left 	= Input1;
		Right 	= Input2;
	}
	
	if (View3D == "Stereo Pair")
	{
		Resize1 = Resize(Left,width/2,height);	
		Resize2 = Resize(Right,width/2,height);
		Pan1	= Pan(Resize2,width,0);
		Window1 = Window(Pan1, 0, 0, 2*width, height);
		Final	= Over(Resize1,Window1);
	}
	else if (View3D == "Interlace Upper L Lower R")
	{
		Scroll1 = Scroll(Right, -ConvergenceOffset, 0, 0, 0.5, 0);
		Crop1 	= Crop(Scroll1, ConvergenceOffset>0?Scroll1.width-ConvergenceOffset-1:-ConvergenceOffset, 0, ConvergenceOffset>0?width-ConvergenceOffset:-ConvergenceOffset+1, height);
		Resize1 = Resize(Crop1, ConvergenceOffset>0?ConvergenceOffset:-ConvergenceOffset, height, "default", 0);
		Pan1 	= Pan(Resize1, ConvergenceOffset>0?Scroll1.width-ConvergenceOffset:0, 0, 0, 0.5, 0);
		Over1 	= Over(Pan1, Scroll1, 1, 0, 0);
		
		Scroll2 = Scroll(Left, ConvergenceOffset, 0, 0, 0.5, 0);
		Crop2 	= Crop(Scroll2, ConvergenceOffset>0?ConvergenceOffset:Scroll2.width+ConvergenceOffset-1, 0, ConvergenceOffset>0?ConvergenceOffset+1:Scroll2.width+ConvergenceOffset, height);
		Resize2 = Resize(Crop2, ConvergenceOffset>0?ConvergenceOffset:-ConvergenceOffset, height, "default", 0);
		Pan2 	= Pan(Resize2, ConvergenceOffset>0?0:Scroll2.width+ConvergenceOffset, 0, 0, 0.5, 0);
		Over2 	= Over(Pan2, Scroll2, 1, 0, 0);
	
		Final	= Interlace(Over1, Over2, 1, 0, 0);
	}
	else if (View3D == "Red Green LR")
	{
		Scroll1 = Scroll(Right, -ConvergenceOffset, 0, 0, 0.5, 0);
		Crop1 	= Crop(Scroll1, ConvergenceOffset>0?Scroll1.width-ConvergenceOffset-1:-ConvergenceOffset, 0, ConvergenceOffset>0?width-ConvergenceOffset:-ConvergenceOffset+1, height);
		Resize1 = Resize(Crop1, ConvergenceOffset>0?ConvergenceOffset:-ConvergenceOffset, height, "default", 0);
		Pan1 	= Pan(Resize1, ConvergenceOffset>0?Scroll1.width-ConvergenceOffset:0, 0, 0, 0.5, 0);
		Over1 	= Over(Pan1, Scroll1, 1, 0, 0);
		Monochrome1 = Monochrome(Over1, 0.3, 0.59, 0.11);
		
		Scroll2 = Scroll(Left, ConvergenceOffset, 0, 0, 0.5, 0);
		Crop2 	= Crop(Scroll2, ConvergenceOffset>0?ConvergenceOffset:Scroll2.width+ConvergenceOffset-1, 0, ConvergenceOffset>0?ConvergenceOffset+1:Scroll2.width+ConvergenceOffset, height);
		Resize2 = Resize(Crop2, ConvergenceOffset>0?ConvergenceOffset:-ConvergenceOffset, height, "default", 0);
		Pan2 	= Pan(Resize2, ConvergenceOffset>0?0:Scroll2.width+ConvergenceOffset, 0, 0, 0.5, 0);
		Over2 	= Over(Pan2, Scroll2, 1, 0, 0);
		Monochrome2 = Monochrome(Over2, 0.3, 0.59, 0.11);
	
		Final	= LayerX(Monochrome2, Monochrome1, r, r2, 0, a, z);
	}
	else if (View3D == "Red Blue LR")
	{
		Scroll1 = Scroll(Right, -ConvergenceOffset, 0, 0, 0.5, 0);
		Crop1 	= Crop(Scroll1, ConvergenceOffset>0?Scroll1.width-ConvergenceOffset-1:-ConvergenceOffset, 0, ConvergenceOffset>0?width-ConvergenceOffset:-ConvergenceOffset+1, height);
		Resize1 = Resize(Crop1, ConvergenceOffset>0?ConvergenceOffset:-ConvergenceOffset, height, "default", 0);
		Pan1 	= Pan(Resize1, ConvergenceOffset>0?Scroll1.width-ConvergenceOffset:0, 0, 0, 0.5, 0);
		Over1 	= Over(Pan1, Scroll1, 1, 0, 0);
		Monochrome1 = Monochrome(Over1, 0.3, 0.59, 0.11);
		
		Scroll2 = Scroll(Left, ConvergenceOffset, 0, 0, 0.5, 0);
		Crop2 	= Crop(Scroll2, ConvergenceOffset>0?ConvergenceOffset:Scroll2.width+ConvergenceOffset-1, 0, ConvergenceOffset>0?ConvergenceOffset+1:Scroll2.width+ConvergenceOffset, height);
		Resize2 = Resize(Crop2, ConvergenceOffset>0?ConvergenceOffset:-ConvergenceOffset, height, "default", 0);
		Pan2 	= Pan(Resize2, ConvergenceOffset>0?0:Scroll2.width+ConvergenceOffset, 0, 0, 0.5, 0);
		Over2 	= Over(Pan2, Scroll2, 1, 0, 0);
		Monochrome2 = Monochrome(Over2, 0.3, 0.59, 0.11);
	
		Final	= LayerX(Monochrome2, Monochrome1, r, 0, r2, a, z);
	}
	else if (View3D == "Balanced Red Green LR")
	{
		Scroll1 = Scroll(Right, -ConvergenceOffset, 0, 0, 0.5, 0);
		Crop1 	= Crop(Scroll1, ConvergenceOffset>0?Scroll1.width-ConvergenceOffset-1:-ConvergenceOffset, 0, ConvergenceOffset>0?width-ConvergenceOffset:-ConvergenceOffset+1, height);
		Resize1 = Resize(Crop1, ConvergenceOffset>0?ConvergenceOffset:-ConvergenceOffset, height, "default", 0);
		Pan1 	= Pan(Resize1, ConvergenceOffset>0?Scroll1.width-ConvergenceOffset:0, 0, 0, 0.5, 0);
		Over1 	= Over(Pan1, Scroll1, 1, 0, 0);
		Monochrome1 = Monochrome(Over1, 0.3, 0.59, 0.11);
		
		Scroll2 = Scroll(Left, ConvergenceOffset, 0, 0, 0.5, 0);
		Crop2 	= Crop(Scroll2, ConvergenceOffset>0?ConvergenceOffset:Scroll2.width+ConvergenceOffset-1, 0, ConvergenceOffset>0?ConvergenceOffset+1:Scroll2.width+ConvergenceOffset, height);
		Resize2 = Resize(Crop2, ConvergenceOffset>0?ConvergenceOffset:-ConvergenceOffset, height, "default", 0);
		Pan2 	= Pan(Resize2, ConvergenceOffset>0?0:Scroll2.width+ConvergenceOffset, 0, 0, 0.5, 0);
		Over2 	= Over(Pan2, Scroll2, 1, 0, 0);
		Monochrome2 = Monochrome(Over2, 0.3, 0.59, 0.11);
	
		Final	= LayerX(Over2, Over1, (100.0-Balance)/100.0*r+Balance*r2/100.0, (100.0-Balance)/100.0*r2+Balance*r/100.0, 0, a, z);
	}
	else if (View3D == "Balanced Red Blue LR")
	{
		Scroll1 = Scroll(Right, -ConvergenceOffset, 0, 0, 0.5, 0);
		Crop1 	= Crop(Scroll1, ConvergenceOffset>0?Scroll1.width-ConvergenceOffset-1:-ConvergenceOffset, 0, ConvergenceOffset>0?width-ConvergenceOffset:-ConvergenceOffset+1, height);
		Resize1 = Resize(Crop1, ConvergenceOffset>0?ConvergenceOffset:-ConvergenceOffset, height, "default", 0);
		Pan1 	= Pan(Resize1, ConvergenceOffset>0?Scroll1.width-ConvergenceOffset:0, 0, 0, 0.5, 0);
		Over1 	= Over(Pan1, Scroll1, 1, 0, 0);
		Monochrome1 = Monochrome(Over1, 0.3, 0.59, 0.11);
		
		Scroll2 = Scroll(Left, ConvergenceOffset, 0, 0, 0.5, 0);
		Crop2 	= Crop(Scroll2, ConvergenceOffset>0?ConvergenceOffset:Scroll2.width+ConvergenceOffset-1, 0, ConvergenceOffset>0?ConvergenceOffset+1:Scroll2.width+ConvergenceOffset, height);
		Resize2 = Resize(Crop2, ConvergenceOffset>0?ConvergenceOffset:-ConvergenceOffset, height, "default", 0);
		Pan2 	= Pan(Resize2, ConvergenceOffset>0?0:Scroll2.width+ConvergenceOffset, 0, 0, 0.5, 0);
		Over2 	= Over(Pan2, Scroll2, 1, 0, 0);
		Monochrome2 = Monochrome(Over2, 0.3, 0.59, 0.11);
	
		Final	= LayerX(Over2, Over1, (100.0-Balance)/100.0*r+Balance*r2/100.0, 0, (100.0-Balance)/100.0*r2+Balance*r/100.0, a, z);
	}
	return Final;
}

//-- ADBE Solid Composite
image AE_SolidComposite(
	image	background,
	float	SourceOpacity,
	float	rColor,		float	gColor,		float	bColor,
	float	Opacity,
	string	BlendingMode)
{
	Composite1 = ColorX(background, r*SourceOpacity/100.0, g*SourceOpacity/100.0, b*SourceOpacity/100.0, a*SourceOpacity/100.0, z);
	Composite2 = Color(background.width, background.height, 1, rColor*Opacity/100.0, gColor*Opacity/100.0, bColor*Opacity/100.0, Opacity, 0);
	
	FinalNode = AE_Layers(Composite1,Composite2,0,BlendingMode);
		
	return FinalNode;
}

//-- ADBE WRPMESH
image AE_Warp(
	image	background,
	string	WarpStyle,
	string	WarpAxis,
	float	Bend,
	float	HorizontalDistort,
	float	VerticalDistort)
{
	Warp1 = WarpX(	background, 1, 
			width/2  - (width/2-x)  * (1/(1+(1-2*y/height) * VerticalDistort/100.0)),
			height/2 + (y-height/2) * (1/(1+(1-2*x/width)  * HorizontalDistort/100.0)), 
			0, xDelta);
	if (WarpStyle == "Arc")
	{
		if (WarpAxis == "Horizontal")
		{
			WarpX1 = WarpX(	Warp1, 1, 	
					x*(1-Bend/100.0)+Bend/100.0*(width-atan2d(y,x-width/2)/180*width), 
		    			y*(1-Bend/100.0)+Bend/100.0*(distance(x,y,width/2,0)-width/2), 0, xDelta);
		}
		else
		{
			Flop1 = Flop(Warp1);
			WarpX1 = WarpX(	Flop1, 1, 	
		    			x*(1-Bend/100.0)+Bend/100.0*(distance(x,y,width,height/2)-height/2),
					y*(1-Bend/100.0)+Bend/100.0*(height-(90+atan2d(height/2-y,width-x))/180*height), 0, xDelta);
		}

	}
	else if (WarpStyle == "Flag")
	{
		if (WarpAxis == "Horizontal")
			WarpX1 = WarpX(	Warp1, 1,
					x,
					y+Bend/100.0*sind(360*x/width)*height/2,
					0, xDelta);
		else
			WarpX1 = WarpX(	Warp1, 1,
					x+Bend/100.0*sind(360*y/height)*width/2,
					y,
					0, xDelta);
	}
	else if (WarpStyle == "Wave")
	{
		if (WarpAxis == "Horizontal")
			WarpX1 = WarpX(	Warp1, 1,
					x,
					y<height/2*(1+Bend/200.0*sind(360*x/width))?
						y/(1+Bend/200.0*sind(360*x/width)):
						height/2+(y-height/2-Bend/100.0*sind(360*x/width)*height/4)/(1-Bend/200.0*sind(360*x/width)),
					0, xDelta);
		else
			WarpX1 = WarpX(	Warp1, 1,
					x<width/2*(1+Bend/200.0*sind(360*y/height))?
						x/(1+Bend/200.0*sind(360*y/height)):
						width/2+(x-width/2-Bend/100.0*sind(360*y/height)*width/4)/(1-Bend/200.0*sind(360*y/height)),
					y,
					0, xDelta);
	}
	else if (WarpStyle == "Rise")
	{
		if (WarpAxis == "Horizontal")
			WarpX1 = WarpX(	Warp1, 1,
					x,
					y+Bend/100.0*height*(cosd(-180+180*x/width)-1.0),
					0, xDelta);
		else
			WarpX1 = WarpX(	Warp1, 1,
					x-Bend/100.0*width*(cosd(-180+180*y/height)+1.0),
					y,
					0, xDelta);
	}

	return WarpX1;
}
	
//-- DE Whirlix
image DE_Whirlix(
	image	background,
	float	size,
	float	strength,
	float	xCenter,
	float	yCenter,
	float	Blend)
{
	RGrad1 = RGrad(background.width, background.height, 1, xCenter, yCenter, 1, size*108.0/15.0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0);
	Twirl1 = Twirl(background, 0, strength*20.0, 1, xCenter, yCenter, width/2, width/2, 1, 0);
	Mask(Twirl1, RGrad1, "A", 100, 0, 1);
	Mix1 = Mix(background, Twirl1, 0, Blend, "rgba");
	return Mix1;
}

//-- DE SpotLights
image DE_Spotlights(
	image	background,
	float	SpotLight1_Size,
	float	xSpotLight1,		float	ySpotLight1,
	float	SpotLight1_Brightness,
	float	SpotLight2_Size,
	float	xSpotLight2,		float	ySpotLight2,
	float	SpotLight2_Brightness,
	float	SpotLight3_Size,
	float	xSpotLight3,		float	ySpotLight3,
	float	SpotLight3_Brightness,
	float	Blend)
{
	RGrad1 = RGrad(background.width, background.height, 1, xSpotLight1, ySpotLight1, 1, 0, 
    			SpotLight1_Size*1.33*width/200, 0.5, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0);
	RGrad2 = RGrad(background.width, background.height, 1, xSpotLight2, ySpotLight2, 1, 0, 
			SpotLight2_Size*1.33*width/200, 0.5, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0);
	RGrad3 = RGrad(background.width, background.height, 1, xSpotLight3, ySpotLight3, 1, 0, 
			SpotLight3_Size*1.33*width/200, 0.5, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0);
	IMult1 = IMult(background, RGrad1, 1, 100, 0);
	IMult2 = IMult(background, RGrad2, 1, 100, 0);
	IMult3 = IMult(background, RGrad3, 1, 100, 0);
	Brightness1 = Brightness(IMult1, SpotLight1_Brightness/25.0);
	Brightness2 = Brightness(IMult2, SpotLight2_Brightness/255.0);
	Brightness3 = Brightness(IMult3, SpotLight3_Brightness/25.0);
	Over1 = Over(Brightness1, Brightness2, 1, 0, 0);
	Over2 = Over(Over1, Brightness3, 1, 0, 0);
	SetAlpha1 = SetAlpha(Over2, 1);
	Mix1 = Mix(background, SetAlpha1, 1, Blend, "rgba");
	return Mix1;
}

//-- DE Lightzoom.AEX
image DE_Lightzoom(
	image	background,
	float	ZoomAmount,
	float	Brightness,
	float	xLight,		float	yLight,
	float	Blend)
{
	RBlur1 = RBlur(background, xLight, yLight, 0, 20, 1, 1, (ZoomAmount+100.0)/100.0, 1, 0, 0);
	Expand1 = Expand(RBlur1, 
				(Brightness<0)?-Brightness/100.0:0,
				(Brightness<0)?-Brightness/100.0:0,
				(Brightness<0)?-Brightness/100.0:0, 
				0, 
				(Brightness>0)?1.0-Brightness/100.0:1, 
				(Brightness>0)?1.0-Brightness/100.0:1,
				(Brightness>0)?1.0-Brightness/100.0:1, 
				1);
	Mix1 = Mix(background, Expand1, 1, Blend, "rgba");
}

//-- DE Interferix
image DE_Interferix(
	image 	background,
	float	Red,	float	RedCenterX,	float	RedCenterY,	float	RedBrightness,
	float	Green,	float	GreenCenterX,	float	GreenCenterY,	float	GreenBrightness,
	float	Blue,	float	BlueCenterX,	float	BlueCenterY,	float	BlueBrightness,
	float	Blend)
{
	ColorX1 = ColorX( 	background, 
			 	((2*Red*distance(x,y,RedCenterX,RedCenterY)/width)%1.0)*RedBrightness/100.0, 
    			 	((2*Green*distance(x,y,BlueCenterX,BlueCenterY)/width)%1.0)*GreenBrightness/100.0, 
    				((2*Blue*distance(x,y,GreenCenterX,GreenCenterY)/width)%1.0)*BlueBrightness/100.0, 
    				0.6, 0);
    	return Mix(background,ColorX1,1,Blend,"rgba");
}

//-- DE Electrofield
image DE_Electrofield(
	image 	background,
	float	RedStrength,	float	RedCenterX,	float	RedCenterY,	float	RedBrightness,
	float	GreenStrength,	float	GreenCenterX,	float	GreenCenterY,	float	GreenBrightness,
	float	BlueStrength,	float	BlueCenterX,	float	BlueCenterY,	float	BlueBrightness,
	float	Blend)
{
	ColorX1 = ColorX(background, 
				((RedStrength/distance(x,y,RedCenterX,RedCenterY))%1.0)*RedBrightness/100.0, 
    				((GreenStrength/distance(x,y,RedCenterX,RedCenterY) + GreenStrength/distance(x,y,GreenCenterX,GreenCenterY))%1.0)*GreenBrightness/100.0, 
    				((BlueStrength/distance(x,y,RedCenterX,RedCenterY)  + BlueStrength/distance(x,y,GreenCenterX,GreenCenterY) + BlueStrength/distance(x,y,BlueCenterX,BlueCenterY))%1.0)*BlueBrightness/100.0, 
    				a, z);
    	return Mix(background,ColorX1,1,Blend,"rgba");
}

//-- DE EarthQuake
image DE_EarthQuake(
	image	background,
	float	HorizVibration,	float	VertVibration,
	float	Blend)
{
	Move2D1 = Move2D(background, 
			 HorizVibration*sin(time)*cos(time/3), 
			 VertVibration*sin(time)*cos(time/3), 
    			 0, 1, 1, xScale, 0, 0, width/2, height/2, "default", xFilter, 
    			 "trsx", 
    			 0, 25, 10, 
    			 0, 0, 
    			 time);
    	return Mix(background,Move2D1,1,Blend,"rgba");
}


//-- DE Color Spotlights
image DE_ColorSpotlights(
	image	background,
	float	Red,	float	RedCenterX,	float	RedCenterY,	float	RedBrightness,
	float	Green,	float	GreenCenterX,	float	GreenCenterY,	float	GreenBrightness,
	float	Blue,	float	BlueCenterX,	float	BlueCenterY,	float	BlueBrightness,
	float	Blend)
{
	RGrad1 = RGrad(background.width, background.height, 1, RedCenterX, RedCenterY, 1, 0, Red, 
	    0.5, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0);
	RGrad2 = RGrad(background.width, background.height, 1, GreenCenterX, GreenCenterY, 1, 0, Green, 
	    0.5, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0);
	RGrad3 = RGrad(background.width, background.height, 1, BlueCenterX, BlueCenterY, 1, 0, Blue, 
	    0.5, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0);
	    
	ColorX1 = ColorX(RGrad2, r*RedBrightness/100.0, 	g*RedBrightness/100.0, 	b*RedBrightness/100.0, 	a*RedBrightness/100.0, 	z);
	ColorX2 = ColorX(RGrad1, r*GreenBrightness/100.0, 	g*GreenBrightness/100.0,	b*GreenBrightness/100.0, 	a*GreenBrightness/100.0, 	z);
	ColorX3 = ColorX(RGrad3, r*BlueBrightness/100.0, 	g*BlueBrightness/100.0, 	b*BlueBrightness/100.0, 	a*BlueBrightness/100.0, 	z);
	
	Over1 = Over(background, ColorX1, 1, 0, 0);
	Over2 = Over(Over1, 	 ColorX2, 1, 0, 0);
	Over3 = Over(Over2, 	 ColorX3, 1, 0, 0);
	
	return Mix(background,Over3,1,Blend,"rgba");
}


//-- DE 3D Lighting
image DE_3DLighting(
	image	background,
	float	rLight,		float	gLight,		float	bLight,
	float	LightX,		float	LightY,		float	LightHeight,
	float	rGloss,		float	gGloss,		float	bGloss,
	float	rDiffuse,	float	gDiffuse,	float	bDiffuse,
	float	rSpecular,	float	gSpecular,	float	bSpecular,
	float	Relief,
	float	Smoothness,
	float	Blend)
{
	ColorX2 = ColorX(0, 
		fabs(x-LightX)/sqrt(sqr(x-LightX)+sqr(y-LightY)+sqr(-LightHeight)), 
    		fabs(y-LightY)/sqrt(sqr(x-LightX)+sqr(y-LightY)+sqr(-LightHeight)), 
    		fabs(-LightHeight)/sqrt(sqr(x-LightX)+sqr(y-LightY)+sqr(-LightHeight)), 
    		a, z);
	Monochrome1 = Monochrome(background, 0.3, 0.59, 0.11);
	Scroll1 = Scroll(Monochrome1, 1, 0, 0, 0.5, 0);
	Scroll2 = Scroll(Monochrome1, -1, 0, 0, 0.5, 0);
	Scroll3 = Scroll(Monochrome1, 0, 1, 0, 0.5, 0);
	Scroll4 = Scroll(Monochrome1, 0, -1, 0, 0.5, 0);
	LayerX2 = LayerX(Scroll3, Scroll4, (r-r2)/2.0+0.5, 0, 0, a, z);
	LayerX3 = LayerX(Scroll1, Scroll2, (r-r2)/2.0+0.5, 0, 0, a, z);
	LayerX1 = LayerX(LayerX2, LayerX3, r2, r, (1.0-sqrt(sqr((r2-0.5)*2)+sqr((r-0.5)*2))), a, z);
	ColorX5 = ColorX(LayerX1, (r-0.5)*Relief/100.0+0.5, g, b, a, z);
	Blur1 = Blur(ColorX5, Smoothness, xPixels, 0, "gauss", xFilter, "rgba");
	AE_Layers1 = AE_Layers(Blur1, ColorX2, 0, "IntenseLight");
	ColorX4 = ColorX(AE_Layers1, 1.0-sqrt(sqr(r)+sqr(g)), sqr(1.0-sqrt(sqr(r)+sqr(g))), b, a, z);
	LayerX4 = LayerX(ColorX4, background, 
				rLight*(1.0-rGloss+r2*r*rDiffuse)+g*g*rSpecular, 
				gLight*(1.0-gGloss+g2*r*gDiffuse)+g*g*gSpecular, 
				bLight*(1.0-bGloss+b2*r*bDiffuse)+g*g*bSpecular, 
				a, z);

	return Mix(background,LayerX4,1,Blend,"rgba");
}

//-- DE AgedFilm
image	DE_AgedFilm(
	image	background,
	float	FilmResponse,
	float	GrainAmount,
	float	DustSize,	float	DustAmount,
	float	rDust,		float	gDust,		float	bDust,
	float	HairSize,	float	HairAmount,
	float	rHair,		float	gHair,		float	bHair,
	float	ScratchAmount,	float	ScratchVelocity,float	ScratchLifespan,	float	ScratchOpacity,
	float	FrameJitterMaxOffset,	float	FrameJitterProbability,
	int	ConvertToGray,
	float	rGrayTint,	float	gGrayTint,	float	bGrayTint,
	float	FlickerSpeed,	float	FlickerAmount,
	int	RevealBlackLayer,
	int	RandomSeed,
	float	Blend)
{
	if (ConvertToGray == 1.0)
	{
		background2 = Monochrome(background);
		FinalBackground = AE_Tint(background2, 0, 0, 0, rGrayTint, gGrayTint, bGrayTint, 100);
	}
	else
		FinalBackground = background;
	Final = Color(background.width,background.height,1, 1,1,1, 0,0);
	Scratch = Color(1, background.height, 1, 0, 0, 0, ScratchOpacity, 0);
	float	fade_factor = (time-ScratchLifespan*((int)time/(int)ScratchLifespan))/ScratchLifespan;
	ScratchFade = Fade(Scratch,fade_factor<0.5?fade_factor*2:1.0-(fade_factor-0.5)*2);
	
	for (int count=1;count<ScratchAmount;count++)
	{
		Final = Over(	Move2D(ScratchFade,
					// --------------- initial pos ---------------------------------- + ------- per frame speed -------*--- rnd factor ---*
					rnd(1024*count + (int)time/(int)ScratchLifespan)*background.width + ScratchVelocity/framesPerSecond*(rnd(count)*2-1.0)*(time-((int)time/(int)ScratchLifespan)*ScratchLifespan),
					0,
					0,1,1,1,0,0,
					width/2,height/2,
					"default",xFilter,
					"trsx",0,
					0,0.5,0,
					0,time),
				Final);
	}

	Grain1 = Grain( FinalBackground, 1, GrainAmount/50, time, 0, 0.1, 0.5, rGain, rGain, 1, 
    			1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
	IMult1 = IMult(Final, Grain1, 1, 100, 0);
	QuickShape1 = QuickShape(720, 486, 1, 0, 0, 0, 1, 1, xScale, 
    			width/2, height/2, 0, 0.5, 0, 128, 328.994934, 245.723984, 
    			1, 0, 1, 0, 1, 1, 328.4949, 243.573975, 1, 0, 1, 0, 1, 1, 
    			331.211578, 245.3073, 1, 0, 1, 0, 1, 1, 332.711578, 246.1573, 
    			1, 0, 1, 0, 1, 1, 331.2449, 246.973969, 1, 0, 1, 0, 1, 1);
 	Resize1 = Resize(QuickShape1, background.width, background.height, "default", 0);
   	Scale1 = Scale(Resize1, DustSize/50, xScale, width/2, height/2, 0, 0.5, 0);
	Pan1 = Pan(Scale1, 	(rnd((int)((int)time/(int)3))-0.5)*width, 
    				(rnd((int)((int)time/(int)3)+2)-0.5)*height, 0, 0.5, 0);
	Invert1 = Invert(Pan1, "rgba");
	Brightness1 = Brightness(Invert1, 11.0-DustAmount/10.0);
	IMult2 = IMult(Brightness1, IMult1, 1, 100, 0);
	
	Gamma1 = Gamma(IMult2, sind(180*time/FlickerSpeed)*FlickerAmount/100+1.0, rGamma, rGamma, 1);

	return Mix(background,Gamma1,1,Blend,"rgba");
}

//-- DE Blizzard
image DE_Blizzard(
	image	background,
	int	NumberOfFlakes,
	float	WindAmount,
	float	Gravity,
	float	LiftAmount,	float	LiftFrequency,
	float	rFlake,		float	gFlake,		float	bFlake,
	float	MinSize,	float	MaxSize,
	int	BrightenOrSolid,
	float	Opacity,
	float	Blend)
{
	Final = Black(background.width,background.height,1);
	Flake = RGrad(  MaxSize, MaxSize, 1, width/2, height/2, 1, 
			min(width,height)/4, min(width,height)/4, 0.5, 
			rFlake, gFlake, bFlake, Opacity/100.0, 
			0, 0, 0, 0, 
			0, 0);

	for (int count=1; count<=NumberOfFlakes; count++)
	{
		Final = IAdd(	Move2D(	Flake,
				      	(2*rnd(count)-0.5)*background.width + LiftAmount*sind(180.0*(time+count)/(2*LiftFrequency)) + WindAmount*time,
				      	2*rnd(2*count)*background.height - 3*Gravity*(rnd(3*count)+0.5)*time,
					0,1,
					MinSize/MaxSize+rnd(3*count)*(MaxSize-MinSize)/MaxSize,xScale,
					0,0,
					width/2,height/2,
					"default",xFilter,
					"trsx",0,
					0,0.5,0,
					0,time),
				Final);
	}

	if (BrightenOrSolid == 1.0)
		FinalComp = IAdd(Final,background);
	else
	{
		ColorX1 = ColorX(Final,r*a,g*a,b*a,a,z);
		FinalComp = Over(ColorX1,background);
	}
	return Mix(background,FinalComp,1,Blend,"rgba");
}

//-- DE BumpMaker
image DE_BumpMaker(
	image	background,
	float	Direction,
	float	Elevation,
	float	Height,
	float	Grain,
	float	RandomSeed,
	float	Smoothness,
	float	rTint,		float	gTint,		float	bTint,
	float	Blend)
{
	ColorX1 = ColorX(background, turbulence2d(x,y,RandomSeed,RandomSeed), 0, 0, a, z);
	Blur1 = Blur(ColorX1, Smoothness/10.0, xPixels, 0, "gauss", xFilter, "rgba");
	Monochrome1 = Monochrome(Blur1, 0.3, 0.59, 0.11);
	Scroll1 = Scroll(Monochrome1, 1, 0, 0, 0.5, 0);
	Scroll2 = Scroll(Monochrome1, -1, 0, 0, 0.5, 0);
	Scroll3 = Scroll(Monochrome1, 0, 1, 0, 0.5, 0);
	Scroll4 = Scroll(Monochrome1, 0, -1, 0, 0.5, 0);
	LayerX3 = LayerX(Scroll3, Scroll4, (r-r2)/2+0.5, g, b, a, z);
	LayerX4 = LayerX(Scroll1, Scroll2, (r-r2)/2+0.5, g, b, a, z);
	LayerX1 = LayerX(LayerX3, LayerX4, (2*(r-0.5)*sind(Direction)+2*(r2-0.5)*cosd(Direction))*25, 0, 0, a, z);
	Invert1 = Invert(LayerX1, "rgba");
	IMult1 = IMult(Blur1, Invert1, 1, 100, 0);
	ColorX2 = ColorX(IMult1, r*rTint*Grain/100.0, r*gTint*Grain/100.0, r*bTint*Grain/100.0, a, z);
	Mix1 = Mix(background, ColorX2, 1, Blend, "rgba");
	
	return Mix1;
}

//-- DE EdgeX
image	DE_EdgeX(
	image	background,
	float	RedEdge,	float	RedSoftness,
	float	GreenEdge,	float	GreenSoftness,
	float	BlueEdge,	float	BlueSoftness,
	float	Blend)
{
	ColorX1 = ColorX(background, (r<RedEdge/255)?0:1, (g<GreenEdge/255)?0:1, (b<BlueEdge/255)?0:1, a, z);
	Blur1 = Blur(ColorX1, RedSoftness, xPixels, 0, "gauss", xFilter, "r");
	Blur2 = Blur(Blur1, GreenSoftness, xPixels, 0, "gauss", xFilter, "g");
	Blur3 = Blur(Blur2, BlueSoftness, xPixels, 0, "gauss", xFilter, "b");
	Mix1 = Mix(background, Blur3, 1, Blend, "rgba");
	
	return Mix1;
}


//-- DE VideoLook
image DE_VideoLook(
	image	background,
	float	EvenBlend,		float	OddBlend,
	float	EvenBrightness,		float	OddBrightness,
	float	EvenNoise,		float	OddNoise,
	int	EvenColorizeNoise,	int	OddColorizeNoise,
	int	EvenSize,		int	OddSize,
	string	EvenDecayAmount,	string	OddDecayAmount,
	int	EvenAltBleed,		int	OddAltBleed,
	string	EvenDistortionMethod,	string	OddDistortionMethod,
	float	EvenHorizontalShift,	float	OddHorizontalShift,
	string	EvenCombMask,		string	OddCombMask,
	int	EvenGrayConvert,	int	OddGrayConvert,
	float	rEvenGrayTint,float gEvenGrayTint,float bEvenGrayTint,
	float	rOddGrayTint,float gOddGrayTint,float bOddGrayTint,
	float	TimeVaryValue,
	float	Blend)
{
	// Split Fields
	EvenField = Field(background, 0);
	OddField = Field(background, 1);
	
	// Gray scale & Tint
	if (EvenGrayConvert == 1)
		EvenFieldGray = ColorX(	Monochrome(EvenField, 0.3, 0.59, 0.11),
					r*rEvenGrayTint,g*gEvenGrayTint,b*bEvenGrayTint,a,z);
	else
		EvenFieldGray = EvenField;
	if (OddGrayConvert == 1)
		OddFieldGray = ColorX(	Monochrome(OddField, 0.3, 0.59, 0.11),
					r*rOddGrayTint,g*gOddGrayTint,b*bOddGrayTint,a,z);
	else
		OddFieldGray = OddField;
	
	// Set Brightness
	EvenBrightness1 = Brightness(EvenFieldGray, EvenBrightness/50.0);
	OddBrightness1 = Brightness(OddFieldGray, OddBrightness/50.0);

	// Set "Decay"
	if (EvenDecayAmount == "Level 1")
		EvenDecay1 = ContrastLum(EvenBrightness1, 1, 0.5, 0);
	else if (EvenDecayAmount == "Level 2")
		EvenDecay1 = ContrastLum(EvenBrightness1, 2, 0.5, 0);
	else if (EvenDecayAmount == "Level 3")
		EvenDecay1 = ContrastLum(EvenBrightness1, 4, 0.5, 0);
	else if (EvenDecayAmount == "Level 4")
		EvenDecay1 = ContrastLum(EvenBrightness1, 7, 0.5, 0);
	else if (EvenDecayAmount == "Level 5")
		EvenDecay1 = ContrastLum(EvenBrightness1, 10, 0.5, 0);
	if (OddDecayAmount == "Level 1")
		OddDecay1 = ContrastLum(OddBrightness1, 1, 0.5, 0);
	else if (OddDecayAmount == "Level 2")
		OddDecay1 = ContrastLum(OddBrightness1, 2, 0.5, 0);
	else if (OddDecayAmount == "Level 3")
		OddDecay1 = ContrastLum(OddBrightness1, 4, 0.5, 0);
	else if (OddDecayAmount == "Level 4")
		OddDecay1 = ContrastLum(OddBrightness1, 7, 0.5, 0);
	else if (OddDecayAmount == "Level 5")
		OddDecay1 = ContrastLum(OddBrightness1, 10, 0.5, 0);
	
	// Add Noise
	EvenRand1 = Rand(background.width, background.height/2, 1, EvenNoise/100.0, time+1);
	if (EvenColorizeNoise == 0)
		EvenNoise1 = Invert(Monochrome(EvenRand1, 0.3, 0.59, 0.11),"rgba");
	else
		EvenNoise1 = Invert(EvenRand1);
	EvenIMult1 = IMult(EvenDecay1, EvenNoise1, 1, 100, 0);
	OddRand1 = Rand(background.width, background.height/2, 1, OddNoise/100.0, time+1);
	if (OddColorizeNoise == 0)
		OddNoise1 = Invert(Monochrome(OddRand1, 0.3, 0.59, 0.11),"rgba");
	else
		OddNoise1 = Invert(OddRand1);
	OddIMult1 = IMult(OddDecay1, OddNoise1, 1, 100, 0);
	
	// Add Comb Mask
	if (EvenCombMask == "Off")
		EvenComb1 = EvenIMult1;
	else if (EvenCombMask == "Mode 1")
		EvenComb1 = ColorX(EvenIMult1, ((int)(x)&1)?r:0, ((int)(x)&1)?g:0, ((int)(x)&1)?b:0, a, z);
	else if (EvenCombMask == "Mode 2")
		EvenComb1 = ColorX(EvenIMult1, ((int)(x/2)&1)?r:0, ((int)(x/2)&1)?g:0, ((int)(x/2)&1)?b:0, a, z);
	else if (EvenCombMask == "Mode 3")
		EvenComb1 = ColorX(EvenIMult1, ((int)(x)%4)?0:r, ((int)(x)%4)?0:g, ((int)(x)%4)?0:b, a, z);
	else if (EvenCombMask == "Mode 4")
		EvenComb1 = ColorX(EvenIMult1, ((int)(x/4)&1)?0:r, ((int)(x/4)&1)?0:g, ((int)(x/4)&1)?0:b, a, z);
	if (OddCombMask == "Off")
		OddComb1 = OddIMult1;
	else if (OddCombMask == "Mode 1")
		OddComb1 = ColorX(OddIMult1, ((int)(x)&1)?r:0, ((int)(x)&1)?g:0, ((int)(x)&1)?b:0, a, z);
	else if (OddCombMask == "Mode 2")
		OddComb1 = ColorX(OddIMult1, ((int)(x/2)&1)?r:0, ((int)(x/2)&1)?g:0, ((int)(x/2)&1)?b:0, a, z);
	else if (OddCombMask == "Mode 3")
		OddComb1 = ColorX(OddIMult1, ((int)(x)%4)?0:r, ((int)(x)%4)?0:g, ((int)(x)%4)?0:b, a, z);
	else if (OddCombMask == "Mode 4")
		OddComb1 = ColorX(OddIMult1, ((int)(x/4)&1)?0:r, ((int)(x/4)&1)?0:g, ((int)(x/4)&1)?0:b, a, z);
	
	// Shift Fields
	if (EvenDistortionMethod == "Shift")
		EvenShift1 = WarpX(EvenComb1,1,(x-EvenHorizontalShift)%width,y,0,0);
	else if (EvenDistortionMethod == "Linear")
		EvenShift1 = WarpX(EvenComb1,1,(x-EvenHorizontalShift*(height-y))%width,y,0,0);
	else if (EvenDistortionMethod == "Noisy")
		EvenShift1 = WarpX(EvenComb1,1,(x-EvenHorizontalShift*rnd(TimeVaryValue+height-y))%width,y,0,0);
	else if (EvenDistortionMethod == "Sawtooth")
		EvenShift1 = WarpX(EvenComb1,1,(x-EvenHorizontalShift*((TimeVaryValue+height-y)%24)/24.0)%width,y,0,0);
	else if (EvenDistortionMethod == "Wave")
		EvenShift1 = WarpX(EvenComb1,1,(x-EvenHorizontalShift*sin(6*2*M_PI*(TimeVaryValue+height-y)/height))%width,y,0,0);
	if (OddDistortionMethod == "Shift")
		OddShift1 = WarpX(OddComb1,1,(x-OddHorizontalShift)%width,y,0,0);
	else if (OddDistortionMethod == "Linear")
		OddShift1 = WarpX(OddComb1,1,(x-OddHorizontalShift*(height-y))%width,y,0,0);
	else if (OddDistortionMethod == "Noisy")
		OddShift1 = WarpX(OddComb1,1,(x-OddHorizontalShift*rnd(TimeVaryValue+height-y))%width,y,0,0);
	else if (OddDistortionMethod == "Sawtooth")
		OddShift1 = WarpX(OddComb1,1,(x-OddHorizontalShift*((TimeVaryValue+height-y)%24)/24.0)%width,y,0,0);
	else if (OddDistortionMethod == "Wave")
		OddShift1 = WarpX(OddComb1,1,(x-OddHorizontalShift*sin(6*2*M_PI*(TimeVaryValue+height-y)/height))%width,y,0,0);

	// Blend Fields with original image
	EvenBlend1 = Mix(EvenField, EvenShift1, 1, EvenBlend, "rgba");
	OddBlend1 = Mix(OddField, OddShift1, 1, OddBlend, "rgba");
	
	// Reassemble Fields
	Interlace1 = Interlace(EvenBlend1, OddBlend1, 1, 0, 1);
	
	// Blend with original image
	return Mix(background,Interlace1,1,Blend,"rgba");
}


//-- DE Tilos
image	DE_Tilos(
	image	background,
	float	size,
	float	TileOffsetX,
	float	TileOffsetY,
	float	Blend)
{
	Scroll1 = Scroll(background, width/2-TileOffsetX, height/2-TileOffsetY, 0, 0.5, 0);
	Flip1 = Flip(Scroll1);
	WarpX1 = WarpX(Flip1, 1, width*(x%tilesizeX)/tilesizeX, height*((height-y)%tilesizeY)/tilesizeY, 0, height,
			float	tilesizeX = width*1/(1+size/5),
			float	tilesizeY = height*1/(1+size/5));
	return Mix(background,WarpX1,1,Blend,"rgba");
}

//-- DE StarField
image	DE_StarField(
	image	background,
	int	NumberOfStars,
	int	RandomSeed,
	float	Speed,
	float	Twist,
	float	WarpCenterX,	float	WarpCenterY,
	float	StreakAmount,
	float	BirthFadeUp,
	float	MinSize,	float	MaxSize,
	int	RandomColor,
	int	ColorStars,
	float	rMinColor,	float	gMinColor,	float	bMinColor,
	float	rMaxColor,	float	gMaxColor,	float	bMaxColor,
	float	Blend)
{
	Stars = NGLRender(
			width, height, 1,
			"
	      		nglPushMatrix();
	
			float	RotAngle = time * Twist/100.0;
			for(int i=0 ;i<NumberOfStars;i++ )
			{
				float	ZPos = rnd(3*i+RandomSeed)*1000 - time*(rnd(4*i+RandomSeed)*6+4)*Speed/100.0;
				float	XX = rnd(i+RandomSeed)*width-width/2;
				float	YY = rnd(2*i+RandomSeed)*height-height/2;
				float	XPos = XX*cosd(RotAngle) - YY*sind(RotAngle);
				float	YPos = XX*sind(RotAngle) + YY*cosd(RotAngle);
				int	a = 0;
				while (ZPos<=0)
				{
					XX = rnd(i+a+RandomSeed)*width-WarpCenterX;
					YY = rnd(2*i+a+RandomSeed)*height-WarpCenterY;
					XPos = XX*cosd(RotAngle) - YY*sind(RotAngle);
					YPos = XX*sind(RotAngle) + YY*cosd(RotAngle);
					ZPos = (rnd(3*i+RandomSeed)+a)*1000 - time*(rnd(4*i+a+RandomSeed)*6+4);
					a++;
				}
				nglBegin(NGL_POINTS);
					if (RandomColor == 1)
					{
						nglColor4f(	rnd(17*i+RandomSeed),
								rnd(23*i+RandomSeed),
								rnd(42*i+RandomSeed),
								1);
					}
					else if (ColorStars == 1)
					{
						nglColor4f(	rMinColor+rnd(17*i+RandomSeed)*(rMaxColor-rMinColor),
								gMinColor+rnd(23*i+RandomSeed)*(gMaxColor-gMinColor),
								bMinColor+rnd(42*i+RandomSeed)*(bMaxColor-bMinColor),
								1);
					}
					else
					{
						nglColor4f(1,1,1,1);	
					}
					nglVertex2f(WarpCenterX+XPos/ZPos*100,WarpCenterY+YPos/ZPos*100);
				nglEnd();
			}
	      		nglPopMatrix();"
	      	);
	Final = Over(Stars,background);
	return Mix(background,Final,1,Blend,"rgba");    			
}

//-- Trapcode tc Shine
image TC_Shine(
	image	background,
	float	Threshold,
	int	UseMask,float MaskRadius,float MaskFeather,
	float	SourceX,float SourceY,
	float	RayLength,
	float	Amount,float Detail,int	SourceAffect,float Radius,
	int ReduceFlickering,float Phase,int UseLoop,int Revolutions,
	float	BoostLight,
	string	Colorize,
	string	BaseOn,
	float	rHighlight,float gHighlight,float bHighlight,
	float	rMidHigh,float gMidHigh,float bMidHigh,
	float	rMidTones,float gMidTones,float bMidTones,
	float	rMidLow,float gMidLow,float bMidLow,
	float	rShadows,float gShadows,float bShadows,
	int	EdgeThickness,
	float	SourceOpacity,
	float	ShineOpacity,
	string	TransferMode)
{
	Monochrome1 = Monochrome(background, 0.3, 0.59, 0.11);
	AE_Threshold1 = AE_Threshold(Monochrome1, Threshold);
	Reorder1 = Reorder(AE_Threshold1, "rgbr");
	if (UseMask == 1)
	{
		RGrad1 = RGrad(background.width, background.height, 1, SourceX, SourceY, 1, MaskRadius, MaskFeather, 0.5, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0);
	    	LayerX1 = LayerX(Reorder1,RGrad1,r*a2,g*a2,b*a2,a*a2,z);
	}
	else
	{
		LayerX1 = Reorder1;	
	}
	
	LayerX2 = LayerX(background,LayerX1,r*r2,g*r2,b*r2,a2,z);
	RBlur2 = RBlur(LayerX2, SourceX, SourceY, 0, width, 1, 1,   (RayLength+2)/10.0,   1, 0, 0);
	ContrastRGB1 = ContrastRGB(RBlur2, 	1.0+BoostLight/6, rValue, rValue, rValue, 
						0, rCenter, rCenter, rCenter, 
						0, rSoftClip, rSoftClip, rSoftClip);
	if (Colorize == "None")
	{
		Lookup3 = ContrastRGB1;
    	}
	else
	{
		if (BaseOn == "Lightness")
		{
			ColorizeImage = ColorX(ContrastRGB1,0.30*r+0.59*g+0.11*b,0.30*r+0.59*g+0.11*b,0.30*r+0.59*g+0.11*b,a,z);
		}
		else if (BaseOn == "Luminance")
		{
			ColorizeImage = ColorX(ContrastRGB1, (r+g+b)/3,(r+g+b)/3,(r+g+b)/3,a,z);
		}
		else if (BaseOn == "Alpha")
		{
			ColorizeImage = Reorder(ContrastRGB1,"aaaa");
		}
		else if (BaseOn == "Alpha Edges")
		{
			Reorder2 = Reorder(ContrastRGB1,"aaaa");
			EdgeDetect1 = EdgeDetect(Reorder2);
			ColorizeImage = DilateErode(EdgeDetect1, "rgba", EdgeThickness, EdgeThickness, 0, 0, 0);
		}
		else if (BaseOn == "Red")
		{
			ColorizeImage = Reorder(ContrastRGB1,"rrra");
		}
		else if (BaseOn == "Green")
		{
			ColorizeImage = Reorder(ContrastRGB1,"ggga");
		}
		else if (BaseOn == "Blue")
		{
			ColorizeImage = Reorder(ContrastRGB1,"bbba");
		}
		
		
		if (Colorize == "One Color")
		{
			Lookup3 = Lookup(ColorizeImage,	JSplineV(x,1,0@0,rMidTones@1), 
	    						JSplineV(x,1,0@0,gMidTones@1), 
	    						JSplineV(x,1,0@0,bMidTones@1), 
	    						JSplineV(x,1,0@0,1@1));
	    	}
		else if (Colorize == "3 Color Gradient")
		{
			Lookup3 = Lookup(ColorizeImage, JSplineV(x,1,0@0,0.33*rShadows@0.33,0.66*rMidTones@0.66,rHighlight@1), 
	    						JSplineV(x,1,0@0,0.33*gShadows@0.33,0.66*gMidTones@0.66,gHighlight@1), 
	    						JSplineV(x,1,0@0,0.33*bShadows@0.33,0.66*bMidTones@0.66,bHighlight@1), 
	    						JSplineV(x,1,0@0,1@1));
	    	}
		else if (Colorize == "5 Color Gradient")
		{
			Lookup3 = Lookup(ColorizeImage, JSplineV(x,1,0@0,0.2*rShadows@0.2,0.4*rMidLow@0.4,0.6*rMidTones@0.6,0.8*rMidHigh@0.8,rHighlight@1), 
	    						JSplineV(x,1,0@0,0.2*gShadows@0.2,0.4*gMidLow@0.4,0.6*gMidTones@0.6,0.8*gMidHigh@0.8,gHighlight@1), 
	    						JSplineV(x,1,0@0,0.2*bShadows@0.2,0.4*bMidLow@0.4,0.6*bMidTones@0.6,0.8*bMidHigh@0.8,bHighlight@1), 
	    						JSplineV(x,1,0@0,1@1));
	    	}
		else if (Colorize == "Fire")
		{
			rHighlight = 1; 	gHighlight = 1;		bHighlight = 1;
			rMidTones = 1;		gMidTones = 0.65;	bMidTones = 0;
			rShadows = 1;		gShadows = 0;		bShadows = 0;
			Lookup3 = Lookup(ColorizeImage,	JSplineV(x,1,0@0,0.33*1@0.33,0.66*1@0.66,1@1), 
	    						JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.65@0.66,1@1), 
	    						JSplineV(x,1,0@0,0.33*0@0.33,0.66*0@0.66,1@1), 
	    						JSplineV(x,1,0@0,1@1));
	    	}
		else if (Colorize == "Mars")
		{
			rHighlight = 1; 	gHighlight = 0.82;	bHighlight = 0.65;
			rMidTones = 1;		gMidTones = 0.5;	bMidTones = 0;
			rShadows = 1;		gShadows = 0.25;	bShadows = 0;
			Lookup3 = Lookup(ColorizeImage, JSplineV(x,1,0@0,0.33*1@0.33,0.66*1@0.66,1@1), 
	    						JSplineV(x,1,0@0,0.33*0.25@0.33,0.66*0.5@0.66,0.82@1), 
	    						JSplineV(x,1,0@0,0.33*0@0.33,0.66*0@0.66,0.65@1), 
	    						JSplineV(x,1,0@0,1@1));
	    	}
		else if (Colorize == "Chemistry")
		{
			rHighlight = 1; 	gHighlight = 1;		bHighlight = 1;
			rMidTones = 0.65;	gMidTones = 1;		bMidTones = 0;
			rShadows = 0;		gShadows = 1;		bShadows = 0;
			Lookup3 = Lookup(ColorizeImage, JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.65@0.66,1@1), 
	    						JSplineV(x,1,0@0,0.33*1@0.33,0.66*1@0.66,1@1), 
	    						JSplineV(x,1,0@0,0.33*0@0.33,0.66*0@0.66,1@1), 
	    						JSplineV(x,1,0@0,1@1));
	    	}
		else if (Colorize == "DeepSea")
		{
			rHighlight = 0.61; 	gHighlight = 1;		bHighlight = 0.98;
			rMidTones = 0.02;	gMidTones = 1;		bMidTones = 0.60;
			rShadows = 0;		gShadows = 1;		bShadows = 0;
			Lookup3 = Lookup(ColorizeImage, JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.02@0.66,0.61@1), 
	    						JSplineV(x,1,0@0,0.33*1@0.33,0.66*1@0.66,1@1), 
	    						JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.6@0.66,0.98@1), 
	    						JSplineV(x,1,0@0,1@1));
	    	}
		else if (Colorize == "Electric")
		{
			rHighlight = 1; 	gHighlight = 1;		bHighlight = 1;
			rMidTones = 0.5;	gMidTones = 1;		bMidTones = 1;
			rShadows = 0;		gShadows = 0;		bShadows = 1;
			Lookup3 = Lookup(ColorizeImage, JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.5@0.66,1@1), 
	    						JSplineV(x,1,0@0,0.33*0@0.33,0.66*1@0.66,1@1), 
	    						JSplineV(x,1,0@0,0.33*1@0.33,0.66*1@0.66,1@1), 
	    						JSplineV(x,1,0@0,1@1));
	    	}
		else if (Colorize == "Spirit")
		{
			rHighlight = 1; 	gHighlight = 1;		bHighlight = 1;
			rMidTones = 1;  	gMidTones = 0.5;	bMidTones = 1;
			rShadows = 0;		gShadows = 0;		bShadows = 1;
			Lookup3 = Lookup(ColorizeImage, JSplineV(x,1,0@0,0.33*0@0.33,0.66*1@0.66,1@1), 
	    						JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.5@0.66,1@1), 
	    						JSplineV(x,1,0@0,0.33*1@0.33,0.66*1@0.66,1@1), 
	    						JSplineV(x,1,0@0,1@1));
	    	}
		else if (Colorize == "Aura")
		{
			rHighlight = 1; 	gHighlight = 0.75;	bHighlight = 0.87;
			rMidTones = 0.92;  	gMidTones = 0.5;	bMidTones = 0.75;
			rShadows = 1;		gShadows = 0;		bShadows = 1;
			Lookup3 = Lookup(ColorizeImage,	JSplineV(x,1,0@0,0.33*1@0.33,0.66*0.92@0.66,1@1), 
	    						JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.5@0.66,0.75@1), 
	    						JSplineV(x,1,0@0,0.33*1@0.33,0.66*0.75@0.66,0.87@1), 
	    						JSplineV(x,1,0@0,1@1));
	    	}
		else if (Colorize == "Heaven")
		{
			rHighlight = 1; 	gHighlight = 1;		bHighlight = 1;
			rMidTones = 1;  	gMidTones = 0.91;	bMidTones = 0.91;
			rShadows = 1;		gShadows = 0.5;		bShadows = 1;
			Lookup3 = Lookup(ColorizeImage,	JSplineV(x,1,0@0,0.33*1@0.33,0.66*1@0.66,1@1), 
	    						JSplineV(x,1,0@0,0.33*0.5@0.33,0.66*0.91@0.66,1@1), 
	    						JSplineV(x,1,0@0,0.33*1@0.33,0.66*0.91@0.66,1@1), 
	    						JSplineV(x,1,0@0,1@1));
	    	}
		else if (Colorize == "Romance")
		{
			rHighlight = 1; 	gHighlight = 1;		bHighlight = 1;
			rMidTones = 1;  	gMidTones = 0.5;	bMidTones = 1;
			rShadows = 1;		gShadows = 0;		bShadows = 0;
			Lookup3 = Lookup(ColorizeImage, JSplineV(x,1,0@0,0.33*1@0.33,0.66*1@0.66,1@1), 
	    						JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.5@0.66,1@1), 
	    						JSplineV(x,1,0@0,0.33*0@0.33,0.66*1@0.66,1@1), 
	    						JSplineV(x,1,0@0,1@1));
	    	}
		else if (Colorize == "Magic")
		{
			rHighlight = 1; 	gHighlight = 1;		bHighlight = 0.82;
			rMidTones = 1;  	gMidTones = 0.5;	bMidTones = 0;
			rShadows = 0.79;	gShadows = 0;		bShadows = 1;
			Lookup3 = Lookup(ColorizeImage,	JSplineV(x,1,0@0,0.33*0.79@0.33,0.66*1@0.66,1@1), 
	    						JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.5@0.66,1@1), 
	    						JSplineV(x,1,0@0,0.33*1@0.33,0.66*0@0.66,0.82@1), 
	    						JSplineV(x,1,0@0,1@1));
	    	}
		else if (Colorize == "USA")
		{
			rHighlight = 1; 	gHighlight = 1;		bHighlight = 1;
			rMidTones = 1;  	gMidTones = 0;		bMidTones = 0;
			rShadows = 0;		gShadows = 0;		bShadows = 1;
			Lookup3 = Lookup(ColorizeImage, JSplineV(x,1,0@0,0.33*0@0.33,0.66*1@0.66,1@1), 
	    						JSplineV(x,1,0@0,0.33*0@0.33,0.66*0@0.66,1@1), 
	    						JSplineV(x,1,0@0,0.33*1@0.33,0.66*0@0.66,1@1), 
	    						JSplineV(x,1,0@0,1@1));
	    	}
		else if (Colorize == "Rastafari")
		{
			rHighlight = 1; 	gHighlight = 0;		bHighlight = 0;
			rMidTones = 1;  	gMidTones = 0.84;	bMidTones = 0.06;
			rShadows = 0;		gShadows = 1;		bShadows = 0;
			Lookup3 = Lookup(ColorizeImage, JSplineV(x,1,0@0,0.33*0@0.33,0.66*1@0.66,1@1), 
	    						JSplineV(x,1,0@0,0.33*1@0.33,0.66*0.84@0.66,0@1), 
	    						JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.06@0.66,0@1), 
	    						JSplineV(x,1,0@0,1@1));
	    	}
		else if (Colorize == "Enlightenment")
		{
			rHighlight = 1; 	gHighlight = 1;		bHighlight = 1;
			rMidHigh = 0.99;	gMidHigh = 0.79;	bMidHigh = 0.63;
			rMidTones = 0.95;  	gMidTones = 0.55;	bMidTones = 0.77;
			rMidLow = 0.57;		gMidLow = 0.48;		bMidLow = 0.94;
			rShadows = 0.27;	gShadows = 0.27;	bShadows = 1;
			Lookup3 = Lookup(ColorizeImage, JSplineV(x,1,0@0,0.2*0.27@0.2,0.4*0.57@0.4,0.6*0.95@0.6,0.8*0.99@0.8,1@1), 
	    						JSplineV(x,1,0@0,0.2*0.27@0.2,0.4*0.48@0.4,0.6*0.55@0.6,0.8*0.79@0.8,1@1), 
	    						JSplineV(x,1,0@0,0.2*1@0.2,0.4*0.97@0.4,0.6*0.77@0.6,0.8*0.63@0.8,1@1), 
	    						JSplineV(x,1,0@0,1@1));
	    	}
		else if (Colorize == "Radioaktiv")
		{
			rHighlight = 0.01; 	gHighlight = 0.2;	bHighlight = 0.09;
			rMidHigh = 0.03;	gMidHigh = 0.66;	bMidHigh = 0.09;
			rMidTones = 0.69;  	gMidTones = 1;		bMidTones = 0.02;
			rMidLow = 0;		gMidLow = 1;		bMidLow = 0.08;
			rShadows = 0.03;	gShadows = 0.38;	bShadows = 0.08;
			Lookup3 = Lookup(ColorizeImage, JSplineV(x,1,0@0,0.2*rShadows@0.2,0.4*rMidLow@0.4,0.6*rMidTones@0.6,0.8*rMidHigh@0.8,rHighlight@1), 
	    						JSplineV(x,1,0@0,0.2*gShadows@0.2,0.4*gMidLow@0.4,0.6*gMidTones@0.6,0.8*gMidHigh@0.8,gHighlight@1), 
	    						JSplineV(x,1,0@0,0.2*bShadows@0.2,0.4*bMidLow@0.4,0.6*bMidTones@0.6,0.8*bMidHigh@0.8,bHighlight@1), 
	    						JSplineV(x,1,0@0,1@1));
	    	}
		else if (Colorize == "IR Vision")
		{
			rHighlight = 1; 	gHighlight = 0;		bHighlight = 0;
			rMidHigh = 0.95;	gMidHigh = 0.93;	bMidHigh = 0.03;
			rMidTones = 0.13;  	gMidTones = 1;		bMidTones = 0.04;
			rMidLow = 0.13;		gMidLow = 0.83;		bMidLow = 0.2;
			rShadows = 0.08;	gShadows = 0.03;	bShadows = 1;
			Lookup3 = Lookup(ColorizeImage, JSplineV(x,1,0@0,0.2*0.08@0.2,0.4*0.13@0.4,0.6*0.13@0.6,0.8*0.95@0.8,1@1), 
	    						JSplineV(x,1,0@0,0.2*0.03@0.2,0.4*0.83@0.4,0.6*1@0.6,0.8*0.93@0.8,0@1), 
	    						JSplineV(x,1,0@0,0.2*1@0.2,0.4*0.2@0.4,0.6*0.04@0.6,0.8*0.03@0.8,0@1), 
	    						JSplineV(x,1,0@0,1@1));
	    	}
		else if (Colorize == "Lysergic")
		{
			rHighlight = 0; 	gHighlight = 1;		bHighlight = 1;
			rMidHigh = 1;		gMidHigh = 0.65;	bMidHigh = 1;
			rMidTones = 0;  	gMidTones = 1;		bMidTones = 0;
			rMidLow = 1;		gMidLow = 0.65;		bMidLow = 1;
			rShadows = 0;		gShadows = 1;		bShadows = 1;
			Lookup3 = Lookup(ColorizeImage, JSplineV(x,1,0@0,0.2*0@0.2,0.4*1@0.4,0.6*0@0.6,0.8*1@0.8,0@1), 
	    						JSplineV(x,1,0@0,0.2*1@0.2,0.4*0.65@0.4,0.6*1@0.6,0.8*0.65@0.8,1@1), 
	    						JSplineV(x,1,0@0,0.2*1@0.2,0.4*1@0.4,0.6*0@0.6,0.8*1@0.8,1@1), 
	    						JSplineV(x,1,0@0,1@1));
	    	}
		else if (Colorize == "Rainbow")
		{
			rHighlight = 1; 	gHighlight = 0;		bHighlight = 0;
			rMidHigh = 1;		gMidHigh = 1;		bMidHigh = 0;
			rMidTones = 0;  	gMidTones = 1;		bMidTones = 0;
			rMidLow = 0;		gMidLow = 0.5;		bMidLow = 0;
			rShadows = 0.5;		gShadows = 0;		bShadows = 1;
			Lookup3 = Lookup(ColorizeImage, JSplineV(x,1,0@0,0.2*0.5@0.2,0.4*0@0.4,0.6*0@0.6,0.8*1@0.8,1@1), 
	    						JSplineV(x,1,0@0,0.2*0@0.2,0.4*0.5@0.4,0.6*1@0.6,0.8*1@0.8,0@1), 
	    						JSplineV(x,1,0@0,0.2*1@0.2,0.4*0@0.4,0.6*0@0.6,0.8*0@0.8,0@1), 
	    						JSplineV(x,1,0@0,1@1));
	    	}
		else if (Colorize == "RGB")
		{
			rHighlight = 0; 	gHighlight = 0;		bHighlight = 0;
			rMidHigh = 1;		gMidHigh = 0;		bMidHigh = 0;
			rMidTones = 0;  	gMidTones = 1;		bMidTones = 0;
			rMidLow = 0;		gMidLow = 0;		bMidLow = 1;
			rShadows = 0;		gShadows = 0;		bShadows = 0;
			Lookup3 = Lookup(ColorizeImage,	JSplineV(x,1,0@0,0.2*0@0.2,0.4*0@0.4,0.6*0@0.6,0.8*1@0.8,0@1), 
	    						JSplineV(x,1,0@0,0.2*0@0.2,0.4*0@0.4,0.6*1@0.6,0.8*0@0.8,0@1), 
	    						JSplineV(x,1,0@0,0.2*0@0.2,0.4*1@0.4,0.6*0@0.6,0.8*0@0.8,0@1), 
	    						JSplineV(x,1,0@0,1@1));
	    	}
		else if (Colorize == "Technicolor")
		{
			rHighlight = 1; 	gHighlight = 1;		bHighlight = 1;
			rMidHigh = 1;		gMidHigh = 1;		bMidHigh = 0;
			rMidTones = 1;  	gMidTones = 0;		bMidTones = 0;
			rMidLow = 0;		gMidLow = 1;		bMidLow = 0;
			rShadows = 0;		gShadows = 0;		bShadows = 1;
			Lookup3 = Lookup(ColorizeImage, JSplineV(x,1,0@0,0.2*0@0.2,0.4*0@0.4,0.6*1@0.6,0.8*1@0.8,1@1), 
	    						JSplineV(x,1,0@0,0.2*0@0.2,0.4*1@0.4,0.6*0@0.6,0.8*1@0.8,1@1), 
	    						JSplineV(x,1,0@0,0.2*1@0.2,0.4*0@0.4,0.6*0@0.6,0.8*0@0.8,1@1), 
	    						JSplineV(x,1,0@0,1@1));
	    	}
		else if (Colorize == "Chess")
		{
			rHighlight = 1; 	gHighlight = 1;		bHighlight = 1;
			rMidHigh = 0;		gMidHigh = 0;		bMidHigh = 0;
			rMidTones = 1;  	gMidTones = 1;		bMidTones = 1;
			rMidLow = 0;		gMidLow = 0;		bMidLow = 0;
			rShadows = 1;		gShadows = 1;		bShadows = 1;
			Lookup3 = Lookup(ColorizeImage, JSplineV(x,1,0@0,0.2*1@0.2,0.4*0@0.4,0.6*1@0.6,0.8*0@0.8,1@1), 
	    						JSplineV(x,1,0@0,0.2*1@0.2,0.4*0@0.4,0.6*1@0.6,0.8*0@0.8,1@1), 
	    						JSplineV(x,1,0@0,0.2*1@0.2,0.4*0@0.4,0.6*1@0.6,0.8*0@0.8,1@1), 
	    						JSplineV(x,1,0@0,1@1));
	    	}
		else if (Colorize == "Pastell")
		{
			rHighlight = 1; 	gHighlight = 1;		bHighlight = 0.68;
			rMidHigh = 0.38;	gMidHigh = 0.84;	bMidHigh = 0.67;
			rMidTones = 0.95;  	gMidTones = 0.55;	bMidTones = 0.77;
			rMidLow = 0.57;		gMidLow = 0.48;		bMidLow = 0.94;
			rShadows = 0.28;	gShadows = 0.28;	bShadows = 1;
			Lookup3 = Lookup(ColorizeImage,	JSplineV(x,1,0@0,0.2*0.28@0.2,0.4*0.57@0.4,0.6*0.95@0.6,0.8*0.38@0.8,1.0@1), 
	    						JSplineV(x,1,0@0,0.2*0.28@0.2,0.4*0.48@0.4,0.6*0.55@0.6,0.8*0.84@0.8,1.0@1), 
	    						JSplineV(x,1,0@0,0.2*1.0@0.2,0.4*0.94@0.4,0.6*0.77@0.6,0.8*0.67@0.8,0.68@1), 
	    						JSplineV(x,1,0@0,1@1));
	    	}
		else if (Colorize == "Desert Sun")
		{
			rHighlight = 1; 	gHighlight = 0.8;	bHighlight = 0.09;
			rMidHigh = 1;		gMidHigh = 0.5;		bMidHigh = 0.06;
			rMidTones = 0.96;  	gMidTones = 0.24;	bMidTones = 0.1;
			rMidLow = 0.96;		gMidLow = 0.25;		bMidLow = 0.1;
			rShadows = 1;		gShadows = 0;		bShadows = 0;
			Lookup3 = Lookup(ColorizeImage, JSplineV(x,1,0@0,0.2*1@0.2,0.4*0.96@0.4,0.6*0.96@0.6,0.8*1.0@0.8,1@1), 
	    						JSplineV(x,1,0@0,0.2*0@0.2,0.4*0.25@0.4,0.6*0.24@0.6,0.8*0.5@0.8,0.8@1), 
	    						JSplineV(x,1,0@0,0.2*1@0.2,0.4*0.10@0.4,0.6*0.10@0.6,0.8*0.06@0.8,0.09@1), 
	    						JSplineV(x,1,0@0,1@1));
	    	}
	}
	Fade1 = Fade(background,SourceOpacity/100.0);
	Fade2 = Fade(Lookup3,ShineOpacity/100.0);
	AE_Layers1 = AE_Layers(Fade2, Fade1, 0, TransferMode);
	return AE_Layers1;
}

//-- TC StarGlow
image TC_StarGlow(
	image	background,
	string	InputChannel,
	float	Threshold, float ThresholdSoft,
	int	UseMask, float MaskRadius, float MaskFeather,float MaskPosX,float MaskPosY,
	float	StreakLength,
	float	BoostLight,
	float	UpLength,float DownLength,float	LeftLength,float RightLength,
	float	UpLeftLength,float UpRightLength,float DownLeftLength,float DownRightLength,
	string	UpColor,string DownColor,string	LeftColor,string RightColor,
	string	UpLeftColor,string UpRightColor,string DownLeftColor,string DownRightColor,
	string	ColorMapAType,
	float 	rHighlightA,float gHighlightA,float bHighlightA,
	float	rMidHighA,float gMidHighA,float bMidHighA,
	float	rMidTonesA,float gMidTonesA,float bMidTonesA,
	float	rMidLowA,float gMidLowA,float bMidLowA,
	float	rShadowsA,float gShadowsA,float bShadowsA,
	string	ColorMapBType,
	float 	rHighlightB,float gHighlightB,float bHighlightB,
	float	rMidHighB,float gMidHighB,float bMidHighB,
	float	rMidTonesB,float gMidTonesB,float bMidTonesB,
	float	rMidLowB,float gMidLowB,float bMidLowB,
	float	rShadowsB,float gShadowsB,float bShadowsB,
	string	ColorMapCType,
	float 	rHighlightC,float gHighlightC,float bHighlightC,
	float	rMidHighC,float gMidHighC,float bMidHighC,
	float	rMidTonesC,float gMidTonesC,float bMidTonesC,
	float	rMidLowC,float gMidLowC,float bMidLowC,
	float	rShadowsC,float gShadowsC,float bShadowsC,
	float	Amount,float Detail,float Phase,int UseLoop,int Revolutions,
	float	SourceOpacity,
	float	StarglowOpacity,
	string	TransferMode)
{
	float ColorMapAr(float x)
	{
		if (ColorMapAType == "One Color")
			return JSplineV(x,1,0@0,rMidTonesA@1);
		else if (ColorMapAType == "3 Color Gradient")
			return JSplineV(x,1,0@0,0.33*rShadowsA@0.33,0.66*rMidTonesA@0.66,rHighlightA@1); 
		else if (ColorMapAType == "5 Color Gradient")
			return JSplineV(x,1,0@0,0.2*rShadowsA@0.2,0.4*rMidLowA@0.4,0.6*rMidTonesA@0.6,0.8*rMidHighA@0.8,rHighlightA@1); 
		else if (ColorMapAType == "Fire")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*1@0.66,1@1);
		else if (ColorMapAType == "Mars")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*1@0.66,1@1);
		else if (ColorMapAType == "Chemistry")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.65@0.66,1@1);
		else if (ColorMapAType == "DeepSea")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.02@0.66,0.61@1);
		else if (ColorMapAType == "Electric")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.5@0.66,1@1);
		else if (ColorMapAType == "Spirit")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*1@0.66,1@1);
		else if (ColorMapAType == "Aura")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*0.92@0.66,1@1);
		else if (ColorMapAType == "Heaven")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*1@0.66,1@1);
		else if (ColorMapAType == "Romance")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*1@0.66,1@1);
		else if (ColorMapAType == "Magic")
			return JSplineV(x,1,0@0,0.33*0.79@0.33,0.66*1@0.66,1@1);
		else if (ColorMapAType == "USA")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*1@0.66,1@1);
		else if (ColorMapAType == "Rastafari")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*1@0.66,1@1);
		else if (ColorMapAType == "Enlightenment")
			return JSplineV(x,1,0@0,0.2*0.27@0.2,0.4*0.57@0.4,0.6*0.95@0.6,0.8*0.99@0.8,1@1);
		else if (ColorMapAType == "Radioaktiv")
			return JSplineV(x,1,0@0,0.2*0.03@0.2,0.4*0@0.4,0.6*0.69@0.6,0.8*0.03@0.8,0.01@1);
		else if (ColorMapAType == "IR Vision")
			return JSplineV(x,1,0@0,0.2*0.08@0.2,0.4*0.13@0.4,0.6*0.13@0.6,0.8*0.95@0.8,1@1);
		else if (ColorMapAType == "Lysergic")
			return JSplineV(x,1,0@0,0.2*0@0.2,0.4*1@0.4,0.6*0@0.6,0.8*1@0.8,0@1);
		else if (ColorMapAType == "Rainbow")
			return JSplineV(x,1,0@0,0.2*0.5@0.2,0.4*0@0.4,0.6*0@0.6,0.8*1@0.8,1@1);
		else if (ColorMapAType == "RGB")
			return JSplineV(x,1,0@0,0.2*0@0.2,0.4*0@0.4,0.6*0@0.6,0.8*1@0.8,0@1);
		else if (ColorMapAType == "Technicolor")
			return JSplineV(x,1,0@0,0.2*0@0.2,0.4*0@0.4,0.6*1@0.6,0.8*1@0.8,1@1);
		else if (ColorMapAType == "Chess")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*0@0.4,0.6*1@0.6,0.8*0@0.8,1@1);
		else if (ColorMapAType == "Pastell")
			return JSplineV(x,1,0@0,0.2*0.28@0.2,0.4*0.57@0.4,0.6*0.95@0.6,0.8*0.38@0.8,1.0@1);
		else if (ColorMapAType == "Desert Sun")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*0.96@0.4,0.6*0.96@0.6,0.8*1.0@0.8,1@1);
    	}
    	
	float ColorMapAg(float x)
	{
		if (ColorMapAType == "One Color")
			return JSplineV(x,1,0@0,gMidTonesA@1);
		else if (ColorMapAType == "3 Color Gradient")
			return JSplineV(x,1,0@0,0.33*gShadowsA@0.33,0.66*gMidTonesA@0.66,gHighlightA@1); 
		else if (ColorMapAType == "5 Color Gradient")
			return JSplineV(x,1,0@0,0.2*gShadowsA@0.2,0.4*gMidLowA@0.4,0.6*gMidTonesA@0.6,0.8*gMidHighA@0.8,gHighlightA@1); 
		else if (ColorMapAType == "Fire")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.65@0.66,1@1);
		else if (ColorMapAType == "Mars")
			return JSplineV(x,1,0@0,0.33*0.25@0.33,0.66*0.5@0.66,0.82@1);
		else if (ColorMapAType == "Chemistry")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*1@0.66,1@1);
		else if (ColorMapAType == "DeepSea")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*1@0.66,1@1);
		else if (ColorMapAType == "Electric")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*1@0.66,1@1);
		else if (ColorMapAType == "Spirit")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.5@0.66,1@1);
		else if (ColorMapAType == "Aura")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.5@0.66,0.75@1);
		else if (ColorMapAType == "Heaven")
			return JSplineV(x,1,0@0,0.33*0.5@0.33,0.66*0.91@0.66,1@1);
		else if (ColorMapAType == "Romance")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.5@0.66,1@1);
		else if (ColorMapAType == "Magic")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.5@0.66,1@1);
		else if (ColorMapAType == "USA")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0@0.66,1@1);
		else if (ColorMapAType == "Rastafari")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*0.84@0.66,0@1);
		else if (ColorMapAType == "Enlightenment")
			return JSplineV(x,1,0@0,0.2*0.27@0.2,0.4*0.48@0.4,0.6*0.55@0.6,0.8*0.79@0.8,1@1);
		else if (ColorMapAType == "Radioaktiv")
			return JSplineV(x,1,0@0,0.2*0.38@0.2,0.4*1@0.4,0.6*1@0.6,0.8*0.66@0.8,0.2@1);
		else if (ColorMapAType == "IR Vision")
			return JSplineV(x,1,0@0,0.2*0.03@0.2,0.4*0.83@0.4,0.6*1@0.6,0.8*0.93@0.8,0@1);
		else if (ColorMapAType == "Lysergic")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*0.65@0.4,0.6*1@0.6,0.8*0.65@0.8,1@1);
		else if (ColorMapAType == "Rainbow")
			return JSplineV(x,1,0@0,0.2*0@0.2,0.4*0.5@0.4,0.6*1@0.6,0.8*1@0.8,0@1);
		else if (ColorMapAType == "RGB")
			return JSplineV(x,1,0@0,0.2*0@0.2,0.4*0@0.4,0.6*1@0.6,0.8*0@0.8,0@1);
		else if (ColorMapAType == "Technicolor")
			return JSplineV(x,1,0@0,0.2*0@0.2,0.4*1@0.4,0.6*0@0.6,0.8*1@0.8,1@1);
		else if (ColorMapAType == "Chess")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*0@0.4,0.6*1@0.6,0.8*0@0.8,1@1);
		else if (ColorMapAType == "Pastell")
			return JSplineV(x,1,0@0,0.2*0.28@0.2,0.4*0.48@0.4,0.6*0.55@0.6,0.8*0.84@0.8,1.0@1);
		else if (ColorMapAType == "Desert Sun")
			return JSplineV(x,1,0@0,0.2*0@0.2,0.4*0.25@0.4,0.6*0.24@0.6,0.8*0.5@0.8,0.8@1);
    	}
    	
	float ColorMapAb(float x)
	{
		if (ColorMapAType == "One Color")
			return JSplineV(x,1,0@0,bMidTonesA@1);
		else if (ColorMapAType == "3 Color Gradient")
			return JSplineV(x,1,0@0,0.33*bShadowsA@0.33,0.66*bMidTonesA@0.66,bHighlightA@1); 
		else if (ColorMapAType == "5 Color Gradient")
			return JSplineV(x,1,0@0,0.2*bShadowsA@0.2,0.4*bMidLowA@0.4,0.6*bMidTonesA@0.6,0.8*bMidHighA@0.8,bHighlightA@1); 
		else if (ColorMapAType == "Fire")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0@0.66,1@1);
		else if (ColorMapAType == "Mars")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0@0.66,0.65@1);
		else if (ColorMapAType == "Chemistry")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0@0.66,1@1);
		else if (ColorMapAType == "DeepSea")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.6@0.66,0.98@1);
		else if (ColorMapAType == "Electric")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*1@0.66,1@1);
		else if (ColorMapAType == "Spirit")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*1@0.66,1@1);
		else if (ColorMapAType == "Aura")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*0.75@0.66,0.87@1);
		else if (ColorMapAType == "Heaven")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*0.91@0.66,1@1);
		else if (ColorMapAType == "Romance")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*1@0.66,1@1);
		else if (ColorMapAType == "Magic")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*0@0.66,0.82@1);
		else if (ColorMapAType == "USA")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*0@0.66,1@1);
		else if (ColorMapAType == "Rastafari")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.06@0.66,0@1);
		else if (ColorMapAType == "Enlightenment")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*0.97@0.4,0.6*0.77@0.6,0.8*0.63@0.8,1@1);
		else if (ColorMapAType == "Radioaktiv")
			return JSplineV(x,1,0@0,0.2*0.08@0.2,0.4*0.08@0.4,0.6*0.02@0.6,0.8*0.09@0.8,0.09@1);
		else if (ColorMapAType == "IR Vision")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*0.2@0.4,0.6*0.04@0.6,0.8*0.03@0.8,0@1);
		else if (ColorMapAType == "Lysergic")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*1@0.4,0.6*0@0.6,0.8*1@0.8,1@1);
		else if (ColorMapAType == "Rainbow")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*0@0.4,0.6*0@0.6,0.8*0@0.8,0@1);
		else if (ColorMapAType == "RGB")
			return JSplineV(x,1,0@0,0.2*0@0.2,0.4*1@0.4,0.6*0@0.6,0.8*0@0.8,0@1);
		else if (ColorMapAType == "Technicolor")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*0@0.4,0.6*0@0.6,0.8*0@0.8,1@1);
		else if (ColorMapAType == "Chess")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*0@0.4,0.6*1@0.6,0.8*0@0.8,1@1);
		else if (ColorMapAType == "Pastell")
			return JSplineV(x,1,0@0,0.2*1.0@0.2,0.4*0.94@0.4,0.6*0.77@0.6,0.8*0.67@0.8,0.68@1);
		else if (ColorMapAType == "Desert Sun")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*0.10@0.4,0.6*0.10@0.6,0.8*0.06@0.8,0.09@1);
    	}
    	
	float ColorMapBr(float x)
	{
		if (ColorMapBType == "One Color")
			return JSplineV(x,1,0@0,rMidTonesB@1);
		else if (ColorMapBType == "3 Color Gradient")
			return JSplineV(x,1,0@0,0.33*rShadowsB@0.33,0.66*rMidTonesB@0.66,rHighlightB@1); 
		else if (ColorMapBType == "5 Color Gradient")
			return JSplineV(x,1,0@0,0.2*rShadowsB@0.2,0.4*rMidLowB@0.4,0.6*rMidTonesB@0.6,0.8*rMidHighB@0.8,rHighlightB@1); 
		else if (ColorMapBType == "Fire")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*1@0.66,1@1);
		else if (ColorMapBType == "Mars")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*1@0.66,1@1);
		else if (ColorMapBType == "Chemistry")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.65@0.66,1@1);
		else if (ColorMapBType == "DeepSea")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.02@0.66,0.61@1);
		else if (ColorMapBType == "Electric")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.5@0.66,1@1);
		else if (ColorMapBType == "Spirit")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*1@0.66,1@1);
		else if (ColorMapBType == "Aura")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*0.92@0.66,1@1);
		else if (ColorMapBType == "Heaven")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*1@0.66,1@1);
		else if (ColorMapBType == "Romance")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*1@0.66,1@1);
		else if (ColorMapBType == "Magic")
			return JSplineV(x,1,0@0,0.33*0.79@0.33,0.66*1@0.66,1@1);
		else if (ColorMapBType == "USA")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*1@0.66,1@1);
		else if (ColorMapBType == "Rastafari")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*1@0.66,1@1);
		else if (ColorMapBType == "Enlightenment")
			return JSplineV(x,1,0@0,0.2*0.27@0.2,0.4*0.57@0.4,0.6*0.95@0.6,0.8*0.99@0.8,1@1);
		else if (ColorMapBType == "Radioaktiv")
			return JSplineV(x,1,0@0,0.2*0.03@0.2,0.4*0@0.4,0.6*0.69@0.6,0.8*0.03@0.8,0.01@1);
		else if (ColorMapBType == "IR Vision")
			return JSplineV(x,1,0@0,0.2*0.08@0.2,0.4*0.13@0.4,0.6*0.13@0.6,0.8*0.95@0.8,1@1);
		else if (ColorMapBType == "Lysergic")
			return JSplineV(x,1,0@0,0.2*0@0.2,0.4*1@0.4,0.6*0@0.6,0.8*1@0.8,0@1);
		else if (ColorMapBType == "Rainbow")
			return JSplineV(x,1,0@0,0.2*0.5@0.2,0.4*0@0.4,0.6*0@0.6,0.8*1@0.8,1@1);
		else if (ColorMapBType == "RGB")
			return JSplineV(x,1,0@0,0.2*0@0.2,0.4*0@0.4,0.6*0@0.6,0.8*1@0.8,0@1);
		else if (ColorMapBType == "Technicolor")
			return JSplineV(x,1,0@0,0.2*0@0.2,0.4*0@0.4,0.6*1@0.6,0.8*1@0.8,1@1);
		else if (ColorMapBType == "Chess")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*0@0.4,0.6*1@0.6,0.8*0@0.8,1@1);
		else if (ColorMapBType == "Pastell")
			return JSplineV(x,1,0@0,0.2*0.28@0.2,0.4*0.57@0.4,0.6*0.95@0.6,0.8*0.38@0.8,1.0@1);
		else if (ColorMapBType == "Desert Sun")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*0.96@0.4,0.6*0.96@0.6,0.8*1.0@0.8,1@1);
    	}
    	
	float ColorMapBg(float x)
	{
		if (ColorMapBType == "One Color")
			return JSplineV(x,1,0@0,gMidTonesB@1);
		else if (ColorMapBType == "3 Color Gradient")
			return JSplineV(x,1,0@0,0.33*gShadowsB@0.33,0.66*gMidTonesB@0.66,gHighlightB@1); 
		else if (ColorMapBType == "5 Color Gradient")
			return JSplineV(x,1,0@0,0.2*gShadowsB@0.2,0.4*gMidLowB@0.4,0.6*gMidTonesB@0.6,0.8*gMidHighB@0.8,gHighlightB@1); 
		else if (ColorMapBType == "Fire")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.65@0.66,1@1);
		else if (ColorMapBType == "Mars")
			return JSplineV(x,1,0@0,0.33*0.25@0.33,0.66*0.5@0.66,0.82@1);
		else if (ColorMapBType == "Chemistry")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*1@0.66,1@1);
		else if (ColorMapBType == "DeepSea")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*1@0.66,1@1);
		else if (ColorMapBType == "Electric")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*1@0.66,1@1);
		else if (ColorMapBType == "Spirit")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.5@0.66,1@1);
		else if (ColorMapBType == "Aura")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.5@0.66,0.75@1);
		else if (ColorMapBType == "Heaven")
			return JSplineV(x,1,0@0,0.33*0.5@0.33,0.66*0.91@0.66,1@1);
		else if (ColorMapBType == "Romance")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.5@0.66,1@1);
		else if (ColorMapBType == "Magic")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.5@0.66,1@1);
		else if (ColorMapBType == "USA")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0@0.66,1@1);
		else if (ColorMapBType == "Rastafari")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*0.84@0.66,0@1);
		else if (ColorMapBType == "Enlightenment")
			return JSplineV(x,1,0@0,0.2*0.27@0.2,0.4*0.48@0.4,0.6*0.55@0.6,0.8*0.79@0.8,1@1);
		else if (ColorMapBType == "Radioaktiv")
			return JSplineV(x,1,0@0,0.2*0.38@0.2,0.4*1@0.4,0.6*1@0.6,0.8*0.66@0.8,0.2@1);
		else if (ColorMapBType == "IR Vision")
			return JSplineV(x,1,0@0,0.2*0.03@0.2,0.4*0.83@0.4,0.6*1@0.6,0.8*0.93@0.8,0@1);
		else if (ColorMapBType == "Lysergic")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*0.65@0.4,0.6*1@0.6,0.8*0.65@0.8,1@1);
		else if (ColorMapBType == "Rainbow")
			return JSplineV(x,1,0@0,0.2*0@0.2,0.4*0.5@0.4,0.6*1@0.6,0.8*1@0.8,0@1);
		else if (ColorMapBType == "RGB")
			return JSplineV(x,1,0@0,0.2*0@0.2,0.4*0@0.4,0.6*1@0.6,0.8*0@0.8,0@1);
		else if (ColorMapBType == "Technicolor")
			return JSplineV(x,1,0@0,0.2*0@0.2,0.4*1@0.4,0.6*0@0.6,0.8*1@0.8,1@1);
		else if (ColorMapBType == "Chess")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*0@0.4,0.6*1@0.6,0.8*0@0.8,1@1);
		else if (ColorMapBType == "Pastell")
			return JSplineV(x,1,0@0,0.2*0.28@0.2,0.4*0.48@0.4,0.6*0.55@0.6,0.8*0.84@0.8,1.0@1);
		else if (ColorMapBType == "Desert Sun")
			return JSplineV(x,1,0@0,0.2*0@0.2,0.4*0.25@0.4,0.6*0.24@0.6,0.8*0.5@0.8,0.8@1);
    	}
    	
	float ColorMapBb(float x)
	{
		if (ColorMapBType == "One Color")
			return JSplineV(x,1,0@0,bMidTonesB@1);
		else if (ColorMapBType == "3 Color Gradient")
			return JSplineV(x,1,0@0,0.33*bShadowsB@0.33,0.66*bMidTonesB@0.66,bHighlightB@1); 
		else if (ColorMapBType == "5 Color Gradient")
			return JSplineV(x,1,0@0,0.2*bShadowsB@0.2,0.4*bMidLowB@0.4,0.6*bMidTonesB@0.6,0.8*bMidHighB@0.8,bHighlightB@1); 
		else if (ColorMapBType == "Fire")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0@0.66,1@1);
		else if (ColorMapBType == "Mars")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0@0.66,0.65@1);
		else if (ColorMapBType == "Chemistry")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0@0.66,1@1);
		else if (ColorMapBType == "DeepSea")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.6@0.66,0.98@1);
		else if (ColorMapBType == "Electric")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*1@0.66,1@1);
		else if (ColorMapBType == "Spirit")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*1@0.66,1@1);
		else if (ColorMapBType == "Aura")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*0.75@0.66,0.87@1);
		else if (ColorMapBType == "Heaven")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*0.91@0.66,1@1);
		else if (ColorMapBType == "Romance")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*1@0.66,1@1);
		else if (ColorMapBType == "Magic")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*0@0.66,0.82@1);
		else if (ColorMapBType == "USA")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*0@0.66,1@1);
		else if (ColorMapBType == "Rastafari")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.06@0.66,0@1);
		else if (ColorMapBType == "Enlightenment")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*0.97@0.4,0.6*0.77@0.6,0.8*0.63@0.8,1@1);
		else if (ColorMapBType == "Radioaktiv")
			return JSplineV(x,1,0@0,0.2*0.08@0.2,0.4*0.08@0.4,0.6*0.02@0.6,0.8*0.09@0.8,0.09@1);
		else if (ColorMapBType == "IR Vision")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*0.2@0.4,0.6*0.04@0.6,0.8*0.03@0.8,0@1);
		else if (ColorMapBType == "Lysergic")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*1@0.4,0.6*0@0.6,0.8*1@0.8,1@1);
		else if (ColorMapBType == "Rainbow")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*0@0.4,0.6*0@0.6,0.8*0@0.8,0@1);
		else if (ColorMapBType == "RGB")
			return JSplineV(x,1,0@0,0.2*0@0.2,0.4*1@0.4,0.6*0@0.6,0.8*0@0.8,0@1);
		else if (ColorMapBType == "Technicolor")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*0@0.4,0.6*0@0.6,0.8*0@0.8,1@1);
		else if (ColorMapBType == "Chess")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*0@0.4,0.6*1@0.6,0.8*0@0.8,1@1);
		else if (ColorMapBType == "Pastell")
			return JSplineV(x,1,0@0,0.2*1.0@0.2,0.4*0.94@0.4,0.6*0.77@0.6,0.8*0.67@0.8,0.68@1);
		else if (ColorMapBType == "Desert Sun")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*0.10@0.4,0.6*0.10@0.6,0.8*0.06@0.8,0.09@1);
    	}
    	
	float ColorMapCr(float x)
	{
		if (ColorMapCType == "One Color")
			return JSplineV(x,1,0@0,rMidTonesC@1);
		else if (ColorMapCType == "3 Color Gradient")
			return JSplineV(x,1,0@0,0.33*rShadowsC@0.33,0.66*rMidTonesC@0.66,rHighlightC@1); 
		else if (ColorMapCType == "5 Color Gradient")
			return JSplineV(x,1,0@0,0.2*rShadowsC@0.2,0.4*rMidLowC@0.4,0.6*rMidTonesC@0.6,0.8*rMidHighC@0.8,rHighlightC@1); 
		else if (ColorMapCType == "Fire")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*1@0.66,1@1);
		else if (ColorMapCType == "Mars")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*1@0.66,1@1);
		else if (ColorMapCType == "Chemistry")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.65@0.66,1@1);
		else if (ColorMapCType == "DeepSea")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.02@0.66,0.61@1);
		else if (ColorMapCType == "Electric")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.5@0.66,1@1);
		else if (ColorMapCType == "Spirit")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*1@0.66,1@1);
		else if (ColorMapCType == "Aura")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*0.92@0.66,1@1);
		else if (ColorMapCType == "Heaven")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*1@0.66,1@1);
		else if (ColorMapCType == "Romance")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*1@0.66,1@1);
		else if (ColorMapCType == "Magic")
			return JSplineV(x,1,0@0,0.33*0.79@0.33,0.66*1@0.66,1@1);
		else if (ColorMapCType == "USA")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*1@0.66,1@1);
		else if (ColorMapCType == "Rastafari")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*1@0.66,1@1);
		else if (ColorMapCType == "Enlightenment")
			return JSplineV(x,1,0@0,0.2*0.27@0.2,0.4*0.57@0.4,0.6*0.95@0.6,0.8*0.99@0.8,1@1);
		else if (ColorMapCType == "Radioaktiv")
			return JSplineV(x,1,0@0,0.2*0.03@0.2,0.4*0@0.4,0.6*0.69@0.6,0.8*0.03@0.8,0.01@1);
		else if (ColorMapCType == "IR Vision")
			return JSplineV(x,1,0@0,0.2*0.08@0.2,0.4*0.13@0.4,0.6*0.13@0.6,0.8*0.95@0.8,1@1);
		else if (ColorMapCType == "Lysergic")
			return JSplineV(x,1,0@0,0.2*0@0.2,0.4*1@0.4,0.6*0@0.6,0.8*1@0.8,0@1);
		else if (ColorMapCType == "Rainbow")
			return JSplineV(x,1,0@0,0.2*0.5@0.2,0.4*0@0.4,0.6*0@0.6,0.8*1@0.8,1@1);
		else if (ColorMapCType == "RGB")
			return JSplineV(x,1,0@0,0.2*0@0.2,0.4*0@0.4,0.6*0@0.6,0.8*1@0.8,0@1);
		else if (ColorMapCType == "Technicolor")
			return JSplineV(x,1,0@0,0.2*0@0.2,0.4*0@0.4,0.6*1@0.6,0.8*1@0.8,1@1);
		else if (ColorMapCType == "Chess")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*0@0.4,0.6*1@0.6,0.8*0@0.8,1@1);
		else if (ColorMapCType == "Pastell")
			return JSplineV(x,1,0@0,0.2*0.28@0.2,0.4*0.57@0.4,0.6*0.95@0.6,0.8*0.38@0.8,1.0@1);
		else if (ColorMapCType == "Desert Sun")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*0.96@0.4,0.6*0.96@0.6,0.8*1.0@0.8,1@1);
    	}
    	
	float ColorMapCg(float x)
	{
		if (ColorMapCType == "One Color")
			return JSplineV(x,1,0@0,gMidTonesC@1);
		else if (ColorMapCType == "3 Color Gradient")
			return JSplineV(x,1,0@0,0.33*gShadowsC@0.33,0.66*gMidTonesC@0.66,gHighlightC@1); 
		else if (ColorMapCType == "5 Color Gradient")
			return JSplineV(x,1,0@0,0.2*gShadowsC@0.2,0.4*gMidLowC@0.4,0.6*gMidTonesC@0.6,0.8*gMidHighC@0.8,gHighlightC@1); 
		else if (ColorMapCType == "Fire")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.65@0.66,1@1);
		else if (ColorMapCType == "Mars")
			return JSplineV(x,1,0@0,0.33*0.25@0.33,0.66*0.5@0.66,0.82@1);
		else if (ColorMapCType == "Chemistry")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*1@0.66,1@1);
		else if (ColorMapCType == "DeepSea")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*1@0.66,1@1);
		else if (ColorMapCType == "Electric")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*1@0.66,1@1);
		else if (ColorMapCType == "Spirit")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.5@0.66,1@1);
		else if (ColorMapCType == "Aura")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.5@0.66,0.75@1);
		else if (ColorMapCType == "Heaven")
			return JSplineV(x,1,0@0,0.33*0.5@0.33,0.66*0.91@0.66,1@1);
		else if (ColorMapCType == "Romance")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.5@0.66,1@1);
		else if (ColorMapCType == "Magic")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.5@0.66,1@1);
		else if (ColorMapCType == "USA")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0@0.66,1@1);
		else if (ColorMapCType == "Rastafari")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*0.84@0.66,0@1);
		else if (ColorMapCType == "Enlightenment")
			return JSplineV(x,1,0@0,0.2*0.27@0.2,0.4*0.48@0.4,0.6*0.55@0.6,0.8*0.79@0.8,1@1);
		else if (ColorMapCType == "Radioaktiv")
			return JSplineV(x,1,0@0,0.2*0.38@0.2,0.4*1@0.4,0.6*1@0.6,0.8*0.66@0.8,0.2@1);
		else if (ColorMapCType == "IR Vision")
			return JSplineV(x,1,0@0,0.2*0.03@0.2,0.4*0.83@0.4,0.6*1@0.6,0.8*0.93@0.8,0@1);
		else if (ColorMapCType == "Lysergic")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*0.65@0.4,0.6*1@0.6,0.8*0.65@0.8,1@1);
		else if (ColorMapCType == "Rainbow")
			return JSplineV(x,1,0@0,0.2*0@0.2,0.4*0.5@0.4,0.6*1@0.6,0.8*1@0.8,0@1);
		else if (ColorMapCType == "RGB")
			return JSplineV(x,1,0@0,0.2*0@0.2,0.4*0@0.4,0.6*1@0.6,0.8*0@0.8,0@1);
		else if (ColorMapCType == "Technicolor")
			return JSplineV(x,1,0@0,0.2*0@0.2,0.4*1@0.4,0.6*0@0.6,0.8*1@0.8,1@1);
		else if (ColorMapCType == "Chess")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*0@0.4,0.6*1@0.6,0.8*0@0.8,1@1);
		else if (ColorMapCType == "Pastell")
			return JSplineV(x,1,0@0,0.2*0.28@0.2,0.4*0.48@0.4,0.6*0.55@0.6,0.8*0.84@0.8,1.0@1);
		else if (ColorMapCType == "Desert Sun")
			return JSplineV(x,1,0@0,0.2*0@0.2,0.4*0.25@0.4,0.6*0.24@0.6,0.8*0.5@0.8,0.8@1);
    	}
    	
	float ColorMapCb(float x)
	{
		if (ColorMapCType == "One Color")
			return JSplineV(x,1,0@0,bMidTonesC@1);
		else if (ColorMapCType == "3 Color Gradient")
			return JSplineV(x,1,0@0,0.33*bShadowsC@0.33,0.66*bMidTonesC@0.66,bHighlightC@1); 
		else if (ColorMapCType == "5 Color Gradient")
			return JSplineV(x,1,0@0,0.2*bShadowsC@0.2,0.4*bMidLowC@0.4,0.6*bMidTonesC@0.6,0.8*bMidHighC@0.8,bHighlightC@1); 
		else if (ColorMapCType == "Fire")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0@0.66,1@1);
		else if (ColorMapCType == "Mars")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0@0.66,0.65@1);
		else if (ColorMapCType == "Chemistry")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0@0.66,1@1);
		else if (ColorMapCType == "DeepSea")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.6@0.66,0.98@1);
		else if (ColorMapCType == "Electric")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*1@0.66,1@1);
		else if (ColorMapCType == "Spirit")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*1@0.66,1@1);
		else if (ColorMapCType == "Aura")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*0.75@0.66,0.87@1);
		else if (ColorMapCType == "Heaven")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*0.91@0.66,1@1);
		else if (ColorMapCType == "Romance")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*1@0.66,1@1);
		else if (ColorMapCType == "Magic")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*0@0.66,0.82@1);
		else if (ColorMapCType == "USA")
			return JSplineV(x,1,0@0,0.33*1@0.33,0.66*0@0.66,1@1);
		else if (ColorMapCType == "Rastafari")
			return JSplineV(x,1,0@0,0.33*0@0.33,0.66*0.06@0.66,0@1);
		else if (ColorMapCType == "Enlightenment")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*0.97@0.4,0.6*0.77@0.6,0.8*0.63@0.8,1@1);
		else if (ColorMapCType == "Radioaktiv")
			return JSplineV(x,1,0@0,0.2*0.08@0.2,0.4*0.08@0.4,0.6*0.02@0.6,0.8*0.09@0.8,0.09@1);
		else if (ColorMapCType == "IR Vision")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*0.2@0.4,0.6*0.04@0.6,0.8*0.03@0.8,0@1);
		else if (ColorMapCType == "Lysergic")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*1@0.4,0.6*0@0.6,0.8*1@0.8,1@1);
		else if (ColorMapCType == "Rainbow")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*0@0.4,0.6*0@0.6,0.8*0@0.8,0@1);
		else if (ColorMapCType == "RGB")
			return JSplineV(x,1,0@0,0.2*0@0.2,0.4*1@0.4,0.6*0@0.6,0.8*0@0.8,0@1);
		else if (ColorMapCType == "Technicolor")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*0@0.4,0.6*0@0.6,0.8*0@0.8,1@1);
		else if (ColorMapCType == "Chess")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*0@0.4,0.6*1@0.6,0.8*0@0.8,1@1);
		else if (ColorMapCType == "Pastell")
			return JSplineV(x,1,0@0,0.2*1.0@0.2,0.4*0.94@0.4,0.6*0.77@0.6,0.8*0.67@0.8,0.68@1);
		else if (ColorMapCType == "Desert Sun")
			return JSplineV(x,1,0@0,0.2*1@0.2,0.4*0.10@0.4,0.6*0.10@0.6,0.8*0.06@0.8,0.09@1);
    	}
    	
	if (InputChannel == "Alpha")
		Reorder1 = Reorder(background, "aaaa");
	else if (InputChannel == "Red")
		Reorder1 = Reorder(background, "rrra");
	else if (InputChannel == "Green")
		Reorder1 = Reorder(background, "ggga");
	else if (InputChannel == "Blue")
		Reorder1 = Reorder(background, "bbba");
	else if (InputChannel == "Luminance")
		Reorder1 = ColorX(background,(r+g+b)/3,(r+g+b)/3,(r+g+b)/3,a,z);
	else if (InputChannel == "Lightness")
		Reorder1 = ColorX(background,0.30*r+0.59*g+0.11*b,0.30*r+0.59*g+0.11*b,0.30*r+0.59*g+0.11*b,a,z);
	else
		Reorder1 = background;
	
	if (UseMask == 1)
	{
		RGrad1 = RGrad(background.width, background.height, 1, MaskPosX, MaskPosY, 1, MaskRadius, MaskFeather, 0.5, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0);
	    	LayerX1 = LayerX(Reorder1,RGrad1,r*a2,g*a2,b*a2,a*a2,z);
	}
	else
	{
		LayerX1 = Reorder1;	
	}

	AE_Threshold1 = AE_Threshold(LayerX1, Threshold);

	// Build  Streaks
	UpStreaktemp = Move2D(AE_Threshold1, 0, Linear(2,0@1,StreakLength*UpLength@2), 0, 1, 1,1, 0,0, width/2, height/2, "default", xFilter, "trsx", 0, 5, 1, 0, 0, time);
	DownStreaktemp = Move2D(AE_Threshold1, 0, Linear(2,0@1,-StreakLength*DownLength@2), 0, 1, 1,1, 0,0, width/2, height/2, "default", xFilter, "trsx", 0, 5, 1, 0, 0, time);
	LeftStreaktemp = Move2D(AE_Threshold1, Linear(2,0@1,-StreakLength*LeftLength@2),0, 0, 1, 1,1, 0,0, width/2, height/2, "default", xFilter, "trsx", 0, 5, 1, 0, 0, time);
	RightStreaktemp = Move2D(AE_Threshold1, Linear(2,0@1,StreakLength*RightLength@2),0, 0, 1, 1,1, 0,0, width/2, height/2, "default", xFilter, "trsx", 0, 5, 1, 0, 0, time);
	UpLeftStreaktemp = Move2D(AE_Threshold1, Linear(2,0@1,-StreakLength*UpLeftLength@2), Linear(2,0@1,StreakLength*UpLeftLength@2), 0, 1, 1,1, 0,0, width/2, height/2, "default", xFilter, "trsx", 0, 5, 1, 0, 0, time);
	UpRightStreaktemp = Move2D(AE_Threshold1, Linear(2,0@1,StreakLength*UpRightLength@2), Linear(2,0@1,StreakLength*UpRightLength@2), 0, 1, 1,1, 0,0, width/2, height/2, "default", xFilter, "trsx", 0, 5, 1, 0, 0, time);
	DownLeftStreaktemp = Move2D(AE_Threshold1, Linear(2,0@1,-StreakLength*DownLeftLength@2),Linear(2,0@1,-StreakLength*DownLeftLength@2), 0, 1, 1,1, 0,0, width/2, height/2, "default", xFilter, "trsx", 0, 5, 1, 0, 0, time);
	DownRightStreaktemp = Move2D(AE_Threshold1, Linear(2,0@1,StreakLength*DownRightLength@2),Linear(2,0@1,-StreakLength*DownRightLength@2), 0, 1, 1,1, 0,0, width/2, height/2, "default", xFilter, "trsx", 0, 5, 1, 0, 0, time);

	// Boost Light
	UpStreak = ContrastRGB(UpStreaktemp,1.0+BoostLight/6, rValue, rValue, rValue, 0, rCenter, rCenter, rCenter, 0, rSoftClip, rSoftClip, rSoftClip);
	DownStreak = ContrastRGB(DownStreaktemp,1.0+BoostLight/6, rValue, rValue, rValue, 0, rCenter, rCenter, rCenter, 0, rSoftClip, rSoftClip, rSoftClip);
	LeftStreak = ContrastRGB(LeftStreaktemp,1.0+BoostLight/6, rValue, rValue, rValue, 0, rCenter, rCenter, rCenter, 0, rSoftClip, rSoftClip, rSoftClip);
	RightStreak = ContrastRGB(RightStreaktemp,1.0+BoostLight/6, rValue, rValue, rValue, 0, rCenter, rCenter, rCenter, 0, rSoftClip, rSoftClip, rSoftClip);
	UpLeftStreak = ContrastRGB(UpLeftStreaktemp,1.0+BoostLight/6, rValue, rValue, rValue, 0, rCenter, rCenter, rCenter, 0, rSoftClip, rSoftClip, rSoftClip);
	UpRightStreak = ContrastRGB(UpRightStreaktemp,1.0+BoostLight/6, rValue, rValue, rValue, 0, rCenter, rCenter, rCenter, 0, rSoftClip, rSoftClip, rSoftClip);
	DownLeftStreak = ContrastRGB(DownLeftStreaktemp,1.0+BoostLight/6, rValue, rValue, rValue, 0, rCenter, rCenter, rCenter, 0, rSoftClip, rSoftClip, rSoftClip);
	DownRightStreak = ContrastRGB(DownRightStreaktemp,1.0+BoostLight/6, rValue, rValue, rValue, 0, rCenter, rCenter, rCenter, 0, rSoftClip, rSoftClip, rSoftClip);

	if (UpColor == "ColorMap A")
		UpColored = Lookup(UpStreak,ColorMapAr(x),ColorMapAg(x),ColorMapAb(x),JSplineV(x,1,0@0,1@1));
	else if (UpColor == "ColorMap B")
		UpColored = Lookup(UpStreak,ColorMapBr(x),ColorMapBg(x),ColorMapBb(x),JSplineV(x,1,0@0,1@1));
	else if (UpColor == "ColorMap C")
		UpColored = Lookup(UpStreak,ColorMapCr(x),ColorMapCg(x),ColorMapCb(x),JSplineV(x,1,0@0,1@1));

	if (DownColor == "ColorMap A")
		DownColored = Lookup(DownStreak,ColorMapAr(x),ColorMapAg(x),ColorMapAb(x),JSplineV(x,1,0@0,1@1));
	else if (DownColor == "ColorMap B")
		DownColored = Lookup(DownStreak,ColorMapBr(x),ColorMapBg(x),ColorMapBb(x),JSplineV(x,1,0@0,1@1));
	else if (DownColor == "ColorMap C")
		DownColored = Lookup(DownStreak,ColorMapCr(x),ColorMapCg(x),ColorMapCb(x),JSplineV(x,1,0@0,1@1));
	AE_Layers2 = AE_Layers(DownColored,UpColored,0,TransferMode);
		
	if (LeftColor == "ColorMap A")
		LeftColored = Lookup(LeftStreak,ColorMapAr(x),ColorMapAg(x),ColorMapAb(x),JSplineV(x,1,0@0,1@1));
	else if (LeftColor == "ColorMap B")
		LeftColored = Lookup(LeftStreak,ColorMapBr(x),ColorMapBg(x),ColorMapBb(x),JSplineV(x,1,0@0,1@1));
	else if (LeftColor == "ColorMap C")
		LeftColored = Lookup(LeftStreak,ColorMapCr(x),ColorMapCg(x),ColorMapCb(x),JSplineV(x,1,0@0,1@1));
	AE_Layers3 = AE_Layers(LeftColored,AE_Layers2,0,TransferMode);
		
	if (RightColor == "ColorMap A")
		RightColored = Lookup(RightStreak,ColorMapAr(x),ColorMapAg(x),ColorMapAb(x),JSplineV(x,1,0@0,1@1));
	else if (RightColor == "ColorMap B")
		RightColored = Lookup(RightStreak,ColorMapBr(x),ColorMapBg(x),ColorMapBb(x),JSplineV(x,1,0@0,1@1));
	else if (RightColor == "ColorMap C")
		RightColored = Lookup(RightStreak,ColorMapCr(x),ColorMapCg(x),ColorMapCb(x),JSplineV(x,1,0@0,1@1));
	AE_Layers4 = AE_Layers(RightColored,AE_Layers3,0,TransferMode);
		
	if (UpLeftColor == "ColorMap A")
		UpLeftColored = Lookup(UpLeftStreak,ColorMapAr(x),ColorMapAg(x),ColorMapAb(x),JSplineV(x,1,0@0,1@1));
	else if (UpLeftColor == "ColorMap B")
		UpLeftColored = Lookup(UpLeftStreak,ColorMapBr(x),ColorMapBg(x),ColorMapBb(x),JSplineV(x,1,0@0,1@1));
	else if (UpLeftColor == "ColorMap C")
		UpLeftColored = Lookup(UpLeftStreak,ColorMapCr(x),ColorMapCg(x),ColorMapCb(x),JSplineV(x,1,0@0,1@1));
	AE_Layers5 = AE_Layers(UpLeftColored,AE_Layers4,0,TransferMode);

	if (UpRightColor == "ColorMap A")
		UpRightColored = Lookup(UpRightStreak,ColorMapAr(x),ColorMapAg(x),ColorMapAb(x),JSplineV(x,1,0@0,1@1));
	else if (UpRightColor == "ColorMap B")
		UpRightColored = Lookup(UpRightStreak,ColorMapBr(x),ColorMapBg(x),ColorMapBb(x),JSplineV(x,1,0@0,1@1));
	else if (UpRightColor == "ColorMap C")
		UpRightColored = Lookup(UpRightStreak,ColorMapCr(x),ColorMapCg(x),ColorMapCb(x),JSplineV(x,1,0@0,1@1));
	AE_Layers6 = AE_Layers(UpRightColored,AE_Layers5,0,TransferMode);
		
	if (DownLeftColor == "ColorMap A")
		DownLeftColored = Lookup(DownLeftStreak,ColorMapAr(x),ColorMapAg(x),ColorMapAb(x),JSplineV(x,1,0@0,1@1));
	else if (DownLeftColor == "ColorMap B")
		DownLeftColored = Lookup(DownLeftStreak,ColorMapBr(x),ColorMapBg(x),ColorMapBb(x),JSplineV(x,1,0@0,1@1));
	else if (DownLeftColor == "ColorMap C")
		DownLeftColored = Lookup(DownLeftStreak,ColorMapCr(x),ColorMapCg(x),ColorMapCb(x),JSplineV(x,1,0@0,1@1));
	AE_Layers7 = AE_Layers(DownLeftColored,AE_Layers6,0,TransferMode);
		
	if (DownRightColor == "ColorMap A")
		DownRightColored = Lookup(DownRightStreak,ColorMapAr(x),ColorMapAg(x),ColorMapAb(x),JSplineV(x,1,0@0,1@1));
	else if (DownRightColor == "ColorMap B")
		DownRightColored = Lookup(DownRightStreak,ColorMapBr(x),ColorMapBg(x),ColorMapBb(x),JSplineV(x,1,0@0,1@1));
	else if (DownRightColor == "ColorMap C")
		DownRightColored = Lookup(DownRightStreak,ColorMapCr(x),ColorMapCg(x),ColorMapCb(x),JSplineV(x,1,0@0,1@1));
	AE_Layers8 = AE_Layers(DownRightColored,AE_Layers7,0,TransferMode);
		
	Fade1 = Fade(background,SourceOpacity/100.0);
	Fade2 = Fade(AE_Layers8,StarglowOpacity/100.0);
	AE_Layers1 = AE_Layers(Fade2,Fade1,0,TransferMode);
	return AE_Layers1;
}


//-- DF FeedBack
image DF_FeedBack(
	image	Background=0,
	float	CenterX=width/2,
	float	CenterY=height/2,
	float	Velocity=50.0,
	float	Opacity=100.0,
	int	Iterations=10,
	int	Flip=0)
{
	image	Final = Background;
	int 	i=1;
	while (i<Iterations)
	{
		Resized = Scale(Background,pow(Velocity/100.0,i),xScale,CenterX,CenterY);
		if ((Flip == 1) && (i&1))
			Resized = Flop(Resized);
		Resized = Fade(Resized,Opacity/100.0);
		Final = Over(Resized,Final);
		i++;
	}
	return Final;
}

//-- AE Checkerboard
image AE_CheckerBoard(
	image	background,
	float	xCenter=width/2,
	float	yCenter=height/2,
	string	SizeFrom="Width Sliders",
	float	xCorner=width/2,
	float	yCorner=height/2,
	int	CheckerWidth=100,
	int	CheckerHeight=CheckerWidth,
	float	FeatherWidth=0.0,
	float	FeatherHeight=0.0,
	float	rColor=1,
	float	gColor=1,
	float	bColor=1,
	float	Opacity=100.0,
	string	BlendingMode="Normal")
{
	if (SizeFrom == "Width Sliders")
	{
		ColorX1 = ColorX(0, x>xCenter?(1+(int)(x-xCenter)/CheckerWidth)&1:1-(1+(int)(xCenter-x)/CheckerWidth)&1, g, b, a, z);
		ColorX2 = ColorX(0, y>yCenter?(1+(int)(y-yCenter)/CheckerWidth)&1:1-(1+(int)(yCenter-y)/CheckerWidth)&1, g, b, a, z);
		Reorder1 = Reorder(ColorX1, "rrrr");
		Reorder2 = Reorder(ColorX2, "rrrr");
		Layer1 = Layer(Reorder1, Reorder2, "Xor", 0, 0);
		Mult1 = Mult(Layer1, rColor, gColor, bColor, Opacity/100.0, 1);
		Final = Blur(Mult1, FeatherWidth, FeatherHeight, 0, "gauss", xFilter, "rgba");
	}
	else if (SizeFrom == "Width & Height Sliders")
	{
		ColorX1 = ColorX(0, x>xCenter?(1+(int)(x-xCenter)/CheckerWidth)&1 :1-(1+(int)(xCenter-x)/CheckerWidth)&1, g, b, a, z);
		ColorX2 = ColorX(0, y>yCenter?(1+(int)(y-yCenter)/CheckerHeight)&1:1-(1+(int)(yCenter-y)/CheckerHeight)&1, g, b, a, z);
		Reorder1 = Reorder(ColorX1, "rrrr");
		Reorder2 = Reorder(ColorX2, "rrrr");
		Layer1 = Layer(Reorder1, Reorder2, "Xor", 0, 0);
		Mult1 = Mult(Layer1, rColor, gColor, bColor, Opacity/100.0, 1);
		Final = Blur(Mult1, FeatherWidth, FeatherHeight, 0, "gauss", xFilter, "rgba");
	}
	else if (SizeFrom == "Corner Point")
	{
		ColorX1 = ColorX(0, x>xCorner?(1+(int)(x-xCorner)/abs(xCenter - xCorner))&1 :1-(1+(int)(xCorner-x)/abs(xCenter - xCorner))&1, g, b, a, z);
		ColorX2 = ColorX(0, y>yCorner?(1+(int)(y-yCorner)/abs(yCenter - yCorner))&1:1-(1+(int)(yCorner-y)/abs(yCenter - yCorner))&1, g, b, a, z);
		Reorder1 = Reorder(ColorX1, "rrrr");
		Reorder2 = Reorder(ColorX2, "rrrr");
		Layer1 = Layer(Reorder1, Reorder2, "Xor", 0, 0);
		Mult1 = Mult(Layer1, rColor, gColor, bColor, Opacity/100.0, 1);
		Final = Blur(Mult1, FeatherWidth, FeatherHeight, 0, "gauss", xFilter, "rgba");
	}
	
	AE_Layers1 = AE_Layers(Final,background,0,BlendingMode);
	return AE_Layers1;
}

//-- AE EyeDrop Fill
image AE_EyeDropFill(
	image	background,
	float	SampleX=width/2,
	float	SampleY=height/2,
	int	MaintainAlpha=0,
	float	Blend=0.0)
{
	WarpX1 = WarpX(background, 1, SampleX, SampleY, 0, xDelta);
	
	if (MaintainAlpha == 1)
	{
		Copy1 = Copy(WarpX1, background, 1, "a");
		Final = ColorX(Copy1, r*a, g*a, b*a, a, z);
	}
	else
		Final = WarpX1;

	return Mix(background,Final,1,100.0-Blend,"rgba");    			
}

//-- AE Magnify
image AE_Magnify(
	image	background,
	string	Shape="Circle",
	float	CenterX=width/2,
	float	CenterY=height/2,
	float	Magnification=150.0,
	string	Link="None",
	float	Size=100.0,
	float	Feather=0.0,
	float	Opacity=100.0,
	string	BlendingMode="Normal")
{
	float	TheSize;
	float	TheFeather;
	if (Link == "None")
	{
		TheSize = Size;
		TheFeather = Feather;
	}
	else if (Link == "Size To Magnification")
	{
		TheSize = Size + (Magnification-100.0);
		TheFeather = Feather;	
	}	
	else
	{
		TheSize = Size + (Magnification-100.0);
		TheFeather = Magnification/100.0*Feather;	
	}
	
	if (Shape == "Circle")
	{
		RGrad1 = RGrad(background.width, background.height, 1, CenterX, CenterY, 1, TheSize, TheFeather, 0.5, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0);
		Scale1 = Scale(background, Magnification/100.0, xScale, CenterX, CenterY, 0, 0.5, 0);
		IMult1 = IMult(Scale1, RGrad1, 1, 100, 0);
		Final = Fade(IMult1,Opacity/100.0);
	}
	else
	{
		Color1 = Color(2*TheSize, 2*TheSize, 1, 1, 1, 1, 1, 0);
		Color2 = Color(background.width, background.height, 1, 0, red, red, 0, 0);
		Pan1 = Pan(Color1, CenterX-TheSize, CenterY-TheSize, 0, 0.5, 0);
		Over1 = Over(Pan1, Color2, 1, 0, 0);
		Blur1 = Blur(Over1, TheFeather, xPixels, 0, "gauss", xFilter, "rgba");
		Scale1 = Scale(background, Magnification/100.0, xScale, CenterX, CenterY, 0, 0.5, 0);
		IMult1 = IMult(Scale1, Blur1, 1, 100, 0);
		Final = Fade(IMult1,Opacity/100.0);
	}
	
	AE_Layers1 = AE_Layers(Final,background,0,BlendingMode);
	return AE_Layers1;
}

// AE NoiseAlpha2
image AE_NoiseAlpha2(
	image	background,
	float	Amount,
	float	RandomSeed)
{
	Rand1 = Rand(background.width, background.height, 1, Amount/100.0, RandomSeed);
	Monochrome1 = Monochrome(Rand1, 0.3, 0.59, 0.11);
	Invert1 = Invert(Monochrome1, "rgba");
	Reorder1 = Reorder(Invert1, "111a");
	Final = IMult(background, Reorder1, 1, 100, 0);
	return Final;	
}
	
// AE NoiseHLS
image AE_NoiseHLS(
	image	background,
	float	HueAmount=0,
	float	LightnessAmount=0,
	float	SaturationAmount=0,
	float	NoisePhase=0)
{
	Rand1 = Rand(background.width, background.height, 1, 1, NoisePhase);
	ColorSpace1 = ColorSpace(background, "rgb", "hls", 0.3, 0.59, 0.11);
	Monochrome1 = Monochrome(Rand1, 0.3, 0.59, 0.11);
	ColorX1 = ColorX(Monochrome1, r*HueAmount/100.0, g*LightnessAmount/100.0, b*SaturationAmount/100.0, 0, 0);
	Invert1 = Invert(ColorX1, "rgba");
	IMult1 = IMult(ColorSpace1, Invert1, 1, 100, 0);
	Final = ColorSpace(IMult1, "hls", "rgb", 0.3, 0.59, 0.11);
	return Final;
}

// AE Change Color
image AE_ChangeColor(
	image	background,
	float	HueTransform=0.0,
	float	LightnessTransform=0.0,
	float	SaturationTransform=0.0,
	float	rColor=1.0,
	float	gColor=1.0,
	float	bColor=1.0,
	float	MatchingTolerance=25.0,
	float	MatchingSoftness=0.0,
	int	InvertColorCorrectionMask=0)
{
	ColorSpace1 = ColorSpace(background, "rgb", "hls", 0.3, 0.59, 0.11);
	ColorX4 = ColorX(background, 	fabs(r-rColor)<=MatchingTolerance/100.0?1:
						fabs(r-rColor)<=(MatchingTolerance+MatchingSoftness)/100.0?
							1.0-(fabs(r-rColor)-MatchingTolerance/100.0)/(MatchingSoftness/100.0):0, 
		    			fabs(g-gColor)<=MatchingTolerance/100.0?1:
						fabs(g-gColor)<=(MatchingTolerance+MatchingSoftness)/100.0?
							1.0-(fabs(g-gColor)-MatchingTolerance/100.0)/(MatchingSoftness/100.0):0, 
	    				fabs(b-bColor)<=MatchingTolerance/100.0?1:
						fabs(b-bColor)<=(MatchingTolerance+MatchingSoftness)/100.0?
							1.0-(fabs(b-bColor)-MatchingTolerance/100.0)/(MatchingSoftness/100.0):0, 
	    				0, 0);
	ColorX5 = ColorX(ColorX4, 1, 1, 1, r*g*b, 0);
	if (InvertColorCorrectionMask)
		ColorCorrectionMask = Invert(ColorX5);
	else
		ColorCorrectionMask = ColorX5;
	ColorX3 = ColorX(ColorSpace1, r+HueTransform/180.0, g+LightnessTransform/100.0, b+SaturationTransform/100.0, a, z);
	Mask(ColorX3, ColorCorrectionMask, "A", 100, 0, 1);
	Final = ColorSpace(ColorX3, "hls", "rgb", 0.3, 0.59, 0.11);
	return Final;
}

float	RGB2HLS(float r,float g,float b,float * h,float * l ,float * s)
{
	float	MaxVal = max3(r,g,b);
	float	MinVal = min3(r,g,b);
	
	*l = (MaxVal+MinVal)/2.0;
	if (MaxVal == MinVal)
	{
		*s = 0.0;
		*h = 0.0;
	}
	else
	{
		*s = 1.0;
		*h = 0.5;
	}
}

// AE Change To Color
image AE_ChangeToColor(
	image	background,
	float	rFrom=1,
	float	gFrom=1,
	float	bFrom=1,
	float	rTo=1,
	float	gTo=1,
	float	bTo=1,
	string	ChangeChannels="Hue",
	string	ChangeBy="Setting To Color",
	float	HueTolerance=5.0,
	float	LightnessTolerance=50.0,
	float	SaturationTolerance=50.0,
	float	Softness=0.0,
	int	ViewCorrectionMatte=0)
{
	ColorSpace1 = ColorSpace(background, "rgb", "hls", 0.3, 0.59, 0.11);
	Color1 = Color(background.width, background.height, 1, rFrom, gFrom, bFrom, 1, 0);
	ColorSpace2 = ColorSpace(Color1, "rgb", "hls", 0.3, 0.59, 0.11);

	LayerX1 = LayerX(ColorSpace1,ColorSpace2,
				 	fabs(r-r2)<=HueTolerance/100.0?
				 		1.0:
						fabs(r-r2)<=(HueTolerance+Softness)/100.0?
							1.0-(fabs(r-r2)-HueTolerance/100.0)/(Softness/100.0):
							0.0, 
		    			fabs(g-g2)<=LightnessTolerance/100.0?
		    				1.0:
						fabs(g-g2)<=(LightnessTolerance+Softness)/100.0?
							1.0-(fabs(g-g2)-LightnessTolerance/100.0)/(Softness/100.0):
							0.0, 
	    				fabs(b-b2)<=SaturationTolerance/100.0?
	    					1.0:
						fabs(b-b2)<=(SaturationTolerance+Softness)/100.0?
							1.0-(fabs(b-b2)-SaturationTolerance/100.0)/(Softness/100.0):
							0.0, 
	    				0.0, 0.0);
	ColorX1 = ColorX(LayerX1,1,1,1,r*g*b,0);
	
	if (ViewCorrectionMatte == 1)
		return Reorder(ColorX1, "aaaa");

	Color2 = Color(background.width, background.height, 1, rTo, gTo, bTo, 1, 0);
	ColorSpace3 = ColorSpace(Color2, "rgb", "hls", 0.3, 0.59, 0.11);

	if (ChangeBy == "Setting To Color")
	{
		if (ChangeChannels == "Hue")
			Final = LayerX(ColorSpace1,ColorSpace3,r2,g,b,a,z);
		else if (ChangeChannels == "Hue & Lightness")
			Final = LayerX(ColorSpace1,ColorSpace3,r2,g2,b,a,z);
		else if (ChangeChannels == "Hue & Saturation")
			Final = LayerX(ColorSpace1,ColorSpace3,r2,g,b2,a,z);
		else
			Final = LayerX(ColorSpace1,ColorSpace3,r2,g2,b2,a,z);
		Mask(Final, ColorX1, "A", 100, 0, 1);
	}
	else
	{
		ColorSpace3 = Copy(ColorSpace3, ColorX1, 1, "a");

		if (ChangeChannels == "Hue")
			Final = LayerX(ColorSpace1,ColorSpace3,r+(r2-r)*a2,g,b,a,z);
		else if (ChangeChannels == "Hue & Lightness")
			Final = LayerX(ColorSpace1,ColorSpace3,r+(r2-r)*a2,g+(g2-g)*a2,b,a,z);
		else if (ChangeChannels == "Hue & Saturation")
			Final = LayerX(ColorSpace1,ColorSpace3,r+(r2-r)*a2,g,b+(b2-b)*a2,a,z);
		else
			Final = LayerX(ColorSpace1,ColorSpace3,r+(r2-r)*a2,g+(g2-g)*a2,b+(b2-b)*a2,a,z);
	}
	return ColorSpace(Final, "hls", "rgb", 0.3, 0.59, 0.11);
}

// AE Circle
image AE_Circle(
	image	background,
	float	xCenter=width/2,
	float	yCenter=height/2,
	float	Radius=100.0,
	string	Edge="None",
	float	Thickness=50.0,
	float	FeatherOuterEdge=0.0,
	float	FeatherInnerEdge=0.0,
	int	InvertCircle=0,
	float	rColor=1.0,
	float	gColor=1.0,
	float	bColor=1.0,
	float	Opacity=100.0,
	string	BlendingMode="Over")
{
	if (Edge == "None")
	{
		AE_Arc1 = AE_Arc(	background.width, background.height, 
					rColor, gColor, bColor, 
					0, 360, 
					0, Radius, 
					xCenter, yCenter, 
					100);
		Blur1 = Blur(AE_Arc1, 3*FeatherOuterEdge, xPixels, 0, "gauss", xFilter, "rgba");
		if (InvertCircle)
		{
			Color1 = Color(background.width, background.height, 1, rColor, gColor, bColor, 1, 0);
			Final = ISubA(Color1, Blur1, 1, 100);
		}
		else
			Final = Blur1;
	}
	else if (Edge == "Edge Radius")
	{
		AE_Arc1 = AE_Arc(	background.width, background.height, 
					rColor, gColor, bColor, 
					0, 360, 
					0, Radius, 
					xCenter, yCenter, 
					100);
		Blur1 = Blur(AE_Arc1, 3*FeatherOuterEdge, xPixels, 0, "gauss", xFilter, "rgba");
		AE_Arc2 = AE_Arc(	background.width, background.height, 
					rColor, gColor, bColor, 
					0, 360, 
					0, Thickness, 
					xCenter, yCenter, 
					100);
		Blur2 = Blur(AE_Arc2, 3*FeatherInnerEdge, xPixels, 0, "gauss", xFilter, "rgba");

		if (!InvertCircle)
		{
			Final = ISubA(Blur2, Blur1, 1, 100);
		}
		else
		{
			Color1 = Color(background.width, background.height, 1, rColor, gColor, bColor, 1, 0);
			if (Thickness>Radius)
			{
				Outside2 = Outside(Color1, Blur2, 1);
				Final = ISubA(Outside2, Blur1, 1, 100);
			}
			else
			{
				Outside2 = Outside(Color1, Blur1, 1);
				Final = ISubA(Outside2, Blur2, 1, 100);
			}
		}
	}
	else if (Edge == "Thickness")
	{
		AE_Arc1 = AE_Arc(	background.width, background.height, 
					rColor, gColor, bColor, 
					0, 360, 
					0, Radius-Thickness/2, 
					xCenter, yCenter, 
					100);
		Blur1 = Blur(AE_Arc1, 3*FeatherInnerEdge, xPixels, 0, "gauss", xFilter, "rgba");
		AE_Arc2 = AE_Arc(	background.width, background.height, 
					rColor, gColor, bColor, 
					0, 360, 
					0, Radius+Thickness/2, 
					xCenter, yCenter, 
					100);
		Blur2 = Blur(AE_Arc2, 3*FeatherOuterEdge, xPixels, 0, "gauss", xFilter, "rgba");

		if (!InvertCircle)
		{
			Final = ISubA(Blur2, Blur1, 1, 100);
		}
		else
		{
			Color1 = Color(background.width, background.height, 1, rColor, gColor, bColor, 1, 0);
			if (Thickness>Radius)
			{
				Outside2 = Outside(Color1, Blur2, 1);
				Final = ISubA(Outside2, Blur1, 1, 100);
			}
			else
			{
				Outside2 = Outside(Color1, Blur1, 1);
				Final = ISubA(Outside2, Blur2, 1, 100);
			}
		}
	}
	else if (Edge == "Thickness * Radius")
	{
		AE_Arc1 = AE_Arc(	background.width, background.height, 
					rColor, gColor, bColor, 
					0, 360, 
					0, Radius-Radius*Thickness/200.0, 
					xCenter, yCenter, 
					100);
		Blur1 = Blur(AE_Arc1, 3*FeatherInnerEdge, xPixels, 0, "gauss", xFilter, "rgba");
		AE_Arc2 = AE_Arc(	background.width, background.height, 
					rColor, gColor, bColor, 
					0, 360, 
					0, Radius+Radius*Thickness/200.0, 
					xCenter, yCenter, 
					100);
		Blur2 = Blur(AE_Arc2, 3*FeatherOuterEdge, xPixels, 0, "gauss", xFilter, "rgba");
		
		if (!InvertCircle)
		{
			Final = ISubA(Blur2, Blur1, 1, 100);
		}
		else
		{
			Color1 = Color(background.width, background.height, 1, rColor, gColor, bColor, 1, 0);
			if (Thickness>Radius)
			{
				Outside2 = Outside(Color1, Blur2, 1);
				Final = ISubA(Outside2, Blur1, 1, 100);
			}
			else
			{
				Outside2 = Outside(Color1, Blur1, 1);
				Final = ISubA(Outside2, Blur2, 1, 100);
			}
		}
	}
	else if (Edge == "Thickness&Feather * Radius")
	{
		AE_Arc1 = AE_Arc(	background.width, background.height, 
					rColor, gColor, bColor, 
					0, 360, 
					0, Radius-Radius*Thickness/200.0, 
					xCenter, yCenter, 
					100);
		Blur1 = Blur(AE_Arc1, 3*Radius*FeatherInnerEdge/100.0, xPixels, 0, "gauss", xFilter, "rgba");
		AE_Arc2 = AE_Arc(	background.width, background.height, 
					rColor, gColor, bColor, 
					0, 360, 
					0, Radius+Radius*Thickness/200.0, 
					xCenter, yCenter, 
					100);
		Blur2 = Blur(AE_Arc2, 3*Radius*FeatherOuterEdge/100.0, xPixels, 0, "gauss", xFilter, "rgba");
		
		if (!InvertCircle)
		{
			Final = ISubA(Blur2, Blur1, 1, 100);
		}
		else
		{
			Color1 = Color(background.width, background.height, 1, rColor, gColor, bColor, 1, 0);
			if (Thickness>Radius)
			{
				Outside2 = Outside(Color1, Blur2, 1);
				Final = ISubA(Outside2, Blur1, 1, 100);
			}
			else
			{
				Outside2 = Outside(Color1, Blur1, 1);
				Final = ISubA(Outside2, Blur2, 1, 100);
			}
		}
	}
	FinalOpacity = Fade(Final,Opacity/100.0);
	AE_Layers1 = AE_Layers(FinalOpacity, background, 0, BlendingMode);
	
	return AE_Layers1;
}

// AE Explode
image AE_Explode(image 	background=0,
		 int	XTiles=3,
		 int	YTiles=3,
		 int	StartFrame=25,
		 float	Gravity=9.8,
		 float	RotationSpeed=60.0)
{
	Final = Black(720,486);
	float thetime = (time-StartFrame)/framesPerSecond;
	for (i=0;i<XTiles;i++)
	{
		for (j=0;j<YTiles;j++)
		{
			Tile = Crop(	background, 
					i*background.width/XTiles,
					j*background.height/YTiles,
					(i+1)*background.width/XTiles,
					(j+1)*background.height/YTiles);
			TileMove = Move3D( Tile, 
					   (int)(i*background.width/XTiles),
					   time<=StartFrame?
					   (int)(j*background.height/YTiles):
					   (int)(j*background.height/YTiles-0.5*10*(0.5+noise2d(i,j))*Gravity*thetime*thetime),
					   0,
					   time<=StartFrame?0:RotationSpeed*(noise2d(6*i,5*j)-0.5)*thetime,
					   time<=StartFrame?0:RotationSpeed*(noise2d(3*i,2*j)-0.5)*thetime, 
					   time<=StartFrame?0:RotationSpeed*(noise2d(9*i,7*j)-0.5)*thetime, 
					   1, 1, xScale, yScale, width/2, height/2, 0, 0);
			Final = Over(TileMove,Final);
		}	
	}
	return Final;
}		

// SB Cube
image SB_Cube(	image	background=0,
		float	XPos=0.0,
		float	YPos=0.0,
		float	ZPos=0.0,
		float	XAngle=0.0,
		float	YAngle=0.0,
		float	ZAngle=0.0,
		float	Scale=50.0,
		float	RColor=1.0,
		float	GColor=0.5,
		float	BColor=0.0,
		float	XLightVector= 0.0,
		float	YLightVector= 0.0,
		float	ZLightVector= 1.0)
{
	float	cx = cosd(XAngle);
	float	cy = cosd(YAngle);
	float	cz = cosd(ZAngle);

	float	sx = sind(XAngle);
	float	sy = sind(YAngle);
	float	sz = sind(ZAngle);

	float	M00 = cz*cy;		float	M01 = sz*cx+cz*sy*sx;	float	M02 = sz*sx-cz*sy*cx;
	float	M10 = -sz*cy;		float	M11 = cz*cx-sz*sy*sx;	float	M12 = cz*sx+sz*sy*cx;
	float	M20 = sy;		float	M21 = -cy*sx;		float	M22 = cy*cx;

	// Front
	// n = (0,0,1)
	float	f1 = 0.5+0.5*( XLightVector*M02+YLightVector*M12+ZLightVector*M22);
	// Right
	// n = (1,0,0)
	float	f2 = 0.5+0.5*(-XLightVector*M00-YLightVector*M10-ZLightVector*M20);
	// Bottom
	// n = (0,0,-1)
	float	f3 = 0.5+0.5*(-XLightVector*M02-YLightVector*M12-ZLightVector*M22);
	// Left
	// n = (-1,0,0)
	float	f4 = 0.5+0.5*( XLightVector*M00+YLightVector*M10+ZLightVector*M20);
	// Top
	// n = (0,1,0)
	float	f5 = 0.5+0.5*(-XLightVector*M01-YLightVector*M11-ZLightVector*M21);
	// Back
	// n = (0,-1,0)
	float	f6 = 0.5+0.5*( XLightVector*M01+YLightVector*M11+ZLightVector*M21);

	TheCube = NGLRender(
		width,
		height,
		1,
		"
		nglMatrixMode(NGL_PROJECTION);
		nglPerspective(45.0,(float)(width)/(float)(height),-(float)(width/2),-(float)(height/2),(float)(width/2),(float)(height/2));
		nglMatrixMode(NGL_MODELVIEW);
		nglPushMatrix();
		nglScalef(Scale,Scale,Scale);
		nglRotatef(XAngle,1.0f,0.0f,0.0f);
		nglRotatef(YAngle,0.0f,1.0f,0.0f);
		nglRotatef(ZAngle,0.0f,0.0f,1.0f);
		nglTranslatef(XPos,YPos,ZPos);
		if (M22>0.1)
		{
			nglColor3f(f1*RColor,f1*GColor,f1*BColor);
			nglBegin(NGL_POLYGON);
				// Front
				// n = (0,0,1)
				nglVertex3f(-1.0f, 1.0f, 1.0f);
				nglVertex3f( 1.0f, 1.0f, 1.0f);
				nglVertex3f( 1.0f,-1.0f, 1.0f);
				nglVertex3f(-1.0f,-1.0f, 1.0f);
			nglEnd();
		}
		if (-M22>0.1)
		{
			nglColor3f(f3*RColor,f3*GColor,f3*BColor);
			nglBegin(NGL_POLYGON);
				// Back
				// n =(0,0,-1)
				nglVertex3f(-1.0f, 1.0f,-1.0f);
				nglVertex3f( 1.0f, 1.0f,-1.0f);
				nglVertex3f( 1.0f,-1.0f,-1.0f);
				nglVertex3f(-1.0f,-1.0f,-1.0f);
			nglEnd();
		}
		if (-M20>0.1)
		{
			nglColor3f(f2*RColor,f2*GColor,f2*BColor);
			nglBegin(NGL_POLYGON);
				// Right
				// n = (1,0,0)
				nglVertex3f( 1.0f, 1.0f, 1.0f);
				nglVertex3f( 1.0f, 1.0f,-1.0f);
				nglVertex3f( 1.0f,-1.0f,-1.0f);
				nglVertex3f( 1.0f,-1.0f, 1.0f);
			nglEnd();
		}
		if (M20>0.1)
		{
			nglColor3f(f4*RColor,f4*GColor,f4*BColor);
			nglBegin(NGL_POLYGON);
				// Left
				// n =(-1,0,0)
				nglVertex3f(-1.0f, 1.0f, 1.0f);
				nglVertex3f(-1.0f, 1.0f,-1.0f);
				nglVertex3f(-1.0f,-1.0f,-1.0f);
				nglVertex3f(-1.0f,-1.0f, 1.0f);
			nglEnd();
		}
		if (-M21>0.1)
		{
			nglColor3f(f5*RColor,f5*GColor,f5*BColor);
			nglBegin(NGL_POLYGON);
				// Top
				// n = (0,1,0)
				nglVertex3f( 1.0f, 1.0f, 1.0f);
				nglVertex3f( 1.0f, 1.0f,-1.0f);
				nglVertex3f(-1.0f, 1.0f,-1.0f);
				nglVertex3f(-1.0f, 1.0f, 1.0f);
			nglEnd();
		}
		if (M21>0.1)
		{
			nglColor3f(f6*RColor,f6*GColor,f6*BColor);
			nglBegin(NGL_POLYGON);
				// Bottom
				// n = (0,-1,0)
				nglVertex3f( 1.0f,-1.0f, 1.0f);
				nglVertex3f( 1.0f,-1.0f,-1.0f);
				nglVertex3f(-1.0f,-1.0f,-1.0f);
				nglVertex3f(-1.0f,-1.0f, 1.0f);
			nglEnd();
		}
		nglPopMatrix();
		");
	return Over(TheCube,background);
}

image SB_Sphere(
		image	background=0,
		float	XPos=0.0,
		float	YPos=0.0,
		float	ZPos=0.0,
		float	XAngle=0.0,
		float	YAngle=0.0,
		float	ZAngle=0.0,
		float	radius=100.0,
		int	XSubdivisions=15,
		int	YSubdivisions=15,
		float	RColor=1.0,
		float	GColor=0.5,
		float	BColor=0.0,
		float	XLightVector= 0.0,
		float	YLightVector= 0.0,
		float	ZLightVector= 1.0)
{
	float	cx = cosd(XAngle);
	float	cy = cosd(YAngle);
	float	cz = cosd(ZAngle);

	float	sx = sind(XAngle);
	float	sy = sind(YAngle);
	float	sz = sind(ZAngle);

	float	M00 = cz*cy;		float	M01 = sz*cx+cz*sy*sx;	float	M02 = sz*sx-cz*sy*cx;
	float	M10 = -sz*cy;		float	M11 = cz*cx-sz*sy*sx;	float	M12 = cz*sx+sz*sy*cx;
	float	M20 = sy;		float	M21 = -cy*sx;		float	M22 = cy*cx;

	TheSphere = NGLRender(
		width,
		height,
		1,
		"
		nglMatrixMode(NGL_PROJECTION);
		nglPerspective(45.0,(float)(width)/(float)(height),-(float)(width/2),-(float)(height/2),(float)(width/2),(float)(height/2));
		nglMatrixMode(NGL_MODELVIEW);
		nglPushMatrix();
		nglRotatef(XAngle,1.0f,0.0f,0.0f);
		nglRotatef(YAngle,0.0f,1.0f,0.0f);
		nglRotatef(ZAngle,0.0f,0.0f,1.0f);
		nglTranslatef(XPos,YPos,ZPos);
		float Y = -radius;
		float Angle2 = -90.0;
		for (int j=0;j<YSubdivisions;j++)
		{
			float Angle = 0.0;
			float YNew = Y + 2*radius/(float)(YSubdivisions);
			float CurrentRadius = radius*sqrt(1-sqr((float)(2*j)/YSubdivisions-1.0));
			float NextRadius    = radius*sqrt(1-sqr((float)(2*(j+1))/YSubdivisions-1.0));
			for (int i=0;i<XSubdivisions;i++)
			{
				float AngleNew = Angle + 360.0/(float)(XSubdivisions);
				float NormalAngle = (AngleNew+Angle)/2.0;
				float Nx = cosd(Angle2)*cosd(NormalAngle);
				float Ny = sind(Angle2);
				float Nz = -cosd(Angle2)*sind(NormalAngle);
				float Nl = sqrt(sqr(Nx)+sqr(Ny)+sqr(Nz));
				Nx /= Nl;
				Ny /= Nl;
				Nz /= Nl;
				if (M20*Nx + M21*Ny + M22*Nz <= 0.0)
				{
					float ColorCoef = 0.5+0.5*(XLightVector*(Nx*M00+Ny*M01+Nz*M02)+YLightVector*(Nx*M10+Ny*M11+Nz*M12)+ZLightVector*(Nx*M20+Ny*M21+Nz*M22));
					nglColor3f(ColorCoef*RColor,ColorCoef*GColor,ColorCoef*BColor);
					nglBegin(NGL_POLYGON);
						nglVertex3f(CurrentRadius*cosd(Angle),    Y, 	CurrentRadius*sind(Angle));
						nglVertex3f(CurrentRadius*cosd(AngleNew), Y, 	CurrentRadius*sind(AngleNew));
						nglVertex3f(NextRadius*cosd(AngleNew), 	  YNew, NextRadius*sind(AngleNew));
						nglVertex3f(NextRadius*cosd(Angle),	  YNew, NextRadius*sind(Angle));	
					nglEnd();
				}
				Angle = AngleNew;
			}
			Y = YNew;
			Angle2 += 180.0/(float)(YSubdivisions);
		}
		Y = -radius;
		Angle2 = -90.0;
		for (j=0;j<YSubdivisions;j++)
		{
			float Angle = 0.0;
			float YNew = Y + 2*radius/(float)(YSubdivisions);
			float CurrentRadius = radius*sqrt(1-sqr((float)(2*j)/YSubdivisions-1.0));
			float NextRadius    = radius*sqrt(1-sqr((float)(2*(j+1))/YSubdivisions-1.0));
			for (int i=0;i<XSubdivisions;i++)
			{
				float AngleNew = Angle + 360.0/(float)(XSubdivisions);
				float NormalAngle = (AngleNew+Angle)/2.0;
				float Nx = cosd(Angle2)*cosd(NormalAngle);
				float Ny = sind(Angle2);
				float Nz = -cosd(Angle2)*sind(NormalAngle);
				float Nl = sqrt(sqr(Nx)+sqr(Ny)+sqr(Nz));
				Nx /= Nl;
				Ny /= Nl;
				Nz /= Nl;
				if (M20*Nx + M21*Ny + M22*Nz > 0.0)
				{
					float ColorCoef = 0.5+0.5*(XLightVector*(Nx*M00+Ny*M01+Nz*M02)+YLightVector*(Nx*M10+Ny*M11+Nz*M12)+ZLightVector*(Nx*M20+Ny*M21+Nz*M22));
					nglColor3f(ColorCoef*RColor,ColorCoef*GColor,ColorCoef*BColor);
					nglBegin(NGL_POLYGON);
						nglVertex3f(CurrentRadius*cosd(Angle),    Y, 	CurrentRadius*sind(Angle));
						nglVertex3f(CurrentRadius*cosd(AngleNew), Y, 	CurrentRadius*sind(AngleNew));
						nglVertex3f(NextRadius*cosd(AngleNew), 	  YNew, NextRadius*sind(AngleNew));
						nglVertex3f(NextRadius*cosd(Angle),	  YNew, NextRadius*sind(Angle));	
					nglEnd();
				}
				Angle = AngleNew;
			}
			Y = YNew;
			Angle2 += 180.0/(float)(YSubdivisions);
		}
		nglPopMatrix();
		"
		);
	return Over(TheSphere,background);	
}

image SB_Cylinder(
		image	background=0,
		float	XPos=0.0,
		float	YPos=0.0,
		float	ZPos=0.0,
		float	XAngle=0.0,
		float	YAngle=0.0,
		float	ZAngle=0.0,
		float	radius=100.0,
		float	length=250.0,
		int	Subdivisions=25,
		float	RColor=1.0,
		float	GColor=0.5,
		float	BColor=0.0,
		float	XLightVector= 0.0,
		float	YLightVector= 0.0,
		float	ZLightVector= 1.0)
{
	float	cx = cosd(XAngle);
	float	cy = cosd(YAngle);
	float	cz = cosd(ZAngle);

	float	sx = sind(XAngle);
	float	sy = sind(YAngle);
	float	sz = sind(ZAngle);

	float	M00 = cz*cy;		float	M02 = sz*sx-cz*sy*cx;
	float	M10 = -sz*cy;		float	M12 = cz*sx+sz*sy*cx;
	float	M20 = sy;		float	M22 = cy*cx;

	TheSphere = NGLRender(
		width,
		height,
		1,
		"
		nglMatrixMode(NGL_PROJECTION);
		nglPerspective(45.0,(float)(width)/(float)(height),-(float)(width/2),-(float)(height/2),(float)(width/2),(float)(height/2));
		nglMatrixMode(NGL_MODELVIEW);
		nglPushMatrix();
		nglRotatef(XAngle,1.0f,0.0f,0.0f);
		nglRotatef(YAngle,0.0f,1.0f,0.0f);
		nglRotatef(ZAngle,0.0f,0.0f,1.0f);
		nglTranslatef(XPos,YPos,ZPos);
		float Angle = 0.0;
		for (int i=0;i<Subdivisions;i++)
		{
			float AngleNew = Angle + 360.0/(float)(Subdivisions);
			float NormalAngle = (AngleNew+Angle)/2.0;
			float Nx = cosd(NormalAngle);
			float Nz = -sind(NormalAngle);
			if (M20*Nx + M22*Nz > 0.0)
			{
				float ColorCoef = 0.5-0.5*(XLightVector*(Nx*M00+Nz*M02)+YLightVector*(Nx*M10+Nz*M12)+ZLightVector*(Nx*M20+Nz*M22));
				nglColor3f(ColorCoef*RColor,ColorCoef*GColor,ColorCoef*BColor);
				nglBegin(NGL_POLYGON);
					nglVertex3f(radius*cosd(Angle),    -length/2.0, 	radius*sind(Angle));
					nglVertex3f(radius*cosd(AngleNew), -length/2.0, 	radius*sind(AngleNew));
					nglVertex3f(radius*cosd(AngleNew),  length/2.0, 	radius*sind(AngleNew));
					nglVertex3f(radius*cosd(Angle),	    length/2.0, 	radius*sind(Angle));	
				nglEnd();
			}
			Angle = AngleNew;
		}
		Angle = 0.0;
		for (i=0;i<Subdivisions;i++)
		{
			float AngleNew = Angle + 360.0/(float)(Subdivisions);
			float NormalAngle = (AngleNew+Angle)/2.0;
			float Nx = cosd(NormalAngle);
			float Nz = -sind(NormalAngle);
			if (M20*Nx + M22*Nz <= 0.0)
			{
				float ColorCoef = 0.5-0.5*(XLightVector*(Nx*M00+Nz*M02)+YLightVector*(Nx*M10+Nz*M12)+ZLightVector*(Nx*M20+Nz*M22));
				nglColor3f(ColorCoef*RColor,ColorCoef*GColor,ColorCoef*BColor);
				nglBegin(NGL_POLYGON);
					nglVertex3f(radius*cosd(Angle),    -length/2.0, 	radius*sind(Angle));
					nglVertex3f(radius*cosd(AngleNew), -length/2.0, 	radius*sind(AngleNew));
					nglVertex3f(radius*cosd(AngleNew),  length/2.0, 	radius*sind(AngleNew));
					nglVertex3f(radius*cosd(Angle),	    length/2.0, 	radius*sind(Angle));	
				nglEnd();
			}
			Angle = AngleNew;
		}
		nglPopMatrix();
		"
		);
	return Over(TheSphere,background);	
}

//------------------------------------------------------------------------------------
// The End ... for now :)
//------------------------------------------------------------------------------------
