//-------------------------------------//
//             ProFlares               //
//                                     //
//    Copyright 2003 Francois Sugny    //
// Contact : francois.sugny@strangebrain.com //
//-------------------------------------//


image	RayStarFlare(
	image 	background,
	image	AlphaInput,
	float	PosX="width/2",		float	PosY="height/2",
	float	xPos=width/2,		float	yPos=height/2,
	int	UseAlpha=0,
	string	TransferMode="Screen",
	float	Blend=100.0,
	float	Brightness=1.0,
	float	Quality=1.0,
	int	Ratio=0,
	float	rColor=1,		float	gColor=0.55,		float	bColor=0.32,
	int	MainGlowEnable=1,	float	MainGlowScale=1.0,	float	MainGlowOpacity=1.0, 		float	MainGlowBlur=10.0,
	int	RedReflectionEnable=1,	float	RedReflectionScale=1.0,	float	RedReflectionOpacity=1.0,	float	RedReflectionBlur=25.0,
	int	BrownReflectionEnable=1,float	BrownReflectionScale=1.0,float	BrownReflectionOpacity=1.0,	float	BrownReflectionBlur=25.0,
	int	RaysEnable=1,		float	RaysScale=1.0,		float	RaysOpacity=1.0,
	int	ArcReflectionEnable=1,	float	ArcReflectionScale=1.0,	float	ArcReflectionOpacity=1.0,	float	ArcReflectionBlur=1.0)
{

	Arc1 = AE_Arc(background.width,background.height, 1, 1, 1, 45, 90, 35, 50, width/2, height/2, 100);
	ArcPolar = Reorder(ColorX(Arc1, rnd((int)(atan2d(y-height/2,x-width/2))), g, b, a, z), "rrrr");


	Rays = NGLRender(
		width,
		Ratio>0?2*height:height,
		1,
		"
		    nglPushMatrix();
		    	if (RaysEnable == 1.0)
		    	{
			    for(int i=0;i<6;i++)
			    {
			    	nglBegin(NGL_LINES);
			    	nglColor4f(RaysOpacity,RaysOpacity,RaysOpacity,RaysOpacity);	nglVertex2f( 	PosX , PosY );
			    	nglColor4f(0,0,0,0);	nglVertex2f( 	PosX + RaysScale*background.width/2.5*cosd(43+i),  PosY + RaysScale*background.width/2.5*sind(43+i));
			    	nglColor4f(RaysOpacity,RaysOpacity,RaysOpacity,RaysOpacity);   	nglVertex2f( 	PosX , PosY );
			    	nglColor4f(0,0,0,0);   	nglVertex2f( 	PosX + RaysScale*background.width/2.5*cosd(133+i), PosY + RaysScale*background.width/2.5*sind(133+i));
			    	nglColor4f(RaysOpacity,RaysOpacity,RaysOpacity,RaysOpacity);   	nglVertex2f( 	PosX , PosY );
			    	nglColor4f(0,0,0,0);   	nglVertex2f( 	PosX + RaysScale*background.width/2.5*cosd(223+i), PosY + RaysScale*background.width/2.5*sind(223+i));
			    	nglColor4f(RaysOpacity,RaysOpacity,RaysOpacity,RaysOpacity);   	nglVertex2f( 	PosX , PosY );
			    	nglColor4f(0,0,0,0);   	nglVertex2f( 	PosX + RaysScale*background.width/2.5*cosd(313+i), PosY + RaysScale*background.width/2.5*sind(313+i));
			    	nglEnd();
			    }
			}
			if (MainGlowEnable == 1.0)
			{
			    float Radius = Brightness*MainGlowScale*background.width/7;
			    for(i=0;i<Quality*500;i++)
			    {
				nglBegin(NGL_LINES);
				nglColor4f(rColor,gColor,bColor,MainGlowOpacity);	nglVertex2f( PosX , PosY );
				nglColor4f(0,0,0,0);					nglVertex2f( PosX + (rnd(2*i)+0.5)*Radius*cosd(rnd(i)*360.0) , PosY + (rnd(2*i)+0.5)*Radius*sind(rnd(i)*360.0));
				nglEnd();
			    }
			}
			if (RaysEnable == 1.0)
			{
			    for(i=0;i<Quality*25;i++)
			    {
			    	nglBegin(NGL_LINES);
				nglColor4f(RaysOpacity*rColor,RaysOpacity*gColor,RaysOpacity*bColor,RaysOpacity);	nglVertex2f( PosX , PosY );
				nglColor4f(0,0,0,0);			nglVertex2f( PosX + RaysScale*background.width/2*cosd(rnd(i)*20.0 + 35.0) , PosY + RaysScale*background.width/2*sind(rnd(i)*20.0 + 35.0));
				nglColor4f(RaysOpacity*rColor,RaysOpacity*gColor,RaysOpacity*bColor,RaysOpacity);	nglVertex2f( PosX , PosY );
				nglColor4f(0,0,0,0);			nglVertex2f( PosX + RaysScale*background.width/2*cosd(rnd(i)*20.0 + 125.0) , PosY + RaysScale*background.width/2*sind(rnd(i)*20.0 + 125.0));
				nglColor4f(RaysOpacity*rColor,RaysOpacity*gColor,RaysOpacity*bColor,RaysOpacity);	nglVertex2f( PosX , PosY );
				nglColor4f(0,0,0,0);			nglVertex2f( PosX + RaysScale*background.width/2*cosd(rnd(i)*20.0 + 215.0) , PosY + RaysScale*background.width/2*sind(rnd(i)*20.0 + 215.0));
				nglColor4f(RaysOpacity*rColor,RaysOpacity*gColor,RaysOpacity*bColor,RaysOpacity);	nglVertex2f( PosX , PosY );
				nglColor4f(0,0,0,0);			nglVertex2f( PosX + RaysScale*background.width/2*cosd(rnd(i)*20.0 + 305.0) , PosY + RaysScale*background.width/2*sind(rnd(i)*20.0 + 305.0));
				nglEnd();
			    }
			}
		    nglPopMatrix();
		"
    	);

	if (Ratio>0)
		Rays = Scale(Rays,1,0.5,width/2,PosY);

	MainGlow = Brightness(Rays, 3);

	Halo1 = RGrad(background.width,2*background.height, 1, PosX, PosY, 1, Brightness*MainGlowScale*background.width/15, Brightness*MainGlowScale*background.width/15, 0.5, MainGlowOpacity*1, MainGlowOpacity*0.66, MainGlowOpacity*0.44, MainGlowOpacity, 0, 0, 0, 0, 0, 0);

	if (Ratio>0)
		Halo1 = Scale(Halo1,1,0.5,width/2,PosY);

	if (MainGlowEnable == 1.0)
		MainGlow = Blur(IAdd(MainGlow,Halo1,1,100),MainGlowBlur,xPixels);

	MainGlow = Fade(MainGlow,Brightness>1.0?1.0:Brightness);

	if (ArcReflectionEnable == 1.0)
	{
		ArcGrad1 = RGrad(background.width,background.height, 1, width/2, height/2, 1, 49.8715134, 23.74192, 0.5, 0.57, 0.523649156, 0.3845966, 1, 0, 0, 0.228360474, 0.455, 1, 0);
		ArcBlur1 = Blur(Arc1, 60, xPixels, 0, "gauss", xFilter, "rgba");
		ArcIMult1 = IMult(ArcGrad1, ArcBlur1, 1, 99.9, 0);
		ArcIMult2 = IMult(ArcPolar,ArcIMult1,1, 99.9, 0);
		ArcPan1 = Pan(ArcIMult2, 0, -60, 0, 0.5, 0);
		ArcReflection = Blur(Fade(Move2D(ArcPan1, (xPos-background.width/2)+(PosX-xPos)*1.4, (yPos-background.height/2)+(PosY-yPos)*1.4, atan2d(PosY-yPos,PosX-xPos)-90, 1, ArcReflectionScale, (Ratio>0)?0.5*xScale:xScale, 0, 0, background.width/2, background.height/2),(Brightness>1.0)?ArcReflectionOpacity:ArcReflectionOpacity*Brightness),ArcReflectionBlur,xPixels);
	}
	else
		ArcReflection = Black(background.width,background.height);

	if (BrownReflectionEnable == 1.0)
	{
		Color2 = Color(background.width/5, background.height/7, 1, 0.2,0.155, 0.1023896669, 0.2, 0);
		Color2Pan = Pan(Color2,background.width/2-background.width/10,background.height/2-background.height/14);
		Move2D12 = Move2D(Color2Pan, (xPos-background.width/2)-(PosX-xPos), (yPos-background.height/2)-(PosY-yPos), atan2d(PosY-yPos,PosX-xPos)-90, 1, BrownReflectionScale, (Ratio>0)?0.5*xScale:xScale, -1, 0, background.width/2, background.height/2, "default", xFilter, "trsx", 0, 0, 0.5, 0, 0, time);
		Over10 = Over(Move2D12, Black(background.width,background.height), 1, 0, 0);
		BrownReflection = Fade(Blur(Over10, BrownReflectionBlur, xPixels),(Brightness>1.0)?BrownReflectionOpacity:BrownReflectionOpacity*Brightness);
	}
	else
		BrownReflection = Black(background.width,background.height);

	if (RedReflectionEnable == 1.0)
	{
		Color3 = Color(background.width/10, background.height/13, 1, 0.2, 0.02, 0.03, 0.2, 0);
		Color3Pan = Pan(Color3,background.width/2-background.width/20,background.height/2-background.height/26);
		Color3Move = Move2D(Color3Pan, (xPos-background.width/2)+(PosX-xPos)*1.6, (yPos-background.height/2)+(PosY-yPos)*1.6, atan2d(PosY-yPos,PosX-xPos)-90, 1, RedReflectionScale, (Ratio>0)?0.5*xScale:xScale, -1, 0, background.width/2, background.height/2, "default", xFilter, "trsx", 0, 0, 0.5, 0, 0, time);
		Color3Over = Over(Color3Move, Black(background.width,background.height), 1, 0, 0);
		RedReflection = Fade(Blur(Color3Over, RedReflectionBlur, xPixels),(Brightness>1.0)?RedReflectionOpacity:RedReflectionOpacity*Brightness);
	}
	else
		RedReflection = Black(background.width,background.height);

	Final1 = IAdd(ArcReflection, MainGlow, 1, 100);
	Final2 = IAdd(BrownReflection, Final1, 1, 100);
	Final = IAdd(RedReflection, Final2, 1, 100);

	if (UseAlpha == 1.0)
	{
		HideCoef = WarpX(AlphaInput, 1, PosX, PosY, width, height);
		HideFinal = LayerX(HideCoef,Final,(1-a)*r2,(1-a)*g2,(1-a)*b2,(1-a)*a2,z);
	}
	else
		HideFinal = Final;

	FinalNode = AE_Layers(HideFinal,background,0,TransferMode);
	return Mix(background,FinalNode,1,Blend,"rgba");
}

image	RainbowRing(
	image	background,
	float	PosX=width/2,		float	PosY=height/2,
	float	xPos=width/2,		float	yPos=height/2,
	float	Quality=1,
	int	Ratio=0)
{
	Final = Black(background.width,background.height);

	TheRainbowRing = NGLRender(
		width,
		Ratio>0?2*height:height,
		1,
		"
		    nglPushMatrix();
		    	    int 	nbpts = Quality*500;
			    float 	radius = distance(PosX,PosY,xPos,yPos);
			    float	maxlength = 2*radius/(background.width/2)*max(background.width/10,background.height/10);
			    float 	angle,length,vx,vy,dirx,diry,dirnx,dirny;
			    for(int i=0;i<nbpts;i++)
			    {
			    	angle = rnd(i)*3.0-1.5+i*360.0/nbpts;
			    	vx = width/2  + radius*cosd(angle);
			    	vy = height/2 + radius*sind(angle);

			    	dirx  = vx;
			    	diry  = vy-height/2;
			    	dirnx = dirx/distance(0,0,dirx,diry);
			    	dirny = diry/distance(0,0,dirx,diry);

				length = (i<=nbpts/2) ? maxlength-maxlength*i/nbpts*2 : maxlength*(i-nbpts/2)/nbpts*2;

				nglBegin(NGL_LINES);
				nglColor4f(0,1,0.5,0);
				nglVertex2f( vx , vy );
				nglColor4f(0,1,0.5,1);
				nglVertex2f( vx + length*dirnx*0.25 , vy + length*dirny*0.25);
				nglColor4f(0,1,0.5,1);
				nglVertex2f( vx + length*dirnx*0.25 , vy + length*dirny*0.25);
				nglColor4f(0.75,1,0.5,1);
				nglVertex2f( vx + length*dirnx*0.75 , vy + length*dirny*0.75);
				nglColor4f(0.75,1,0.5,1);
				nglVertex2f( vx + length*dirnx*0.75 , vy + length*dirny*0.75);
				nglColor4f(1,1,0.5,0);
				nglVertex2f( vx + length*dirnx , vy + length*dirny);
				nglEnd();
			    }
		    nglPopMatrix();
		"
    );

    TheRainbowRing1 = Resize(TheRainbowRing,background.width,background.height);

    Move2D1 = Move2D(TheRainbowRing1, xPos-background.width/2,yPos-background.height/2,180.0+atan2d(PosY-yPos,PosX-xPos));
    ColorSpace1 = ColorSpace(Move2D1, "hsv", "rgb", 0.3, 0.59, 0.11);

    return ColorSpace1;
}

image	RainbowFlare(
	image 	background,
	image	AlphaInput,
	float	PosX=width/2,		float	PosY=height/2,
	float	xPos=width/2,		float	yPos=height/2,
	int	UseAlpha=0,
	string	TransferMode="Screen",
	float	Blend=100.0,
	float	Brightness=1.0,
	float	Quality=1.0,
	int	Ratio=0,
	float	rColor=1.0,		float	gColor=1.0,		float	bColor=1.0,
	int	HaloEnable=1.0,		float	HaloScale=1.0,		float	HaloOpacity=1.0,
	int	RaysEnable=1.0,		float	RaysScale=1.0,		float	RaysOpacity=1.0,	float	RaysBlur=5.0,
	int	RedRingEnable=1.0,	float	RedRingScale=1.0,	float	RedRingOpacity=1.0,
	int	RainbowRingEnable=1.0,	float	RainbowRingOpacity=1.0,	float	RainbowRingBlur=2.0,
	int	StreaksEnable=1.0,	float	StreaksOpacity=1.0,	float	StreaksBlur=1.0,
	int	ReflectionsEnable=1.0,	float	ReflectionsScale=1.0,	float	ReflectionsOpacity=1.0,
	int	Reflections2Enable=1.0,	float	Reflections2Scale=1.0,	float	Reflections2Opacity=1.0)
{
	BlackBackground = Black(background.width,background.height);

	if (HaloEnable == 1.0)
	{
		Halo1RGrad   = RGrad(background.width,background.height, 1, width/2,height/2, 1, 0, Brightness*height/3, 0.5, 0.9, 0.9, 0.9, 1, 0, 0, 0, 0, 0, 0);
		Halo1Move2D  = Move2D(Halo1RGrad,(xPos-background.width/2)+PosX-xPos,(yPos-background.height/2)+PosY-yPos,0,1,HaloScale,(Ratio>0)?0.5*xScale:xScale);
		Halo1	     = Fade(Halo1Move2D,(Brightness>1.0)?HaloOpacity:HaloOpacity*Brightness);
	}
	else
		Halo1        = BlackBackground;

	if (RaysEnable == 1.0)
	{
		Rays = NGLRender(
			width,
			(Ratio>0?2:1)*height,
			1,
			"
			    nglPushMatrix();
				if (RaysEnable == 1.0)
				{
				    float Radius = Brightness*RaysScale*background.width/4;
				    for(i=0;i<Quality*500;i++)
				    {
					nglBegin(NGL_LINES);
					nglColor4f(rColor,gColor,bColor,1);	nglVertex2f( PosX , PosY );
					nglColor4f(0,0,0,0);			nglVertex2f( PosX + (rnd(2*i)+0.5)*Radius*cosd(rnd(i)*360.0) , PosY + (rnd(2*i)+0.5)*Radius*sind(rnd(i)*360.0));
					nglEnd();
				    }
				}
			    nglPopMatrix();
			"
	    	);
	    	if (Ratio>0)
	    		Rays = Scale(Rays,1,0.5,width/2,PosY);
		RaysBlur1    = Blur(Rays, RaysBlur, xPixels, 0, "gauss", xFilter, "rgba");
		RaysMove2D1  = Fade(RaysBlur1,(Brightness>1.0)?RaysOpacity:RaysOpacity*Brightness);
	}
	else
		RaysMove2D1 = BlackBackground;

	if (RedRingEnable == 1.0)
	{
		RingArc1     = AE_Arc(background.width,background.height, 1, 0, 0, 0, 360, 60, 3, width/2, height/2, 100);
		RingBlur1    = Blur(RingArc1, 50, xPixels, 0, "gauss", xFilter, "rgba");
		RingMove2D1  = Fade(Move2D(RingBlur1, (xPos-background.width/2)+(PosX-xPos),(yPos-background.height/2)+(PosY-yPos), 0, 1, RedRingScale, (Ratio>0)?0.5*xScale:xScale, 0, 0, width/2, height/2),(Brightness>1.0)?RedRingOpacity:RedRingOpacity*Brightness);
	}
	else
		RingMove2D1  = BlackBackground;

	if (RainbowRingEnable == 1.0)
	{
		Chroma1Ring  = RainbowRing(BlackBackground,PosX,PosY,xPos,yPos,Quality,(Ratio>0)?1:0);
		Chroma1Fade  = Fade(Chroma1Ring,(Brightness>1.0)?RainbowRingOpacity:RainbowRingOpacity*Brightness);
		Chroma1RBlur = RBlur(Chroma1Fade,PosX,PosY,0,50,1,1,0.5,1,0);
		Chroma1      = Blur(Chroma1RBlur,RainbowRingBlur,xPixels);
	}
	else
		Chroma1      = BlackBackground;

	if (StreaksEnable == 1.0)
	{
		ColorX1Horiz = Reorder(ColorX(BlackBackground, rnd(y), 0, 0, 0, 0),"rrrr");
		BlueStreak1  = RGrad(background.width,background.height, 1, width/2, height/2, 0.03, min(width,height)/4, min(width,height)/4, 0.5, 0.245973825, 0.251499832, 0.59, 1, 0, 0, 0, 0, 0, 0);
		BlueStreak2  = IMult(BlueStreak1,ColorX1Horiz,1,99.9);
		BlueStreak3  = Blur(BlueStreak2,StreaksBlur,xPixels);
		BlueStreak4  = Fade(Move2D(BlueStreak3,(xPos-background.width/2)+(PosX-xPos),(yPos-background.height/2)+(PosY-yPos),atan2d(PosY-yPos,PosX-xPos), 1, 1, (Ratio>0)?0.5*xScale:xScale, 0, 0, width/2, height/2, "default", xFilter, "trsx", 0, 0, 0.5, 0, 0, time),(Brightness>1.0)?StreaksOpacity:StreaksOpacity*Brightness);
		BlueStreak5  = Fade(Move2D(BlueStreak3,(xPos-background.width/2)+(xPos-PosX),(yPos-background.height/2)+(yPos-PosY),0,1,1.5,(Ratio>0)?0.5:1),(Brightness>1.0)?StreaksOpacity:StreaksOpacity*Brightness);
	}
	else
	{
		BlueStreak4  = BlackBackground;
		BlueStreak5  = BlackBackground;
	}

	if (ReflectionsEnable == 1.0)
	{
		ReflectionShape = QuickShape(720,486, 1, Hermite(0,[0,0,0]@5), Hermite(0,[0,0,0]@5),
		    Hermite(0,[0,0,0]@5), 1, Hermite(0,[1,0,0]@5), Hermite(0,[1,0,0]@5),
		    width/2, height/2, 0, 0.5, 0, 128, Hermite(0,[301.661,0,0]@5),
		    Hermite(0,[170.66,0,0]@5), Hermite(0,[0.8545,0,0]@5), Hermite(0,[0.5195,0,0]@5),
		    Hermite(0,[0.8545,0,0]@5), Hermite(0,[0.5195,0,0]@5), Hermite(0,[1.1811,0,0]@5),
		    Hermite(0,[1.1811,0,0]@5), Hermite(0,[400.661,0,0]@5), Hermite(0,[170.66,0,0]@5),
		    Hermite(0,[0.8455,0,0]@5), Hermite(0,[-0.534,0,0]@5), Hermite(0,[0.8455,0,0]@5),
		    Hermite(0,[-0.5341,0,0]@5), Hermite(0,[1.2487,0,0]@5), Hermite(0,[1.2487,0,0]@5),
		    Hermite(0,[409.661,0,0]@5), Hermite(0,[190.16,0,0]@5), Hermite(0,[0.8723,0,0]@5),
		    Hermite(0,[0.489,0,0]@5), Hermite(0,[0.8723,0,0]@5), Hermite(0,[0.489,0,0]@5),
		    Hermite(0,[1.0993,0,0]@5), Hermite(0,[1.0993,0,0]@5), Hermite(0,[363.633,0,0]@5),
		    Hermite(0,[262.16,0,0]@5), Hermite(0,[0.8824,0,0]@5), Hermite(0,[-0.4704,0,0]@5),
		    Hermite(0,[0.8824,0,0]@5), Hermite(0,[-0.4704,0,0]@5), Hermite(0,[0.9923,0,0]@5),
		    Hermite(0,[0.9923,0,0]@5), Hermite(0,[344.633,0,0]@5), Hermite(0,[262.16,0,0]@5),
		    Hermite(0,[0.899,0,0]@5), Hermite(0,[0.4379,0,0]@5), Hermite(0,[0.899,0,0]@5),
		    Hermite(0,[0.4379,0,0]@5), Hermite(0,[1.0043,0,0]@5), Hermite(0,[1.0043,0,0]@5),
		    Hermite(0,[293.161,0,0]@5), Hermite(0,[188.16,0,0]@5), Hermite(0,[0.8621,0,0]@5),
		    Hermite(0,[-0.5068,0,0]@5), Hermite(0,[0.8621,0,0]@5), Hermite(0,[-0.5068,0,0]@5),
		    Hermite(0,[0.9484,0,0]@5), Hermite(0,[0.9484,0,0]@5));
		Reflection_  = ColorX(ReflectionShape, r*0.8, g*0.2, b*0.2, a, z);
		Reflection2_ = ColorX(ReflectionShape, r*0.1, g*0.5, b*0.2, a, z);
		Reflection   = Resize(Reflection_,background.width,background.height);
		Reflection2  = Resize(Reflection2_,background.width,background.height);

		Reflections = BlackBackground;
		for (i=0;i<20;i++)
		{
			Reflections_0 = Move2D(Reflection,0,0,atan2d(PosY-yPos,PosX-xPos)-90,1,ReflectionsScale*rnd(3*i)*0.5,xScale);
			Reflections_1 = Move2D(Reflections_0,(xPos-background.width/2)+(xPos-PosX)*(rnd(i)*4-0.5),(yPos-background.height/2)+(yPos-PosY)*(rnd(i)*4-0.5),0,1,1,((Ratio>0)?0.5:1));
			Reflections_2 = Blur(Reflections_1,rnd(5*i)*30,xPixels);
			Reflections_3 = Fade(Reflections_2,(Brightness>1.0)?ReflectionsOpacity*rnd(7*i):ReflectionsOpacity*rnd(7*i)*Brightness);
			Reflections   = IAdd(Reflections_3,Reflections);
		}
		for (i=0;i<10;i++)
		{
			Reflections_0 = Move2D(Reflection2,0,0,atan2d(PosY-yPos,PosX-xPos)-90,1,ReflectionsScale*rnd(11*i)*0.5,xScale);
			Reflections_1 = Move2D(Reflections_0,(xPos-background.width/2)-(PosX-xPos)*(rnd(8*i)*4-0.5),(yPos-background.height/2)-(PosY-yPos)*(rnd(8*i)*4-0.5),0,1,1,((Ratio>0)?0.5:1));
			Reflections_2 = Blur(Reflections_1,rnd(13*i)*30,xPixels);
			Reflections_3 = Fade(Reflections_2,(Brightness>1.0)?ReflectionsOpacity*rnd(7*i):ReflectionsOpacity*rnd(7*i)*Brightness);
			Reflections   = IAdd(Reflections_3,Reflections);
		}
	}
	else
		Reflections = BlackBackground;

	if (Reflections2Enable == 1.0)
	{
		Reflection2 = RGrad(background.width,background.height, 1, width/2, height/2, 1, 0, width/100, 0.5, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0);
		Reflections2 = BlackBackground;
		for (i=0;i<10;i++)
		{
			Reflections2_0 = Move2D(Reflection2,0,0,0,1,Reflections2Scale*rnd(8*i),xScale);
			Reflections2_1 = Move2D(Reflection2,(xPos-background.width/2)+(xPos-PosX)*(rnd(7*i)*4-0.5),(yPos-background.height/2)+(yPos-PosY)*(rnd(7*i)*4-0.5),0,1,Reflections2Scale,((Ratio>0)?0.5:1)*xScale);
			Reflections2_2 = Fade(Reflections2_1,(Brightness>1.0)?Reflections2Opacity*rnd(7*i):Reflections2Opacity*rnd(9*i)*Brightness);
			Reflections2   = IAdd(Reflections2,Reflections2_2);
		}
	}
	else
		Reflections2 = BlackBackground;

	Final1 = IAdd(BlueStreak4 ,Halo1,1,100);
	Final2 = IAdd(BlueStreak5 ,Final1,1,100);
	Final3 = IAdd(Chroma1,Final2,1,100);
	Final4 = IAdd(RaysMove2D1,Final3,1,100);
	Final5 = IAdd(RingMove2D1,Final4,1,100);
	Final6 = IAdd(Reflections,Final5,1,100);
	Final7 = IAdd(Reflections2,Final6,1,100);

	Final = Final7;

	if (UseAlpha == 1.0)
	{
		HideCoef = WarpX(AlphaInput, 1, PosX, PosY, width, height);
		HideFinal = LayerX(HideCoef,Final,(1-a)*r2,(1-a)*g2,(1-a)*b2,(1-a)*a2,z);
	}
	else
		HideFinal = Final;

	FinalNode = AE_Layers(HideFinal,background,0,TransferMode);

	return Mix(background,FinalNode,1,Blend,"rgba");
}

image	HotFlare(
	image 	background,
	image	AlphaInput,
	float	PosX=width/2,		float	PosY=height/2,
	float	xPos=width/2,		float	yPos=height/2,
	int	UseAlpha=0,
	string	TransferMode="Screen",
	float	Blend=100.0,
	float	Brightness=1,
	int	Ratio=0,
	int	HaloEnable=1,		float	HaloScale=1.0,		float	HaloOpacity=1.0,	float	HaloBlur=0.0,
	int	RaysEnable=1,		float	RaysScale=1.0,		float	RaysOpacity=1.0,	float	RaysBlur=0.0,
	int	Rays2Enable=1,		float	Rays2Scale=1.0,		float	Rays2Opacity=1.0,	float	Rays2Blur=0.0,
	int	Reflections1Enable=1,	float	Reflections1Scale=1.0,	float	Reflections1Opacity=1.0,float	Reflections1Blur=0.0,
	int	Reflections2Enable=1,	float	Reflections2Scale=1.0,	float	Reflections2Opacity=1.0,float	Reflections2Blur=0.0)
{
	if (HaloEnable == 1.0)
	{
		HaloGrad1 = RGrad(background.width,background.height, 1, width/2, height/2, 1, min(width*10/720,height*10/486), min(width*50/720,height*50/486), 0.051, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0);
		HaloGrad2 = RGrad(background.width,background.height, 1, width/2, height/2, 1, 0, min(width*160/720,height*160/486), 0.5, 0.645, 0.4888844, 0.265599579, 1, 0, 0, 0, 0, 0, 0);
		HaloScreen = Screen(HaloGrad1, HaloGrad2, 1);
		HaloMove2D  = Blur(Fade(Move2D(HaloScreen, (xPos-background.width/2)+(PosX-xPos),(yPos-background.height/2)+(PosY-yPos),0,1,Brightness*HaloScale,((Ratio>0)?0.5:1)*xScale),HaloOpacity),HaloBlur,xPixels);
	}
	else
		HaloMove2D = Black(background.width,background.height);

	if (RaysEnable == 1.0)
	{
		RayShapeBase = QuickShape(720, 486, 1, 0, 0, 0, 1, 1, xScale,
		    width/2, height/2, 0, 0.5, 0, 128, 4.88235474, 4.352951,
		    0.7880305, -0.6156362, 0.788030446, -0.6156362, 1.308043,
		    1.30803192, 255.941162, 6.470627, 0.175567508, 0.9844674,
		    0.175565585, 0.984467745, 0.7456667, 0.7456933, 117.389343,
		    4.31257629, 0.9995735, -0.0292024277, 0.9995735, -0.029202424,
		    2.2867043, 2.28667545, 68.05882, -1.76469421, 0.100415818,
		    0.9949456, 0.100415826, 0.9949456, 2.09821534, 2.098202,
		    228.176514, -6.705826, 0.08987537, 0.995953, 0.08987538,
		    0.995953, 1.25574148, 1.2557447, 162.235291, -9.352951, 1,
		    0, 1, 0, 1, 1, 2.29412842, -5.99998474, 0.597143054, 0.802134752,
		    0.597143054, 0.8021348, 1.24166131, 1.24166965);
		RayShape = Resize(RayShapeBase,background.width,background.height);

		Rays = Black(background.width,background.height);
		for (int count=0;count<50;count++)
		{
			Rays = Screen(Rays,Fade(Move2D(RayShape, background.width/2, background.height/2, rnd(count)*360, 1, 0.5*rnd(2*count), 0.1,0,0,0,0),rnd(3*count)));
		}
		RaysMove2D  = Blur(Fade(Move2D(Rays, (xPos-background.width/2)+(PosX-xPos),(yPos-background.height/2)+(PosY-yPos), atan2d(PosY-yPos,PosX-xPos), 1, RaysScale, ((Ratio>0)?0.5:1)*xScale, 0, 0, width/2, height/2, "default", xFilter, "trsx", 0, 0, 0.5, 0, 0, time),((Brightness>1.0)?1.0:Brightness)*RaysOpacity),RaysBlur,xPixels);
	}
	else
		RaysMove2D = Black(background.width,background.height);

	if (Rays2Enable == 1.0)
	{
		Rays2 = Black(background.width,background.height);
		Rays2Ramp = Ramp(background.width/2, 1, 1, 1, 0.5, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0);
		for (count=0;count<50;count++)
		{
			Rays2 = IAdd(Rays2,Fade(Move2D(Rays2Ramp, background.width/2, background.height/2, rnd(count)*90, 1, 0.5+rnd(2*count)*0.4-0.2, xScale, 0, 0, 0, height/2),rnd(3*count)),0,100);
		}
		Rays2 = Blur(Rays2,5,xPixels);
		Rays3 = IAdd(Rays2,Rotate(Rays2,90));
		Rays4 = IAdd(Rays3,Rotate(Rays3,180));
		Rays2Move2D  = Blur(Fade(Move2D(Rays4, (xPos-background.width/2)+(PosX-xPos),(yPos-background.height/2)+(PosY-yPos), atan2d(PosY-yPos,PosX-xPos), 1, Rays2Scale, ((Ratio>0)?0.5:1)*xScale, 0, 0, width/2, height/2, "default", xFilter, "trsx", 0, 0, 0.5, 0, 0, time),((Brightness>1.0)?1.0:Brightness)*Rays2Opacity),Rays2Blur,xPixels);
	}
	else
		Rays2Move2D = Black(background.width,background.height);

	if (Reflections1Enable == 1.0)
	{
		CenterArc1 = AE_Arc(background.width,background.height, 0.7, 0.496305436, 0.188952044, 0, 360, 0, 17.3000011, width/2, height/2, 5);
		CenterBlur1 = Blur(CenterArc1, 20, xPixels, 0, "gauss", xFilter, "rgba");
		CenterScale1 = Scale(CenterBlur1, 0.5, xScale, width/2, height/2, 0, 0.5, 0);
		CenterScale2 = Scale(CenterBlur1, 0.2, xScale, width/2, height/2, 0, 0.5, 0);
		CenterIAdd1 = IAdd(CenterBlur1, CenterScale1, 1, 100);
		CenterIAdd = IAdd(CenterIAdd1, CenterScale2, 1, 100);

		Reflection1 = Black(background.width,background.height);
		for (count=0;count<5;count++)
		{
			Reflection1_0 = Move2D(CenterIAdd, 0, 0, atan2d(PosY-yPos,PosX-xPos), 1, Reflections1Scale*2.0*rnd(2*count), xScale, 0, 0, width/2, height/2);
			Reflection1_1 = Move2D(Reflection1_0, (xPos-background.width/2)+(xPos-PosX)*(rnd(count)*2-0.5), (yPos-background.height/2)+(yPos-PosY)*(rnd(count)*2-0.5), 0, 1, 1, Ratio>0?0.5:1, 0, 0, width/2, height/2);
			Reflection1_2 = Fade(Reflection1_1,((Brightness>1.0)?1.0:Brightness)*Reflections1Opacity*(0.5+0.5*rnd(3*count)));
			Reflection1_3 = Blur(Reflection1_2,rnd(4*count)*20,xPixels);
			Reflection1 = IAdd(Reflection1,Reflection1_3,0,100);
		}
		Reflection1 = Blur(Reflection1,Reflections1Blur,xPixels);
	}
	else
		Reflection1 = Black(background.width,background.height);

	if (Reflections2Enable == 1.0)
	{
		CenterArc2 = AE_Arc(background.width,background.height, 0.415, 0, 0.189048782, 0, 360, 0, 35, width/2, height/2, 5);
		CenterBlur2 = Blur(CenterArc2, 20, xPixels, 0, "gauss", xFilter, "rgba");
		Reflection2 = Black(background.width,background.height);
		for (count=0;count<5;count++)
		{
			Reflection2_0 = Move2D(CenterBlur2, 0, 0, atan2d(PosY-yPos,PosX-xPos), 1, Reflections2Scale*2.0*rnd(9*count), xScale, 0, 0, width/2, height/2);
			Reflection2_1 = Move2D(Reflection2_0, (xPos-background.width/2)+(xPos-PosX)*(rnd(7*count)*2-0.5), (yPos-background.height/2)+(yPos-PosY)*(rnd(7*count)*2-0.5), 0, 1, 1, Ratio>0?0.5:1, 0, 0, width/2, height/2);
			Reflection2_2 = Fade(Reflection2_1,((Brightness>1.0)?1.0:Brightness)*Reflections2Opacity*(0.5+0.5*rnd(10*count)));
			Reflection2_3 = Blur(Reflection2_2,rnd(4*count)*20,xPixels);
			Reflection2 = IAdd(Reflection2,Reflection2_3,0,100);
		}
		Reflection2 = Blur(Reflection2,Reflections2Blur,xPixels);
	}
	else
		Reflection2 = Black(background.width,background.height);

	Final1 = IAdd(RaysMove2D,HaloMove2D,1,100);
	Final2 = IAdd(Reflection1,Final1,1,100);
	Final3 = IAdd(Reflection2,Final2,1,100);
	Final  = AE_Layers(Rays2Move2D,Final3,0,"Luminosity");

	if (UseAlpha == 1.0)
	{
		HideCoef = WarpX(AlphaInput, 1, PosX, PosY, width, height);
		HideFinal = LayerX(HideCoef,Final,(1-a)*r2,(1-a)*g2,(1-a)*b2,(1-a)*a2,z);
	}
	else
		HideFinal = Final;

	FinalNode = AE_Layers(HideFinal,background,0,TransferMode);

	return Mix(background,FinalNode,1,Blend,"rgba");
}


image	Flare35mm(
	image	background,
	image	AlphaInput,
	float	PosX=width/2,		float	PosY=height/2,
	float	xPos=width/2,		float	yPos=height/2,
	int	UseAlpha=0,
	string	TransferMode="Screen",
	float	Blend=100.0,
	float	Brightness=1.0,
	int	Ratio=0,
	float	PerspectiveAngle=30.0,
	int	GlowEnable=1,		float	GlowScale=1.0,		float	GlowOpacity=1.0,
	int	RaysEnable=1,		float	RaysScale=1.0,		float	RaysOpacity=1.0,		int	RaysCount=6,	int	RaysWidth=10,	int	RaysAngle=0,
	int	OutterRingEnable=1,	float	OutterRingScale=1.0,	float	OutterRingOpacity=1.0,
	int	StarEnable=1,		float	StarScale=1.0,		float	StarOpacity=1.0,
	int	Reflections1Enable=1,	float	Reflections1Scale=1.0,	float	Reflections1Opacity=1.0,
	int	Reflections2Enable=1,	float	Reflections2Scale=1.0,	float	Reflections2Opacity=1.0,
	int	Reflections3Enable=1,	float	Reflections3Scale=1.0,	float	Reflections3Opacity=1.0)
{
	BlackBackground = Black(background.width,background.height);

	if (GlowEnable == 1.0)
	{
		Arc1 = AE_Arc(background.width, background.height, 0.205, 0, 0, 0, 360, 40, 3, PosX, PosY, 100);
		RGrad1 = RGrad(background.width, background.height, 1, PosX, PosY, 1, 0, 243, 0.05, 0.7558689, 0.8576227, 1, 1, 0, 0, 0, 0, 0, 0);
		Blur1 = Blur(Arc1, 10, xPixels, 0, "gauss", xFilter, "rgba");
		ContrastLum1 = ContrastLum(RGrad1, 1.3, 0, 0);
		RGrad1 = RGrad(background.width, background.height, 1, PosX, PosY, 1, 0, 243, 0.5, 0.0294899233, 0.101111025, 0.185, 1, 0, 0, 0, 0, 0, 0);
		IAdd1 = IAdd(ContrastLum1, RGrad1, 1, 100);
		Glow = Scale(IAdd(Blur1, IAdd1, 1, 100),Brightness*GlowScale,xScale,PosX,PosY);
		Glow1 = Fade(Scale(Glow,1,Ratio>0?0.5:1,PosX,PosY),GlowOpacity);
	}
	else
		Glow1 = BlackBackground;

	if (RaysEnable == 1.0)
	{
		Color1 = Color(background.width, background.height, 1, 0, 0, 0, 0, 0);
		Ramp1 = Ramp(RaysScale*width/8, 1, 1, 1, 0.5, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0);
		Move2D1 = Move2D(Ramp1, Color1.width/2, Color1.height/2, Linear(2,0@1,RaysWidth@2), 1, 1, xScale, 0, 0, 0, height/2, "default", xFilter, "trsx", 0, 1, 0.5, 0, 0, time);
		Over1 = Over(Move2D1, Color1, 1, 0, 0);
		Brightness1 = Brightness(Over1, 4.1);
		Rays = IAdd(Brightness1,BlackBackground);
		for (i=0;i<RaysCount;i++)
		{
			Rotate1 = Rotate(Brightness1, i*360/RaysCount, 1, width/2, height/2, 0, 0.5, 0);
			Rays = IAdd(Rotate1,Rays,1,100);
		}
		Fade1 = Fade(Rays,((Brightness>1.0)?1.0:Brightness)*RaysOpacity*0.2);
		RaysMove = Move2D(Fade1,PosX-width/2,PosY-height/2,RaysAngle);
		Rays = Scale(RaysMove,1,Ratio>0?0.5:1,PosX,PosY);
	}
	else
		Rays = BlackBackground;

	if (OutterRingEnable == 1.0)
	{
		Arc1 = AE_Arc(background.width, background.height, 0.495, 0.304773, 0.0432464, 0, 360, 150, 4, width/2, height/2, 100);
		Arc2 = AE_Arc(background.width, background.height, 0.109408021, 0.22587052, 0.495, 0, 360, 147, 3, width/2, height/2, 100);
		IAdd2 = IAdd(Arc2, Arc1, 1, 100);
		Blur1 = Blur(IAdd2, 15, xPixels, 0, "gauss", xFilter, "rgba");
		OutterRing1  = Move3D(Blur1,PosX-width/2, PosY-height/2, 0, -PerspectiveAngle*(PosY-yPos)/(height/2), PerspectiveAngle*(PosX-xPos)/(width/2), 0);
		RGrad1 = RGrad(background.width, background.height, 1, width/2+0.6*(PosX-width/2), height/2+0.6*(PosY-height/2), 1, 150, 50, 0.5, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0);
		IMult1 = IMult(RGrad1, OutterRing1, 1, 100, 0);
		OutterRing = Scale(Fade(IMult1, ((Brightness>1.0)?1.0:Brightness)*OutterRingOpacity*0.5),OutterRingScale,(Ratio>0?0.5:1)*xScale,PosX, PosY);
	}
	else
		OutterRing = BlackBackground;

	if (StarEnable == 1.0)
	{
		Arc1 = AE_Arc(background.width, background.height, 0.05, 0.16, 0.25, atan2d(PosY-height/2,PosX-width/2), 360, 0, 50, width/2, height/2, 6);
		Blur1 = Blur(Arc1, 85.9, xPixels, 0, "lanczos", xFilter, "rgba");
		StarCaustic1 = Fade(Scale(Blur1,StarScale,(Ratio>0?0.5:1)*xScale),((Brightness>1.0)?1.0:Brightness)*StarOpacity*((distance(PosX,PosY,width/2,height/2)>height/2)?1.0:distance(PosX,PosY,width/2,height/2)/(height/2)));
		StarCaustic = Move2D(StarCaustic1, (xPos-background.width/2)+-0.2*(PosX-xPos), (yPos-background.height/2)-0.2*(PosY-yPos), 0, 1, 1.0, xScale, 0, 0, width/2, height/2, "default", xFilter, "trsx", 0, 0, 0.5, 0, 0, time);
	}
	else
		StarCaustic = BlackBackground;

	if (Reflections1Enable == 1.0)
	{
		Reflection1 = RGrad(background.width, background.height, 1, xPos-1.3*(PosX-xPos), yPos-1.3*(PosY-yPos), 1, 0, 38.1353226, 0.5, 0.24, 0.161713943, 0.07741882, 1, 0, 0, 0, 0, 0, 0);
		Reflections1 = Fade(Scale(Reflection1,Reflections1Scale,(Ratio>0?0.5:1)*xScale,xPos-1.3*(PosX-xPos), yPos-1.3*(PosY-yPos)),((Brightness>1.0)?1.0:Brightness)*Reflections1Opacity);
	}
	else
		Reflections1 = BlackBackground;

	if (Reflections2Enable == 1.0)
	{
		Reflection2Arc5 = AE_Arc(background.width, background.height, 0.3, 0.3, 0.0, 0, 360, Reflections2Scale*width/10, Reflections2Scale*width/200, width/2, height/2, 100);
		Reflection2RGrad4 = RGrad(background.width, background.height, 1, width/2, height/2, 1, 0, Reflections2Scale*width/10, 0.7, 1, 1, 1, 1, 0, 1, 0.9, 1, 0, 0);
		Reflection2RGrad5 = RGrad(background.width, background.height, 1, width/2, height/2, 1, 0, Reflections2Scale*width/10, 1.0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0);
		Reflection2Invert1 = Invert(Reflection2RGrad4, "rgba");
		Reflection2IMult1 = IMult(Reflection2RGrad5, Reflection2Invert1, 1, 100, 0);
		Reflection2IAdd16 = IAdd(Reflection2Arc5, Reflection2IMult1, 1, 100);
		Reflection2Blur5 = Fade(Blur(Reflection2IAdd16, 30, xPixels, 0, "gauss", xFilter, "rgba"),((Brightness>1.0)?1.0:Brightness)*Reflections2Opacity);
		Reflection2Scale1 = Scale(Reflection2Blur5,1,Ratio>0?0.5:1);
		Reflections2 = Move2D(Reflection2Scale1, (xPos-background.width/2)-(PosX-xPos)*1.2, (yPos-background.height/2)-(PosY-yPos)*1.2, 0, 1, 1, xScale, 0, 0, width/2, height/2, "default", xFilter, "trsx", 0, 0, 0.5, 0, 0, time);
	}
	else
		Reflections2 = BlackBackground;

	if (Reflections3Enable == 1.0)
	{
		Reflection_3 = RGrad(background.width, background.height, 1, width/2, height/2, 1, 5, 5, 0.5, 0.24, 0.161713943, 0.07741882, 1, 0, 0, 0, 0, 0, 0);
		Reflection3 = Fade(Scale(Reflection_3,Reflections3Scale,(Ratio>0?0.5:1)*xScale),((Brightness>1.0)?1.0:Brightness)*Reflections3Opacity);

		Reflections3 = BlackBackground;
		for (i=0;i<10;i++)
		{
			Reflections3 = Over(Pan(Reflection3,(xPos-background.width/2)+(turbulence(3*i,1)*3.0-2.0)*(PosX-xPos),(yPos-background.height/2)+(turbulence(3*i,1)*3.0-2.0)*(PosY-yPos)),Reflections3);
		}
		RGrad1 = RGrad(background.width, background.height, 1, xPos/2+PosX/2, yPos/2+PosY/2, 1, 0, 25, 0.8, 0.03, 0.1, 0.2, 1, 0, 0, 0, 0, 0, 0);
		RGrad2 = Fade(Scale(RGrad1,Reflections3Scale,(Ratio>0?0.5:1)*xScale, xPos/2+PosX/2, yPos/2+PosY/2),((Brightness>1.0)?1.0:Brightness)*Reflections3Opacity);
		Reflections3 = Over(RGrad2,Reflections3);
	}
	else
		Reflections3 = BlackBackground;

	Final1 = Over(Reflections1,BlackBackground);
	Final2 = Over(Reflections2,Final1);
	Final3 = Over(Reflections3,Final2);
	Final4 = Over(OutterRing,Final3);
	Final5 = IAdd(StarCaustic,Final4);
	Final6 = IAdd(Rays,Final5);
	Final7 = IAdd(Glow1,Final6);
	Final = Final7;

	if (UseAlpha == 1.0)
	{
		HideCoef = WarpX(AlphaInput, 1, PosX, PosY, width, height);
		HideFinal = LayerX(HideCoef,Final,(1-a)*r2,(1-a)*g2,(1-a)*b2,(1-a)*a2,z);
	}
	else
		HideFinal = Final;

	FinalNode = AE_Layers(HideFinal,background,0,TransferMode);

	return Mix(background,FinalNode,1,Blend,"rgba");
}


image	BrightFlare(
	image	background,
	image	AlphaInput,
	float	PosX=width/2,		float	PosY=height/2,
	float	xPos=width/2,		float	yPos=height/2,
	int	UseAlpha=0,
	string	TransferMode="Screen",
	float	Blend=100.0,
	float	Brightness=1.0,
	float	Quality=1.0,
	int	Ratio=0,
	float	rColor=0.44,		float	gColor=0.51,		float	bColor=1.0,
	int	Halo1Enable=1,		float	Halo1Scale=1.0,		float	Halo1Opacity=1.0,
	int	RaysEnable=1,		float	RaysScale=1.0,		float	RaysOpacity=1.0,		float	RaysBlur=10.0,
	int	Reflections1Enable=1,	float	Reflections1Scale=1.0,	float	Reflections1Opacity=1.0,	float	Reflections1Blur=0.0,
	int	Reflections2Enable=1,	float	Reflections2Scale=1.0,	float	Reflections2Opacity=1.0)
{
	BlackBackground = Black(background.width,background.height);
	if (RaysEnable == 1.0)
	{
		Rays = NGLRender(
			background.width,
			(Ratio>0?2:1)*background.height,
			1,
			"
			    nglPushMatrix();
			    float Radius = Brightness*RaysScale*background.width/5;
			    for(i=0;i<Quality*500;i++)
			    {
				nglBegin(NGL_LINES);
				nglColor4f(rColor*1.5,gColor*1.5,bColor*1.5,0.5+0.5*rnd(9*i));
				nglVertex2f( PosX , (Ratio>0?2:1)*PosY );
				nglColor4f(0,0,0,0);
				nglVertex2f( PosX + (rnd(2*i)+0.5)*Radius*cosd(rnd(i)*360.0) , (Ratio>0?2:1)*PosY + (rnd(2*i)+0.5)*Radius*sind(rnd(i)*360.0));
				nglEnd();
			    }
			    nglPopMatrix();
			"
	    	);
	    	if (Ratio>0)
	    		Rays = Resize(Rays,background.width,background.height);
	}
	else
		Rays = BlackBackground;
	RaysBlur1 = Blur(Rays, RaysBlur, xPixels, 0, "gauss", xFilter, "rgba");
	RaysFinal = Fade(RaysBlur1,((Brightness>1.0)?1.0:Brightness)*RaysOpacity);

	if (Halo1Enable == 1.0)
	{
		Halo1RGrad = RGrad(background.width,background.height, 1, PosX , PosY, 1, 0, Halo1Scale*Brightness*width/4.5, 0.5, rColor,gColor,bColor, Halo1Opacity, 0, 0, 0, 0, 0, 0);
		Halo1 = Fade(Scale(Halo1RGrad,1,Ratio>0?0.5:1,PosX,PosY),((Brightness>1.0)?1.0:Brightness)*Halo1Opacity);
	}
	else
		Halo1 = BlackBackground;

	if (Reflections1Enable == 1.0)
	{
		Reflection1 = RGrad(background.width,background.height, 1, width/2, height/2, 1, 0, width/33, 0.5, rColor,gColor,bColor, 1, 0, 0, 0, 0, 0, 0);
		Reflections1 = BlackBackground;
		for (i=0;i<20;i++)
		{
			Reflections1 = IAdd(Reflections1,Blur(Fade(Move2D(Reflection1,(xPos-background.width/2)+(xPos-PosX)*(rnd(i)*3-1.0),(yPos-background.height/2)+(yPos-PosY)*(rnd(i)*3-1.0),0,1,Reflections1Scale*rnd(2*i),(Ratio>0?0.5:1)*xScale),((Brightness>1.0)?1.0:Brightness)*Reflections1Opacity*rnd(3*i)),rnd(i)*width/33),1,100);
		}
		Reflections1 = Blur(Reflections1,Reflections1Blur,xPixels);
	}
	else
		Reflections1 = BlackBackground;

	if (Reflections2Enable == 1.0)
	{
		Reflections2RGrad1  = RGrad(background.width,background.height, 1, 2*xPos-PosX, 2*yPos-PosY, 1, 0, Reflections2Scale*height/2.5, 0.5, 1, 1, 1, 0, 0, 1, 0.9454837, 0.8089503, 0.5, 0);
		Reflections2RGrad3  = RGrad(background.width,background.height, 1, 2*xPos-PosX, 2*yPos-PosY, 1, Reflections2Scale*height/2.5, 0, 0.5, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0);
		Reflections2Invert1 = Invert(Reflections2RGrad1, "rgb");
		Reflections2IMult2  = IMult(Reflections2Invert1, Reflections2RGrad3, 1, 100, 0);
		Reflections2Blur1   = Blur(Reflections2IMult2, 76, xPixels);

		Reflections2RGrad2  = RGrad(background.width,background.height, 1, PosX, PosY, 1, height/2, height/4, 0.5, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0);
		Reflections2Scale   = Scale(Reflections2RGrad2,1,Ratio>0?0.5:1,PosX,PosY);
		Reflections2        = Fade(IMult(Reflections2RGrad2, Reflections2Blur1, 1, 100, 0),((Brightness>1.0)?1.0:Brightness)*Reflections2Opacity);
	}
	else
		Reflections2 = BlackBackground;

	IAdd1 = IAdd(RaysFinal, Halo1, 1, 100);
	IAdd2 = IAdd(IAdd1,Reflections1,1,100);
	IAdd3 = IAdd(IAdd2,Reflections2,1,100);
    Final = IAdd3;

	if (UseAlpha == 1.0)
	{
		HideCoef = WarpX(AlphaInput, 1, PosX, PosY, width, height);
		HideFinal = LayerX(HideCoef,Final,(1-a)*r2,(1-a)*g2,(1-a)*b2,(1-a)*a2,z);
	}
	else
		HideFinal = Final;

	FinalNode = AE_Layers(HideFinal,background,0,TransferMode);

	return Mix(background,FinalNode,1,Blend,"rgba");
}

image	LaserFlare(
	image	background,
	image	AlphaInput,
	float	PosX=width/2,		float	PosY=height/2,
	int	UseAlpha=0,
	string	TransferMode="Screen",
	float	Blend=100.0,
	float	Brightness=1.0,
	float	Quality=1.0,
	int	Ratio=0,
	float	rColor=1.0,		float	gColor=0.0,		float	bColor=0.0,
	int	HaloEnable=1,		float	HaloScale=1.0,		float	HaloOpacity=1.0,
	int	RaysEnable=1,		float	RaysOpacity=1.0,	float	RaysBlur=20.0,
	int	ArcEnable=1,		float	ArcScale=1.0,		float	ArcOpacity=1.0,		float	ArcBlur=width/20)
{
	BlackBackground = Black(background.width,background.height);

	if (RaysEnable == 1.0)
	{
		Rays = NGLRender(
			background.width,
			(Ratio>0?2:1)*background.height,
			1,
			"
			    nglPushMatrix();
			    float Radius = sqrt(width*width+height*height);
			    for(i=0;i<Quality*1000;i++)
			    {
				nglBegin(NGL_LINES);
				nglColor4f(1,1,1,1);
				nglVertex2f( PosX , (Ratio>0?2:1)*PosY );
				nglColor4f(rColor,gColor,bColor,rnd(3*i));
				nglVertex2f( PosX + Radius/(rnd(2*i)*10+10)*cosd(rnd(i)*360.0) , (Ratio>0?2:1)*PosY + Radius/(rnd(2*i)*10+10)*sind(rnd(i)*360.0));
				nglColor4f(rColor,gColor,bColor,rnd(3*i));
				nglVertex2f( PosX + Radius/(rnd(2*i)*10+10)*cosd(rnd(i)*360.0) , (Ratio>0?2:1)*PosY + Radius/(rnd(2*i)*10+10)*sind(rnd(i)*360.0));
				nglColor4f(0,0,0,0);
				nglVertex2f( PosX + Radius*cosd(rnd(i)*360.0) , (Ratio>0?2:1)*PosY + Radius*sind(rnd(i)*360.0));
				nglEnd();
			    }
			    nglPopMatrix();
			"
	    	);
    		RaysBlur1 = Blur(Rays, RaysBlur, xPixels, 0, "gauss", xFilter, "rgba");
		Rays1 = Fade(RaysBlur1,((Brightness>1.0)?1.0:Brightness)*RaysOpacity);
		Rays = Resize(Rays1,background.width,background.height);
	}
	else
		Rays = BlackBackground;

	if (ArcEnable == 1.0)
	{
		Arc1 = AE_Arc(background.width,background.height, rColor, gColor, bColor, 0, 360, ArcScale*width/7, ArcScale*width/35, PosX, PosY, 100);
		Arc2  = Scale(Arc1,1,Ratio>0?0.5:1,PosX,PosY);
		Arc = Fade(Blur(Arc2,ArcBlur,xPixels),((Brightness>1.0)?1.0:Brightness)*ArcOpacity);
	}
	else
		Arc = BlackBackground;

	if (HaloEnable == 1.0)
	{
		Halo1 = RGrad(background.width,background.height, 1, PosX, PosY, 1, 0, Brightness*HaloScale*width/7, 0.5, rColor, gColor, bColor, 1, 0, 0, 0, 0, 0, 0);
		Halo = Scale(Fade(Halo1,((Brightness>1.0)?1.0:Brightness)*HaloOpacity),1,Ratio>0?0.5:1,PosX,PosY);
	}
	else
		Halo = BlackBackground;

	Final1 = Screen(Rays,Arc);
	Final  = IAdd(Halo,Final1);

	if (UseAlpha == 1.0)
	{
		HideCoef = WarpX(AlphaInput, 1, PosX, PosY, width, height);
		HideFinal = LayerX(HideCoef,Final,(1-a)*r2,(1-a)*g2,(1-a)*b2,(1-a)*a2,z);
	}
	else
		HideFinal = Final;

	FinalNode = AE_Layers(HideFinal,background,0,TransferMode);

	return Mix(background,FinalNode,1,Blend,"rgba");
}

image	LaserFlare2(
	image	background,
	image	AlphaInput,
	float	PosX=width/2,		float	PosY=height/2,
	float	xPos=width/2,		float	yPos=height/2,
	int	UseAlpha=0,
	string	TransferMode="Screen",
	float	Blend=100.0,
	float	Brightness=1.0,
	float	Quality=1.0,
	int	Ratio=0,
	float	rColor=0.5,		float	gColor=0.0,		float	bColor=0.0,
	float	rFanColor=1.0,		float	gFanColor=0.65,		float	bFanColor=0.15,
	int	HaloEnable=1,		float	HaloScale=1.0,		float	HaloOpacity=1.0,
	int	RaysEnable=1,		float	RaysScale=1.0,		float	RaysOpacity=1.0,	float	RaysBlur=5.0,
	int	FanEnable=1,		float	FanScale=1.0,		float	FanOpacity=1.0,		float	FanBlur=5.0,
	int	HorizLineEnable=1,	float	HorizLineScale=1.0,	float	HorizLineOpacity=1.0,
	int	ReflectionsEnable=1,	float	ReflectionsScale=1.0,	float	ReflectionsOpacity=1.0,	float	ReflectionsBlur=5.0,
	int	BackLensEnable=1,	float	BackLensScale=1.0,	float	BackLensOpacity=1.0)
{
	BlackBackground = Black(background.width,background.height);

	if (RaysEnable == 1.0)
	{
		Rays = NGLRender(
			background.width,
			(Ratio>0?2:1)*background.height,
			1,
			"
			    nglPushMatrix();
			    for (j=0;j<4;j++)
			    {
				    float Radius = Brightness*width/(2+j*2);
				    for(i=0;i<Quality*200*(j+1);i++)
				    {
					nglBegin(NGL_LINES);
					nglColor4f(0,0,0,0);
					nglVertex2f( 	PosX + turbulence(i,1)*Radius*cos(2*M_PI*turbulence(17*i,1)),
							PosY + turbulence(i,1)*Radius*sin(2*M_PI*turbulence(17*i,1)));
					nglColor4f(rColor,gColor,bColor,turbulence(3*i,1));
					nglVertex2f( 	PosX + (turbulence(i,1)*Radius+(0.5+0.5*turbulence(9*i,1))*50)*cos(2*M_PI*turbulence(17*i,1)),
							PosY + (turbulence(i,1)*Radius+(0.5+0.5*turbulence(9*i,1))*50)*sin(2*M_PI*turbulence(17*i,1)));

					nglColor4f(rColor,gColor,bColor,turbulence(3*i,1));
					nglVertex2f( 	PosX + (turbulence(i,1)*Radius+(0.5+0.5*turbulence(9*i,1))*51)*cos(2*M_PI*turbulence(17*i,1)),
							PosY + (turbulence(i,1)*Radius+(0.5+0.5*turbulence(9*i,1))*51)*sin(2*M_PI*turbulence(17*i,1)));
					nglColor4f(0,0,0,0);
					nglVertex2f( 	PosX + (turbulence(i,1)*Radius+(0.5+0.5*turbulence(9*i,1))*100)*cos(2*M_PI*turbulence(17*i,1)),
							PosY + (turbulence(i,1)*Radius+(0.5+0.5*turbulence(9*i,1))*100)*sin(2*M_PI*turbulence(17*i,1)));
					nglEnd();
				    }
			    }
			    nglPopMatrix();
			");
    		RaysBlur1 = Scale(Blur(Rays, RaysBlur, xPixels, 0, "gauss", xFilter, "rgba"),RaysScale,xScale,PosX,PosY);
		Rays1 = Fade(RaysBlur1,((Brightness>1.0)?1.0:Brightness)*RaysOpacity);
		Rays = Scale(Rays1,1,Ratio>0?0.5:1,PosX,PosY);
	}
	else
		Rays = BlackBackground;



	if (HaloEnable == 1.0)
	{
		HaloRGrad1 = RGrad(background.width,background.height, 1, PosX, PosY, 1, 0, Brightness*width/3.5, 0.5, rColor, gColor, bColor, 1, 0, 0, 0, 0, 0, 0);
		HaloRGrad2 = RGrad(background.width,background.height, 1, PosX, PosY, 1, 0, Brightness*width/20, 0.5, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0);
		Halo = Fade(Scale(Over(HaloRGrad2, HaloRGrad1, 1, 0, 0),HaloScale,(Ratio>0?0.5:1)*xScale,PosX,PosY),((Brightness>1.0)?1.0:Brightness)*HaloOpacity);
	}
	else
		Halo = BlackBackground;

	if (FanEnable == 1.0)
	{
		Fan1 = NGLRender(
			background.width,
			background.height,
			1,
			"
			    nglPushMatrix();
			    for (j=0;j<4;j++)
			    {
				    float Radius = Brightness*width/4;
				    for(i=0;i<Quality*40;i++)
				    {
					nglBegin(NGL_LINES);
					nglColor4f(rFanColor,gFanColor,bFanColor,1);
					nglVertex2f( 	PosX, PosY);
					nglColor4f(0,0,0,0);
					nglVertex2f( 	PosX + (0.5+0.5*turbulence(3*i,1))*Radius*cos(2*M_PI*turbulence(4*i,1)),
							PosY + (0.5+0.5*turbulence(3*i,1))*Radius*sin(2*M_PI*turbulence(4*i,1)));
					nglEnd();
				    }
			    }
			    nglPopMatrix();
			");
		FanBlur1 = Rotate(Fan1, Linear(2,0@1,5@2), 1, PosX, PosY, 1, 1, 0);
    		FanBlur2 = Scale(Blur(FanBlur1, FanBlur, xPixels, 0, "gauss", xFilter, "rgba"),FanScale,(Ratio>0?0.5:1)*xScale,PosX,PosY);
		Fan = Fade(FanBlur2,((Brightness>1.0)?1.0:Brightness)*FanOpacity);
	}
	else
		Fan = BlackBackground;

	if (HorizLineEnable == 1.0)
	{
		HorizLineRGrad1 = RGrad(background.width,background.height, 1, PosX, PosY, 1, 0, width/3, 0.5, rColor, gColor, bColor, 1, 0, 0, 0, 0, 0, 0);
		HorizLineRGrad2 = RGrad(background.width,background.height, 1, PosX, PosY, 1, 0, width/3, 0.5, rFanColor, gFanColor, bFanColor, 1, 0, 0, 0, 0, 0, 0);
		HorizLineScale1 = Scale(HorizLineRGrad2, 0.9, 0.2, PosX, PosY, 0, 0.5, 0);
		HorizLineScreen1 = Fade(Screen(HorizLineScale1, HorizLineRGrad1, 1),((Brightness>1.0)?1.0:Brightness)*HorizLineOpacity);
		HorizLine = Scale(HorizLineScreen1, 3*HorizLineScale, (Ratio>0?0.5:1)*0.08*HorizLineScale, PosX, PosY, 0, 0.5, 0);
	}
	else
		HorizLine = BlackBackground;

	if (ReflectionsEnable == 1.0)
	{
		Reflection1 = AE_Arc(background.width,background.height, rColor, gColor, bColor, atan2d(PosY-height/2,PosX-width/2), 360, 0, 50, width/2, height/2, 5);

		Reflections = BlackBackground;
		for (int count=0;count<15;count++)
		{
			float Position = 0.5-turbulence(count,1)*2;
			float TheScale = turbulence(7*count,1)*ReflectionsScale;
			Reflections1 = Move2D(Reflection1,(xPos-background.width/2)+Position*(PosX-xPos),(yPos-background.height/2)+Position*(PosY-yPos),0,1,TheScale,(Ratio>0?0.5:1)*xScale);
			Reflections2 = Blur(Reflections1,(0.1+turbulence(8*count,1))*ReflectionsBlur,xPixels);
			Reflections3 = Fade(Reflections2,((Brightness>1.0)?1.0:Brightness)*turbulence(5*count,1)*ReflectionsOpacity);
			Reflections = IAdd(Reflections,Reflections3,1,100);
		}
	}
	else
		Reflections = BlackBackground;

	if (BackLensEnable == 1.0)
	{
		BackLensRGrad1 = RGrad(background.width,background.height, 1, width/2, height/2, 1, 0, width/7, 0.85, 0, 0, 0, 0, 0, rColor, gColor, bColor, 1, 0);
		BackLensRGrad2 = RGrad(background.width,background.height, 1, width/2, height/2, 1, width/7, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0);
		BackLensRGrad3 = RGrad(background.width,background.height, 1, width/2, height/2, 1, 0, width/3.5, 0.5, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0);
		BackLensIMult1 = IMult(BackLensRGrad1, BackLensRGrad2, 1, 100, 0);
		BackLensPan1 = Pan(BackLensRGrad3, distance(PosX,PosY,width/2,height/2)*(width/14)/100*cosd(atan2d(PosY-height/2,PosX-width/2)),distance(PosX,PosY,width/2,height/2)*(width/14)/100*sind(atan2d(PosY-height/2,PosX-width/2)), 0, 0.5, 0);
		BackLensBlur1 = Blur(BackLensIMult1, 25, xPixels, 0, "gauss", xFilter, "rgba");
		BackLensIMult2 = Scale(IMult(BackLensBlur1, BackLensPan1, 1, 100, 0),BackLensScale,(Ratio>0?0.5:1)*xScale);
		BackLens = Fade(Pan(BackLensIMult2,(xPos-background.width/2)-(PosX-xPos),(yPos-background.height/2)-(PosY-yPos)),((Brightness>1.0)?1.0:Brightness)*BackLensOpacity);
	}
	else
		BackLens = BlackBackground;

	Final1 = IAdd(Rays,Halo,1,100);
	Final2 = IAdd(Fan,Final1,1,100);
	Final3 = IAdd(HorizLine,Final2,1,100);
	Final4 = IAdd(Reflections,Final3,1,100);
	Final  = IAdd(BackLens,Final4,1,100);

	if (UseAlpha == 1.0)
	{
		HideCoef = WarpX(AlphaInput, 1, PosX, PosY, width, height);
		HideFinal = LayerX(HideCoef,Final,(1-a)*r2,(1-a)*g2,(1-a)*b2,(1-a)*a2,z);
	}
	else
		HideFinal = Final;

	FinalNode = AE_Layers(HideFinal,background,0,TransferMode);

	return Mix(background,FinalNode,1,Blend,"rgba");
}

image	SpaceSun(
	image	background,
	image	AlphaInput,
	float	PosX=width/2,		float	PosY=height/2,
	float	xPos=width/2,		float	yPos=height/2,
	int	Angle=0,
	int	UseAlpha=0,
	string	TransferMode="Screen",
	float	Brightness=1.0,
	float	Quality=1.0,
	float	Blend=100.0,
	int	Ratio=0,
	int	CenterEnable=1,		float CenterScale=1.0,		float CenterOpacity=1.0,
	int	RaysEnable=1,		float RaysScale=1.0,		float RaysOpacity=1.0,
	int	BlueStreaksEnable=1,	float BlueStreaksScale=1.0,	float BlueStreaksOpacity=1.0,
	int	ReflectionsEnable=1,	float ReflectionsScale=1.0,	float ReflectionsOpacity=1.0,
	int	Reflections2Enable=1,	float Reflections2Scale=1.0,	float Reflections2Opacity=1.0)
{
	BlackBackground = Black(background.width,background.height);

	if (CenterEnable == 1.0)
	{
		RGrad1  = RGrad(background.width, background.height, 1, width/2, height/2, 1, 6.54653168, 322.2994, 0.275, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0);
		RGrad2  = RGrad(background.width, background.height, 1, width/2, height/2, 1, 0, 158.597931, 0.5, 1, 1, 1, 1, 0, 1, 0.7119796, 0.337127447, 0, 0);
		IMult1  = IMult(RGrad1, RGrad2, 1, 100, 0);
		Scale1  = Scale(IMult1, Brightness*0.4, xScale, width/2, height/2, 0, 0.5, 0);
		Center1 = Fade(Move2D(Scale1,PosX-width/2,PosY-height/2,Angle,1,CenterScale,(Ratio>0?0.5:1)*xScale,0,0,width/2,height/2,"default","default","trsx",0,1,0.5, 0, 0, time),((Brightness>1.0)?1.0:Brightness)*CenterOpacity);
	}
	else
		Center1 = BlackBackground;

	if (BlueStreaksEnable == 1.0)
	{
		RGrad3 = RGrad(background.width, background.height, 1, width/2, height/2, 1, min(width,height)/4, min(width,height)/4, 0.5, 0.0581522323, 0.101588815, 0.195, 1, 0, 0, 0, 0, 0, 0);
		ColorX1 = ColorX(RGrad3, r*turbulence(y,1), g*turbulence(y,1), b*turbulence(y,1), a, z);
		Scale3 = Scale(ColorX1, 1.3, 0.05, width/2, height/2, 0, 0.5, 0);
		Streak1 = Fade(Move2D(Scale3,PosX-width/2,PosY-height/2,Angle,1,BlueStreaksScale,(Ratio>0?0.5:1)*xScale,0,0,width/2,height/2,"default","default","trsx",0,1,0.5, 0, 0, time),BlueStreaksOpacity);
		Streak2 = Fade(Move2D(Scale3,(xPos-background.width/2)+(xPos-PosX),(yPos-background.height/2)+(yPos-PosY),0,1,BlueStreaksScale,(Ratio>0?0.5:1)*xScale,0,0,width/2,height/2,"default","default","trsx",0,1,0.5, 0, 0, time),((Brightness>1.0)?1.0:Brightness)*BlueStreaksOpacity);
	}
	else
	{
		Streak1 = BlackBackground;
		Streak2 = BlackBackground;
	}

	if (RaysEnable == 1.0)
	{
		TheRays = NGLRender(background.width, (Ratio>0?2:1)*background.height, 4,
					{{
					    nglPushMatrix();
					    float	PosX = width/2;
					    float	PosY = height/2;
					    float 	Radius = (Ratio>0?0.5:1)*height/2;

					    for(int i=0;i<Quality*500;i++)
					    {
						nglBegin(2);
						nglColor4f(1,1,1,1);
						nglVertex2f( 	PosX , PosY );
						nglColor4f(0,0,0,0);
						nglVertex2f( 	PosX + Radius*cosd(turbulence(i,1)*360.0) ,
								PosY + Radius*sind(turbulence(i,1)*360.0));
						nglEnd();
					    }
					    nglPopMatrix();
		 			}});
		Blur1 = Blur(TheRays, 100, xPixels, 0, "gauss", xFilter, "rgba");
		TheRays4 = NGLRender(background.width, (Ratio>0?2:1)*background.height, 4,
					{{
					    nglPushMatrix();
					    float	PosX = width/2;
					    float	PosY = height/2;
					    float 	Radius = (Ratio>0?0.5:1)*height/2;

					    for(int i=0;i<Quality*500;i++)
					    {
						if (turbulence(19*i,1)>Level)
						{
							nglBegin(NGL_LINES);
							nglColor4f(1,1,1,1);
							nglVertex2f( PosX , PosY );
							nglColor4f(0,0,0,0);
							nglVertex2f( 	PosX + Radius*cosd(turbulence(i,1)*720.0-180.0) ,
									PosY + Radius*sind(turbulence(i,1)*720.0-180.0));
							nglEnd();
						}
					    }
					    nglPopMatrix();
					}}, float Level = 0.95);
		Blur2 = Blur(TheRays4, 10, xPixels, 0, "gauss", xFilter, "rgba");
		Brightness1 = Brightness(Blur2, 0.1);
		IAdd3 = IAdd(Blur1, Brightness1, 1, 100);
		Scale2 = Scale(IAdd3, Brightness*1.6, (Ratio>0?0.5:1)*xScale, width/2, height/2, 0, 0.5, 0);
		Move2D1 = Move2D(Scale2,PosX-width/2,PosY-height/2,Angle,1,RaysScale,xScale,0,0,width/2,height/2,"default","default","trsx",0,1,0.5, 0, 0, time);
		Rays1 = Fade(Move2D1,((Brightness>1.0)?1.0:Brightness)*RaysOpacity);
	}
	else
		Rays1 = BlackBackground;

	if (CenterEnable == 1.0)
	{
		RGrad4       = RGrad(background.width, background.height, 1, PosX, PosY, 1, 0, height/2, 0.5, 0.1, 0, 0, 1, 0, 0, 0, 0, 0, 0);
		ReddishGlow1 = Fade(Scale(RGrad4,Brightness*CenterScale,(Ratio>0?0.5:1)*xScale, PosX, PosY),((Brightness>1.0)?1.0:Brightness)*CenterOpacity);
	}
	else
		ReddishGlow1 = BlackBackground;

	if (ReflectionsEnable == 1.0)
	{
		ReflectionShape = QuickShape(720,486, 1, Hermite(0,[0,0,0]@5), Hermite(0,[0,0,0]@5),
		    Hermite(0,[0,0,0]@5), 1, Hermite(0,[1,0,0]@5), Hermite(0,[1,0,0]@5),
		    width/2, height/2, 0, 0.5, 0, 128, Hermite(0,[301.661,0,0]@5),
		    Hermite(0,[170.66,0,0]@5), Hermite(0,[0.8545,0,0]@5), Hermite(0,[0.5195,0,0]@5),
		    Hermite(0,[0.8545,0,0]@5), Hermite(0,[0.5195,0,0]@5), Hermite(0,[1.1811,0,0]@5),
		    Hermite(0,[1.1811,0,0]@5), Hermite(0,[400.661,0,0]@5), Hermite(0,[170.66,0,0]@5),
		    Hermite(0,[0.8455,0,0]@5), Hermite(0,[-0.534,0,0]@5), Hermite(0,[0.8455,0,0]@5),
		    Hermite(0,[-0.5341,0,0]@5), Hermite(0,[1.2487,0,0]@5), Hermite(0,[1.2487,0,0]@5),
		    Hermite(0,[409.661,0,0]@5), Hermite(0,[190.16,0,0]@5), Hermite(0,[0.8723,0,0]@5),
		    Hermite(0,[0.489,0,0]@5), Hermite(0,[0.8723,0,0]@5), Hermite(0,[0.489,0,0]@5),
		    Hermite(0,[1.0993,0,0]@5), Hermite(0,[1.0993,0,0]@5), Hermite(0,[363.633,0,0]@5),
		    Hermite(0,[262.16,0,0]@5), Hermite(0,[0.8824,0,0]@5), Hermite(0,[-0.4704,0,0]@5),
		    Hermite(0,[0.8824,0,0]@5), Hermite(0,[-0.4704,0,0]@5), Hermite(0,[0.9923,0,0]@5),
		    Hermite(0,[0.9923,0,0]@5), Hermite(0,[344.633,0,0]@5), Hermite(0,[262.16,0,0]@5),
		    Hermite(0,[0.899,0,0]@5), Hermite(0,[0.4379,0,0]@5), Hermite(0,[0.899,0,0]@5),
		    Hermite(0,[0.4379,0,0]@5), Hermite(0,[1.0043,0,0]@5), Hermite(0,[1.0043,0,0]@5),
		    Hermite(0,[293.161,0,0]@5), Hermite(0,[188.16,0,0]@5), Hermite(0,[0.8621,0,0]@5),
		    Hermite(0,[-0.5068,0,0]@5), Hermite(0,[0.8621,0,0]@5), Hermite(0,[-0.5068,0,0]@5),
		    Hermite(0,[0.9484,0,0]@5), Hermite(0,[0.9484,0,0]@5));
		Reflection_  = ColorX(ReflectionShape, r*0.8, g*0.2, b*0.2, a, z);
		Reflection2_ = ColorX(ReflectionShape, r*0.1, g*0.5, b*0.2, a, z);
		Reflection   = Resize(Reflection_,background.width,background.height);
		Reflection2  = Resize(Reflection2_,background.width,background.height);

		Reflections = BlackBackground;

		for (int i=0;i<Quality*20;i++)
		{
			Reflections = IAdd(Fade(Blur(Move2D(Reflection,(xPos-background.width/2)+(xPos-PosX)*(rnd(i)*4-0.5),(yPos-background.height/2)+(yPos-PosY)*(rnd(i)*4-0.5),atan2d(PosY-height/2,PosX-width/2)-90,1,ReflectionsScale*rnd(3*i)*0.5,(Ratio>0?0.5:1)*xScale,0,0,width/2,height/2,"default",xFilter,"tsrx",0,1,0.5, 0, 0, time),rnd(5*i)*30,xPixels),((Brightness>1.0)?1.0:Brightness)*ReflectionsOpacity*rnd(7*i)),Reflections);
		}
		for (i=0;i<Quality*10;i++)
		{
			Reflections = IAdd(Fade(Blur(Move2D(Reflection2,(xPos-background.width/2)-(PosX-xPos)*(rnd(8*i)*4-0.5),(yPos-background.height/2)-(PosY-yPos)*(rnd(8*i)*4-0.5),atan2d(PosY-height/2,PosX-width/2)-90,1,ReflectionsScale*rnd(11*i)*0.5,(Ratio>0?0.5:1)*xScale,0,0,width/2,height/2,"default",xFilter,"tsrx",0,1,0.5, 0, 0, time),rnd(13*i)*30,xPixels),((Brightness>1.0)?1.0:Brightness)*ReflectionsOpacity*rnd(7*i)),Reflections);
		}
	}
	else
		Reflections = BlackBackground;

	if (Reflections2Enable == 1.0)
	{
		Reflection2 = RGrad(background.width,background.height, 1, width/2, height/2, 1, 0, width/100, 0.5, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0);
		Reflections2 = BlackBackground;
		for (int i=0;i<Quality*10;i++)
		{
			Reflections2 = IAdd(Reflections2,Fade(Move2D(Reflection2,(xPos-background.width/2)+(xPos-PosX)*(rnd(7*i)*4-0.5),(yPos-background.height/2)+(yPos-PosY)*(rnd(7*i)*4-0.5),0,1,Reflections2Scale*rnd(8*i),(Ratio>0?0.5:1)*xScale,0,0,width/2,height/2,"default",xFilter,"tsrx",0,1,0.5, 0, 0, time),((Brightness>1.0)?1.0:Brightness)*Reflections2Opacity*rnd(7*i)),1,100);
		}
	}
	else
		Reflections2 = BlackBackground;

	Final1 = IAdd(Streak1, Center1, 1, 100);
	Final2 = IAdd(Streak2, Final1, 1, 100);
	Final3 = IAdd(Rays1,Final2,1,100);
	Final4 = IAdd(Reflections,Final3,1,100);
	Final5 = IAdd(Reflections2,Final4,1,100);
	Final  = IAdd(ReddishGlow1, Final5, 1, 100);
	Final = Clamp(Final, 0, rLo, rLo, 0, 1, rHi, rHi, 1);

	if (UseAlpha == 1.0)
	{
		HideCoef = WarpX(AlphaInput, 1, PosX, PosY, width, height);
		HideFinal = LayerX(HideCoef,Final,(1-a)*r2,(1-a)*g2,(1-a)*b2,(1-a)*a2,z);
	}
	else
		HideFinal = Final;

	FinalNode = AE_Layers(HideFinal,background,0,TransferMode);

	return Mix(background,FinalNode,1,Blend,"rgba");
}


image	SunDown(
	image	Background,
	image	AlphaInput,
	float	PosX=width/2,		float	PosY=height/2,
	float	xPos=width/2,		float	yPos=height/2,
	int	UseAlpha=0,
	string	TransferMode="Screen",
	float	Brightness=1.0,
	float	Blend=100.0,
	int	Ratio=0,
	int	CenterEnable=1,		float CenterScale=1.0,		float CenterOpacity=1.0,
	int	GlowEnable=1,		float GlowScale=1.0,		float GlowOpacity=1.0,
	int	ReflectionsEnable=1,	float ReflectionsScale=1.0,	float ReflectionsOpacity=1.0)
{
	BlackBackground = Black(Background.width,Background.height);
	if (CenterEnable == 1.0)
	{
		CenterRGrad2 = RGrad(Background.width,Background.height, 1, width/2, height/2, 1, Brightness*width/35, Brightness*width/60.0, 0.5, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0);
		CenterBlur1 = Blur(CenterRGrad2, width/7, xPixels, 0, "gauss", xFilter, "rgba");
		Center1 = Fade(Move2D(CenterBlur1,PosX-width/2,PosY-height/2,0,1,CenterScale,(Ratio>0?0.5:1)*xScale),((Brightness>1.0)?1.0:Brightness)*CenterOpacity);
	}
	else
		Center1 = BlackBackground;

	if (GlowEnable ==1.0)
	{
		GlowRGrad1 = RGrad(Background.width,Background.height, 1, width/2, height/2, 1, Brightness*width/35, Brightness*width/3.25, 0.5, 1, 0.5568756, 0.158810377, 1, 0, 0, 0, 0, 0, 0);
		Glow1 = Fade(Move2D(GlowRGrad1,PosX-width/2,PosY-height/2,0,1,GlowScale,(Ratio>0?0.5:1)*xScale),((Brightness>1.0)?1.0:Brightness)*GlowOpacity);
	}
	else
		Glow1 = BlackBackground;

	if (ReflectionsEnable == 1.0)
	{
		Reflection = AE_Arc(Background.width,Background.height, 1, 0.55, 0.15, 0, 360, 0, width/10, width/2, height/2, 6);
		Reflections = Black(Background.width,Background.height);
		for (int i=0;i<10;i++)
		{
			Reflection1 = Fade(Blur(Reflection,fnoise(65*i,1)*width/10+width/30,xPixels),((Brightness>1.0)?1.0:Brightness)*ReflectionsOpacity*fnoise(78*i,1));
			Reflections = IAdd(Reflections,Move2D(Reflection1,(xPos-Background.width/2)+(fnoise(i,1)*2.0-0.5)*(xPos-PosX),(yPos-Background.height/2)+(fnoise(i,1)*2.0-0.5)*(yPos-PosY),90+atan2d(PosY-height/2,PosY-width/2),1,0.5*ReflectionsScale*fnoise(54*i,1),(Ratio>0?0.5:1)*xScale,0,0,width/2,height/2,"default",xFilter,"tsrx"),1,100);
		}
	}
	else
		Reflections = BlackBackground;

	Final1 = IAdd(Center1, Glow1, 1, 100);
	Final  = IAdd(Reflections,Final1,1,100);

	if (UseAlpha == 1.0)
	{
		HideCoef = WarpX(AlphaInput, 1, PosX, PosY, width, height);
		HideFinal = LayerX(HideCoef,Final,(1-a)*r2,(1-a)*g2,(1-a)*b2,(1-a)*a2,z);
	}
	else
		HideFinal = Final;

	FinalNode = AE_Layers(HideFinal,Background,0,TransferMode);

	return Mix(Background,FinalNode,1,Blend,"rgba");
}


image	SixStar(
	image	background,
	image	AlphaInput,
	float	PosX=width/2,		float	PosY=height/2,
	float	xPos=width/2,		float	yPos=height/2,
	int	UseAlpha=0,
	string	TransferMode="Screen",
	float	Brightness=1.0,
	float	Blend=100.0,
	int	Ratio=0,
	int	CenterEnable=1,		float CenterScale=1.0,		float CenterOpacity=1.0,
	int	GlowEnable=1,		float GlowScale=1.0,		float GlowOpacity=1.0,
	int	ReflectionsEnable=1,	float ReflectionsScale=1.0,	float ReflectionsOpacity=1.0)
{
	if (CenterEnable == 1.0)
	{
		Center  = RGrad(background.width,background.height, 1, width/2, height/2, 1, 0, width/30, 0.5, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0);
		Center1 = Fade(Move2D(Center,PosX-width/2,PosY-height/2,0,1,Brightness*CenterScale,(Ratio>0?0.5:1)*xScale),((Brightness>1.0)?1.0:Brightness)*CenterOpacity);
	}
	else
		Center1 = Black(background.width,background.height);

	if (GlowEnable == 1.0)
	{
		RGrad1 = RGrad(background.width,background.height, 1, width/2, height/2, 1, width/100, width/7,  0.5, 1, 0.5328864, 0.177078426, 1, 0, 0, 0, 0, 0, 0);
		Scale1 = Scale(RGrad1, 2, 0.1, width/2, height/2, 0, 0.5, 0);
		Scale2 = Scale(RGrad1, 1.5, xScale, width/2, height/2, 0, 0.5, 0);
		Fade1 = Fade(Scale2, 0.5);
		IAdd1 = IAdd(RGrad1, Scale1, 1, 100);
		Rotate1 = Rotate(Scale1, 60, 1.11111116, width/2, height/2, 0, 0.5, 0);
		Rotate2 = Rotate(Scale1, 120, 1.11111116, width/2, height/2, 0, 0.5, 0);
		IAdd2 = IAdd(IAdd1, Rotate1, 1, 100);
		IAdd3 = IAdd(IAdd2, Rotate2, 1, 100);
		Brightness1 = Brightness(IAdd3, 3.5);
		Blur1 = Blur(Brightness1, width/10, xPixels, 0, "gauss", xFilter, "rgba");
		IAdd4 = IAdd(Fade1, Blur1, 1, 100);
		Glow1 = Fade(Move2D(IAdd4,PosX-width/2,PosY-height/2,0,1,Brightness*GlowScale,(Ratio>0?0.5:1)*xScale),((Brightness>1.0)?1.0:Brightness)*GlowOpacity);
	}
	else
		Glow1 = Black(background.width,background.height);

	Reflections = Black(background.width,background.height);
	if (ReflectionsEnable == 1.0)
	{
		Reflection = RGrad(background.width,background.height, 1, width/2, height/2, 1, width/20, width/150, 0.5, 1, 0.5920025, 0.275500178, 1, 0, 0, 0, 0, 0, 0);
		for (int i=0;i<10;i++)
		{
			Reflection1 = Fade(Blur(Reflection,fnoise(65*i,1)*width/10+width/30,xPixels),((Brightness>1.0)?1.0:Brightness)*ReflectionsOpacity*fnoise(78*i,1));
			Reflections = IAdd(Reflections,Move2D(Reflection1,(xPos-background.width/2)+(fnoise(i,1)*2.0-0.5)*(xPos-PosX),(yPos-background.height/2)+(fnoise(i,1)*2.0-0.5)*(yPos-PosY),90+atan2d(PosY-height/2,PosY-width/2),1,0.5*ReflectionsScale*fnoise(54*i,1),((Ratio>0)?0.5:1)*xScale,0,0,width/2,height/2,"default",xFilter,"tsrx"),1,100);
		}
	}

	CenterGlow1 = IAdd(Center1, Glow1, 1, 100);
	Blur2 = Blur(CenterGlow1, width/10, xPixels, 0, "gauss", xFilter, "rgba");
	Final = IAdd(Reflections,Blur2,1,100);

	if (UseAlpha == 1.0)
	{
		HideCoef = WarpX(AlphaInput, 1, PosX, PosY, width, height);
		HideFinal = LayerX(HideCoef,Final,(1-a)*r2,(1-a)*g2,(1-a)*b2,(1-a)*a2,z);
	}
	else
		HideFinal = Final;

	FinalNode = AE_Layers(HideFinal,background,0,TransferMode);

	return Mix(background,FinalNode,1,Blend,"rgba");
}

image	SpaceBlue(
	image	background,
	image	AlphaInput,
	float	PosX=width/2,		float	PosY=height/2,
	float	xPos=width/2,		float	yPos=height/2,
	int	UseAlpha=0,
	string	TransferMode="Screen",
	float	Brightness=1.0,
	float	Blend=100.0,
	int	Ratio=0,
	int	CenterEnable=1,		float CenterScale=1.0,		float CenterOpacity=1.0,
	int	GlowEnable=1,		float GlowScale=1.0,		float GlowOpacity=1.0,
	int	BlueLineEnable=1,	float BlueLineScale=1.0,	float BlueLineOpacity=1.0,
	int	ReflectionsEnable=1,	float ReflectionsScale=1.0,	float ReflectionsOpacity=1.0)
{
	if (CenterEnable == 1.0)
	{
		TheRays4 = NGLRender(background.width,(Ratio>0?2:1)*background.height, 1, {{
							    nglPushMatrix();
							    for(i=0;i<500;i++)
							    {
								if (turbulence(19*i,1)>0.85)
								{
									nglBegin(2);
									nglColor4f(1,1,1,1);
									nglVertex2f( width/2 , height/2 );
									nglColor4f(0,0,0,0);
									nglVertex2f( 	width/2  + height/2*cosd(turbulence(i,1)*720.0-180.0) ,
											height/2 + height/2*sind(turbulence(i,1)*720.0-180.0));
									nglEnd();
								}
							    }
							    nglPopMatrix();
		 }}
		 );
		TheRays4 = Resize(TheRays4,background.width,background.height);
		RGrad3 = RGrad(background.width,background.height, 1, width/2, height/2, 1, 0,        width/17, 0.5, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0);
		Blur2 = Blur(RGrad3, width/10, xPixels, 0, "gauss", xFilter, "rgba");
		Blur3 = Blur(TheRays4, width/70, xPixels, 0, "gauss", xFilter, "rgba");
		IMult1 = IMult(Blur3, Blur2, 1, 100, 0);
		IAdd4 = IAdd(IMult1, Blur2, 1, 100);
		Center = Fade(Move2D(IAdd4,PosX-width/2,PosY-height/2,0,1,Brightness*2*CenterScale,xScale),((Brightness>1.0)?1.0:Brightness)*CenterOpacity);
	}
	else
		Center = Black(background.width,background.height);

	if (GlowEnable == 1.0)
	{
		RGrad5 = RGrad(background.width,background.height, 1, width/2, height/2, 1, 0,         width/3, 0.5, 0.551, 0.551, 0.551, 1, 0, 0, 0, 0, 0, 0);
		TheRays5 = NGLRender(background.width,(Ratio>0?2:1)*background.height, 1, {{
							    nglPushMatrix();
							    for(i=0;i<500;i++)
							    {
								nglBegin(2);
								nglColor4f(1,1,1,1);
								nglVertex2f( width/2 , height/2 );
								nglColor4f(0,0,0,0);
								nglVertex2f( 	width/2  + width/2*cosd(turbulence(i,1)*720.0-180.0) ,
										height/2 + width/2*sind(turbulence(i,1)*720.0-180.0));
								nglEnd();
							    }
							    nglPopMatrix();
		 }});
		TheRays5 = Resize(TheRays5,background.width,background.height);
		Scale1 = Scale(TheRays5, 0.5, xScale, width/2, height/2, 0, 0.5, 0);
		IMult2 = IMult(Scale1, RGrad5, 1, 100, 0);
		Blur4 = Blur(IMult2, width/7, xPixels, 0, "gauss", xFilter, "rgba");

		Glow = Fade(Move2D(Blur4,PosX-width/2,PosY-height/2,0,1,Brightness*2*GlowScale,xScale,0,0,width/2,height/2,"default",xFilter,"tsrx"),((Brightness>1.0)?1.0:Brightness)*GlowOpacity);
	}
	else
		Glow = Black(background.width,background.height);

	if (BlueLineEnable == 1.0)
	{
		RGrad1 = RGrad(background.width,background.height, 1, width/2, height/2, 1, 0,         width/3, 0.5, 0.137089, 0.227342442, 0.4286, 1, 0, 0, 0, 0, 0, 0);
		RGrad2 = RGrad(background.width,background.height, 1, width/2, height/2, 1, width/16, width/45, 0.5, 0.356494, 0.524123, 1, 1, 0, 0, 0, 0, 0, 0);
		Scale3 = Scale(RGrad2, 2, 0.2, width/2, height/2, 0, 0.5, 0);
		IAdd1 = IAdd(RGrad1, Scale3, 1, 100);
		Scale2 = Scale(IAdd1, 2.1, 0.05, width/2, height/2, 0, 0.5, 0);
		BlueLine = Fade(Move2D(Scale2,PosX-width/2,PosY-height/2,0,1,2*BlueLineScale,(Ratio>0?0.5:1)*xScale),((Brightness>1.0)?1.0:Brightness)*BlueLineOpacity);
	}
	else
		BlueLine = Black(background.width,background.height);

	Reflections = Black(background.width,background.height);
	if (ReflectionsEnable == 1.0)
	{
		Reflection = RGrad(background.width,background.height, 1, width/2, height/2, 1, 0, width/10, 0.5, 0.179421932, 0.3504665, 0.58, 1, 0, 0, 0, 0, 0, 0);
		for (int i=0;i<25;i++)
		{
			Reflection1 = Fade(Blur(Reflection,fnoise(65*i,1)*width/20+width/40,xPixels),((Brightness>1.0)?1.0:Brightness)*ReflectionsOpacity*fnoise(78*i,1));
			Reflections = IAdd(Reflections,Move2D(Reflection1,(xPos-background.width/2)+(fnoise(i,1)*3.0-1.0)*(xPos-PosX),(yPos-background.height/2)+(fnoise(i,1)*3.0-1.0)*(yPos-PosY),90+atan2d(PosY-height/2,PosY-width/2),1,0.5*ReflectionsScale*fnoise(54*i,1),(Ratio>0?0.5:1)*xScale),1,100);
		}
	}

	Final1 = IAdd(Glow,Center,1,100);
	Final2 = IAdd(BlueLine,Final1,100);
	Final  = IAdd(Reflections,Final2,1,100);

	if (UseAlpha == 1.0)
	{
		HideCoef = WarpX(AlphaInput, 1, PosX, PosY, width, height);
		HideFinal = LayerX(HideCoef,Final,(1-a)*r2,(1-a)*g2,(1-a)*b2,(1-a)*a2,z);
	}
	else
		HideFinal = Final;

	FinalNode = AE_Layers(HideFinal,background,0,TransferMode);

	return Mix(background,FinalNode,1,Blend,"rgba");
}


image	RockyFlare(
	image	background,
	image	AlphaInput,
	float	PosX=width/2,		float	PosY=height/2,
	float	xPos=width/2,		float	yPos=height/2,
	int	UseAlpha=0,
	string	TransferMode="Screen",
	float	Brightness=1.0,
	float	Blend=100.0,
	float	Quality=1.0,
	int	Ratio=0,
	float	rColor=0.1,		float gColor=0.2,		float bColor=0.3,
	int	HaloEnable=1,		float HaloScale=1.0,		float HaloOpacity=1.0,
	int	RaysEnable=1,		float RaysScale=1.0,		float RaysOpacity=1.0,		float RaysBlur=15.0,
	int	ReflectionsEnable=1,	float ReflectionsScale=1.0,	float ReflectionsOpacity=1.0)
{
	BlackBackground = Black(background.width,background.height);

	if (RaysEnable == 1.0)
	{
		Rays = NGLRender(
			background.width,
			(Ratio>0?2:1)*background.height,
			1,
			"
			    nglPushMatrix();
			    float Radius = Brightness*RaysScale*background.width/5;
			    for(i=0;i<Quality*500;i++)
			    {
				nglBegin(NGL_LINES);
				nglColor4f(1,1,1,0.5+0.5*turbulence(9*i,1));
				nglVertex2f( PosX , (Ratio>0?2:1)*PosY );
				nglColor4f(0,0,0,0);
				nglVertex2f( PosX + (turbulence(2*i,1)+0.5)*Radius*cosd(turbulence(i,1)*360.0) , (Ratio>0?2:1)*PosY + (turbulence(2*i,1)+0.5)*Radius*sind(turbulence(i,1)*360.0));
				nglEnd();
			    }
			    nglPopMatrix();
			"
	    	);
		if (Ratio>0)
			Rays = Resize(Rays,background.width,background.height);
		RaysBlur1 = Blur(Rays, RaysBlur, xPixels, 0, "gauss", xFilter, "rgba");
		RaysFinal = Fade(RaysBlur1,((Brightness>1.0)?1.0:Brightness)*RaysOpacity);
	}
	else
		RaysFinal = BlackBackground;

	if (HaloEnable == 1.0)
	{
		HaloRGrad = RGrad(background.width,background.height, 1, PosX , PosY, 1, 0, HaloScale*Brightness*width/4.5, 0.5, HaloOpacity*rColor,HaloOpacity*gColor,HaloOpacity*bColor, HaloOpacity, 0, 0, 0, 0, 0, 0);
		Halo = Scale(HaloRGrad,1,Ratio>0?0.5:1,PosX,PosY);
	}
	else
		Halo = BlackBackground;


	if (ReflectionsEnable == 1.0)
	{
		Reflections = BlackBackground;
		Reflection1 = AE_Arc(background.width,background.height, rColor,gColor,bColor, 0, 360, 0, 15, width/2, height/2, 5);
		for (int count=0;count<Quality*40;count++)
		{
			float Position = turbulence(7*count,1)*4-1.5;
			float TheScale = 0.5+ReflectionsScale*rnd(9*count);
			Reflection1_0 = Move2D(Reflection1, 0, 0, atan2d(PosY-height/2,PosX-width/2), 1, TheScale, TheScale, 0, 0, width/2, height/2);
			Reflection1_1 = Move2D(Reflection1_0, (xPos-background.width/2)+(xPos-PosX)*Position, (yPos-background.height/2)+(yPos-PosY)*Position, 0, 1, 1, Ratio>0?0.5:1, 0, 0, width/2, height/2);
			Reflection1_2 = Fade(Reflection1_1,((Brightness>1.0)?1.0:Brightness)*ReflectionsOpacity*(0.5+0.5*rnd(10*count)));
			Reflection1_3 = Blur(Reflection1_2,rnd(4*count)*20,xPixels);
			Reflections   = IAdd(Reflections,Reflection1_3,0,100);
		}
	}
	else
		Reflections = BlackBackground;

	Final1 = IAdd(RaysFinal,Halo,1,100);
	Final  = IAdd(Reflections,Final1,1,100);

	if (UseAlpha == 1.0)
	{
		HideCoef = WarpX(AlphaInput, 1, PosX, PosY, width, height);
		HideFinal = LayerX(HideCoef,Final,(1-a)*r2,(1-a)*g2,(1-a)*b2,(1-a)*a2,z);
	}
	else
		HideFinal = Final;

	FinalNode = AE_Layers(HideFinal,background,0,TransferMode);

	return Mix(background,FinalNode,1,Blend,"rgba");
}

image	SimpleFlare(
	image	background,
	image	AlphaInput,
	float	PosX=width/2,		float	PosY=height/2,
	float	xPos=width/2,		float	yPos=height/2,
	int	UseAlpha=0,
	string	TransferMode="Screen",
	float	Brightness=1.0,
	float	Blend=100.0,
	float	Quality=1.0,
	int	Ratio=0,
	float	rColor=0.95,		float gColor=0.8,		float bColor=1.0,
	int	HaloEnable=1,		float HaloScale=1.0,		float HaloOpacity=1.0,
	int	RaysEnable=1,		int   NbRays=8,			float RaysScale=1.0,		float RaysOpacity=1.0,		float RaysBlur=15.0,
	int	ReflectionsEnable=1,	float ReflectionsScale=1.0,	float ReflectionsOpacity=1.0,
	int	RedReflectionEnable=1,	float RedReflectionScale=1.0,	float RedReflectionOpacity=1.0)
{
	BlackBackground = Black(background.width,background.height);

	if (HaloEnable == 1.0)
	{
		Arc1 = AE_Arc(background.width,background.height, 0.08, 0, 0, 0, 360, width/7, 5, PosX, PosY, 100);
		Arc2 = Scale(Arc1,1,0.66,PosX,PosY);
		RGrad1 = RGrad(background.width,background.height, 1, PosX, PosY, 1, 0, Brightness*width/3, 0.5, rColor, gColor, bColor, 1, 0, 0, 0, 0, 0, 0);
		RGrad2 = RGrad(background.width,background.height, 1, PosX, PosY, 1, Brightness*2, Brightness*width/3, 0.02, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0);
		Blur1 = Blur(Arc2, 10, xPixels, 0, "gauss", xFilter, "rgba");
		IAdd1 = IAdd(RGrad1, RGrad2, 1, 100);
		Halo = Fade(Scale(IAdd(IAdd1, Blur1, 1, 100),HaloScale,((Ratio>0)?0.5:1)*HaloScale,PosX,PosY),((Brightness>1.0)?1.0:Brightness)*HaloOpacity);
	}
	else
		Halo = BlackBackground;

	if (RaysEnable == 1.0)
	{
		Rays = NGLRender(
			background.width,
			(Ratio>0?2:1)*background.height,
			1,
			"
			    nglPushMatrix();
			    float Radius = 1.5*Brightness*RaysScale*background.width/5;
			    for(i=0;i<NbRays;i++)
			    {
			    	float TheLength = (turbulence(2*i,1)+0.5)*Radius;
				nglBegin(NGL_LINES);
				nglColor4f(1,1,1,1);
				nglVertex2f( PosX , (Ratio>0?2:1)*PosY );
				nglColor4f(0,0,0,0);
				nglVertex2f( PosX + TheLength*cosd(i*360/NbRays) , (Ratio>0?2:1)*PosY + TheLength*sind(i*360/NbRays));
				nglEnd();
			    }
			    nglPopMatrix();
			"
	    	);
		if (Ratio>0)
			Rays = Resize(Rays,background.width,background.height);
		RaysBlur1 = Blur(Rays, RaysBlur, xPixels, 0, "gauss", xFilter, "rgba");
		RaysFade  = Fade(RaysBlur1,((Brightness>1.0)?1.0:Brightness)*RaysOpacity);
		RaysFinal = Move2D(RaysFade,0,0,Linear(2,0@1,(90/NbRays)@2),1,1,1,0,0,PosX,PosY,"default",xFilter,"trsx",0,1,1,0,0,time);
	}
	else
		RaysFinal = BlackBackground;

	if (ReflectionsEnable == 1.0)
	{
		Reflections = BlackBackground;
		Reflection1 = AE_Arc(background.width,background.height, rColor,gColor,bColor, 0, 360, 0, 15, width/2, height/2, 5);
		for (int count=0;count<Quality*10;count++)
		{
			float Position = turbulence(7*count,1)*2-0.5;
			float TheScale = 0.5+ReflectionsScale*rnd(9*count);
			Reflection1_0 = Move2D(Reflection1, 0, 0, atan2d(PosY-height/2,PosX-width/2), 1, TheScale, TheScale, 0, 0, width/2, height/2);
			Reflection1_1 = Move2D(Reflection1_0, (xPos-background.width/2)+(xPos-PosX)*Position, (yPos-background.height/2)+(yPos-PosY)*Position, 0, 1, 1, Ratio>0?0.5:1, 0, 0, width/2, height/2);
			Reflection1_2 = Fade(Reflection1_1,((Brightness>1.0)?1:Brightness)*ReflectionsOpacity*(0.5+0.5*rnd(10*count)));
			Reflection1_3 = Blur(Reflection1_2,rnd(4*count)*20,xPixels);
			Reflections   = IAdd(Reflections,Reflection1_3,0,100);
		}
	}
	else
		Reflections = BlackBackground;

	if (RedReflectionEnable == 1.0)
	{
		RedRing1 = AE_Arc(background.width,background.height, 0.08, 0, 0, 0, 360, width/3.5, 5, xPos-0.6*(PosX-xPos), yPos-0.6*(PosY-yPos), 100);
		RedRing = Fade(Scale(RedRing1,RedReflectionScale,(Ratio>0?0.5:1)*0.66*RedReflectionScale,xPos-0.6*(PosX-xPos), yPos-0.6*(PosY-yPos)),((Brightness>1.0)?1:Brightness)*RedReflectionOpacity);
	}
	else
		RedRing = BlackBackground;

	Final1 = IAdd(RaysFinal,Halo);
	Final2 = IAdd(Reflections,Final1);
	Final  = IAdd(RedRing,Final2);

	if (UseAlpha == 1.0)
	{
		HideCoef = WarpX(AlphaInput, 1, PosX, PosY, width, height);
		HideFinal = LayerX(HideCoef,Final,(1-a)*r2,(1-a)*g2,(1-a)*b2,(1-a)*a2,z);
	}
	else
		HideFinal = Final;

	FinalNode = AE_Layers(HideFinal,background,0,TransferMode);

	return Mix(background,FinalNode,1,Blend,"rgba");
}

image	Flare50300mm(
	image	background,
	image	AlphaInput,
	float	PosX=width/2,		float	PosY=height/2,
	float	xPos=width/2,		float	yPos=height/2,
	int	UseAlpha=0,
	string	TransferMode="Screen",
	float	Brightness=1.0,
	float	Blend=100.0,
	int	Ratio=0,
	int	HaloEnable=1,		float HaloScale=1.0,		float HaloOpacity=1.0,
	int	RaysEnable=1,		int   NbRays=25,		float RaysScale=1.0,		float RaysOpacity=0.25,		float RaysBlur=5.0,
	int	ReflectionsEnable=1,	float ReflectionsScale=1.0,	float ReflectionsOpacity=1.0,	float ReflectionsBlur=25.0)
{
	BlackBackground = Black(background.width,background.height);

	if (HaloEnable == 1.0)
	{
		Arc1 = AE_Arc(background.width,background.height, 0.2, 0.0, 0.0, 0, 360, width/10, 5, PosX, PosY, 100);
		RGrad1 = RGrad(background.width,background.height, 1, PosX, PosY, 1,  0, Brightness*width/5, 0.5, 0.3, 0.05, 0.05, 1, 0, 0, 0, 0, 0, 0);
		RGrad2 = RGrad(background.width,background.height, 1, PosX, PosY, 1, 15, Brightness*width/2, 0.02, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0);
		Blur1 = Blur(Arc1, 10, xPixels, 0, "gauss", xFilter, "rgba");
		IAdd1 = IAdd(RGrad1, RGrad2, 1, 100);
		Halo = Fade(Scale(IAdd(IAdd1, Blur1, 1, 100),HaloScale,((Ratio>0)?0.5:1)*xScale,PosX,PosY),((Brightness>1.0)?1.0:Brightness)*HaloOpacity);
	}
	else
		Halo = BlackBackground;

	if (RaysEnable == 1.0)
	{
		Rays = NGLRender(
			background.width,
			(Ratio>0?2:1)*background.height,
			1,
			"
			    nglPushMatrix();
			    float Radius = Brightness*RaysScale*background.width/5;
			    for(i=0;i<NbRays;i++)
			    {
			    	float TheLength = (turbulence(2*i,1)+0.5)*Radius;
			    	float TheAngle  = turbulence(7*i,1)*360.0;
				nglBegin(NGL_LINES);
				nglColor4f(1,1,1,1);
				nglVertex2f( PosX , (Ratio>0?2:1)*PosY );
				nglColor4f(0,0,0,0);
				nglVertex2f( 	PosX + TheLength*cosd(TheAngle) ,
						PosY*(Ratio>0?2:1) + TheLength*sind(TheAngle));
				nglEnd();
			    }
			    nglPopMatrix();
			"
	    	);
		if (Ratio>0)
			Rays = Resize(Rays,background.width,background.height);
		RaysBlur1 = Blur(Rays, RaysBlur, xPixels, 0, "gauss", xFilter, "rgba");
		RaysFinal = Fade(RaysBlur1,((Brightness>1.0)?1.0:Brightness)*RaysOpacity);
	}
	else
		RaysFinal = BlackBackground;

	Reflections = BlackBackground;
	if (ReflectionsEnable == 1.0)
	{
		Reflection1 = RGrad(background.width,background.height, 1, width/2, height/2, 1, width/25, 0, 0.5, 0.05, 0.1, 0.2, 0.3, 0, 0, 0, 0, 0, 0);
		if (Ratio>0)
			Reflection1 = Scale(Reflection1,ReflectionsScale,0.5*xScale);
		else
			Reflection1 = Scale(Reflection1,ReflectionsScale,xScale);
		Reflection1 = Blur(Reflection1,ReflectionsBlur,xPixels);
		Reflections = IAdd(Reflections,Pan(Reflection1,                     (xPos-background.width/2)+0.33*(PosX-xPos),(yPos-background.height/2)+0.33*(PosY-yPos)));
		Reflections = IAdd(Reflections,Pan(Scale(Reflection1,0.5,0.5),      (xPos-background.width/2)+0.30*(PosX-xPos),(yPos-background.height/2)+0.30*(PosY-yPos)));
		Reflections = IAdd(Reflections,Pan(Scale(Reflection1,0.3,0.3),      (xPos-background.width/2)+0.36*(PosX-xPos),(yPos-background.height/2)+0.36*(PosY-yPos)));

		Reflection2 = RGrad(background.width,background.height, 1, width/2, height/2, 1, width/35, 0, 0.5, 0.4, 0.25, 0.07, 0.3, 0, 0, 0, 0, 0, 0);
		if (Ratio>0)
			Reflection2 = Scale(Reflection2,ReflectionsScale,0.5*xScale);
		else
			Reflection2 = Scale(Reflection2,ReflectionsScale,xScale);
		Reflection2 = Blur(Reflection2,ReflectionsBlur,xPixels);
		Reflections = IAdd(Reflections,Pan(Scale(Reflection2,0.5,0.5),      (xPos-background.width/2)+0.15*(PosX-xPos),(yPos-background.height/2)+0.15*(PosY-yPos)));
		Reflections = IAdd(Reflections,Pan(Reflection2,                     (xPos-background.width/2)-0.30*(PosX-xPos),(yPos-background.height/2)-0.30*(PosY-yPos)));
		Reflections = IAdd(Reflections,Pan(Scale(Reflection2,0.4,0.4),      (xPos-background.width/2)-0.35*(PosX-xPos),(yPos-background.height/2)-0.35*(PosY-yPos)));

		Reflection3 = RGrad(background.width,background.height, 1, width/2, height/2, 1, height/2-25, 25, 0.5, 0.05, 0.02, 0.005, 0.1, 0, 0, 0, 0, 0, 0);
		if (Ratio>0)
			Reflection3 = Scale(Reflection3,ReflectionsScale,0.5*xScale);
		else
			Reflection3 = Scale(Reflection3,ReflectionsScale,xScale);
		Reflections = IAdd(Reflections,Pan(Reflection3,		      	    (xPos-background.width/2)+1.5*(PosX-xPos),(yPos-background.height/2)+1.5*(PosY-yPos)));

		Reflection4Arc1 = AE_Arc(background.width,background.height, 0.53, 0.313222, 0.0228312463, 0, 360, width/10, 3, width/2, height/2, 100);
		Reflection4RGrad1 = RGrad(background.width,background.height, 1, width/2, height/2, 1, width/20, width/20, 0.5, 0, 0, 0, 0, 0, 0.05732328, 0.19, 0.001430378, 0.5, 0);
		Reflection4RGrad2 = RGrad(background.width,background.height, 1, width/2, height/2, 1, width/10, 0, 0.5, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0);
		Reflection4IMult1 = IMult(Reflection4RGrad1, Reflection4RGrad2, 1, 100, 0);
		Reflection4IAdd1 = IAdd(Reflection4Arc1, Reflection4IMult1, 1, 100);
		Reflection4Blur1 = Blur(Reflection4IAdd1, ReflectionsBlur, xPixels);
		if (Ratio>0)
			Reflection4Blur1 = Scale(Reflection4Blur1,ReflectionsScale,0.5*xScale);
		else
			Reflection4Blur1 = Scale(Reflection4Blur1,ReflectionsScale,xScale);
		Reflections = IAdd(Reflections,Pan(Reflection4Blur1,                (xPos-background.width/2)-1.0*(PosX-xPos),(yPos-background.height/2)-1.0*(PosY-yPos)));
		Reflections = IAdd(Reflections,Pan(Scale(Reflection4Blur1,0.3,0.3), (xPos-background.width/2)+1.2*(PosX-xPos),(yPos-background.height/2)+1.2*(PosY-yPos)));

		Reflection5Arc1 = AE_Arc(background.width,background.height, 0.13, 0.0755620152, 0.0113505106, 0, 360, width/5, width/100, width/2, height/2, 100);
		Reflection5Blur1 = Blur(Reflection5Arc1, ReflectionsBlur, xPixels);
		if (Ratio>0)
			Reflection5Blur1 = Scale(Reflection5Blur1,ReflectionsScale,0.5*xScale);
		else
			Reflection5Blur1 = Scale(Reflection5Blur1,ReflectionsScale,xScale);
		Reflections = IAdd(Reflections,Pan(Reflection5Blur1, 		    (xPos-background.width/2)-2.0*(PosX-xPos),(yPos-background.height/2)-2.0*(PosY-yPos)));

		Reflection6RGrad1 = RGrad(background.width,background.height, 1, width/2, height/2, 1, 0, width/70, 0.5, 0.5, 0.5, 1, 1, 0, 0, 0, 0, 0, 0);
		Reflection6Blur1 = Blur(Reflection6RGrad1, ReflectionsBlur/5, xPixels);
		if (Ratio>0)
			Reflection6Blur1 = Scale(Reflection6Blur1,ReflectionsScale,0.5*xScale);
		else
			Reflection6Blur1 = Scale(Reflection6Blur1,ReflectionsScale,xScale);
		Reflections = IAdd(Reflections,Pan(Reflection6Blur1, 		    (xPos-background.width/2)-0.8*(PosX-xPos),(yPos-background.height/2)-0.8*(PosY-yPos)));
		Reflections = IAdd(Reflections,Pan(Scale(Reflection6Blur1,0.6,0.6), (xPos-background.width/2)-0.4*(PosX-xPos),(yPos-background.height/2)-0.4*(PosY-yPos)));
		Reflections = IAdd(Reflections,Pan(Scale(Reflection6Blur1,0.3,0.3), (xPos-background.width/2)+0.3*(PosX-xPos),(yPos-background.height/2)+0.3*(PosY-yPos)));

		Reflections = Fade(Reflections,((Brightness>1.0)?1.0:Brightness)*ReflectionsOpacity);
	}

	Final1 = IAdd(RaysFinal,Halo);
	Final  = IAdd(Reflections,Final1);

	if (UseAlpha == 1.0)
	{
		HideCoef = WarpX(AlphaInput, 1, PosX, PosY, width, height);
		HideFinal = LayerX(HideCoef,Final,(1-a)*r2,(1-a)*g2,(1-a)*b2,(1-a)*a2,z);
	}
	else
		HideFinal = Final;

	FinalNode = AE_Layers(HideFinal,background,0,TransferMode);

	return Mix(background,FinalNode,1,Blend,"rgba");
}

image	Flare105mm(
	image	background,
	image	AlphaInput,
	float	PosX=width/2,		float	PosY=height/2,
	float	xPos=width/2,		float	yPos=height/2,
	int	UseAlpha=0,
	string	TransferMode="Screen",
	float	Brightness=1.0,
	float	Blend=100.0,
	int	Ratio=0,
	int	HaloEnable=1,		float HaloScale=1.0,		float HaloOpacity=1.0,
	int	RaysEnable=1,		int   NbRays=20,		float RaysScale=1.0,		float RaysOpacity=0.25,		float RaysBlur=5.0,
	int	ReflectionsEnable=1,	float ReflectionsScale=1.0,	float ReflectionsOpacity=1.0,	float ReflectionsBlur=25.0)
{
	BlackBackground = Black(background.width,background.height);

	if (HaloEnable == 1.0)
	{
		Arc1 = AE_Arc(background.width,background.height, 0.1, 0.0165318921, 0.010797984, 0, 360, width/10, width/150, PosX, PosY, 100);
		RGrad1 = RGrad(background.width,background.height, 1, PosX, PosY, 1, 0, height/2, 0.5, 0.358, 0.358, 0.358, 1, 0, 0, 0, 0, 0, 0);
		RGrad2 = RGrad(background.width,background.height, 1, PosX, PosY, 1, 0, width/5, 0.2, 0.255449057, 0.454222977, 0.68, 1, 0, 0, 0, 0, 0, 0);
		RGrad3 = RGrad(background.width,background.height, 1, PosX, PosY, 1, 0, width/10, 0.2, 0.68, 0.670383334, 0.677595854, 1, 0, 0, 0, 0, 0, 0);
		Grain1 = Grain(RGrad1, 1, 0.1, time, -1, 0.5, 0.5, rGain, rGain, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
		IAdd1 = IAdd(Grain1, RGrad2, 1, 100);
		Mask(Grain1, RGrad1, "A", 100, 0, 1);
		IAdd2 = Scale(IAdd(IAdd1, RGrad3, 1, 100),Brightness*HaloScale,xScale,PosX,PosY);
		IAdd3 = IAdd(IAdd2, Arc1, 1, 100);
		Halo = Fade(Scale(IAdd3,1,(Ratio>0?0.5:1),PosX,PosY),((Brightness>1.0)?1.0:Brightness)*HaloOpacity);
	}
	else
		Halo = BlackBackground;

	if (RaysEnable == 1.0)
	{
		Rays = NGLRender(
			background.width,
			(Ratio>0?2:1)*background.height,
			1,
			"
			    nglPushMatrix();
			    float Radius = Brightness*RaysScale*background.width/5;
			    for(i=0;i<NbRays;i++)
			    {
			    	float TheLength = (turbulence(2*i,1)+0.5)*Radius;
			    	float TheAngle  = turbulence(7*i,1)*360.0;
				nglBegin(NGL_LINES);
				nglColor4f(1,1,1,1);
				nglVertex2f( PosX , (Ratio>0?2:1)*PosY );
				nglColor4f(0,0,0,0);
				nglVertex2f( 	PosX + TheLength*cosd(TheAngle) ,
						PosY*(Ratio>0?2:1) + TheLength*sind(TheAngle));
				nglEnd();
			    }
			    nglPopMatrix();
			"
	    	);
		if (Ratio>0)
			Rays = Resize(Rays,background.width,background.height);
		RaysBlur1 = Blur(Rays, RaysBlur, xPixels, 0, "gauss", xFilter, "rgba");
		RaysFinal = Fade(RaysBlur1,((Brightness>1.0)?1.0:Brightness)*RaysOpacity);
	}
	else
		RaysFinal = BlackBackground;

	Reflections = BlackBackground;
	if (ReflectionsEnable == 1.0)
	{
		Reflection1 = RGrad(background.width,background.height, 1, width/2, height/2, 1, width/20, width/100, 0.5, 0.04, 0.1, 0.15, 0.2, 0, 0, 0, 0, 0, 0);
		Reflection1 = Fade(Scale(Reflection1,ReflectionsScale,xScale),((Brightness>1.0)?1.0:Brightness)*ReflectionsOpacity);
		Reflections = IAdd(Reflections,Pan(Blur(Scale(Reflection1,0.5,0.5*(Ratio>0?0.5:1)),ReflectionsBlur,xPixels),  (xPos-background.width/2)+0.60*(PosX-xPos),(yPos-background.height/2)+0.60*(PosY-yPos)));
		Reflections = IAdd(Reflections,Pan(Blur(Scale(Reflection1,0.7,0.7*(Ratio>0?0.5:1)),ReflectionsBlur,xPixels),  (xPos-background.width/2)+0.20*(PosX-xPos),(yPos-background.height/2)+0.20*(PosY-yPos)));
		Reflections = IAdd(Reflections,Pan(Blur(Scale(Reflection1,0.1,0.1*(Ratio>0?0.5:1)),ReflectionsBlur,xPixels),  (xPos-background.width/2)-1.20*(PosX-xPos),(yPos-background.height/2)-1.20*(PosY-yPos)));
		Reflections = IAdd(Reflections,Pan(Blur(Scale(Reflection1,0.1,0.1*(Ratio>0?0.5:1)),ReflectionsBlur,xPixels),  (xPos-background.width/2)-1.30*(PosX-xPos),(yPos-background.height/2)-1.30*(PosY-yPos)));
		Reflections = IAdd(Reflections,Pan(Blur(Scale(Reflection1,0.1,0.1*(Ratio>0?0.5:1)),ReflectionsBlur,xPixels),  (xPos-background.width/2)+2.00*(PosX-xPos),(yPos-background.height/2)+2.00*(PosY-yPos)));
		Reflections = IAdd(Reflections,Pan(Blur(Scale(Reflection1,0.05,0.05*(Ratio>0?0.5:1)),ReflectionsBlur,xPixels),(xPos-background.width/2)+0.05*(PosX-xPos),(yPos-background.height/2)+0.05*(PosY-yPos)));
		Reflections = IAdd(Reflections,Pan(Blur(Scale(Reflection1,0.3,0.3*(Ratio>0?0.5:1)),ReflectionsBlur,xPixels),  (xPos-background.width/2)-0.60*(PosX-xPos),(yPos-background.height/2)-0.60*(PosY-yPos)));
		Reflections = IAdd(Reflections,Pan(Blur(Scale(Reflection1,0.4,0.4*(Ratio>0?0.5:1)),ReflectionsBlur,xPixels),  (xPos-background.width/2)-0.70*(PosX-xPos),(yPos-background.height/2)-0.70*(PosY-yPos)));

		Reflection2 = RGrad(background.width,background.height, 1, width/2, height/2, 1, height/2-height/50, height/50, 0.5, 0.05, 0.02, 0.02, 0.1, 0, 0, 0, 0, 0, 0);
		Reflection2 = Fade(Scale(Reflection2,ReflectionsScale,xScale),((Brightness>1.0)?1.0:Brightness)*ReflectionsOpacity);
		Reflections = IAdd(Reflections,Pan(Blur(Scale(Reflection2,1.0,1.0*(Ratio>0?0.5:1)),ReflectionsBlur,xPixels),  (xPos-background.width/2)+2.00*(PosX-xPos),(yPos-background.height/2)+2.00*(PosY-yPos)));

		Reflection3 = RGrad(background.width,background.height, 1, width/2, height/2, 1, height/2-height/40, height/40, 0.25, 0.125, 0.1, 0.1, 0.1, 0, 0, 0, 0, 0, 0);
		Reflection3 = Fade(Scale(Reflection3,ReflectionsScale,xScale),((Brightness>1.0)?1.0:Brightness)*ReflectionsOpacity);
		Reflections = IAdd(Reflections,Pan(Blur(Scale(Reflection3,0.5,0.5*(Ratio>0?0.5:1)),8*ReflectionsBlur,xPixels),(xPos-background.width/2)-0.50*(PosX-xPos),(yPos-background.height/2)-0.50*(PosY-yPos)));
		Reflections = IAdd(Reflections,Pan(Blur(Scale(Reflection3,0.2,0.2*(Ratio>0?0.5:1)),8*ReflectionsBlur,xPixels),(xPos-background.width/2)-0.80*(PosX-xPos),(yPos-background.height/2)-0.80*(PosY-yPos)));

		Reflection4 = RGrad(background.width,background.height, 1, width/2, height/2, 1, 0, width/100, 0.05, 0.8, 0.8, 1, 0.2, 0, 0, 0, 0, 0, 0);
		Reflection4 = Fade(Scale(Reflection4,ReflectionsScale,xScale),((Brightness>1.0)?1.0:Brightness)*ReflectionsOpacity);
		Reflections = IAdd(Reflections,Pan(Blur(Scale(Reflection4,1,1*(Ratio>0?0.5:1)),ReflectionsBlur,xPixels),      (xPos-background.width/2),                   (yPos-background.height/2)));
		Reflections = IAdd(Reflections,Pan(Blur(Scale(Reflection4,1,1*(Ratio>0?0.5:1)),ReflectionsBlur,xPixels),      (xPos-background.width/2)-1.25*(PosX-xPos),(yPos-background.height/2)-1.25*(PosY-yPos)));
		Reflections = IAdd(Reflections,Pan(Blur(Scale(Reflection4,1,1*(Ratio>0?0.5:1)),ReflectionsBlur,xPixels),      (xPos-background.width/2)+2.00*(PosX-xPos),(yPos-background.height/2)+2.00*(PosY-yPos)));
	}
	else
		Reflections = BlackBackground;

	Final1 = IAdd(RaysFinal,Halo);
	Final  = IAdd(Reflections,Final1);

	if (UseAlpha == 1.0)
	{
		HideCoef = WarpX(AlphaInput, 1, PosX, PosY, width, height);
		HideFinal = LayerX(HideCoef,Final,(1-a)*r2,(1-a)*g2,(1-a)*b2,(1-a)*a2,z);
	}
	else
		HideFinal = Final;

	FinalNode = AE_Layers(HideFinal,background,0,TransferMode);

	return Mix(background,FinalNode,1,Blend,"rgba");
}




image AE_Layers(image Foreground, image Background, image BackgroundColor,string mode)
{
	if (mode == "Over")
		return AE_Layer_Over(Foreground, Background);
	else if (mode == "Normal")
		return AE_Layer_Over(Foreground, Background);
	else if (mode == "Mult")
		return AE_Layer_Mult(Foreground, Background);
	else if (mode == "Add")
		return AE_Layer_Add(Foreground, Background);
	else if (mode == "Screen")
		return AE_Layer_Screen(Foreground, Background);
	else if (mode == "Sub")
		return AE_Layer_Sub(Foreground, Background);
	else if (mode == "Xor")
		return AE_Layer_Xor(Foreground, Background);
	else if (mode == "AddAlpha")
		return AE_Layer_AddAlpha(Foreground, Background);
	else if (mode == "Premult")
		return AE_Layer_Premult(Foreground, Background);
	else if (mode == "Dissolve")
		return AE_Layer_Dissolve(Foreground, Background);
	else if (mode == "Darken")
		return AE_Layer_Darken(Foreground, Background);
	else if (mode == "ColorBurn")
		return AE_Layer_ColorBurn(Foreground, Background);
	else if (mode == "Lighten")
		return AE_Layer_Lighten(Foreground, Background);
	else if (mode == "ColorDodge")
		return AE_Layer_ColorDodge(Foreground, Background);
	else if (mode == "Overlay")
		return AE_Layer_Overlay(Foreground, Background);
	else if (mode == "SoftLight")
		return AE_Layer_Softlight(Foreground, Background);
	else if (mode == "HardLight")
		return AE_Layer_HardLight(Foreground, Background);
	else if (mode == "LinearLight")
		return AE_Layer_LinearLight(Foreground, Background);
	else if (mode == "IntenseLight")
		return AE_Layer_IntenseLight(Foreground, Background);
	else if (mode == "Hue")
		return AE_Layer_Hue(Foreground, Background);
	else if (mode == "Saturation")
		return AE_Layer_Saturation(Foreground, Background);
	else if (mode == "Color")
		return AE_Layer_Color(Foreground, Background);
	else if (mode == "Luminosity")
		return AE_Layer_Luminosity(Foreground, Background);
	else if (mode == "ModelAlpha")
		return AE_Layer_ModelAlpha(Foreground, Background, BackgroundColor);
	else if (mode == "LuminanceMode")
		return AE_Layer_LuminanceModel(Foreground, Background);
	else if (mode == "NotAlpha")
		return AE_Layer_NotAlpha(Foreground, Background, BackgroundColor);
	else if (mode == "NotLuminance")
		return AE_Layer_NotLuminance(Foreground, Background);
	else if (mode == "Freeze")
		return AE_Layer_Freeze(Foreground, Background);
	else if (mode == "Heat")
		return AE_Layer_Heat(Foreground, Background);
	else if (mode == "Substractive")
		return AE_Layer_Substractive(Foreground, Background);
	else if (mode == "Interpolation")
		return AE_Layer_Interpolation(Foreground, Background);
	return Background;
}

image AE_Layer_Over(image	Foreground,image Background)
{
	return Over(Foreground,Background);
}

image AE_Layer_Mult(image	Foreground,image Background)
{
	return IMult(Foreground,Background,1,100,0);
}

image AE_Layer_Add(image	Foreground,image Background)
{
	return IAdd(Foreground,Background,1,100);
}

image AE_Layer_Screen(image	Foreground,image Background)
{
	return Screen(Foreground,Background,1);
}

image AE_Layer_Sub(image	Foreground,image Background)
{
	return ISubA(Foreground,Background,1,100);
}

image AE_Layer_Xor(image	Foreground,image Background)
{
	return Xor(Foreground,Background,1,0);
}

image AE_Layer_AddAlpha(image	Foreground,image Background)
{
	return Over(Foreground,Background,1,0,1);
}

image AE_Layer_Premult(image	Foreground,image Background)
{
	return Over(Foreground,Background,1,1,0);
}

image AE_Layer_Saturation(image	Foreground,image Background)
{
	ColorSpace1 = ColorSpace(Foreground, "rgb", "hls", 0.3, 0.59, 0.11);
	ColorSpace2 = ColorSpace(Background, "rgb", "hls", 0.3, 0.59, 0.11);
	Copy1 = Copy(ColorSpace1, ColorSpace2, 1, "rg");
	ColorSpace3 = ColorSpace(Copy1, "hls", "rgb", 0.3, 0.59, 0.11);
	Over1 = Over(ColorSpace3, Background, 1, 0, 0);
	return Over1;
}

image AE_Layer_Color(image Foreground,image Background)
{
	ColorSpace1 = ColorSpace(Foreground, "rgb", "hls", 0.3, 0.59, 0.11);
	ColorSpace2 = ColorSpace(Background, "rgb", "hls", 0.3, 0.59, 0.11);
	Copy1 = Copy(ColorSpace1, ColorSpace2, 1, "g");
	ColorSpace3 = ColorSpace(Copy1, "hls", "rgb", 0.3, 0.59, 0.11);
	Over1 = Over(ColorSpace3, Background, 1, 0, 0);
	return Over1;
}

image AE_Layer_Luminosity(image Foreground,image Background)
{
	ColorSpace1 = ColorSpace(Foreground, "rgb", "hls", 0.3, 0.59, 0.11);
	ColorSpace2 = ColorSpace(Background, "rgb", "hls", 0.3, 0.59, 0.11);
	Copy1 = Copy(ColorSpace1, ColorSpace2, 1, "rb");
	ColorSpace3 = ColorSpace(Copy1, "hls", "rgb", 0.3, 0.59, 0.11);
	Over1 = Over(ColorSpace3, Background, 1, 0, 0);
	return Over1;
}

image AE_Layer_Hue(image Foreground,image Background)
{
	ColorSpace1 = ColorSpace(Background, "rgb", "hls", 0.3, 0.59, 0.11);
	ColorSpace2 = ColorSpace(Foreground, "rgb", "hls", 0.3, 0.59, 0.11);
	Copy1 = Copy(ColorSpace1, ColorSpace2, 1, "r");
	ColorSpace3 = ColorSpace(Copy1, "hls", "rgb", 0.3, 0.59, 0.11);
	return ColorSpace3;
}

image AE_Layer_Lighten(image Foreground,image Background)
{
	Max1 = Max(Foreground,Background,1,100);
	return Max1;
}

image AE_Layer_Darken(image Foreground,image Background)
{
	Min1 = Min(Foreground,Background,1,100);
	return Min1;
}

image AE_Layer_Dissolve(image Foreground,image Background)
{
	Rand1 = Rand(Background.width, Background.height, 1, 0.4, time);
	LayerX1 = LayerX(Rand1, Foreground, r2, g2, b2, a2-a*(1-a2), z);
	Over1 = Over(LayerX1, Background, 1, 0, 0);
	return Over1;
}

image AE_Layer_ModelAlpha(image Foreground,image Background,image BackgroundColor)
{
	LayerX1 = LayerX(Foreground, Background, r2, g2, b2, a*a2, z);
	Over1 = Over(LayerX1, BackgroundColor, 1, 1, 0);
	return Over1;
}

image AE_Layer_LuminanceModel(image Foreground,image Background)
{
	ColorSpace1 = ColorSpace(Foreground, "rgb", "hls", 0.3, 0.59, 0.11);
	LayerX1 = LayerX(ColorSpace1, Background, g*r2, g*g2, g*b2, g*a2, z);
	return LayerX1;
}

image AE_Layer_NotAlpha(image Foreground,image Background,image BackgroundColor)
{
	LayerX1 = LayerX(Foreground, Background, r2, g2, b2, (1-a)*a2, z);
	Over1 = Over(LayerX1, BackgroundColor, 1, 1, 0);
	return Over1;
}

image AE_Layer_NotLuminance(image Foreground,image Background)
{
	ColorSpace1 = ColorSpace(Foreground, "rgb", "hls", 0.3, 0.59, 0.11);
	LayerX1 = LayerX(ColorSpace1, Background, (1.0-g)*r2, (1.0-g)*g2, (1.0-g)*b2, (1.0-g)*a2, z);
	return LayerX1;
}

image AE_Layer_Overlay(image Foreground,image Background)
{
	LayerX1 = LayerX(	Foreground, Background,
				r2<0.5?2*r*r2:1.0-2.0*(1-r)*(1-r2),
				g2<0.5?2*g*g2:1.0-2.0*(1-g)*(1-g2),
				b2<0.5?2*b*b2:1.0-2.0*(1-b)*(1-b2),
				a2, z);
	return LayerX1;
}

image AE_Layer_ColorBurn(image Foreground,image Background)
{
	LayerX1 = LayerX(	Foreground, Background,
				r==0?0:1.0-(1-r2)/r,
				g==0?0:1.0-(1-g2)/g,
				b==0?0:1.0-(1-b2)/b,
				a2, z);
	return LayerX1;
}


image AE_Layer_ColorDodge(image Foreground,image Background)
{
	LayerX1 = LayerX(	Foreground, Background,
				r==1?1:r2/(1-r),
				g==1?1:g2/(1-g),
				b==1?1:b2/(1-b),
    				a2, z);
    	return LayerX1;
}

image AE_Layer_HardLight(image Foreground,image Background)
{
	LayerX1 = LayerX(	Foreground, Background,
				r<0.5?2*r*r2:1.0-2.0*(1-r)*(1-r2),
				g<0.5?2*g*g2:1.0-2.0*(1-g)*(1-g2),
				b<0.5?2*b*b2:1.0-2.0*(1-b)*(1-b2),
				a2, z);
	return LayerX1;
}

image AE_Layer_Softlight(image Foreground,image Background)
{
	LayerX1 = LayerX(	Foreground, Background,
				2*r*r2+r2*r2-2*r2*r2*r,
				2*g*g2+g2*g2-2*g2*g2*g,
	    			2*b*b2+b2*b2-2*b2*b2*b,
	    			a2, z);
	return LayerX1;
}

image AE_Layer_LinearLight(image Foreground,image Background)
{
	// Lumiere vive
	LayerX1 = LayerX(	Foreground, Background,
				r2==1?1:r*r/(1-r2),
				g2==1?1:g*g/(1-g2),
    				b2==1?1:b*b/(1-b2),
    				a, z);
    	return LayerX1;
}

image AE_Layer_Freeze(image Foreground,image Background)
{
	LayerX1 = LayerX(	Foreground, Background,
				r==0?0:1.0-(1.0-r2)*(1.0-r2)/r,
				g==0?0:1.0-(1.0-g2)*(1.0-g2)/g,
    				b==0?0:1.0-(1.0-b2)*(1.0-b2)/b,
    				a, z);
    	return LayerX1;
}

image AE_Layer_Heat(image Foreground,image Background)
{
	LayerX1 = LayerX(	Foreground, Background,
				r2==0?0:1.0-(1.0-r)*(1.0-r)/r2,
				g2==0?0:1.0-(1.0-g)*(1.0-g)/g2,
    				b2==0?0:1.0-(1.0-b)*(1.0-b)/b2,
    				a, z);
    	return LayerX1;
}

image AE_Layer_Substractive(image Foreground,image Background)
{
	LayerX1 = LayerX(	Foreground, Background,
				r+r2-1,
				g+g2-1,
    				b+b2-1,
    				a, z);
    	return LayerX1;
}

image AE_Layer_Interpolation(image Foreground,image Background)
{
	LayerX1 = LayerX(	Foreground, Background,
				0.5-0.25*cos(M_PI*r2)-0.25*cos(M_PI*r),
				0.5-0.25*cos(M_PI*g2)-0.25*cos(M_PI*g),
    				0.5-0.25*cos(M_PI*b2)-0.25*cos(M_PI*b),
    				a, z);
    	return LayerX1;
}

image AE_Layer_IntenseLight(image Foreground,image Background)
{
	LayerX1 = LayerX(	Foreground, Background,
				2*r+r2-1,
				2*g+g2-1,
    				2*b+b2-1,
    				a, z);
    	return LayerX1;
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
