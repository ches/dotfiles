/*  This file downloaded from Highend3d.com
''  
''  Highend3d.com File Information:
''  
''    Script Name: Particle v1.1
''    Author: Florian Schroeder
''    Last Updated: September 8, 2004
''    Update/Change this file at:
''    http://www.highend3d.com/shake/macros/?section=image#3072
''  
''  Please do not alter any information above this line
''  it is generated dynamically by Highend3d.com and will
''  be changed automatically on any updates.
*/

image Particle(
image BG=0, 
image Source=0,

float left=GetDefaultWidth()/2-25,
float right=GetDefaultWidth()/2+25,
float bottom=GetDefaultHeight()/2-25,
float top=GetDefaultHeight()/2+25,
float Direction=45,
int Start=1,
int Stop=500,
float Gravity=5,
int Seed=0,

string Preset="Balls",
int Number=10,
int Life=25,
float Speed=10,
float Spread=45,
float Size=30,
float Rotation=1,
float Fuzz=0,

float GrowSmooth=0.75,
float MotionSmooth=0.75,
float FadeSmooth=0.75,
int MotionBlur=0,
string Text="Contact:\nflorianschr@yahoo.de"

)
{	
// Exposee

	// Menue Translation
	menue = 2;
	if (Preset=="Custom Input"){menue=1;} 
	else if (Preset=="Balls"){menue=2;}
	else if (Preset=="Stars"){menue=3;}
	else if (Preset=="Text"){menue=4;}	

	// Particle	Presets

	// Stranger
		Black = Color(max(Source.width,Source.height), width, 1, 0, red, red, 0, 0);
		Pan = Pan(Source, Source.width<Source.height?(Source.height-Source.width)/2:0, 
    				Source.width>Source.height?(Source.width-Source.height)/2:0, 0, 0.5, 0);
		Place = Over(Pan, Black, 1, 0, 0);		
		Stranger = Resize(Place, Size, Size, "default", 0);

	// Balls
		Balls = RGrad(Size, Size, 1, width/2, height/2, 1, min(width,height)/10, 
			min(width,height)/4, 0.5, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0);
	
	// Star
		StarColor = Color(Size*2, Size/4, 1, 1, 1, 1, 1, 0);
		Randomize = Randomize(StarColor, 1, 0, Size/10, xAmplitude, 0, 0);
		BGStarColor = Color(Size*2,width, 1, 0, 0, 0, 0, 0);
		M1 = Move2D(Randomize, 0, 0, 90, 1, 1, xScale, 0, 0, width/2, height/2, 
		    "default", xFilter, "trsx", 0, 0, 0.5, 0, 0, time);
		O1 = Over(Randomize, M1, 1, 0, 0);
		M2 = Move2D(O1, 0, 0, 45, 1, 0.6, 0.7, 0, 0, width/2, height/2, 
		    "default", xFilter, "trsx", 0, 0, 0.5, 0, 0, time);
		O2 = Over(O1, M2, 1, 0, 0);
		M3 = Move2D(O2, 0, Size-(Size/8), 0, 1, 0.6, 0.7, 0, 0, width/2, height/2, 
		    "default", xFilter, "trsx", 0, 0, 0.5, 0, 0, time);
		Star = Over(M3, BGStarColor, 1, 0, 0);

	// Text
		Text = Text(Size, width, 1, "{Text}", 
		    "Verdana", Size*0.5, xFontScale, 1, width/2, height/2, 0, 2, 2, 
		    1, 1, 1, 1, 0, 0, 0, 45, 0, 1);

	// Select
		Select = Select(menue, Stranger, Balls, Star, Text);

	// Variables
	Result = BG;
	EmitterDuration = Stop-Start+1;
	curve float EmitterWidth = abs(right-left+1);
	curve float EmitterHeight = abs(top-bottom+1);
	curve float ParticleCenterX = (left+right)/2-Size/2;
	curve float ParticleCenterY = (bottom+top)/2-Size/2;

// Loop 

	for (int i=1;i<=Number;++i){

		// chaos variables
		StartChaos = i==1?0:noise(i+Seed)*(Life<=EmitterDuration?Life:EmitterDuration);
		LifeChaos = i==1?0:noise((i+Seed)*Life)*Life/3;
		SpeedChaos = i==1?0:noise((i+Seed)*Speed)*Speed/2;
		SizeChaos = i==1?1:1-(noise(i+Seed)/2);
		FuzzChaos = noise(i+Seed)*Fuzz;
		GravityChaos = noise((i+Seed)*Gravity)*Gravity/2;			
		RotationChaos = i==1?0:(noise(i+Seed)-0.5)*Rotation*50;		
		
		// more variables
		plife = Life-LifeChaos;
		pgrow = plife*GrowSmooth/2;
		pspeed = Speed-SpeedChaos;
		motionstart = Start+StartChaos;
		motionmid =	motionstart+plife/3;
		motionend = motionstart+plife;
		gravitymid = motionstart+plife/1.5;	
		gravity = -(Gravity-GravityChaos);
		fadesmooth = plife*FadeSmooth/2;
		fuzz = FuzzChaos*sin(time+motionstart);
	
		// curves  ..sort of
		eactivation = Step(0, 0@0, 2@motionstart, 0@motionend+(plife*(floor((Stop-motionstart)/plife))));
		repeat = Step(4,0@motionstart,30@motionend);
		age = Step(4,0@motionstart,1@motionend);

		// even more variables
		SpreadChaos = noise((i+Seed+repeat)*6)*Spread-(Spread/2);
		StartxPosChaos = i+repeat==1?0:(noise((i+Seed+repeat)*2)*EmitterWidth)-(EmitterWidth/2);
		StartyPosChaos = i+repeat==1?0:(noise((i+Seed+repeat)*4)*EmitterHeight)-(EmitterHeight/2);		

		// some results
		MasterMotion = CSpline(eactivation,0@motionstart,plife*pspeed/3+plife*pspeed/3*MotionSmooth@motionmid,plife*pspeed@motionend);
		MasterAngle = Direction+SpreadChaos+fuzz,
		MasterGravity = CSpline(eactivation,0@motionstart,plife*gravity/3+plife*gravity/3*MotionSmooth@gravitymid,plife*gravity@motionend);
		MasterX = ParticleCenterX@@motionstart+(age*plife);
		MasterY = ParticleCenterY@@motionstart+(age*plife);

		// Destiny
		Move2D1 = Move2D(Select, (cosd(MasterAngle)*MasterMotion)+MasterX+StartxPosChaos, 
					(sind(MasterAngle)*MasterMotion)+MasterY+StartyPosChaos+MasterGravity, 
					SpreadChaos+Linear(1,0@motionstart,RotationChaos@motionstart+1), 1, 
					JSpline(eactivation,0@motionstart,SizeChaos@motionstart+pgrow/2,SizeChaos@motionend-pgrow,0@motionend), 
					xScale, 0, 0, width/2, height/2, "default", xFilter, "trsx", 0, MotionBlur, 0.5, 0, 0, time);
		Fade1 = Fade(Move2D1, JSpline(eactivation,0@motionstart,1@motionstart+fadesmooth/2,1@motionend-fadesmooth-1,0@motionend));

		// Comp
		Result = Over(Fade1, Result);

	}

// Return

	return Result;
}

