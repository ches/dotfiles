/* PxF_ScreenClean 1.0 - (c) 2007 - PixelFudger
   Contact: Xavier Bourque - xbourque@gmail.com

Description:
	Yet another screen correction tool.

Used for:
	Cleaning uneven green/blue/red screens using a clean plate.

Controls:
	ScreenColor:
		The color towards which the screen will be corrected. Usually a color close to the average
		color of the green/blue/red screen.
	ScreenType:
		Select the type of screen (red, green or blue).

*/


image PxF_ScreenClean(
image Foreground=0,
image CleanPlate=0,
float ScreenColorR=0.1,
float ScreenColorG=0.7,
float ScreenColorB=0.1,
char *ScreenType="green",
const char *version = "PxF_ScreenClean 1.0 - (c) 2007 - Xavier Bourque - www.pixelfudger.com"

)
{
    
    Color1 = Color(Foreground.width, Foreground.height, 1, ScreenColorR, ScreenColorG, ScreenColorB, 1, 0);
    
    ISub3 = ISub(CleanPlate, Color1, 1, 100);
    Clamp1 = Clamp(ISub3, 0, rLo, rLo, 0, 1, rHi, rHi, 1);
    ISub4 = ISub(Color1, CleanPlate, 1, 100);
    Clamp2 = Clamp(ISub4, 0, rLo, rLo, 0, 1, rHi, rHi, 1);
    
    if (ScreenType == "red") { 
        cpHi = Reorder(CleanPlate, "rrr");
        cpLo1 = Reorder(CleanPlate, "ggg");
        cpLo2 = Reorder(CleanPlate, "bbb");
        frtHi = Reorder(Foreground, "rrr");
        frtLo1 = Reorder(Foreground, "ggg");
        frtLo2 = Reorder(Foreground, "bbb");
    }
    else if (ScreenType == "green") {
        cpHi = Reorder(CleanPlate, "ggg");
        cpLo1 = Reorder(CleanPlate, "bbb");
        cpLo2 = Reorder(CleanPlate, "rrr");
        frtHi = Reorder(Foreground, "ggg");
        frtLo1 = Reorder(Foreground, "bbb");
        frtLo2 = Reorder(Foreground, "rrr");
    }
    else if (ScreenType == "blue") {
        cpHi = Reorder(CleanPlate, "bbb");
        cpLo1 = Reorder(CleanPlate, "ggg");
        cpLo2 = Reorder(CleanPlate, "rrr");
        frtHi = Reorder(Foreground, "bbb");
        frtLo1 = Reorder(Foreground, "ggg");
        frtLo2 = Reorder(Foreground, "rrr");
    }
    
    Max1 = Max(frtLo1, frtLo2, 1, 100);
    Max2 = Max(cpLo1, cpLo2, 1, 100);
    ISub1 = ISub(frtHi, Max1, 1, 100);
    ISub2 = ISub(cpHi, Max2, 1, 100);
    IDiv1 = IDiv(ISub1, ISub2, 1, 100, 1);
    Clamp3 = Clamp(IDiv1, 0, rLo, rLo, 0, 1, rHi, rHi, 1);
    IMult1 = IMult(Clamp3, Clamp1, 1, 100, 0);
    IMult2 = IMult(Clamp3, Clamp2, 1, 100, 0);
    IAdd1 = IAdd(Foreground, IMult2, 1, 100);
    ISub5 = ISub(IAdd1, IMult1, 1, 100);
    
    return ISub5;
}




