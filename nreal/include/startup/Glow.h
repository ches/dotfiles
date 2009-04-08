image Glow(
image In=0,
image Background=0,
float glowSize=20,
float glowBrightness=1,
float glowLoVal=0,
float glowHiVal=1
)
{
    LumaKey1 = LumaKey(In, glowLoVal, glowHiVal, 0, 0, 1);
    Blur1 = Blur(LumaKey1, glowSize, xPixels/GetDefaultAspect(), 
        0, "gauss", xFilter, "rgba");
    Brightness1 = Brightness(Blur1, glowBrightness);
    Screen1 = Screen(Brightness1, Background, 1);
    
    return Screen1;
}

