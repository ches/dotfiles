/*  This file downloaded from Highend3d.com
''  
''  Highend3d.com File Information:
''  
''    Script Name: FLGlow3 v3.0
''    Author: Francois Lord
''    Last Updated: June 10, 2002
''    Update/Change this file at:
''    http://www.highend3d.com/shake/macros/?section=filters#1780
''  
''  Please do not alter any information above this line
''  it is generated dynamically by Highend3d.com and will
''  be changed automatically on any updates.
*/

image FLGlow3(
image In=0,
image InMatte=0,
float Spread=20,
float GlowFade=1,
float Glow_Lo=0,
float Glow_Hi=1,
float red=1,
float green=1,
float blue=1,
const char * MatteChannel="N",
int Burn=0,
float BurnFade=1,
float Burn_Lo=0.8,
float Burn_Hi=1.2,
float Aspect=defaultAspect
)
{
    SwitchMatte1 = SwitchMatte(In, InMatte, 1, MatteChannel, 1, 0);
    ClipHiLo = LumaKey(SwitchMatte1, Glow_Lo, Glow_Hi, 0, 0, 
        1);
    Bytes1 = Bytes(ClipHiLo, Burn==1 ? 4 : bytes == 4 ? 4 : 2);    /* If Burn is active, 32bits. Elsif input bytes == 4, 32 bits. Else 16 bits. */
    LargeBlur = Blur(Bytes1, Spread * 3, Spread * 3 * Aspect, 0, 
        "gauss", "gauss", "rgba");
    SmallBlur = Blur(Bytes1, Spread, Spread * Aspect, 0, "gauss", 
        "gauss", "rgba");
    IAdd1 = IAdd(SmallBlur, LargeBlur, 1, 100);
    Color = Mult(IAdd1, red, green, blue, 1, 1);
    CompositeGlow = IAdd(In, Color, 1, GlowFade * 100);
    BurnDetect = LumaKey(CompositeGlow , Burn_Lo, Burn_Hi, 0, 0, 1);
    BurnCreate = Monochrome(BurnDetect, 0.3, 0.59, 0.11);
    BurnClamp = ColorX(BurnCreate, clamp(r,0,10000000), clamp(g,0,10000000), 
        clamp(b,0,10000000), clamp(a,0,1), z);
    BurnFade = Fade(BurnClamp, BurnFade);
    Over1 = Over(BurnFade, CompositeGlow , 1, 0, 0);
    Burn_on_off = Select(Burn==0?1:2, CompositeGlow , Over1, 0, 0);
    SwitchMatte1 = SwitchMatte(Burn_on_off, In, 1, "A", 0, 0);
    Bytes2 = Bytes(SwitchMatte1, ClipHiLo.bytes);
    
    return Bytes2;
}
