// version 1.5 	 11/13/2006
// history:
// version: 1.1 (30/03/2006) - added ShadowWrap
// version: 1.2 (04/20/2006) - added lightSpread
// version: 1.5 (11/13/2006) - fixed lightspread to spread RGB of BG Edge. added matteOutput options. modified BgIntensity to only work as a gamma node between 0-1.
// written by jan cilliers
// jan@enlightenedfx.com
// http://www.enlightenedfx.com
// DO NOT DISTRIBUTE WITHOUT WRITTEN AUTHORIZATION FROM THE AUTHOR

image DLightwrap(
image Foreground=0,
image Background=0,
int clipMode=0,
int ViewMode=3,
float InnerEdge_Blur=20,
float OuterEdge_Blur=10,
float OuterEdge_Gain=2,
float Edge_Gain=1,
float Overall_Gain=1,
float Overall_Blur=0,
float Background_Intensity=1,
float lightSpread=0,
float ShadowWrap_Mix=0,
int preMultiply=0,
int MatteOutput=0
)
{
    Background = Reorder(Background, "");
    Foreground = Reorder(Foreground, "");
    BackgroundResize1 = Viewport(Foreground, 0, 0, Background.width, 
        Background.height);
    BackgroundResize2 = Viewport(Background, 0, 0, Background.width, 
        Background.height);
    ForegroundResize1 = Viewport(Foreground, 0, 0, Foreground.width, 
        Foreground.height);
    ForegroundResize2 = Viewport(Background, 0, 0, Foreground.width, 
        Foreground.height);
    ClipMode = Select(clipMode+1, BackgroundResize1, ForegroundResize1, 
        0);
    ClipMode1 = Select(ClipMode.branch, BackgroundResize2, ForegroundResize2, 
        0);
    AlphaStrip = Reorder(ClipMode1, "rgbn");
    Blue = Reorder(ClipMode1, "bbbb");
    Green = Reorder(ClipMode1, "gggg");
    MultFG = MMult(ClipMode, 0);
    Over1 = Over(ClipMode, ClipMode1, 1, 0, 0);
    Red = Reorder(ClipMode1, "rrrr");
    backgroundSpread_clone1 = Blur(ClipMode1, backgroundSpread.xPixels, 
        backgroundSpread.yPixels, backgroundSpread.spread,  backgroundSpread.xFilter, 
         backgroundSpread.yFilter,  backgroundSpread.channels);
    MultMatteSwitch = Select(preMultiply+1, ClipMode, MultFG, 0);
    RMaxG = Max(Red, Green, 1, 100);
    Alpha_In = Reorder(MultMatteSwitch, "aaaa");
    FGovrBG = Over(MultMatteSwitch, AlphaStrip, 1, 0, 0);
    RGMaxB = Max(RMaxG, Blue, 1, 100);
    Brightness = Brightness(RGMaxB, rValue>1?rValue:1, float rValue = Background_Intensity);
    FG_Alpha = Reorder(FGovrBG, "aaaa");
    innerEdgeBlur = Blur(Alpha_In, InnerEdge_Blur, xPixels/GetDefaultAspect(), 
        0, "gauss", xFilter, "a");
    outerEdgeBlur = Blur(Alpha_In, OuterEdge_Blur, xPixels/GetDefaultAspect(), 
        0, "gauss", xFilter, "a");
    Gamma = Gamma(Brightness, Brightness.rValue, Brightness.rValue, 
        Brightness.rValue, 1);
    outerEdgeGain = Mult(outerEdgeBlur, 1, 1, 1, OuterEdge_Gain, 
        1);
    Outside = Outside(outerEdgeGain, innerEdgeBlur, 1);
    backgroundSpread = Blur(Gamma, lightSpread, xPixels/GetDefaultAspect(), 
        0, "gauss", xFilter, "rgba");
    EdgeGain = Mult(Outside, 1, 1, 1, Edge_Gain, 1);
    makeFGalpha = Reorder(backgroundSpread, "rgbl");
    postBlur = Blur(EdgeGain, Overall_Blur, xPixels/GetDefaultAspect(), 
        0, "gauss", xFilter, "a");
    overallGain = Mult(postBlur, 1, 1, 1, Overall_Gain, 1);
    MatteEdge_Out = Reorder(overallGain, "aaaa");
    Mult = IMult(MatteEdge_Out, makeFGalpha, 1, 100, 0);
    MixDarks = Mix(Mult, MatteEdge_Out, 1, ShadowWrap_Mix, "rgba");
    SwitchMatte = SwitchMatte(backgroundSpread_clone1, MixDarks, 
        1, "A", 1, 0);
    EDGEovrBG = Over(SwitchMatte, FGovrBG, 1, 0, 0);
    EdgeAlpha = Reorder(SwitchMatte, "aaaa");
    Outside1 = Outside(FG_Alpha, EdgeAlpha, 1);
    FGLW_MATTE = Copy(EDGEovrBG, Outside1, 1, "a", 0);
    Over2 = Over(Outside1, ClipMode1, 1, 0, 0);
    CurrentView = Select(ViewMode+1, MatteEdge_Out, makeFGalpha, FGovrBG, 
        FGLW_MATTE, 0);
    FGLW_OVERBG_MATTE = Copy(CurrentView, Over2, 1, "a", 0);
    LWGB_OVERFG_MATTE = Copy(CurrentView, EDGEovrBG, 1, "a", 0);
    OverMatte = Copy(CurrentView, Over1, 1, "a", 0);
    MatteOutput = Select(MatteOutput+1, CurrentView, FGLW_OVERBG_MATTE, 
        OverMatte, LWGB_OVERFG_MATTE, 0);
    Result = Reorder(MatteOutput, "");
    
    return Result;
}

