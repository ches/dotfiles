/*
ParanoiaFX | VolumetricLight 1.1
(c) 2006, Stefan Thamm
kontakt@paranoiafx.com
*/

image VolumetricLight(
image In=0,
float LightCenterX = 0, 
float LightCenterY = 0,
float RaysLength=0.25,
float red=1,
float green=1,
float blue=1,
float RaysBrightness=1,
float RaysSmoothness=10,
float EdgeStrength=1,
float EdgeThreshold=0.1,
string Show="result",
int QualityLevel=3
)
{
	
	if (Show=="result") {
		ShowTranslator=2
	}
	if (Show=="edges") {
		ShowTranslator=1
	}
    Input = Gamma(In, 1, rGamma, rGamma, 1);
    DownscaleImage = Resize(Input, width/QualityLevel, height/QualityLevel, "sinc", 1);
    EdgeDetect = EdgeDetect(DownscaleImage, EdgeStrength, EdgeThreshold, 
        0, 0, 0, 0, 0, xBlur/GetDefaultAspect(), 0, xDilateErode/GetDefaultAspect(), 
        0, 0, "Sobel", 8, 1);
    RBlur = RBlur(EdgeDetect, LightCenterX/QualityLevel, LightCenterY/QualityLevel, 0, width, 
        1, 1, RaysLength, 1, 0, 0);
    UpscaleImage2 = Resize(EdgeDetect, width*QualityLevel, height*QualityLevel, "mitchell", 
        1);
    Brightness = Brightness(RBlur, RaysBrightness);
    Mult = Mult(Brightness, red, green, blue, 1, 1);
    UpscaleImage = Resize(Mult, width*QualityLevel, height*QualityLevel, "mitchell", 1);
    Blur = Blur(UpscaleImage, RaysSmoothness, xPixels/GetDefaultAspect(), 
        0, "gauss", xFilter, "rgba");
    IAdd = IAdd(Blur, Input, 1, 100);
    Output = Select(ShowTranslator, UpscaleImage2, IAdd, 0);
    
    return Output;
}

