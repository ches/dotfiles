image CleanHDV(
image In=0,
image highs_luma_In=0,
image lows_luma_In=0,
float BlurX=2,
float BlurY=xPixels/GetDefaultAspect(),
int analyseGrain=0,
int temporalDegrain=0,
int showRemovedGrain=0,
float medianThreshold=0.4,
float highsLoVal=0.243,
float highsHiVal=0.3,
float highsLoSmooth=0.062,
float highsHiSmooth=0.6,
float lowsLoVal=0.083,
float lowsHiVal=0.2,
float lowsLoSmooth=0.1,
float lowsHiSmooth=0.1
)
{
    Blur1 = Blur(In, BlurX, BlurY, 0, "gauss", xFilter, "rgba");
    highs_luma = LumaKey(highs_luma_In, highsLoVal, highsHiVal, highsLoSmooth, 
        highsHiSmooth, 0);
    lows_luma = LumaKey(lows_luma_In, lowsLoVal, lowsHiVal, lowsLoSmooth, 
        lowsHiSmooth, 0);
    Invert1 = Invert(lows_luma, "rgba");
    Mask(Blur1, highs_luma, "A", 100, 0, 1, 1);
    Median1 = Median(Blur1, "rgba", medianThreshold, 0);
    F_DeGrain1 = F_DeGrain(Median1, analyseGrain, temporalDegrain, 
        0.8, 1.3, 1, 1, "all", showRemovedGrain, 10, 1, 1, 1, 1, 
        0, 0.05, 0.5, 5, 58, 50, 10, 4, 0.00366903446, 0.00337243173, 
        0.00160919467, 0.003192056, 0.0011102556, 0.000801014947, 
        0.00111247622, 0.000458247319, 0.00372534874, 0.00434176251, 
        0.00166176306, 0.00383174419, 0.00111136236, 0.0007286583, 
        0.001107654, 0.00039243768, 0.0037277427, 0.004396759, 0.00166182406, 
        0.00391386868, 0.00111128192, 0.000721032266, 0.00110621389, 
        0.000414379931);
    Mask(Median1, Invert1, "A", 100, 0, 1, 1);
    
    return F_DeGrain1;
}

