// FilmLook v1.0.1
// by Christoph Hasche
// Copyright 2008

// www.drago-design.com


image FilmLook(
image In=0,
string nfo="FilmLook v1.0.1",
float exposure=0.5,
float highlightValue=0.4,
float gammaValue=0.4,
float saturation=0.6,
float highlights=1.75,
float gamma=0.9,
float contrast=1.2,
float contrastCenter=0.1,
float contrastSoften=1,
float contrastR=1,
float compressR_Hi=1,
float compressR_Lo=0,
float gammaR=1,
float contrastG=1,
float compressG_Hi=1,
float compressG_Lo=0,
float gammaG=1,
float contrastB=1,
float compressB_Hi=1,
float compressB_Lo=0,
float gammaB=1,
float overallSaturation=1.30000007
)
{
	GammaExpose = Gamma(In, GammaExpose_rGamma, GammaExpose_rGamma, GammaExpose_rGamma, 1);

MultExpose = Mult(GammaExpose, expValue, expValue, expValue, 1, 1);

    float GammaExpose_rGamma= exposure <1 ? (exposure+gammaValue)-(exposure*gammaValue) : 1+(exposure*(gammaValue/4));

    float expValue= exposure <1 ? (exposure+highlightValue)-(exposure*highlightValue) : exposure;

     Saturation1 = Saturation(MultExpose, saturation);
    Brightness1 = Brightness(Saturation1, highlights);
    Gamma1 = Gamma(Brightness1, gamma, gamma, gamma, 1);
    ContrastLum7 = ContrastLum(Gamma1, contrast, contrastCenter, 
        contrastSoften);
    B = Reorder(ContrastLum7, "bbba");
    G = Reorder(ContrastLum7, "ggga");
    R = Reorder(ContrastLum7, "rrra");
    ContrasB = ContrastRGB(B, contrastB, contrastB, contrastB, 1, 
        0.5, rCenter, rCenter, 0.5, 0, rSoftClip, rSoftClip, 0);
    ContrastG = ContrastRGB(G, contrastG, contrastG, contrastG, 1, 
        0.5, rCenter, rCenter, 0.5, 0, rSoftClip, rSoftClip, 0);
    ContrastR = ContrastRGB(R, contrastR, contrastR, contrastR, 1, 
        0.5, rCenter, rCenter, 0.5, 0, rSoftClip, rSoftClip, 0);
    CompressB = Compress(ContrasB, compressB_Lo, compressB_Lo, compressB_Lo, 0, compressB_Hi, compressB_Hi, compressB_Hi, 
        1);
    CompressG = Compress(ContrastG, compressG_Lo, compressG_Lo, compressG_Lo, 0, compressG_Hi, compressG_Hi, compressG_Hi, 
        1);
    CompressR = Compress(ContrastR, compressR_Lo, compressR_Lo, compressR_Lo, 0, compressR_Hi, 
        compressR_Hi, compressR_Hi, 1);
    GammaB = Gamma(CompressB, gammaB, gammaB, gammaB, 1);
    GammaG = Gamma(CompressG, gammaG, gammaG, gammaG, 1);
    GammaR = Gamma(CompressR, gammaR, gammaR, gammaR, 1);
    CopyR = Copy(ContrastLum7, GammaR, 1, channels="r");
    CopyG = Copy(CopyR, GammaG, 1, channels="g");
    CopyB = Copy(CopyG, GammaB, 1, channels="b");
    overallSaturation = Saturation(CopyB, overallSaturation);
    
    return overallSaturation;
}

