//----------------------------------
// Collection of Shake Macros
// to mimic AE effects
//
// (c) Francois Sugny 2004
// Email : francois.sugny@wanadoo.fr
//----------------------------------

nuiPushToolBox("AE"); 
    // -- Adjust
    nuiToolBoxItem("AE_BrightnessContrast", AE_BrightnessContrast(0,1,1));
    nuiToolBoxItem("AE_ChannelMixer", AE_ChannelMixer(0, 100,0,0,0, 0,100,0,0, 0,0,100,0, 0));
    nuiToolBoxItem("AE_Color_Balance", AE_Color_Balance(0,0,1.0,1.0));
    nuiToolBoxItem("AE_HueSaturation", AE_HueSaturation(0, "Master", 0,0,0, 0,0,0,0));
    nuiToolBoxItem("AE_EasyLevels", AE_EasyLevels(0, 0,255, 1, 0,255, 0,255, 1, 0,255, 0,255, 1, 0,255, 0,255, 1, 0,255, 0,255, 1, 0,255));
    nuiToolBoxItem("AE_Posterize", AE_Posterize(0,32));
    nuiToolBoxItem("AE_Threshold", AE_Threshold(0,0));
    // -- Blur & Sharpen
    nuiToolBoxItem("AE_ChannelBlur", AE_ChannelBlur(0,0,0,0,0,"Both"));
    nuiToolBoxItem("AE_CompoundBlur", AE_CompoundBlur(0,0,0,0,0));
    nuiToolBoxItem("AE_MotionBlur", AE_MotionBlur(0,0,0));
    nuiToolBoxItem("AE_FastBlur", AE_FastBlur(0,0,"Both"));
    nuiToolBoxItem("AE_GaussianBlur", AE_GaussianBlur(0,0,"Both"));
    nuiToolBoxItem("AE_RBlur", AE_RBlur(0,0,width/2,height/2,"Zoom","High"));
    nuiToolBoxItem("AE_Sharpen", AE_Sharpen(0,0));
    nuiToolBoxItem("AE_UnsharpMask", AE_UnsharpMask(0,0,0,0));
    // -- Channel
    nuiToolBoxItem("AE_3DGlasses",AE_3DGlasses(0,0,0,0,"Red Green LR",0.0,"Left View"));
    nuiToolBoxItem("AE_AlphaLevels2", AE_AlphaLevels2(0, 0,255, 1, 0,255));
    nuiToolBoxItem("AE_Arithmetic", AE_Arithmetic(0,"And",0,0,0));
    nuiToolBoxItem("AE_Blend", AE_Blend(0,0, "Fade", 0, "Adapt"));
    nuiToolBoxItem("AE_Cineon_Convert", AE_Cineon_Convert(0,"LogToLin",95,0,832,255,1.7));
    nuiToolBoxItem("AE_CompoundArithmetic", AE_CompoundArithmetic(0,0, "Copy","rgb","Cut",1,0));
    nuiToolBoxItem("AE_Invert", AE_Invert(0,"RGB",0));
    nuiToolBoxItem("AE_Minimax", AE_Minimax(0, "Maximum", 0, "rgba", "Horizontal and Vertical"));
    nuiToolBoxItem("AE_SetChannels", AE_SetChannels(0, "r", 0, "g", 0, "b", 0, "a"));
    nuiToolBoxItem("AE_SetMatte2", AE_SetMatte2(0, 0, "a", 0, 0, 0, 1));
    nuiToolBoxItem("AE_ShiftChannels", AE_ShiftChannels(0,"r","g","b","a"));
    nuiToolBoxItem("AE_SolidComposite",AE_SolidComposite(0,100.0,1,1,1,0,"Normal"));
    // -- Distort
    nuiToolBoxItem("AE_CornerPin", AE_CornerPin(0,0,0,width,0,width,height,0,height));
    nuiToolBoxItem("AE_Bulge", AE_Bulge(0,min(width,height)/4,min(width,height)/4,width/2,height/2,3.0));
    nuiToolBoxItem("AE_DisplacementMap", AE_DisplacementMap(0,0, "r",0, "g",0, "Adapt",0));
    nuiToolBoxItem("AE_Mirror", AE_Mirror(0,width/2,height/2,0));
    nuiToolBoxItem("AE_Scroll", AE_Scroll(0,0,0,0));
    nuiToolBoxItem("AE_PolarCoordinates", AE_PolarCoordinates(0, 100,"RectToPolar"));
    nuiToolBoxItem("AE_Ripple", AE_Ripple(0, 20, width/2,height/2, "Symmetric", 1.0,20.0,20.0, 0));
    nuiToolBoxItem("AE_Spherize", AE_Spherize(0,min(width,height)/4,width/2,height/2));
    nuiToolBoxItem("AE_Geometry2", AE_Geometry2(0, width/2,height/2, width/2,height/2, 1,100,100, 0,0, 0,100, 0,0));
    nuiToolBoxItem("AE_Twirl", AE_Twirl(0,0,width/2,height/2,125));
    nuiToolBoxItem("AE_ProgressiveWave", AE_ProgressiveWave(0,10,100,90,0,"Sinus",1));
    nuiToolBoxItem("AE_OpticsCompensation", AE_OpticsCompensation(0, 0,0,"Horizontal", width/2,height/2, 0,"Off"));
    nuiToolBoxItem("AE_BezierMesh", AE_BezierMesh(0,   0,height,width/3,height,2*width/3,height,   width,height,width,height-height/3,width,height-2*height/3,   width,0,2*width/3,0,width/3,0,   0,0,0,height/3,0,2*height/3));
    nuiToolBoxItem("AE_Warp", AE_Warp(0,"Arc","Horizontal",100,0,0));
    nuiToolBoxItem("AE_Magnify", AE_Magnify(0));
    // -- Image Control
    nuiToolBoxItem("AE_Equalize", AE_Equalize(0, 100));
    nuiToolBoxItem("AE_Gamma", AE_Gamma(0,1,0,1,1,0,1,1,0,1));
    nuiToolBoxItem("AE_Tint", AE_Tint(0, 0,0,0, 1,1,1, 100));
    nuiToolBoxItem("AE_Colorama", AE_Colorama(0,"Intensity", 0,0,"Intensity","Average",0, "All",1,0, 1,0,0,0.5,0.0,"Off", 0,"Off", 1,0));
    nuiToolBoxItem("AE_ChangeColor", AE_ChangeColor(0));
    // -- Keying
    nuiToolBoxItem("AE_DifferenceMatte", AE_DifferenceMatte(0,0, "Center",20,0,0));
    nuiToolBoxItem("AE_ColorKey", AE_ColorKey(0, 0,1,0, 0,0,0));
    nuiToolBoxItem("AE_ColorRange", AE_ColorRange(0, 20, "Lab", 0,255, 0,255, 0,255));
    nuiToolBoxItem("AE_Extract", AE_Extract(0,"Luminance",0,255,0,0,0));
    nuiToolBoxItem("AE_LinearColorKey", AE_LinearColorKey(0, 0,0,0, "RGB",20,0,"Mask"));
    nuiToolBoxItem("AE_LumaKey", AE_LumaKey(0,"Bright Pixels",0,0,0,0));
    // -- Matte Tools
    nuiToolBoxItem("AE_MatteChoker", AE_MatteChoker(0, 0,0,0, 0,0,0, 1));
    nuiToolBoxItem("AE_SimpleChoker", AE_SimpleChoker(0, "Final Output", 0));
    // -- Noise
    nuiToolBoxItem("AE_NoiseAlpha2", AE_NoiseAlpha2(0));
    nuiToolBoxItem("AE_FractalNoise", AE_FractalNoise(0,"Basic","Spline",0, 100,0,"Clamp", 6, 0,100,"Normal"));
    nuiToolBoxItem("AE_Median", AE_Median(0,1,0));
    nuiToolBoxItem("AE_Noise", AE_Noise(0, 0, 1, 1));
    nuiToolBoxItem("AE_NoiseHLS", AE_NoiseHLS(0));
    // -- Perspective
    nuiToolBoxItem("AE_Basic3D", AE_Basic3D(0,0,0,0));
    nuiToolBoxItem("AE_BevelAlpha", AE_BevelAlpha(0,10,0,1,1,1,1));
    nuiToolBoxItem("AE_BevelEdges", AE_BevelEdges(0,0.1,0,1,1,1,1));
    nuiToolBoxItem("AE_DropShadow", AE_DropShadow(0,0,0,0,60,0,40,60,0));
    // -- Render
    nuiToolBoxItem("AE_Ramp4Colors", AE_Ramp4Colors(0, 0,0,1,0,0, width,0,0,1,0, width,height,0,0,1, 0,height,0,0,0, "Normal", 0));
    nuiToolBoxItem("AE_Ellipse", AE_Ellipse(0, width/2,height/2, 100,100,20, 1, 1,1,1, 1,0,0, 1));
    nuiToolBoxItem("AE_Circle", AE_Circle(0));
    nuiToolBoxItem("AE_Fill", AE_Fill(0,0, 0, 1,1,1, 0, 0,0, 100.0));
    nuiToolBoxItem("AE_Fractal", AE_Fractal(0,"Mandelbrot"));
    nuiToolBoxItem("AE_Grid", AE_Grid(0, width/2,height/2, "Corner", width/2+10,height/2+10, 0,0, 5,0, 1,1,1, 100, "None"));
    nuiToolBoxItem("AE_LensFlare", AE_LensFlare(0, width/2,height/2, 100,"50-300mm (zoom)",0));
    nuiToolBoxItem("AE_RadioWaves", AE_RadioWaves(0,0, width/2,height/2, 1,"Polygon", 10,0,0, width/2,height/2,"a",0,0,0,0,0, 1,5,0,0,0,0,5,0, 1,1,1,1,1,1,1,5));
    nuiToolBoxItem("AE_Ramp", AE_Ramp(0,720,486,width*0.33,height*0.33,0,0,0,width*0.66,height*0.66,1,1,1,"Linear",0));
    nuiToolBoxItem("AE_Stroke", AE_Stroke(0,0, 1,1,1, 10,100,100, "On Original Image"));
    nuiToolBoxItem("AE_Laser", AE_Laser(0, width/4,height/2, 3*width/4,height/2, 50,50, 5,5, 1,0,0, 1));
    nuiToolBoxItem("AE_CheckerBoard", AE_CheckerBoard(0));
    nuiToolBoxItem("AE_EyeDropFill", AE_EyeDropFill(0));
    // -- Simulation
    nuiToolBoxItem("AE_ParticlePlayground", AE_ParticlePlayground(0, width/2,height/2,0, 60,0,20, 100,20, 1,0,0, 32, 0,"Relative",0,"None", 120,0,180));
    // -- Stylize
    nuiToolBoxItem("AE_ColorEmboss", AE_ColorEmboss(0,0,1,0,0));
    nuiToolBoxItem("AE_Emboss", AE_Emboss(0,0,128,0));
    nuiToolBoxItem("AE_EdgeDetect", AE_EdgeDetect(0,0,0));
    nuiToolBoxItem("AE_Mosaic", AE_Mosaic(0,width,height));
    nuiToolBoxItem("AE_Tile", AE_Tile(0, width/2,height/2, 100,100, 100,100, 0,0,0));
    nuiToolBoxItem("AE_RoughenEdges", AE_RoughenEdges(0, "Roughen", 0.57,0.32,0.1, 200,1,1,100,0, 0,0, 2,0,0,1,0));
    nuiToolBoxItem("AE_Scatter", AE_Scatter(0,0,"Both",1));
    nuiToolBoxItem("AE_Strobe", AE_Strobe(0,1,1,1,0,25,50,"Colors","Copy"));
    nuiToolBoxItem("AE_Texturize", AE_Texturize(0,0,0,1.0,"Center"));
    nuiToolBoxItem("AE_Glow", AE_Glow(0, "Color Channels", 60,10,4, "On Top","Add", "Horizontal And Vertical"));
    nuiToolBoxItem("AE_WriteOn", AE_WriteOn(0, Hermite(0,[width/2,0,0]@1),Hermite(0,[height/2,0,0]@1), 1,1,1, 25,79,100, 1.0,0.25, "On Transparent"));
    // -- Text
    nuiToolBoxItem("AE_Numbers", AE_Numbers(0, "Number", 0,0.0,2, 0, width/2,height/2, "Fill Only", 1,1,1, 0,0,0, 3,100,0,1));
    nuiToolBoxItem("AE_Text", AE_Text(0,"Text","Arial",width/2,height/2,"Fill Only",1,1,1,1,0,0,3,100,0,1,"Center","Horizontal"));
    nuiToolBoxItem("AE_PathText", AE_PathText(0,"Text","Line", width/4+50,height/2,width/4,height/2, 3*width/4-50,height/2,3*width/4,height/2, 0,   "Fill Only",1,1,1,1,0,0,3,   100,0, 1));
    // -- Transitions
    nuiToolBoxItem("AE_BlockDissolve", AE_BlockDissolve(0,0,10,10,0));
    nuiToolBoxItem("AE_GradientWipe", AE_GradientWipe(0,0, 0,0,"Adjust",0));
    nuiToolBoxItem("AE_IrisWipe", AE_IrisWipe(0, width/2,height/2, 24, 0,0,0,0,0));
    nuiToolBoxItem("AE_LinearWipe", AE_LinearWipe(0,0,0,0));
    nuiToolBoxItem("AE_RadialWipe", AE_RadialWipe(0,0,0,width/2,height/2,"Clockwise",0));
    nuiToolBoxItem("AE_VenitianBlinds", AE_VenitianBlinds(0,0,0,50,0));
    // -- Video
    nuiToolBoxItem("AE_TVColors", AE_TVColors(0));
    nuiToolBoxItem("AE_ReduceInterlaceFlicker", AE_ReduceInterlaceFlicker(0,0));
    nuiToolBoxItem("AE_TimeCode", AE_TimeCode(0));
    // -- DigitalEffects Aurorix
    nuiToolBoxItem("DE_Whirlix", DE_Whirlix(0,15,10,width/2,height/2,100));
    nuiToolBoxItem("DE_Spotlights", DE_Spotlights(0,	10,width/2,height/2,10,	10,width/2,height/2,10,	10,width/2,height/2,10,	100));
    nuiToolBoxItem("DE_ColorSpotlights", DE_ColorSpotlights(0,	100,width/2,height/2,100,100,width/2,height/2,100,100,width/2,height/2,100,	100));
    nuiToolBoxItem("DE_Lightzoom", DE_Lightzoom(0,10,10,width/2,height/2,100));
    nuiToolBoxItem("DE_Interferix", DE_Interferix(0, 14,width/2,height/2,60, 14,width/2,height/2,60, 14,width/2,height/2,60, 100));
    nuiToolBoxItem("DE_Electrofield", DE_Electrofield(0, 14,width/2,height/3,60, 14,width/3,2*height/3,60, 14,2*width/3,2*height/3,60, 100));
    nuiToolBoxItem("DE_EarthQuake", DE_EarthQuake(0,10,10,100));
    nuiToolBoxItem("DE_3DLighting",DE_3DLighting(0, 1,1,1, width/2,height/2,50, 0.9,0.9,0.9, 0.8,0.8,0.8, 0.7,0.7,0.7, 2.0, 0.1, 100.0));
    nuiToolBoxItem("DE_AgedFilm", DE_AgedFilm(0, 50,40, 100,15, 0.5,0.5,0.5, 50,50,0,0,0, 10,25,45,0.4, 100,5, 0,0.5,0.5,0.5, 10,10, 0, 65536,100));
    nuiToolBoxItem("DE_Blizzard",DE_Blizzard(0,10,2,2,10,10,1,1,1,10,30,0,20,100));
    nuiToolBoxItem("DE_BumpMaker",DE_BumpMaker(0,0,25,20,100,10,20,0.4,0.4,1.0,100));
    nuiToolBoxItem("DE_EdgeX",DE_EdgeX(0,67,1,160,1,170,1,100));
    nuiToolBoxItem("DE_VideoLook",DE_VideoLook(0, 100,100, 50,50, 50,50, 0,0, 1,1, "Level 1","Level 1", 0,0, "Shift","Shift", 10,10, "Off","Off", 0,0, 0,1,0,0,0.8,0, 1,100));
    nuiToolBoxItem("DE_Tilos",DE_Tilos(0,5,width/2,height/2,100));
    nuiToolBoxItem("DE_StarField",DE_StarField(0,100,10,100,0,width/2,height/2,10,20,20,100,0,0,1,1,1,1,1,1,100));
    // -- TrapCode
    nuiToolBoxItem("TC_Shine",TC_Shine(0,75, 0,0,0, width/2,height/2, 5.0, 0.0,0.0,0,0.0,0,0.0,0,0,0.0,"One Color","Alpha", 1,1,1, 1,0.8,0, 1,0.6,0, 1,0.4,0, 1,0.2,0, 1,100,100,"Add"));
    nuiToolBoxItem("TC_StarGlow",TC_StarGlow(0,"Alpha", 75,10, 0,100,50,width/2,height/2, 150,0, 1,1,1,1,1,1,1,1, "ColorMap A","ColorMap A","ColorMap A","ColorMap A","ColorMap A","ColorMap A","ColorMap A","ColorMap A", "One Color",1,1,1,1,0.8,0.8,1,0.6,0.6,1,0.4,0.4,1,0.2,0.2,"One Color",1,1,1,1,0.8,0.8,1,0.6,0.6,1,0.4,0.4,1,0.2,0.2,"One Color",1,1,1,1,0.8,0.8,1,0.6,0.6,1,0.4,0.4,1,0.2,0.2, 0,0,0,0,1, 100,100,"Add"));
    // -- Digital Fusion
    nuiToolBoxItem("DF_Feedback",DF_FeedBack(0));
    // -- Custom
    nuiToolBoxItem("AE_Explode",AE_Explode(0));
    nuiToolBoxItem("SB_Cube",SB_Cube());
    nuiToolBoxItem("SB_Sphere",SB_Sphere());
    nuiToolBoxItem("SB_Cylinder",SB_Cylinder());
nuiPopToolBox(); 

//----------------------------------------------------------------
nuiDefSlider("AE_Bulge.xRadius",0,250,1.0);
nuiDefSlider("AE_Bulge.yRadius",0,250,1.0);
nuiDefSlider("AE_Bulge.PosX",0,width,1.0);
nuiDefSlider("AE_Bulge.PosY",0,height,1.0);
nuiAddPointOsc("AE_Bulge.Pos"); 
nuiPushControlGroup("Position");
	nuiGroupControl("AE_Bulge.PosX");
 	nuiGroupControl("AE_Bulge.PosY");
nuiPopControlGroup(); 
nuiDefSlider("AE_Bulge.lensStrength",0,150,1.0);

//----------------------------------------------------------------
nuiDefSlider("AE_Spherize.radius",0,150,1.0);
nuiDefSlider("AE_Spherize.xCenter",0,500,1.0);
nuiDefSlider("AE_Spherize.yCenter",0,500,1.0);

//----------------------------------------------------------------
nuiDefSlider("AE_Twirl.Angle",-360,360,1.0);
nuiDefSlider("AE_Twirl.xCenter",0,500,1.0);
nuiDefSlider("AE_Twirl.yCenter",0,500,1.0);
nuiDefSlider("AE_Twirl.radius",0,200,1.0);

//----------------------------------------------------------------
nuiDefSlider("AE_Mirror.CenterX",0,width,1.0);
nuiDefSlider("AE_Mirror.CenterY",0,height,1.0);
nuiDefSlider("AE_Mirror.Angle",0,360,1.0);

//----------------------------------------------------------------
nuiDefSlider("AE_Scroll.xPan",-500,500,1.0);
nuiDefSlider("AE_Scroll.yPan",-500,500,1.0);
nuiDefSlider("AE_Scroll.percentOfOriginal",0,100,1.0);

//----------------------------------------------------------------
nuiDefSlider("AE_ProgressiveWave.Amplitude",0,100,1.0);
nuiDefSlider("AE_ProgressiveWave.Period",0,width,1.0);
nuiDefSlider("AE_ProgressiveWave.axisAngle",0,360,1.0);
nuiDefSlider("AE_ProgressiveWave.phase",0,360,1.0);
nuxDefMultiChoice("AE_ProgressiveWave.mode", "Sinus|Square|Triangular|Half-Triangular|Circular|Half-Circular|AntiCircular|Noise|Aliased Noise");
nuiDefSlider("AE_ProgressiveWave.speed",0,100,1.0);

//----------------------------------------------------------------
nuxDefMultiChoice("AE_Cineon_Convert.conversionType", "LogToLin|LinToLog");
nuiDefSlider("AE_Cineon_Convert.BlackPoint10bits",0,1023,1.0);
nuiDefSlider("AE_Cineon_Convert.BlackPointInternal",0,255,1.0);
nuiDefSlider("AE_Cineon_Convert.WhitePoint10bits",0,1023,1.0);
nuiDefSlider("AE_Cineon_Convert.WhitePointInternal",0,255,1.0);
nuiDefSlider("AE_Cineon_Convert.Gamma",0.01,5.0,0.1);

//----------------------------------------------------------------
nuiDefSlider("AE_Color_Balance.coef",0,360,1.0);
nuiDefSlider("AE_Color_Balance.luminosity",-100,100,1.0);
nuiDefSlider("AE_Color_Balance.saturation",-100,100,1.0);

//----------------------------------------------------------------
nuiDefSlider("AE_Gamma.rGamma",0,3.0,0.1);
nuiDefSlider("AE_Gamma.rAdd",-2.0,2.0,0.1);
nuiDefSlider("AE_Gamma.rGain",-2.0,2.0,0.1);
nuiPushControlGroup("AE_Gamma.Red");
nuiGroupControl("AE_Gamma.rGamma");
nuiGroupControl("AE_Gamma.rAdd");
nuiGroupControl("AE_Gamma.rGain");
nuiPopControlGroup();

nuiDefSlider("AE_Gamma.gGamma",0,3.0,0.1);
nuiDefSlider("AE_Gamma.gAdd",-2.0,2.0,0.1);
nuiDefSlider("AE_Gamma.gGain",-2.0,2.0,0.1);
nuiPushControlGroup("AE_Gamma.Green");
nuiGroupControl("AE_Gamma.gGamma");
nuiGroupControl("AE_Gamma.gAdd");
nuiGroupControl("AE_Gamma.gGain");
nuiPopControlGroup();

nuiDefSlider("AE_Gamma.bGamma",0,3.0,0.1);
nuiDefSlider("AE_Gamma.bAdd",-2.0,2.0,0.1);
nuiDefSlider("AE_Gamma.bGain",-2.0,2.0,0.1);
nuiPushControlGroup("AE_Gamma.Blue");
nuiGroupControl("AE_Gamma.bGamma");
nuiGroupControl("AE_Gamma.bAdd");
nuiGroupControl("AE_Gamma.bGain");
nuiPopControlGroup();

//----------------------------------------------------------------
nuiDefSlider("AE_Median.radius",0.0,10.0,0.1);
nuxDefExprToggle("AE_Median.useAlpha");

//----------------------------------------------------------------
nuxDefMultiChoice("AE_TimeCode.mode", "TimeCode|FrameNumber|Feet + Frames (35mm)|Feet + Frames (16mm)");
nuiPushControlGroup("Position");
	nuiGroupControl("AE_TimeCode.xPos");
 	nuiGroupControl("AE_TimeCode.yPos");
nuiPopControlGroup(); 
nuxDefExprToggle("AE_TimeCode.DropFrame");
nuiPushControlGroup("AE_TimeCode.Text Color");
    nuiGroupControl("AE_TimeCode.Red");
    nuiGroupControl("AE_TimeCode.Green");
    nuiGroupControl("AE_TimeCode.Blue");
nuiPopControlGroup();

//----------------------------------------------------------------
nuxDefMultiChoice("AE_TVColors.standard", "NTSC|PAL");
nuxDefMultiChoice("AE_TVColors.method", "Luminance|Saturation");
nuiDefSlider("AE_TVColors.maxAmplitude",90.0,110.0,0.1);

//----------------------------------------------------------------
nuiDefSlider("AE_ReduceInterlaceFlicker.strength",0.0,3.0,0.1);

//----------------------------------------------------------------
nuxDefMultiChoice("AE_Invert.channel", "RGB|R|G|B|HLS|H|L|S|YUV|Y|U|V|A");
nuiDefSlider("AE_Invert.mix",0.0,100.0,1.0);

//----------------------------------------------------------------
nuxDefMultiChoice("AE_Arithmetic.operation", "And|Or|Xor|Add|Sub|Difference|Min|Max|Block Above|Block Below|Slice|Mult|Over");
nuiDefSlider("AE_Arithmetic.rValue",0.0,255.0,1.0);
nuiDefSlider("AE_Arithmetic.gValue",0.0,255.0,1.0);
nuiDefSlider("AE_Arithmetic.bValue",0.0,255.0,1.0);

//----------------------------------------------------------------
nuxDefMultiChoice("AE_CompoundArithmetic.Operation", "Copy|Add|Sub|Mult|Difference|And|Or|Xor|Lighten|Darken|Minimum|Maximum|Screen|Overlay|HardLight");
nuxDefMultiChoice("AE_CompoundArithmetic.Channels", "rgb|rgba|a");
nuxDefMultiChoice("AE_CompoundArithmetic.OverflowMethod", "Cut|Loop|Adapt");
nuxDefExprToggle("AE_CompoundArithmetic.Adapt");
nuiDefSlider("AE_CompoundArithmetic.Percent",0.0,100.0,1.0);

//----------------------------------------------------------------
nuxDefMultiChoice("AE_ShiftChannels.rChannel", "r|g|b|a|l|0|1");
nuxDefMultiChoice("AE_ShiftChannels.gChannel", "r|g|b|a|l|0|1");
nuxDefMultiChoice("AE_ShiftChannels.bChannel", "r|g|b|a|l|0|1");
nuxDefMultiChoice("AE_ShiftChannels.aChannel", "r|g|b|a|l|0|1");

//----------------------------------------------------------------
nuiDefSlider("AE_BrightnessContrast.luminosity",-100.0,100.0,1.0);
nuiDefSlider("AE_BrightnessContrast.contrast",-100.0,100.0,1.0);

//----------------------------------------------------------------
nuiDefSlider("AE_Threshold.threshold",0.0,255.0,1.0);

//----------------------------------------------------------------
nuiDefSlider("AE_Text.xPos",0,width,1.0);
nuiDefSlider("AE_Text.yPos",0,height,1.0);
nuxDefMultiChoice("AE_Text.displayMode", "Fill Only|Stroke Only|Fill Over Stroke|Stroke Over Fill");
nuiPushControlGroup("AE_Text.FillColor");
    nuiGroupControl("AE_Text.red");
    nuiGroupControl("AE_Text.green");
    nuiGroupControl("AE_Text.blue");
nuiPopControlGroup();
nuiPushControlWidget("AE_Text.FillColor", nuiConnectColorPCtrl());
nuiPushControlGroup("AE_Text.StrokeColor");
    nuiGroupControl("AE_Text.rOutline");
    nuiGroupControl("AE_Text.gOutline");
    nuiGroupControl("AE_Text.bOutline");
nuiPopControlGroup();
nuiPushControlWidget("AE_Text.StrokeColor", nuiConnectColorPCtrl());
nuiDefSlider("AE_Text.outlineSize",0.0,50.0,1.0);
nuiDefSlider("AE_Text.size",0.0,512.0,1.0);
nuiDefSlider("AE_Text.kerning",-20.0,100.0,1.0);
nuxDefExprToggle("AE_Text.composite");
nuxDefMultiChoice("AE_Text.align", "Left|Center|Right");
nuxDefMultiChoice("AE_Text.direction", "Horizontal|Vertical");

//----------------------------------------------------------------
nuxDefMultiChoice("AE_Numbers.Type", "Number|Number (0s on left)|Timecode (30)|Timecode (25)|Timecode (24)|Numeric Date|Short Date|Long Date|Hexadecimal");
nuxDefExprToggle("AE_Numbers.Random");
nuiDefSlider("AE_Numbers.Value",-1000.0,1000.0,1.0);
nuiDefSlider("AE_Numbers.Decimals",0,10,1);
nuxDefExprToggle("AE_Numbers.TodayDate");
nuiAddPointOsc("AE_Numbers.Pos"); 
nuiPushControlGroup("Position");
	nuiGroupControl("AE_Numbers.PosX");
 	nuiGroupControl("AE_Numbers.PosY");
nuiPopControlGroup(); 
nuiDefSlider("AE_Numbers.PosX",0,width,1.0);
nuiDefSlider("AE_Numbers.PosY",0,height,1.0);
nuxDefMultiChoice("AE_Numbers.LayoutMode", "Fill Only|Stroke Only|Fill Over Stroke|Stroke Over Fill");
nuiPushControlGroup("AE_Numbers.Text Color");
    nuiGroupControl("AE_Numbers.rText");
    nuiGroupControl("AE_Numbers.gText");
    nuiGroupControl("AE_Numbers.bText");
nuiPopControlGroup();
nuiPushControlWidget("AE_Numbers.Text Color", nuiConnectColorPCtrl());
nuiPushControlGroup("AE_Numbers.Stroke Color");
    nuiGroupControl("AE_Numbers.rOutline");
    nuiGroupControl("AE_Numbers.gOutline");
    nuiGroupControl("AE_Numbers.bOutline");
nuiPopControlGroup();
nuiPushControlWidget("AE_Numbers.Stroke Color", nuiConnectColorPCtrl());
nuiDefSlider("AE_Numbers.OutlineWidth",0.0,50.0,1.0);
nuiDefSlider("AE_Numbers.Size",0.0,512.0,1.0);
nuiDefSlider("AE_Numbers.Kerning",-20.0,100.0,1.0);
nuxDefExprToggle("AE_Numbers.Composite");

//----------------------------------------------------------------
nuiAddPointOsc("AE_Ramp.Pos1"); 
nuiAddPointOsc("AE_Ramp.Pos2"); 
nuiPushControlGroup("Point 1");
	nuiGroupControl("AE_Ramp.Pos1X");
 	nuiGroupControl("AE_Ramp.Pos1Y");
nuiPopControlGroup(); 
nuiPushControlGroup("AE_Ramp.Color1");
    nuiGroupControl("AE_Ramp.r1");
    nuiGroupControl("AE_Ramp.g1");
    nuiGroupControl("AE_Ramp.b1");
nuiPopControlGroup();
nuiPushControlWidget("AE_Ramp.Color1", nuiConnectColorPCtrl());
nuiPushControlGroup("Point 2");
	nuiGroupControl("AE_Ramp.Pos2X");
 	nuiGroupControl("AE_Ramp.Pos2Y");
nuiPopControlGroup(); 
nuiPushControlGroup("AE_Ramp.Color2");
    nuiGroupControl("AE_Ramp.r2");
    nuiGroupControl("AE_Ramp.g2");
    nuiGroupControl("AE_Ramp.b2");
nuiPopControlGroup();
nuiPushControlWidget("AE_Ramp.Color2", nuiConnectColorPCtrl());
nuxDefMultiChoice("AE_Ramp.type", "Linear|Radial");
nuiDefSlider("AE_Ramp.mix",0.0,100.0,1.0);

//----------------------------------------------------------------
nuiDefSlider("AE_Ramp4Colors.Pos1X",0.0,width,1.0);
nuiDefSlider("AE_Ramp4Colors.Pos1Y",0.0,height,1.0);
nuiAddPointOsc("AE_Ramp4Colors.Pos1"); 
nuiPushControlGroup("Point 1");
	nuiGroupControl("AE_Ramp4Colors.Pos1X");
 	nuiGroupControl("AE_Ramp4Colors.Pos1Y");
nuiPopControlGroup(); 
nuiPushControlGroup("AE_Ramp4Colors.Color1");
    nuiGroupControl("AE_Ramp4Colors.r1");
    nuiGroupControl("AE_Ramp4Colors.g1");
    nuiGroupControl("AE_Ramp4Colors.b1");
nuiPopControlGroup();
nuiPushControlWidget("AE_Ramp4Colors.Color1", nuiConnectColorPCtrl());
nuiDefSlider("AE_Ramp4Colors.Pos2X",0.0,width,1.0);
nuiDefSlider("AE_Ramp4Colors.Pos2Y",0.0,height,1.0);
nuiAddPointOsc("AE_Ramp4Colors.Pos2"); 
nuiPushControlGroup("Point 2");
	nuiGroupControl("AE_Ramp4Colors.Pos2X");
 	nuiGroupControl("AE_Ramp4Colors.Pos2Y");
nuiPopControlGroup(); 
nuiPushControlGroup("AE_Ramp4Colors.Color2");
    nuiGroupControl("AE_Ramp4Colors.r2");
    nuiGroupControl("AE_Ramp4Colors.g2");
    nuiGroupControl("AE_Ramp4Colors.b2");
nuiPopControlGroup();
nuiPushControlWidget("AE_Ramp4Colors.Color2", nuiConnectColorPCtrl());
nuiDefSlider("AE_Ramp4Colors.Pos3X",0.0,width,1.0);
nuiDefSlider("AE_Ramp4Colors.Pos3Y",0.0,height,1.0);
nuiAddPointOsc("AE_Ramp4Colors.Pos3"); 
nuiPushControlGroup("Point 3");
	nuiGroupControl("AE_Ramp4Colors.Pos3X");
 	nuiGroupControl("AE_Ramp4Colors.Pos3Y");
nuiPopControlGroup(); 
nuiPushControlGroup("AE_Ramp4Colors.Color3");
    nuiGroupControl("AE_Ramp4Colors.r3");
    nuiGroupControl("AE_Ramp4Colors.g3");
    nuiGroupControl("AE_Ramp4Colors.b3");
nuiPopControlGroup();
nuiPushControlWidget("AE_Ramp4Colors.Color3", nuiConnectColorPCtrl());
nuiDefSlider("AE_Ramp4Colors.Pos4X",0.0,width,1.0);
nuiDefSlider("AE_Ramp4Colors.Pos4Y",0.0,height,1.0);
nuiAddPointOsc("AE_Ramp4Colors.Pos4"); 
nuiPushControlGroup("Point 4");
	nuiGroupControl("AE_Ramp4Colors.Pos4X");
 	nuiGroupControl("AE_Ramp4Colors.Pos4Y");
nuiPopControlGroup(); 
nuiPushControlGroup("AE_Ramp4Colors.Color4");
    nuiGroupControl("AE_Ramp4Colors.r4");
    nuiGroupControl("AE_Ramp4Colors.g4");
    nuiGroupControl("AE_Ramp4Colors.b4");
nuiPopControlGroup();
nuiPushControlWidget("AE_Ramp4Colors.Color4", nuiConnectColorPCtrl());
nuxDefMultiChoice("AE_Ramp4Colors.blendMode", "None|Normal");
nuxDefMultiChoice("AE_Ramp4Colors.blendMode", "None|Normal|Add|Mult|Screen|Overlay|SoftLight|HardLight|ColorDodge|ColorBurn|Darken|Lighten|Sub|Xor|Hue|Saturation|Color|Luminosity");
nuiDefSlider("AE_Ramp4Colors.opacity",0.0,100.0,1.0);

//----------------------------------------------------------------
nuxDefMultiChoice("AE_SetMatte2.Channel", "a|r|g|b|h|l|s|1|0");
nuxDefExprToggle("AE_SetMatte2.Invert");
nuxDefExprToggle("AE_SetMatte2.AdaptSize");
nuxDefExprToggle("AE_SetMatte2.Premult");

//----------------------------------------------------------------
nuxDefMultiChoice("AE_SetChannels.rChannel", "a|r|g|b|h|l|s|1|0");
nuxDefMultiChoice("AE_SetChannels.gChannel", "a|r|g|b|h|l|s|1|0");
nuxDefMultiChoice("AE_SetChannels.bChannel", "a|r|g|b|h|l|s|1|0");
nuxDefMultiChoice("AE_SetChannels.aChannel", "a|r|g|b|h|l|s|1|0");

//----------------------------------------------------------------
nuxDefExprToggle("AE_EdgeDetect.Invert");
nuiDefSlider("AE_EdgeDetect.Mix",0.0,100.0,1.0);

//----------------------------------------------------------------
nuiDefSlider("AE_Emboss.Angle",0.0,360.0,1.0);
nuiDefSlider("AE_Emboss.Contrast",0.0,200.0,1.0);
nuiDefSlider("AE_Emboss.Mix",0.0,100.0,1.0);

//----------------------------------------------------------------
nuiDefSlider("AE_ColorEmboss.Angle",0.0,360.0,1.0);
nuiDefSlider("AE_ColorEmboss.Height",0.0,10.0,1.0);
nuiDefSlider("AE_ColorEmboss.Contrast",0.0,200.0,1.0);
nuiDefSlider("AE_ColorEmboss.Percent",0.0,100.0,1.0);

//----------------------------------------------------------------
nuiDefSlider("AE_Scatter.Intensity",0.0,127.0,1.0);
nuxDefMultiChoice("AE_Scatter.Grain", "Horizontal|Vertical|Both");
nuxDefExprToggle("AE_Scatter.RandomizeTime");

//----------------------------------------------------------------
nuiDefSlider("AE_Mosaic.XBox",1.0,200.0,1.0);
nuiDefSlider("AE_Mosaic.YBox",1.0,200.0,1.0);

//----------------------------------------------------------------
nuiDefSlider("AE_Texturize.LightAngle",0.0,360.0,1.0);
nuiDefSlider("AE_Texturize.Contrast",0.0,2.0,0.1);
nuxDefMultiChoice("AE_Texturize.Disposition", "Juxtapose|Center|Resize");

//----------------------------------------------------------------
nuiPushControlGroup("AE_Strobe.Color");
    nuiGroupControl("AE_Strobe.r");
    nuiGroupControl("AE_Strobe.g");
    nuiGroupControl("AE_Strobe.b");
nuiPopControlGroup();
nuiPushControlWidget("AE_Strobe.Color", nuiConnectColorPCtrl());
nuiDefSlider("AE_Strobe.Mix",0.0,100.0,1.0);
nuiDefSlider("AE_Strobe.Length",0.0,100.0,1.0);
nuiDefSlider("AE_Strobe.Period",0.0,100.0,1.0);
nuxDefMultiChoice("AE_Strobe.Mode", "Colors|Translucent");
nuxDefMultiChoice("AE_Strobe.Operation", "Copy|Add|Sub|Mult|Difference|And|Or|Xor|Lighten|Darken|Minimum|Maximum|Over");

//----------------------------------------------------------------
nuiDefSlider("AE_MotionBlur.Angle",0.0,360.0,1.0);
nuiDefSlider("AE_MotionBlur.Distance",0.0,20.0,1.0);

//----------------------------------------------------------------
nuiDefSlider("AE_GaussianBlur.Amount",0.0,100.0,1.0);
nuxDefMultiChoice("AE_GaussianBlur.Type", "Both|Horizontal|Vertical");

//----------------------------------------------------------------
nuiDefSlider("AE_ChannelBlur.RBlur",0.0,100.0,1.0);
nuiDefSlider("AE_ChannelBlur.GBlur",0.0,100.0,1.0);
nuiDefSlider("AE_ChannelBlur.BBlur",0.0,100.0,1.0);
nuiDefSlider("AE_ChannelBlur.ABlur",0.0,100.0,1.0);
nuxDefMultiChoice("AE_ChannelBlur.Type", "Both|Horizontal|Vertical");

//----------------------------------------------------------------
nuiDefSlider("AE_RBlur.Value",0.0,5.0,0.1);
nuiAddPointOsc("AE_RBlur.Pos"); 
nuiPushControlGroup("Center");
	nuiGroupControl("AE_RBlur.PosX");
 	nuiGroupControl("AE_RBlur.PosY");
nuiPopControlGroup(); 
nuxDefMultiChoice("AE_RBlur.Mode", "Rotation|Zoom");
nuxDefMultiChoice("AE_RBlur.Quality", "Low|High");

//----------------------------------------------------------------
nuiDefSlider("AE_Sharpen.Intensity",0.0,100.0,1.0);

//----------------------------------------------------------------
nuiDefSlider("AE_Basic3D.XRotate",0.0,360.0,1.0);
nuiDefSlider("AE_Basic3D.YRotate",0.0,360.0,1.0);
nuiDefSlider("AE_Basic3D.Distance",0.0,100.0,1.0);

//----------------------------------------------------------------
nuiDefSlider("AE_BevelAlpha.Thickness",0.0,10.0,1.0);
nuiDefSlider("AE_BevelAlpha.Angle",0.0,360.0,1.0);
nuiPushControlGroup("AE_BevelAlpha.Color");
    nuiGroupControl("AE_BevelAlpha.r");
    nuiGroupControl("AE_BevelAlpha.g");
    nuiGroupControl("AE_BevelAlpha.b");
nuiPopControlGroup();
nuiPushControlWidget("AE_BevelAlpha.Color", nuiConnectColorPCtrl());
nuiDefSlider("AE_BevelAlpha.Intensity",0.0,1.0,0.1);

//----------------------------------------------------------------
nuiDefSlider("AE_BevelEdges.Thickness",0.0,0.5,0.01);
nuiDefSlider("AE_BevelEdges.Angle",0.0,360.0,1.0);
nuiPushControlGroup("AE_BevelEdges.Color");
    nuiGroupControl("AE_BevelEdges.rColor");
    nuiGroupControl("AE_BevelEdges.gColor");
    nuiGroupControl("AE_BevelEdges.bColor");
nuiPopControlGroup();
nuiPushControlWidget("AE_BevelEdges.Color", nuiConnectColorPCtrl());
nuiDefSlider("AE_BevelEdges.Intensity",0.0,1.0,0.1);

//----------------------------------------------------------------
nuiPushControlGroup("AE_DropShadow.Color");
    nuiGroupControl("AE_DropShadow.red");
    nuiGroupControl("AE_DropShadow.green");
    nuiGroupControl("AE_DropShadow.blue");
nuiPopControlGroup();
nuiPushControlWidget("AE_DropShadow.Color", nuiConnectColorPCtrl());
nuiDefSlider("AE_DropShadow.Opacity",0.0,100.0,1.0);
nuiDefSlider("AE_DropShadow.Angle",0.0,360.0,1.0);
nuiDefSlider("AE_DropShadow.Distance",0.0,120.0,1.0);
nuiDefSlider("AE_DropShadow.Fuzziness",0.0,50.0,1.0);
nuxDefExprToggle("AE_DropShadow.ShadowOnly");

//----------------------------------------------------------------
nuiDefSlider("AE_BlockDissolve.Percent",0.0,100.0,1.0);
nuiDefSlider("AE_BlockDissolve.BlockWidth",1.0,128.0,1.0);
nuiDefSlider("AE_BlockDissolve.BlockHeight",1.0,128.0,1.0);
nuiDefSlider("AE_BlockDissolve.BlurAmount",0.0,100.0,1.0);

//----------------------------------------------------------------
nuiDefSlider("AE_VenitianBlinds.Percent",0.0,100.0,1.0);
nuiDefSlider("AE_VenitianBlinds.Angle",0.0,360.0,1.0);
nuiDefSlider("AE_VenitianBlinds.Thickness",1.0,128.0,1.0);
nuiDefSlider("AE_VenitianBlinds.BlurAmount",0.0,100.0,1.0);

//----------------------------------------------------------------
nuiDefSlider("AE_LinearWipe.Percent",0.0,100.0,1.0);
nuiDefSlider("AE_LinearWipe.Angle",0.0,360.0,1.0);
nuiDefSlider("AE_LinearWipe.BlurAmount",0.0,100.0,1.0);

//----------------------------------------------------------------
nuiDefSlider("AE_RadialWipe.Percent",0.0,100.0,1.0);
nuiDefSlider("AE_RadialWipe.InitialAngle",0.0,360.0,1.0);
nuiDefSlider("AE_RadialWipe.CenterX",0.0,width,1.0);
nuiDefSlider("AE_RadialWipe.CenterY",0.0,height,1.0);
nuxDefMultiChoice("AE_RadialWipe.Mode", "Clockwise|CounterClockwise|FlipFlop");
nuiDefSlider("AE_RadialWipe.BlurAmount",0.0,100.0,1.0);

//----------------------------------------------------------------
nuiDefSlider("AE_Grid.AnchorX",0.0,width,1.0);
nuiDefSlider("AE_Grid.AnchorY",0.0,height,1.0);
nuxDefMultiChoice("AE_Grid.GridSize", "Corner|Width|Width And Height");
nuiDefSlider("AE_Grid.CornerX",0.0,width,1.0);
nuiDefSlider("AE_Grid.CornerY",0.0,height,1.0);
nuiDefSlider("AE_Grid.Width",4.0,200.0,1.0);
nuiDefSlider("AE_Grid.Height",4.0,200.0,1.0);
nuiDefSlider("AE_Grid.LineWidth",0.0,100.0,1.0);
nuxDefExprToggle("AE_Grid.InvertGrid");
nuiPushControlGroup("AE_Grid.Color");
    nuiGroupControl("AE_Grid.rGrid");
    nuiGroupControl("AE_Grid.gGrid");
    nuiGroupControl("AE_Grid.bGrid");
nuiPopControlGroup();
nuiPushControlWidget("AE_Grid.Color", nuiConnectColorPCtrl());
nuiDefSlider("AE_Grid.Opacity",0.0,100.0,1.0);
nuxDefMultiChoice("AE_Grid.TransferMode", "None|Copy|Model Alpha|Add|Mult|Screen|SoftLight|HardLight|ColorDodge|ColorBurn|Darken|Lighten|Difference|Xor|Hue|Saturation|Color|Luminosity");

//----------------------------------------------------------------
nuxDefExprToggle("AE_Fill.AllMasks");
nuiPushControlGroup("AE_Fill.Color");
    nuiGroupControl("AE_Fill.rFill");
    nuiGroupControl("AE_Fill.gFill");
    nuiGroupControl("AE_Fill.bFill");
nuiPopControlGroup();
nuiPushControlWidget("AE_Fill.Color", nuiConnectColorPCtrl());
nuxDefExprToggle("AE_Fill.Invert");
nuiDefSlider("AE_Fill.BlurX",0.0,50.0,1.0);
nuiDefSlider("AE_Fill.BlurY",0.0,50.0,1.0);
nuiDefSlider("AE_Fill.Opacity",0.0,100.0,1.0);

//----------------------------------------------------------------
nuiDefSlider("AE_RadioWaves.CenterX",0.0,width,1.0);
nuiDefSlider("AE_RadioWaves.CenterY",0.0,height,1.0);
nuiDefSlider("AE_RadioWaves.RenderQuality",0,16,1);
nuxDefMultiChoice("AE_RadioWaves.Shape", "Polygon|Source|Mask");

nuiDefSlider("AE_RadioWaves.PolyNbPoints",3,128,1);
nuxDefExprToggle("AE_RadioWaves.PolyStar");
nuiDefSlider("AE_RadioWaves.StarDepth",-1.0,1.0,0.1);
nuiPushControlGroup("AE_RadioWaves.Polygon");
nuiGroupControl("AE_RadioWaves.PolyNbPoints");
nuiGroupControl("AE_RadioWaves.PolyStar");
nuiGroupControl("AE_RadioWaves.StarDepth");
nuiPopControlGroup();

nuiDefSlider("AE_RadioWaves.SourceCenterX",0,width,1);
nuiDefSlider("AE_RadioWaves.SourceCenterY",0,height,1);
nuxDefMultiChoice("AE_RadioWaves.SourceChannel", "A|R|G|B");
nuxDefExprToggle("AE_RadioWaves.InvertSource");
nuiDefSlider("AE_RadioWaves.SourceThreshold",0,255,1);
nuiDefSlider("AE_RadioWaves.PreBlur",0.0,50.0,1.0);
nuiDefSlider("AE_RadioWaves.Tolerance",0.0,50.0,1.0);
nuiDefSlider("AE_RadioWaves.BlurAmount",0.0,50.0,1.0);
nuiPushControlGroup("AE_RadioWaves.Source");
nuiGroupControl("AE_RadioWaves.SourceCenterX");
nuiGroupControl("AE_RadioWaves.SourceCenterY");
nuiGroupControl("AE_RadioWaves.SourceChannel");
nuiGroupControl("AE_RadioWaves.InvertSource");
nuiGroupControl("AE_RadioWaves.SourceThreshold");
nuiGroupControl("AE_RadioWaves.PreBlur");
nuiGroupControl("AE_RadioWaves.Tolerance");
nuiGroupControl("AE_RadioWaves.BlurAmount");
nuiPopControlGroup();


nuiDefSlider("AE_RadioWaves.Frequency",0.0,20.0,1.0);
nuiDefSlider("AE_RadioWaves.Expansion",0.0,20.0,1.0);
nuiDefSlider("AE_RadioWaves.Orientation",0.0,360.0,1.0);
nuiDefSlider("AE_RadioWaves.Direction",0.0,360.0,1.0);
nuiDefSlider("AE_RadioWaves.Speed",0.0,500.0,1.0);
nuiDefSlider("AE_RadioWaves.Rotation",0.0,360.0,1.0);
nuiDefSlider("AE_RadioWaves.Length",0.0,30.0,1.0);
nuxDefExprToggle("AE_RadioWaves.Reflection");
nuiPushControlGroup("AE_RadioWaves.Trajectory");
nuiGroupControl("AE_RadioWaves.Frequency");
nuiGroupControl("AE_RadioWaves.Expansion");
nuiGroupControl("AE_RadioWaves.Orientation");
nuiGroupControl("AE_RadioWaves.Direction");
nuiGroupControl("AE_RadioWaves.Speed");
nuiGroupControl("AE_RadioWaves.Rotation");
nuiGroupControl("AE_RadioWaves.Length");
nuiGroupControl("AE_RadioWaves.Reflection");
nuiPopControlGroup();

nuiPushControlGroup("AE_RadioWaves.Color");
    nuiGroupControl("AE_RadioWaves.rColor");
    nuiGroupControl("AE_RadioWaves.gColor");
    nuiGroupControl("AE_RadioWaves.bColor");
nuiPopControlGroup();
nuiPushControlWidget("AE_RadioWaves.Color", nuiConnectColorPCtrl());
nuiDefSlider("AE_RadioWaves.Opacity",0.0,100.0,1.0);
nuiDefSlider("AE_RadioWaves.FadeBefore",0.0,100.0,1.0);
nuiDefSlider("AE_RadioWaves.FadeAfter",0.0,100.0,1.0);
nuiDefSlider("AE_RadioWaves.StartWidth",1.0,50.0,1.0);
nuiDefSlider("AE_RadioWaves.EndWidth",1.0,50.0,1.0);

nuiPushControlGroup("AE_RadioWaves.Layout");
nuiGroupControl("AE_RadioWaves.Opacity");
nuiGroupControl("AE_RadioWaves.FadeBefore");
nuiGroupControl("AE_RadioWaves.FadeAfter");
nuiGroupControl("AE_RadioWaves.StartWidth");
nuiGroupControl("AE_RadioWaves.EndWidth");
nuiPopControlGroup();

//----------------------------------------------------------------
nuiDefSlider("AE_Noise.Amount",0.0,100.0,1.0);
nuxDefExprToggle("AE_Noise.Color");
nuxDefExprToggle("AE_Noise.Level");

//----------------------------------------------------------------
nuiDefSlider("AE_NoiseHLS.HueAmount",0.0,100.0,1.0);
nuiDefSlider("AE_NoiseHLS.LightnessAmount",0.0,100.0,1.0);
nuiDefSlider("AE_NoiseHLS.SaturationAmount",0.0,100.0,1.0);
nuiDefSlider("AE_NoiseHLS.NoisePhase",0.0,360.0,1.0);

//----------------------------------------------------------------
nuiDefSlider("AE_Equalize.Factor",0.0,100.0,1.0);

//----------------------------------------------------------------
nuiPushControlGroup("AE_Tint.Color 1");
    nuiGroupControl("AE_Tint.red1");
    nuiGroupControl("AE_Tint.green1");
    nuiGroupControl("AE_Tint.blue1");
nuiPopControlGroup();
nuiPushControlWidget("AE_Tint.Color 1", nuiConnectColorPCtrl());
nuiPushControlGroup("AE_Tint.Color 2");
    nuiGroupControl("AE_Tint.red2");
    nuiGroupControl("AE_Tint.green2");
    nuiGroupControl("AE_Tint.blue2");
nuiPopControlGroup();
nuiPushControlWidget("AE_Tint.Color 2", nuiConnectColorPCtrl());
nuiDefSlider("AE_Tint.Factor",0.0,100.0,1.0);

//----------------------------------------------------------------
nuiDefSlider("AE_Geometry2.AnchorX",0,width,1);
nuiDefSlider("AE_Geometry2.AnchorY",0,height,1);
nuiDefSlider("AE_Geometry2.xPan",0,width,1);
nuiDefSlider("AE_Geometry2.yPan",0,height,1);
nuxDefExprToggle("AE_Geometry2.UniformScale");
nuiDefSlider("AE_Geometry2.xScale",-200.0,200.0,1.0);
nuiDefSlider("AE_Geometry2.yScale",-200.0,200.0,1.0);
nuiDefSlider("AE_Geometry2.ShearAmount",-70.0,70.0,1.0);
nuiDefSlider("AE_Geometry2.ShearAngle",0.0,360.0,1.0);
nuiDefSlider("AE_Geometry2.Angle",0.0,360.0,1.0);
nuiDefSlider("AE_Geometry2.Opacity",0.0,100.0,1.0);
nuxDefExprToggle("AE_Geometry2.UseShutter");
nuiDefSlider("AE_Geometry2.ShutterAngle",0.0,360.0,1.0);

//----------------------------------------------------------------
nuxDefMultiChoice("AE_DisplacementMap.HorizontalChannel", "a|r|g|b|h|l|s|1|0");
nuiDefSlider("AE_DisplacementMap.HorizontalStrength",-100.0,100.0,1.0);
nuxDefMultiChoice("AE_DisplacementMap.VerticalChannel", "a|r|g|b|h|l|s|1|0");
nuiDefSlider("AE_DisplacementMap.VerticalStrength",-100.0,100.0,1.0);
nuxDefMultiChoice("AE_DisplacementMap.Disposition", "Center|Adapt|Juxtapose");
nuxDefExprToggle("AE_DisplacementMap.Loop");

//----------------------------------------------------------------
nuiDefSlider("AE_PolarCoordinates.Interpolation",0.0,100.0,1.0);
nuxDefMultiChoice("AE_PolarCoordinates.Type", "RectToPolar|PolarToRect");

//----------------------------------------------------------------
nuxDefMultiChoice("AE_Blend.Mode", "Fade|Color Only|Hue Only|Darken|Lighten");
nuiDefSlider("AE_Blend.Percent",0.0,100.0,1.0);
nuxDefMultiChoice("AE_Blend.Adjust", "Center|Adapt");

//----------------------------------------------------------------
nuxDefMultiChoice("AE_Minimax.Operation", "Minimum|Maximum|Minimum then Maximum|Maximum then Minimum");
nuiDefSlider("AE_Minimax.Radius",0.0,127.0,1.0);
nuxDefMultiChoice("AE_Minimax.Channels", "rgb|rgba|r|g|b|a");
nuxDefMultiChoice("AE_Minimax.Direction", "Horizontal Only|Vertical Only|Horizontal and Vertical");

//----------------------------------------------------------------
nuiDefSlider("AE_AlphaLevels2.BlackInput",0.0,255.0,1.0);
nuiDefSlider("AE_AlphaLevels2.WhiteInput",0.0,255.0,1.0);
nuiDefSlider("AE_AlphaLevels2.Gamma",0.1,10.0,0.1);
nuiDefSlider("AE_AlphaLevels2.BlackOutput",0.0,255.0,1.0);
nuiDefSlider("AE_AlphaLevels2.WhiteOutput",0.0,255.0,1.0);

//----------------------------------------------------------------
nuiDefSlider("AE_Tile.xCenter",0.0,width,1.0);
nuiDefSlider("AE_Tile.yCenter",0.0,height,1.0);
nuiDefSlider("AE_Tile.JuxtaposeWidth",0.0,100.0,1.0);
nuiDefSlider("AE_Tile.JuxtaposeHeight",0.0,100.0,1.0);
nuiDefSlider("AE_Tile.OutputWidth",0.0,100.0,1.0);
nuiDefSlider("AE_Tile.OutputHeight",0.0,100.0,1.0);
nuxDefExprToggle("AE_Tile.Mirror");
nuiDefSlider("AE_Tile.Phase",-360.0,360.0,1.0);
nuxDefExprToggle("AE_Tile.HorizontalPhase");

//----------------------------------------------------------------
nuxDefMultiChoice("AE_RoughenEdges.EdgeType", "Roughen|Roughen Color");
nuiPushControlGroup("AE_RoughenEdges.Edge Color");
    nuiGroupControl("AE_RoughenEdges.rEdge");
    nuiGroupControl("AE_RoughenEdges.gEdge");
    nuiGroupControl("AE_RoughenEdges.bEdge");
nuiPopControlGroup();
nuiPushControlWidget("AE_RoughenEdges.Edge Color", nuiConnectColorPCtrl());
nuiDefSlider("AE_RoughenEdges.EdgeSize",0.0,32.0,1.0);
nuiDefSlider("AE_RoughenEdges.Bluriness",0.0,2.0,0.1);
nuiDefSlider("AE_RoughenEdges.FractalInfluence",0.0,.0,0.1);
nuiDefSlider("AE_RoughenEdges.IncreaseWidthHeight",-5.0,5.0,1.0);
nuiDefSlider("AE_RoughenEdges.xTurbulenceOffset",0.0,100.0,1.0);
nuiDefSlider("AE_RoughenEdges.yTurbulenceOffset",0.0,100.0,1.0);
nuiDefSlider("AE_RoughenEdges.Complexity",1,6,1);
nuiDefSlider("AE_RoughenEdges.Evolution",0.0,360.0,1.0);
nuxDefExprToggle("AE_RoughenEdges.CycleEvolution");
nuiDefSlider("AE_RoughenEdges.Cycle",1,30,1);
nuiDefSlider("AE_RoughenEdges.Random",0,1000,1);

//----------------------------------------------------------------
nuiDefSlider("AE_Ellipse.xCenter",0.0,width,1.0);
nuiDefSlider("AE_Ellipse.yCenter",0.0,height,1.0);
nuiDefSlider("AE_Ellipse.EllipseWidth",0.0,640.0,1.0);
nuiDefSlider("AE_Ellipse.EllipseHeight",0.0,640.0,1.0);
nuiDefSlider("AE_Ellipse.Thickness",0.0,50.0,1.0);
nuiDefSlider("AE_Ellipse.Bluriness",0.0,100.0,1.0);
nuiPushControlGroup("AE_Ellipse.Internal Color");
    nuiGroupControl("AE_Ellipse.rInternal");
    nuiGroupControl("AE_Ellipse.gInternal");
    nuiGroupControl("AE_Ellipse.bInternal");
nuiPopControlGroup();
nuiPushControlWidget("AE_Ellipse.Internal Color", nuiConnectColorPCtrl());
nuiPushControlGroup("AE_Ellipse.External Color");
    nuiGroupControl("AE_Ellipse.rExternal");
    nuiGroupControl("AE_Ellipse.gExternal");
    nuiGroupControl("AE_Ellipse.bExternal");
nuiPopControlGroup();
nuiPushControlWidget("AE_Ellipse.External Color", nuiConnectColorPCtrl());
nuxDefExprToggle("AE_Ellipse.Composite");

//----------------------------------------------------------------
nuiAddPointOsc("AE_LensFlare.Light"); 
nuiPushControlGroup("Light");
	nuiGroupControl("AE_LensFlare.LightX");
 	nuiGroupControl("AE_LensFlare.LightY");
nuiPopControlGroup(); 
nuiDefSlider("AE_LensFlare.Intensity",0.0,300.0,1.0);
nuxDefMultiChoice("AE_LensFlare.LensType", "50-300mm (zoom)|35 mm normal");
nuiDefSlider("AE_LensFlare.Percent",0.0,100.0,1.0);

//----------------------------------------------------------------
nuiDefSlider("AE_EasyLevels.RGBBlackInput",0.0,255.0,1.0);
nuiDefSlider("AE_EasyLevels.RGBWhiteInput",0.0,255.0,1.0);
nuiDefSlider("AE_EasyLevels.RGBGamma",0.1,10.0,0.1);
nuiDefSlider("AE_EasyLevels.RGBBlackOutput",0.0,255.0,1.0);
nuiDefSlider("AE_EasyLevels.RGBWhiteOutput",0.0,255.0,1.0);
nuiPushControlGroup("AE_EasyLevels.RGB");
nuiGroupControl("AE_EasyLevels.RGBBlackInput");
nuiGroupControl("AE_EasyLevels.RGBWhiteInput");
nuiGroupControl("AE_EasyLevels.RGBGamma");
nuiGroupControl("AE_EasyLevels.RGBBlackOutput");
nuiGroupControl("AE_EasyLevels.RGBWhiteOutput");
nuiPopControlGroup();

nuiDefSlider("AE_EasyLevels.RBlackInput",0.0,255.0,1.0);
nuiDefSlider("AE_EasyLevels.RWhiteInput",0.0,255.0,1.0);
nuiDefSlider("AE_EasyLevels.RGamma",0.1,10.0,0.1);
nuiDefSlider("AE_EasyLevels.RBlackOutput",0.0,255.0,1.0);
nuiDefSlider("AE_EasyLevels.RWhiteOutput",0.0,255.0,1.0);
nuiPushControlGroup("AE_EasyLevels.Red");
nuiGroupControl("AE_EasyLevels.RBlackInput");
nuiGroupControl("AE_EasyLevels.RWhiteInput");
nuiGroupControl("AE_EasyLevels.RGamma");
nuiGroupControl("AE_EasyLevels.RBlackOutput");
nuiGroupControl("AE_EasyLevels.RWhiteOutput");
nuiPopControlGroup();

nuiDefSlider("AE_EasyLevels.GBlackInput",0.0,255.0,1.0);
nuiDefSlider("AE_EasyLevels.GWhiteInput",0.0,255.0,1.0);
nuiDefSlider("AE_EasyLevels.GGamma",0.1,10.0,0.1);
nuiDefSlider("AE_EasyLevels.GBlackOutput",0.0,255.0,1.0);
nuiDefSlider("AE_EasyLevels.GWhiteOutput",0.0,255.0,1.0);
nuiPushControlGroup("AE_EasyLevels.Green");
nuiGroupControl("AE_EasyLevels.GBlackInput");
nuiGroupControl("AE_EasyLevels.GWhiteInput");
nuiGroupControl("AE_EasyLevels.GGamma");
nuiGroupControl("AE_EasyLevels.GBlackOutput");
nuiGroupControl("AE_EasyLevels.GWhiteOutput");
nuiPopControlGroup();

nuiDefSlider("AE_EasyLevels.BBlackInput",0.0,255.0,1.0);
nuiDefSlider("AE_EasyLevels.BWhiteInput",0.0,255.0,1.0);
nuiDefSlider("AE_EasyLevels.BGamma",0.1,10.0,0.1);
nuiDefSlider("AE_EasyLevels.BBlackOutput",0.0,255.0,1.0);
nuiDefSlider("AE_EasyLevels.BWhiteOutput",0.0,255.0,1.0);
nuiPushControlGroup("AE_EasyLevels.Blue");
nuiGroupControl("AE_EasyLevels.BBlackInput");
nuiGroupControl("AE_EasyLevels.BWhiteInput");
nuiGroupControl("AE_EasyLevels.BGamma");
nuiGroupControl("AE_EasyLevels.BBlackOutput");
nuiGroupControl("AE_EasyLevels.BWhiteOutput");
nuiPopControlGroup();

nuiDefSlider("AE_EasyLevels.ABlackInput",0.0,255.0,1.0);
nuiDefSlider("AE_EasyLevels.AWhiteInput",0.0,255.0,1.0);
nuiDefSlider("AE_EasyLevels.AGamma",0.1,10.0,0.1);
nuiDefSlider("AE_EasyLevels.ABlackOutput",0.0,255.0,1.0);
nuiDefSlider("AE_EasyLevels.AWhiteOutput",0.0,255.0,1.0);
nuiPushControlGroup("AE_EasyLevels.Alpha");
nuiGroupControl("AE_EasyLevels.ABlackInput");
nuiGroupControl("AE_EasyLevels.AWhiteInput");
nuiGroupControl("AE_EasyLevels.AGamma");
nuiGroupControl("AE_EasyLevels.ABlackOutput");
nuiGroupControl("AE_EasyLevels.AWhiteOutput");
nuiPopControlGroup();

//----------------------------------------------------------------
nuiDefSlider("AE_Posterize.Levels",2,32,1);

//----------------------------------------------------------------
nuiDefSlider("AE_GradientWipe.Percent",0.0,100.0,1.0);
nuiDefSlider("AE_GradientWipe.BlurAmount",0.0,100.0,1.0);
nuxDefExprToggle("AE_GradientWipe.Invert");

//----------------------------------------------------------------
nuiAddPointOsc("AE_IrisWipe.Pos"); 
nuiPushControlGroup("Center");
	nuiGroupControl("AE_IrisWipe.PosX");
 	nuiGroupControl("AE_IrisWipe.PosY");
nuiPopControlGroup(); 
nuiDefSlider("AE_IrisWipe.NbPoints",6,32,1);
nuiDefSlider("AE_IrisWipe.ExternalRadius",0.0,640.0,1.0);
nuxDefExprToggle("AE_IrisWipe.UseInternalRadius");
nuiDefSlider("AE_IrisWipe.InternalRadius",0.0,640.0,1.0);
nuiDefSlider("AE_IrisWipe.Rotation",0.0,360.0,1.0);
nuiDefSlider("AE_IrisWipe.BlurAmount",0.0,100.0,1.0);

//----------------------------------------------------------------
nuxDefMultiChoice("AE_LumaKey.PixelMask", "Bright Pixels|Dark Pixels|Same Pixels|Different Pixels");
nuiDefSlider("AE_LumaKey.Threshold",0,255,1);
nuiDefSlider("AE_LumaKey.Tolerance",0,255,1);
nuiDefSlider("AE_LumaKey.Edges",-5.0,5.0,0.1);
nuiDefSlider("AE_LumaKey.BlurAmount",0.0,10.0,0.1);

//----------------------------------------------------------------
nuxDefMultiChoice("AE_Extract.Channel", "Luminance|Red|Green|Blue|Alpha");
nuiDefSlider("AE_Extract.BlackPoint",0,255,1);
nuiDefSlider("AE_Extract.WhitePoint",0,255,1);
nuiDefSlider("AE_Extract.BlackTransparency",0,255,1);
nuiDefSlider("AE_Extract.WhiteTransparency",0,255,1);
nuxDefExprToggle("AE_Extract.Invert");

//----------------------------------------------------------------
nuxDefMultiChoice("AE_ColorKey.Channel", "Luminance|Red|Green|Blue|Alpha");
nuiPushControlGroup("AE_ColorKey.Key Color");
    nuiGroupControl("AE_ColorKey.rKey");
    nuiGroupControl("AE_ColorKey.gKey");
    nuiGroupControl("AE_ColorKey.bKey");
nuiPopControlGroup();
nuiPushControlWidget("AE_ColorKey.Key Color", nuiConnectColorPCtrl());
nuiDefSlider("AE_ColorKey.Tolerance",0,255,1);
nuiDefSlider("AE_ColorKey.Edges",-5,5,1);
nuiDefSlider("AE_ColorKey.BlurAmount",0.0,10.0,0.1);

//----------------------------------------------------------------
nuiDefSlider("AE_ColorRange.Tolerance",0,255,1);
nuxDefMultiChoice("AE_ColorRange.Color_Space", "Lab|YUV|RGB");
nuiDefSlider("AE_ColorRange.MinLYR",0,255,1);
nuiDefSlider("AE_ColorRange.MaxLYR",0,255,1);
nuiDefSlider("AE_ColorRange.MinaUG",0,255,1);
nuiDefSlider("AE_ColorRange.MaxaUG",0,255,1);
nuiDefSlider("AE_ColorRange.MinbVB",0,255,1);
nuiDefSlider("AE_ColorRange.MaxbVB",0,255,1);

//----------------------------------------------------------------
nuxDefMultiChoice("AE_DifferenceMatte.Adjust", "Center|Adjust");
nuiDefSlider("AE_DifferenceMatte.Tolerance",0,100,1);
nuiDefSlider("AE_DifferenceMatte.BlurAmount",0,100,1);
nuiDefSlider("AE_DifferenceMatte.PreBlur",0,100,1);

//----------------------------------------------------------------
nuiPushControlGroup("AE_LinearColorKey.Key Color");
    nuiGroupControl("AE_LinearColorKey.rKey");
    nuiGroupControl("AE_LinearColorKey.gKey");
    nuiGroupControl("AE_LinearColorKey.bKey");
nuiPopControlGroup();
nuiPushControlWidget("AE_LinearColorKey.Key Color", nuiConnectColorPCtrl());
nuxDefMultiChoice("AE_LinearColorKey.CompareSpace", "RGB|Hue|Chrominance");
nuiDefSlider("AE_LinearColorKey.Tolerance",0,100,1);
nuiDefSlider("AE_LinearColorKey.BlurAmount",0,100,1);
nuxDefMultiChoice("AE_LinearColorKey.Operation", "Mask|Keep");

//----------------------------------------------------------------
nuiDefSlider("AE_CompoundBlur.Maximum",0,100,1);
nuxDefExprToggle("AE_CompoundBlur.Adapt");
nuxDefExprToggle("AE_CompoundBlur.Inversion");

//----------------------------------------------------------------
nuiDefSlider("AE_FastBlur.Intensity",0.0,128.0,1.0);
nuxDefMultiChoice("AE_FastBlur.Direction", "Both|Vertical|Horizontal");

//----------------------------------------------------------------
nuiDefSlider("AE_UnsharpMask.Amount",0.0,100.0,1.0);
nuiDefSlider("AE_UnsharpMask.Radius",0.1,100.0,0.1);
nuiDefSlider("AE_UnsharpMask.Threshold",0,255,1);

//----------------------------------------------------------------
nuxDefMultiChoice("AE_SimpleChoker.Output", "Final Output|Matte");
nuiDefSlider("AE_SimpleChoker.ChokeMatte",-10.0,10.0,0.1);

//----------------------------------------------------------------
nuiDefSlider("AE_MatteChoker.GeomSoftness1",0.0,10.0,0.1);
nuiDefSlider("AE_MatteChoker.Choke1",-127,127,1);
nuiDefSlider("AE_MatteChoker.GrayLevelSoftness1",0.0,100.0,1.0);
nuiDefSlider("AE_MatteChoker.GeomSoftness2",0.0,10.0,0.1);
nuiDefSlider("AE_MatteChoker.Choke2",-127,127,1);
nuiDefSlider("AE_MatteChoker.GrayLevelSoftness2",0.0,100.0,1.0);
nuiDefSlider("AE_MatteChoker.Iterations",1,10,1);

//----------------------------------------------------------------
nuiDefSlider("AE_ChannelMixer.RedRed",-200,200,1);
nuiDefSlider("AE_ChannelMixer.RedGreen",-200,200,1);
nuiDefSlider("AE_ChannelMixer.RedBlue",-200,200,1);
nuiDefSlider("AE_ChannelMixer.RedConst",-200,200,1);
nuiPushControlGroup("AE_ChannelMixer.Red");
nuiGroupControl("AE_ChannelMixer.RedRed");
nuiGroupControl("AE_ChannelMixer.RedGreen");
nuiGroupControl("AE_ChannelMixer.RedBlue");
nuiGroupControl("AE_ChannelMixer.RedConst");
nuiPopControlGroup();
nuiDefSlider("AE_ChannelMixer.GreenRed",-200,200,1);
nuiDefSlider("AE_ChannelMixer.GreenGreen",-200,200,1);
nuiDefSlider("AE_ChannelMixer.GreenBlue",-200,200,1);
nuiDefSlider("AE_ChannelMixer.GreenConst",-200,200,1);
nuiPushControlGroup("AE_ChannelMixer.Green");
nuiGroupControl("AE_ChannelMixer.GreenRed");
nuiGroupControl("AE_ChannelMixer.GreenGreen");
nuiGroupControl("AE_ChannelMixer.GreenBlue");
nuiGroupControl("AE_ChannelMixer.GreenConst");
nuiPopControlGroup();
nuiDefSlider("AE_ChannelMixer.BlueRed",-200,200,1);
nuiDefSlider("AE_ChannelMixer.BlueGreen",-200,200,1);
nuiDefSlider("AE_ChannelMixer.BlueBlue",-200,200,1);
nuiDefSlider("AE_ChannelMixer.BlueConst",-200,200,1);
nuiPushControlGroup("AE_ChannelMixer.Blue");
nuiGroupControl("AE_ChannelMixer.BlueRed");
nuiGroupControl("AE_ChannelMixer.BlueGreen");
nuiGroupControl("AE_ChannelMixer.BlueBlue");
nuiGroupControl("AE_ChannelMixer.BlueConst");
nuiPopControlGroup();
nuxDefExprToggle("AE_ChannelMixer.Monochrome");

//----------------------------------------------------------------
nuxDefMultiChoice("AE_HueSaturation.ChannelControl", "Master|Reds|Yellows|Greens|Cyans|Blues|Magentas");
nuiDefSlider("AE_HueSaturation.MasterHue",0,360,1);
nuiDefSlider("AE_HueSaturation.MasterSaturation",-100,100,1);
nuiDefSlider("AE_HueSaturation.MasterLightness",-100,100,1);
nuxDefExprToggle("AE_HueSaturation.Colorize");
nuiDefSlider("AE_HueSaturation.ColorizeHue",0,360,1);
nuiDefSlider("AE_HueSaturation.ColorizeSaturation",0,100,1);
nuiDefSlider("AE_HueSaturation.ColorizeLightness",-100,100,1);


//----------------------------------------------------------------
nuiAddPointOsc("AE_Ripple.Pos"); 
nuiPushControlGroup("Center");
	nuiGroupControl("AE_Ripple.PosX");
 	nuiGroupControl("AE_Ripple.PosY");
nuiPopControlGroup(); 
nuxDefMultiChoice("AE_Ripple.Conversion", "Asymmetric|Symmetric");
nuiDefSlider("AE_Ripple.Radius",0.0,100.0,1.0);
nuiDefSlider("AE_Ripple.WaveSpeed",-6.0,6.0,1.0);
nuiDefSlider("AE_Ripple.WaveWidth",2.0,100.0,1.0);
nuiDefSlider("AE_Ripple.WaveHeight",0.0,100.0,1.0);
nuiDefSlider("AE_Ripple.RipplePhase",0.0,360.0,1.0);

//----------------------------------------------------------------
nuiAddPointOsc("AE_ParticlePlayground.Pos"); 
nuiPushControlGroup("Center");
	nuiGroupControl("AE_ParticlePlayground.PosX");
 	nuiGroupControl("AE_ParticlePlayground.PosY");
nuiPopControlGroup(); 
nuiDefSlider("AE_ParticlePlayground.BarrelRadius",0.0,400.0,1.0);
nuiDefSlider("AE_ParticlePlayground.ParticlesPerSecond",0.0,500.0,1.0);
nuiDefSlider("AE_ParticlePlayground.Direction",0.0,360.0,1.0);
nuiDefSlider("AE_ParticlePlayground.DirectionRandom",0.0,100.0,1.0);
nuiDefSlider("AE_ParticlePlayground.Velocity",0.0,500.0,1.0);
nuiDefSlider("AE_ParticlePlayground.VelocityRandom",0.0,100.0,1.0);
nuiPushControlGroup("AE_ParticlePlayground.Particle Color");
    nuiGroupControl("AE_ParticlePlayground.rParticle");
    nuiGroupControl("AE_ParticlePlayground.gParticle");
    nuiGroupControl("AE_ParticlePlayground.bParticle");
nuiPopControlGroup();
nuiPushControlWidget("AE_ParticlePlayground.Particle Color", nuiConnectColorPCtrl());
nuiDefSlider("AE_ParticlePlayground.ParticleRadius",0.0,100.0,1.0);
nuiPushControlGroup("AE_ParticlePlayground.Canon");
nuiGroupControl("AE_ParticlePlayground.Pos");
nuiGroupControl("AE_ParticlePlayground.BarrelRadius");
nuiGroupControl("AE_ParticlePlayground.ParticlesPerSecond");
nuiGroupControl("AE_ParticlePlayground.Direction");
nuiGroupControl("AE_ParticlePlayground.DirectionRandom");
nuiGroupControl("AE_ParticlePlayground.Velocity");
nuiGroupControl("AE_ParticlePlayground.VelocityRandom");
nuiGroupControl("AE_ParticlePlayground.Particle Color");
nuiGroupControl("AE_ParticlePlayground.ParticleRadius");
nuiPopControlGroup();

nuxDefMultiChoice("AE_ParticlePlayground.TimeOffsetType", "Relative|Absolute|Relative Random|Absolute Random");
nuiDefSlider("AE_ParticlePlayground.TimeOffset",-10.0,10.0,0.1);
nuxDefMultiChoice("AE_ParticlePlayground.LayerMapAffect", "None|Cannon");
nuiPushControlGroup("AE_ParticlePlayground.Layer Map");
nuiGroupControl("AE_ParticlePlayground.TimeOffsetType");
nuiGroupControl("AE_ParticlePlayground.TimeOffset");
nuiPopControlGroup();

nuiDefSlider("AE_ParticlePlayground.GravityForce",-50.0,180.0,1.0);
nuiDefSlider("AE_ParticlePlayground.GravityForceSpread",0.0,5.0,0.1);
nuiDefSlider("AE_ParticlePlayground.GravityDirection",0.0,360.0,1.0);
nuiPushControlGroup("AE_ParticlePlayground.Gravity");
nuiGroupControl("AE_ParticlePlayground.GravityForce");
nuiGroupControl("AE_ParticlePlayground.GravityForceSpread");
nuiGroupControl("AE_ParticlePlayground.GravityDirection");
nuiPopControlGroup();

//----------------------------------------------------------------
nuiPushControlGroup("AE_Stroke.Brush Color");
    nuiGroupControl("AE_Stroke.rColor");
    nuiGroupControl("AE_Stroke.gColor");
    nuiGroupControl("AE_Stroke.bColor");
nuiPopControlGroup();
nuiPushControlWidget("AE_Stroke.Brush Color", nuiConnectColorPCtrl());
nuiDefSlider("AE_Stroke.BrushSize",0.0,25.0,1.0);
nuiDefSlider("AE_Stroke.BrushHardness",0.0,100.0,1.0);
nuiDefSlider("AE_Stroke.Opacity",0.0,100.0,1.0);
nuxDefMultiChoice("AE_Stroke.PaintStyle", "On Original Image|On Transparent|Reveal Original Image");


//----------------------------------------------------------------

nuiDefSlider("AE_OpticsCompensation.FOV",0.0,90.0,1.0);
nuxDefExprToggle("AE_OpticsCompensation.ReverseDistorsion");
nuxDefMultiChoice("AE_OpticsCompensation.FOVOrientation", "Horizontal|Vertical|Diagonal");
nuiDefSlider("AE_OpticsCompensation.PosX",0.0,width,1.0);
nuiDefSlider("AE_OpticsCompensation.PosY",0.0,height,1.0);
nuiDefSlider("AE_OpticsCompensation.Opacity",0.0,100.0,1.0);
nuxDefExprToggle("AE_OpticsCompensation.OptimalPixels");
nuxDefMultiChoice("AE_OpticsCompensation.Resize", "Off|Max x2|Max x4|Unlimited");

//----------------------------------------------------------------

nuxDefMultiChoice("AE_Colorama.InputPhaseChannel", "Intensity|Red|Green|Blue|Hue|Lightness|Saturation|Alpha|Zero");
nuxDefExprToggle("AE_Colorama.UseSecondPhase");
nuxDefMultiChoice("AE_Colorama.AddPhaseFrom", "Intensity|Red|Green|Blue|Hue|Lightness|Saturation|Alpha|Zero");
nuxDefMultiChoice("AE_Colorama.AddMode", "Wrap|Clamp|Average|Screen");
nuiDefSlider("AE_Colorama.PhaseShift",0.0,360.0,1.0);
nuiPushControlGroup("AE_Colorama.Input Phase");
nuiGroupControl("AE_Colorama.InputPhaseChannel");
nuiGroupControl("AE_Colorama.UseSecondPhase");
nuiGroupControl("AE_Colorama.AddPhaseFrom");
nuiGroupControl("AE_Colorama.AddMode");
nuiGroupControl("AE_Colorama.PhaseShift");
nuiPopControlGroup();

nuxDefMultiChoice("AE_Colorama.ModifyChannels", "All|Red|Green|Blue|RG|GB|RB|None");
nuxDefExprToggle("AE_Colorama.ModifyAlpha");
nuxDefExprToggle("AE_Colorama.ChangeEmptyPixels");
nuiPushControlGroup("AE_Colorama.Modify");
nuiGroupControl("AE_Colorama.ModifyChannels");
nuiGroupControl("AE_Colorama.ModifyAlpha");
nuiGroupControl("AE_Colorama.ChangeEmptyPixels");
nuiPopControlGroup();

nuiPushControlGroup("AE_Colorama.Matching Color");
    nuiGroupControl("AE_Colorama.rMatchingColor");
    nuiGroupControl("AE_Colorama.gMatchingColor");
    nuiGroupControl("AE_Colorama.bMatchingColor");
nuiPopControlGroup();
nuiPushControlWidget("AE_Colorama.Matching Color", nuiConnectColorPCtrl());
nuiDefSlider("AE_Colorama.MatchingTolerance",0.0,1.0,0.1);
nuiDefSlider("AE_Colorama.MatchingSoftness",0.0,1.0,0.1);
nuxDefMultiChoice("AE_Colorama.MatchingMode", "Off|RGB|Hue|Chroma");
nuiPushControlGroup("AE_Colorama.Pixel Selection");
nuiGroupControl("AE_Colorama.Matching Color");
nuiGroupControl("AE_Colorama.MatchingTolerance");
nuiGroupControl("AE_Colorama.MatchingSoftness");
nuiGroupControl("AE_Colorama.MatchingMode");
nuiPopControlGroup();

nuxDefMultiChoice("AE_Colorama.MaskingMode", "Off|Intensity|Alpha|Inverted Intensity|Inverted Alpha");
nuiPushControlGroup("AE_Colorama.Masking");
nuiGroupControl("AE_Colorama.MaskingMode");
nuiPopControlGroup();

nuxDefExprToggle("AE_Colorama.Mix");
nuiDefSlider("AE_Colorama.Percent",0.0,100.0,1.0);

//----------------------------------------------------------------

nuxDefMultiChoice("AE_Fractal.Type", "Mandelbrot|Julia");

//----------------------------------------------------------------

nuxDefMultiChoice("AE_FractalNoise.FractalType", "Basic");
nuxDefMultiChoice("AE_FractalNoise.NoiseType", "Block|Linear|Soft Linear|Spline");
nuxDefExprToggle("AE_FractalNoise.Invert");
nuiDefSlider("AE_FractalNoise.Contrast",0.0,400.0,1.0);
nuiDefSlider("AE_FractalNoise.Brightness",-100.0,100.0,1.0);
nuxDefMultiChoice("AE_FractalNoise.Overflow", "Clip|Soft Clamp|Wrap Back");
nuiDefSlider("AE_FractalNoise.Complexity",0.0,10.0,1.0);
nuiDefSlider("AE_FractalNoise.RandomSeed",0,1000,1);
nuiDefSlider("AE_FractalNoise.RandomSeed",0,1000,1);
nuiDefSlider("AE_FractalNoise.Opacity",0.0,100.0,1.0);
nuxDefMultiChoice("AE_FractalNoise.TransferMode", "None|Normal|Add|Mult|Screen|Overlay|SoftLight|HardLight|ColorDodge|ColorBurn|Darken|Lighten|Difference|Exclusion|Hue|Saturation|Luminosity");

//----------------------------------------------------------------

nuiAddPointOsc("AE_Laser.Start"); 
nuiPushControlGroup("Start");
	nuiGroupControl("AE_Laser.StartX");
 	nuiGroupControl("AE_Laser.StartY");
nuiPopControlGroup(); 
nuiAddPointOsc("AE_Laser.End"); 
nuiPushControlGroup("End");
	nuiGroupControl("AE_Laser.EndX");
 	nuiGroupControl("AE_Laser.EndY");
nuiPopControlGroup(); 
nuiDefSlider("AE_Laser.Length",0.0,100.0,1.0);
nuiDefSlider("AE_Laser.TimePct",0.0,100.0,1.0);
nuiDefSlider("AE_Laser.StartThickness",0.0,50.0,1.0);
nuiDefSlider("AE_Laser.EndThickness",0.0,50.0,1.0);
nuiPushControlGroup("AE_Laser.Inside Color");
    nuiGroupControl("AE_Laser.rInside");
    nuiGroupControl("AE_Laser.gInside");
    nuiGroupControl("AE_Laser.bInside");
nuiPopControlGroup();
nuiPushControlWidget("AE_Laser.Inside Color", nuiConnectColorPCtrl());
nuxDefExprToggle("AE_Laser.Composite");

//----------------------------------------------------------------

nuiAddPointOsc("AE_BezierMesh.TopLeftVertex");
nuiAddPointOsc("AE_BezierMesh.RightTopVertex");
nuiAddPointOsc("AE_BezierMesh.BottomRightVertex");
nuiAddPointOsc("AE_BezierMesh.LeftBottomVertex");

nuiAddPointOsc("AE_BezierMesh.TopLeftTangent");
nuiAddPointOsc("AE_BezierMesh.TopRightTangent");
nuiAddPointOsc("AE_BezierMesh.RightTopTangent");
nuiAddPointOsc("AE_BezierMesh.RightBottomTangent");

nuiAddPointOsc("AE_BezierMesh.BottomRightTangent");
nuiAddPointOsc("AE_BezierMesh.BottomLeftTangent");
nuiAddPointOsc("AE_BezierMesh.LeftBottomTangent");
nuiAddPointOsc("AE_BezierMesh.LeftTopTangent");

//----------------------------------------------------------------

nuxDefMultiChoice("AE_Glow.Base", "Color Channels|Alpha Channel");
nuiDefSlider("AE_Glow.Threshold",0.0,100.0,1.0);
nuiDefSlider("AE_Glow.Radius",0.0,100.0,1.0);
nuiDefSlider("AE_Glow.Intensity",0.0,4.0,0.1);
nuxDefMultiChoice("AE_Glow.CompositeMode", "On Top|Behind|None");
nuxDefMultiChoice("AE_Glow.Operation", "None|Normal|Add|Multiply|Screen|Overlay|SoftLight|HardLight|Darken|Lighten|Difference|Hue|Saturation|Color|Luminosity|ColorDodge|ColorBurn|Exclusion");
nuxDefMultiChoice("AE_Glow.Dimensions", "Horizontal And Vertical|Horizontal|Vertical");

//----------------------------------------------------------------

nuiDefSlider("AE_WriteOn.PosX",0.0,width,1.0);
nuiDefSlider("AE_WriteOn.PosY",0.0,height,1.0);
nuiAddPointOsc("AE_WriteOn.Pos"); 
nuiPushControlGroup("Position");
	nuiGroupControl("AE_WriteOn.PosX");
 	nuiGroupControl("AE_WriteOn.PosY");
nuiPopControlGroup(); 
nuxDefMultiChoice("AE_WriteOn.PaintStyle", "On Original Image|On Transparent|Reveal Original Image");

//----------------------------------------------------------------

nuiDefSlider("AE_PathText.Vertex1X",0.0,width,1.0);
nuiDefSlider("AE_PathText.Vertex1Y",0.0,height,1.0);
nuiAddPointOsc("AE_PathText.Vertex1"); 
nuiPushControlGroup("Point 1");
	nuiGroupControl("AE_PathText.Vertex1X");
 	nuiGroupControl("AE_PathText.Vertex1Y");
nuiPopControlGroup(); 
nuiDefSlider("AE_PathText.Tangent1X",0.0,width,1.0);
nuiDefSlider("AE_PathText.Tangent1Y",0.0,height,1.0);
nuiAddPointOsc("AE_PathText.Tangent1"); 
nuiPushControlGroup("Tangent 1");
	nuiGroupControl("AE_PathText.Tangent1X");
 	nuiGroupControl("AE_PathText.Tangent1Y");
nuiPopControlGroup(); 
nuiDefSlider("AE_PathText.Vertex2X",0.0,width,1.0);
nuiDefSlider("AE_PathText.Vertex2Y",0.0,height,1.0);
nuiAddPointOsc("AE_PathText.Vertex2"); 
nuiPushControlGroup("Point 2");
	nuiGroupControl("AE_PathText.Vertex2X");
 	nuiGroupControl("AE_PathText.Vertex2Y");
nuiPopControlGroup(); 
nuiDefSlider("AE_PathText.Tangent2X",0.0,width,1.0);
nuiDefSlider("AE_PathText.Tangent2Y",0.0,height,1.0);
nuiAddPointOsc("AE_PathText.Tangent2"); 
nuiPushControlGroup("Tangent 2");
	nuiGroupControl("AE_PathText.Tangent2X");
 	nuiGroupControl("AE_PathText.Tangent2Y");
nuiPopControlGroup(); 
nuxDefMultiChoice("AE_PathText.ShapeType", "Bezier|Line|Circle");
nuxDefMultiChoice("AE_PathText.TextOptions", "Fill Only|Stroke Only|Fill Over Stroke|Stroke Over Fill");

//----------------------------------------------------------------

nuiDefSlider("AE_3DGlasses.ConvergenceOffset",-60.0,20.0,1.0);
nuiDefSlider("AE_3DGlasses.Balance",0.0,20.0,1.0);
nuxDefExprToggle("AE_3DGlasses.SwapLeftRight");
nuxDefMultiChoice("AE_3DGlasses.View3D", "Stereo Pair|Interlace Upper L Lower R|Red Green LR|Red Blue LR|Balanced Red Green LR|Balanced Red Blue LR");
nuxDefMultiChoice("AE_3DGlasses.ReferenceSize", "Left View|Right View");

//----------------------------------------------------------------

nuiDefSlider("AE_SolidComposite.SourceOpacity",0.0,100.0,1.0);
nuiPushControlGroup("AE_SolidComposite.Solid Color");
    nuiGroupControl("AE_SolidComposite.rColor");
    nuiGroupControl("AE_SolidComposite.gColor");
    nuiGroupControl("AE_SolidComposite.bColor");
nuiPopControlGroup();
nuiPushControlWidget("AE_SolidComposite.Solid Color", nuiConnectColorPCtrl());
nuiDefSlider("AE_SolidComposite.Opacity",0.0,100.0,1.0);
nuxDefMultiChoice("AE_SolidComposite.BlendingMode", "Normal|Add|Multiply|Screen|Overlay|SoftLight|HardLight|Darken|Lighten|Difference|Hue|Saturation|Color|Luminosity|ColorDodge|ColorBurn|Exclusion");

//----------------------------------------------------------------

nuxDefMultiChoice("AE_Warp.WarpStyle", "Arc|Flag|Rise|Wave");
nuxDefMultiChoice("AE_Warp.WarpAxis", "Horizontal|Vertical");
nuiDefSlider("AE_Warp.Bend",-100.0,100.0,1.0);
nuiDefSlider("AE_Warp.HorizontalDistort",-100.0,100.0,1.0);
nuiDefSlider("AE_Warp.VerticalDistort",-100.0,100.0,1.0);

//----------------------------------------------------------------

nuiDefSlider("DE_Interferix.Red",  0.1,100.0,0.1);
nuiDefSlider("DE_Interferix.RedCenterX",0.0,width,1.0);
nuiDefSlider("DE_Interferix.RedCenterY",0.0,height,1.0);
nuiAddPointOsc("DE_Interferix.RedCenter"); 
nuiPushControlGroup("RedCenter");
	nuiGroupControl("DE_Interferix.RedCenterX");
 	nuiGroupControl("DE_Interferix.RedCenterY");
nuiPopControlGroup(); 
nuiDefSlider("DE_Interferix.RedBrightness",0.1,100.0,0.1);
nuiDefSlider("DE_Interferix.Green",0.1,100.0,0.1);
nuiDefSlider("DE_Interferix.GreenCenterX",0.0,width,1.0);
nuiDefSlider("DE_Interferix.GreenCenterY",0.0,height,1.0);
nuiAddPointOsc("DE_Interferix.GreenCenter"); 
nuiPushControlGroup("GreenCenter");
	nuiGroupControl("DE_Interferix.GreenCenterX");
 	nuiGroupControl("DE_Interferix.GreenCenterY");
nuiPopControlGroup(); 
nuiDefSlider("DE_Interferix.GreenBrightness",0.1,100.0,0.1);
nuiDefSlider("DE_Interferix.Blue", 0.1,100.0,0.1);
nuiDefSlider("DE_Interferix.BlueCenterX",0.0,width,1.0);
nuiDefSlider("DE_Interferix.BlueCenterY",0.0,height,1.0);
nuiAddPointOsc("DE_Interferix.BlueCenter"); 
nuiPushControlGroup("BlueCenter");
	nuiGroupControl("DE_Interferix.BlueCenterX");
 	nuiGroupControl("DE_Interferix.BlueCenterY");
nuiPopControlGroup(); 
nuiDefSlider("DE_Interferix.BlueBrightness",0.1,100.0,0.1);
nuiDefSlider("DE_Interferix.Blend",  0.0,100.0,1.0);

//----------------------------------------------------------------

nuiDefSlider("DE_Electrofield.RedStrength",  0.1,100.0,0.1);
nuiDefSlider("DE_Electrofield.RedCenterX",0.0,width,1.0);
nuiDefSlider("DE_Electrofield.RedCenterY",0.0,height,1.0);
nuiAddPointOsc("DE_Electrofield.RedCenter"); 
nuiPushControlGroup("RedCenter");
	nuiGroupControl("DE_Electrofield.RedCenterX");
 	nuiGroupControl("DE_Electrofield.RedCenterY");
nuiPopControlGroup(); 
nuiDefSlider("DE_Electrofield.RedBrightness",0.1,100.0,0.1);
nuiDefSlider("DE_Electrofield.GreenStrength",0.1,100.0,0.1);
nuiDefSlider("DE_Electrofield.GreenCenterX",0.0,width,1.0);
nuiDefSlider("DE_Electrofield.GreenCenterY",0.0,height,1.0);
nuiAddPointOsc("DE_Electrofield.GreenCenter"); 
nuiPushControlGroup("GreenCenter");
	nuiGroupControl("DE_Electrofield.GreenCenterX");
 	nuiGroupControl("DE_Electrofield.GreenCenterY");
nuiPopControlGroup(); 
nuiDefSlider("DE_Electrofield.GreenBrightness",0.1,100.0,0.1);
nuiDefSlider("DE_Electrofield.BlueStrength", 0.1,100.0,0.1);
nuiDefSlider("DE_Electrofield.BlueCenterX",0.0,width,1.0);
nuiDefSlider("DE_Electrofield.BlueCenterY",0.0,height,1.0);
nuiAddPointOsc("DE_Electrofield.BlueCenter"); 
nuiPushControlGroup("BlueCenter");
	nuiGroupControl("DE_Electrofield.BlueCenterX");
 	nuiGroupControl("DE_Electrofield.BlueCenterY");
nuiPopControlGroup(); 
nuiDefSlider("DE_Electrofield.BlueBrightness",0.1,100.0,0.1);
nuiDefSlider("DE_Electrofield.Blend",  0.0,100.0,1.0);

//----------------------------------------------------------------

nuiDefSlider("DE_EarthQuake.HorizVibration",0.0,width,1.0);
nuiDefSlider("DE_EarthQuake.VertVibration",0.0,height,1.0);
nuiDefSlider("DE_EarthQuake.Blend",  0.0,100.0,1.0);

//----------------------------------------------------------------


nuiDefSlider("DE_ColorSpotlights.Red",  0.1,100.0,0.1);
nuiDefSlider("DE_ColorSpotlights.RedCenterX",0.0,width,1.0);
nuiDefSlider("DE_ColorSpotlights.RedCenterY",0.0,height,1.0);
nuiAddPointOsc("DE_ColorSpotlights.RedCenter"); 
nuiPushControlGroup("RedCenter");
	nuiGroupControl("DE_ColorSpotlights.RedCenterX");
 	nuiGroupControl("DE_ColorSpotlights.RedCenterY");
nuiPopControlGroup(); 
nuiDefSlider("DE_ColorSpotlights.RedBrightness",0.1,100.0,0.1);
nuiDefSlider("DE_ColorSpotlights.Green",0.1,100.0,0.1);
nuiDefSlider("DE_ColorSpotlights.GreenCenterX",0.0,width,1.0);
nuiDefSlider("DE_ColorSpotlights.GreenCenterY",0.0,height,1.0);
nuiAddPointOsc("DE_ColorSpotlights.GreenCenter"); 
nuiPushControlGroup("GreenCenter");
	nuiGroupControl("DE_ColorSpotlights.GreenCenterX");
 	nuiGroupControl("DE_ColorSpotlights.GreenCenterY");
nuiPopControlGroup(); 
nuiDefSlider("DE_ColorSpotlights.GreenBrightness",0.1,100.0,0.1);
nuiDefSlider("DE_ColorSpotlights.Blue", 0.1,100.0,0.1);
nuiDefSlider("DE_ColorSpotlights.BlueCenterX",0.0,width,1.0);
nuiDefSlider("DE_ColorSpotlights.BlueCenterY",0.0,height,1.0);
nuiAddPointOsc("DE_ColorSpotlights.BlueCenter"); 
nuiPushControlGroup("BlueCenter");
	nuiGroupControl("DE_ColorSpotlights.BlueCenterX");
 	nuiGroupControl("DE_ColorSpotlights.BlueCenterY");
nuiPopControlGroup(); 
nuiDefSlider("DE_ColorSpotlights.BlueBrightness",0.1,100.0,0.1);
nuiDefSlider("DE_ColorSpotlights.Blend",  0.0,100.0,1.0);

//----------------------------------------------------------------

nuiPushControlGroup("DE_3DLighting.Light Color");
    nuiGroupControl("DE_3DLighting.rLight");
    nuiGroupControl("DE_3DLighting.gLight");
    nuiGroupControl("DE_3DLighting.bLight");
nuiPopControlGroup();
nuiPushControlWidget("DE_3DLighting.Light Color", nuiConnectColorPCtrl());
nuiDefSlider("DE_3DLighting.LightX",0.0,width,1.0);
nuiDefSlider("DE_3DLighting.LightY",0.0,height,1.0);
nuiAddPointOsc("DE_3DLighting.Light"); 
nuiPushControlGroup("Light");
	nuiGroupControl("DE_3DLighting.LightX");
 	nuiGroupControl("DE_3DLighting.LightY");
nuiPopControlGroup(); 
nuiDefSlider("DE_3DLighting.LightHeight",  0.1,1000.0,1.0);
nuiPushControlGroup("DE_3DLighting.Gloss Color");
    nuiGroupControl("DE_3DLighting.rGloss");
    nuiGroupControl("DE_3DLighting.gGloss");
    nuiGroupControl("DE_3DLighting.bGloss");
nuiPopControlGroup();
nuiPushControlWidget("DE_3DLighting.Gloss Color", nuiConnectColorPCtrl());
nuiPushControlGroup("DE_3DLighting.Diffuse Color");
    nuiGroupControl("DE_3DLighting.rDiffuse");
    nuiGroupControl("DE_3DLighting.gDiffuse");
    nuiGroupControl("DE_3DLighting.bDiffuse");
nuiPopControlGroup();
nuiPushControlWidget("DE_3DLighting.Diffuse Color", nuiConnectColorPCtrl());
nuiPushControlGroup("DE_3DLighting.Specular Color");
    nuiGroupControl("DE_3DLighting.rSpecular");
    nuiGroupControl("DE_3DLighting.gSpecular");
    nuiGroupControl("DE_3DLighting.bSpecular");
nuiPopControlGroup();
nuiPushControlWidget("DE_3DLighting.Specular Color", nuiConnectColorPCtrl());
nuiDefSlider("DE_3DLighting.Relief",  0.1,1000.0,1.0);
nuiDefSlider("DE_3DLighting.Smoothness",  0.1,10.0,1.0);
nuiDefSlider("DE_3DLighting.Blend",  0.1,100.0,1.0);

//----------------------------------------------------------------

nuiDefSlider("DE_AgedFilm.FilmResponse",0.0,100.0,1.0);
nuiDefSlider("DE_AgedFilm.GrainAmount",0.0,100.0,1.0);
nuiDefSlider("DE_AgedFilm.DustSize",0.0,100.0,1.0);
nuiDefSlider("DE_AgedFilm.DustAmount",1.0,100.0,1.0);
nuiPushControlGroup("DE_AgedFilm.Dust Color");
    nuiGroupControl("DE_AgedFilm.rDust");
    nuiGroupControl("DE_AgedFilm.gDust");
    nuiGroupControl("DE_AgedFilm.bDust");
nuiPopControlGroup();
nuiPushControlWidget("DE_AgedFilm.Dust Color", nuiConnectColorPCtrl());
nuiDefSlider("DE_AgedFilm.HairSize",0.0,100.0,1.0);
nuiDefSlider("DE_AgedFilm.HairAmount",1.0,100.0,1.0);
nuiPushControlGroup("DE_AgedFilm.Hair Color");
    nuiGroupControl("DE_AgedFilm.rHair");
    nuiGroupControl("DE_AgedFilm.gHair");
    nuiGroupControl("DE_AgedFilm.bHair");
nuiPopControlGroup();
nuiPushControlWidget("DE_AgedFilm.Hair Color", nuiConnectColorPCtrl());
nuiDefSlider("DE_AgedFilm.ScratchAmount",0.0,100.0,1.0);
nuiDefSlider("DE_AgedFilm.ScratchVelocity",1.0,100.0,1.0);
nuiDefSlider("DE_AgedFilm.ScratchLifespan",0.0,100.0,1.0);
nuiDefSlider("DE_AgedFilm.ScratchOpacity",1.0,100.0,1.0);
nuiDefSlider("DE_AgedFilm.FrameJitterMaxOffset",0.0,100.0,1.0);
nuiDefSlider("DE_AgedFilm.FrameJitterProbability",0.0,100.0,1.0);
nuxDefExprToggle("DE_AgedFilm.ConvertToGray");
nuiPushControlGroup("DE_AgedFilm.Gray Tint Color");
    nuiGroupControl("DE_AgedFilm.rGrayTint");
    nuiGroupControl("DE_AgedFilm.gGrayTint");
    nuiGroupControl("DE_AgedFilm.bGrayTint");
nuiPopControlGroup();
nuiPushControlWidget("DE_AgedFilm.Gray Tint Color", nuiConnectColorPCtrl());
nuiDefSlider("DE_AgedFilm.FlickerSpeed",0.0,100.0,1.0);
nuiDefSlider("DE_AgedFilm.FlickerAmount",1.0,100.0,1.0);
nuxDefExprToggle("DE_AgedFilm.RevealBlackLayer");
nuiDefSlider("DE_AgedFilm.RandomSeed",0,65536000,1);
nuiDefSlider("DE_AgedFilm.Blend",0.0,100.0,1.0);

//----------------------------------------------------------------

nuiDefSlider("DE_Blizzard.NumberOfFlakes",1,5000,1);
nuiDefSlider("DE_Blizzard.WindAmount",-50.0,50.0,1.0);
nuiDefSlider("DE_Blizzard.Gravity",0.0,50.0,1.0);
nuiDefSlider("DE_Blizzard.LiftAmount",0.0,100.0,1.0);
nuiDefSlider("DE_Blizzard.LiftFrequency",0.0,100.0,1.0);
nuiPushControlGroup("DE_Blizzard.Flake Color");
    nuiGroupControl("DE_Blizzard.rFlake");
    nuiGroupControl("DE_Blizzard.gFlake");
    nuiGroupControl("DE_Blizzard.bFlake");
nuiPopControlGroup();
nuiPushControlWidget("DE_Blizzard.Flake Color", nuiConnectColorPCtrl());
nuiDefSlider("DE_Blizzard.MinSize",0.0,100.0,1.0);
nuiDefSlider("DE_Blizzard.MaxSize",0.0,100.0,1.0);
nuxDefExprToggle("DE_Blizzard.BrightenOrSolid");
nuiDefSlider("DE_Blizzard.Opacity",0.0,100.0,1.0);
nuiDefSlider("DE_Blizzard.Blend",0.0,100.0,1.0);

//----------------------------------------------------------------

nuiDefSlider("DE_BumpMaker.Direction",0.0,360.0,1.0);
nuiDefSlider("DE_BumpMaker.Elevation",0.1,100.0,0.1);
nuiDefSlider("DE_BumpMaker.Height",0.1,100.0,1.0);
nuiDefSlider("DE_BumpMaker.Grain",0.0,100.0,1.0);
nuiDefSlider("DE_BumpMaker.RandomSeed",1.0,32000.0,1.0);
nuiDefSlider("DE_BumpMaker.Smoothness",0.1,100.0,1.0);
nuiPushControlGroup("DE_BumpMaker.Tint Color");
    nuiGroupControl("DE_BumpMaker.rTint");
    nuiGroupControl("DE_BumpMaker.gTint");
    nuiGroupControl("DE_BumpMaker.bTint");
nuiPopControlGroup();
nuiPushControlWidget("DE_BumpMaker.Tint Color", nuiConnectColorPCtrl());
nuiDefSlider("DE_BumpMaker.Blend",0.0,100.0,1.0);

//----------------------------------------------------------------

nuiDefSlider("DE_EdgeX.RedEdge",0.0,255.0,1.0);
nuiDefSlider("DE_EdgeX.RedSoftness",0.0,64.0,1.0);
nuiDefSlider("DE_EdgeX.GreenEdge",0.0,255.0,1.0);
nuiDefSlider("DE_EdgeX.GreenSoftness",0.0,64.0,1.0);
nuiDefSlider("DE_EdgeX.BlueEdge",0.0,255.0,1.0);
nuiDefSlider("DE_EdgeX.BlueSoftness",0.0,64.0,1.0);
nuiDefSlider("DE_EdgeX.Blend",0.0,100.0,1.0);

//----------------------------------------------------------------

nuiDefSlider("DE_VideoLook.EvenBlend",0.1,100.0,0.1);
nuiDefSlider("DE_VideoLook.OddBlend",0.1,100.0,0.1);
nuiDefSlider("DE_VideoLook.EvenBrightness",0.1,100.0,0.1);
nuiDefSlider("DE_VideoLook.OddBrightness",0.1,100.0,0.1);
nuiDefSlider("DE_VideoLook.EvenNoise",0.1,100.0,0.1);
nuiDefSlider("DE_VideoLook.OddNoise",0.1,100.0,0.1);
nuxDefExprToggle("DE_VideoLook.EvenColorizeNoise");
nuxDefExprToggle("DE_VideoLook.OddColorizeNoise");
nuiDefSlider("DE_VideoLook.EvenSize",1,10,1);
nuiDefSlider("DE_VideoLook.OddSize",1,10,1);
nuxDefMultiChoice("DE_VideoLook.EvenDecayAmount", "Level 1|Level 2|Level 3|Level 4|Level 5");
nuxDefMultiChoice("DE_VideoLook.OddDecayAmount", "Level 1|Level 2|Level 3|Level 4|Level 5");
nuxDefExprToggle("DE_VideoLook.EvenAltBleed");
nuxDefExprToggle("DE_VideoLook.OddAltBleed");
nuxDefMultiChoice("DE_VideoLook.EvenDistortionMethod", "Shift|Linear|Noisy|Sawtooth|Wave");
nuxDefMultiChoice("DE_VideoLook.OddDistortionMethod", "Shift|Linear|Noisy|Sawtooth|Wave");
nuiDefSlider("DE_VideoLook.EvenHorizontalShift",0.1,100.0,0.1);
nuiDefSlider("DE_VideoLook.OddHorizontalShift",0.1,100.0,0.1);
nuxDefMultiChoice("DE_VideoLook.EvenCombMask", "Off|Mode 1|Mode 2|Mode 3|Mode 4");
nuxDefMultiChoice("DE_VideoLook.OddCombMask", "Off|Mode 1|Mode 2|Mode 3|Mode 4");
nuxDefExprToggle("DE_VideoLook.EvenGrayConvert");
nuxDefExprToggle("DE_VideoLook.OddGrayConvert");
nuiDefSlider("DE_VideoLook.RedEdge",0.0,255.0,1.0);
nuiPushControlGroup("DE_VideoLook.Even Gray Tint Color");
    nuiGroupControl("DE_VideoLook.rEvenGrayTint");
    nuiGroupControl("DE_VideoLook.gEvenGrayTint");
    nuiGroupControl("DE_VideoLook.bEvenGrayTint");
nuiPopControlGroup();
nuiPushControlWidget("DE_VideoLook.Even Gray Tint Color", nuiConnectColorPCtrl());
nuiPushControlGroup("DE_VideoLook.Odd Gray Tint Color");
    nuiGroupControl("DE_VideoLook.rOddGrayTint");
    nuiGroupControl("DE_VideoLook.gOddGrayTint");
    nuiGroupControl("DE_VideoLook.bOddGrayTint");
nuiPopControlGroup();
nuiPushControlWidget("DE_VideoLook.Odd Gray Tint Color", nuiConnectColorPCtrl());
nuiDefSlider("DE_VideoLook.TimeVaryValue",0.1,100.0,1.0);
nuiDefSlider("DE_VideoLook.Blend",0.0,100.0,1.0);

//----------------------------------------------------------------

nuiDefSlider("DE_Tilos.size",0.0,100.0,1.0);
nuiDefSlider("DE_Tilos.TileOffsetX",0,width,1.0);
nuiDefSlider("DE_Tilos.TileOffsetY",0,height,1.0);
nuiAddPointOsc("DE_Tilos.TileOffset"); 
nuiPushControlGroup("TileOffset");
	nuiGroupControl("DE_Tilos.TileOffsetX");
 	nuiGroupControl("DE_Tilos.TileOffsetY");
nuiPopControlGroup();
nuiDefSlider("DE_Tilos.Blend",0.0,100.0,1.0);

//----------------------------------------------------------------

nuiDefSlider("DE_StarField.NumberOfStars",1.0,500.0,1.0);
nuiDefSlider("DE_StarField.RandomSeed",1.0,32000.0,1.0);
nuiDefSlider("DE_StarField.Speed",1.0,100.0,1.0);
nuiDefSlider("DE_StarField.Twist",-100.0,100.0,1.0);
nuiDefSlider("DE_StarField.WarpCenterX",0.0,width,1.0);
nuiDefSlider("DE_StarField.WarpCenterY",0.0,height,1.0);
nuiAddPointOsc("DE_StarField.WarpCenter"); 
nuiPushControlGroup("WarpCenter");
	nuiGroupControl("DE_StarField.WarpCenterX");
 	nuiGroupControl("DE_StarField.WarpCenterY");
nuiPopControlGroup();
nuxDefExprToggle("DE_StarField.RandomColor");
nuxDefExprToggle("DE_StarField.ColorStars");
nuiPushControlGroup("DE_StarField.Min Color");
    nuiGroupControl("DE_StarField.rMinColor");
    nuiGroupControl("DE_StarField.gMinColor");
    nuiGroupControl("DE_StarField.bMinColor");
nuiPopControlGroup();
nuiPushControlWidget("DE_StarField.Min Color", nuiConnectColorPCtrl());
nuiPushControlGroup("DE_StarField.Max Color");
    nuiGroupControl("DE_StarField.rMaxColor");
    nuiGroupControl("DE_StarField.gMaxColor");
    nuiGroupControl("DE_StarField.bMaxColor");
nuiPopControlGroup();
nuiPushControlWidget("DE_StarField.Max Color", nuiConnectColorPCtrl());
nuiDefSlider("DE_StarField.Blend",0.0,100.0,1.0);

//----------------------------------------------------------------

nuiDefSlider("TC_Shine.Threshold",0,255,1);
nuxDefExprToggle("TC_Shine.UseMask");
nuiDefSlider("TC_Shine.MaskRadius",0.0,500.0,1.0);
nuiDefSlider("TC_Shine.MaskFeather",0.0,500.0,1.0);
nuiPushControlGroup("TC_Shine.Pre Process");
	nuiGroupControl("TC_Shine.Threshold");
	nuiGroupControl("TC_Shine.UseMask");
	nuiGroupControl("TC_Shine.MaskRadius");
	nuiGroupControl("TC_Shine.MaskFeather");
nuiPopControlGroup();

nuiDefSlider("TC_Shine.SourceX",0.0,width,1.0);
nuiDefSlider("TC_Shine.SourceY",0.0,height,1.0);
nuiAddPointOsc("TC_Shine.Source"); 
nuiPushControlGroup("Source");
	nuiGroupControl("TC_Shine.SourceX");
 	nuiGroupControl("TC_Shine.SourceY");
nuiPopControlGroup();

nuiDefSlider("TC_Shine.RayLength",0.0,10.0,0.1);

nuiDefSlider("TC_Shine.Amount",0.0,400.0,1.0);
nuiDefSlider("TC_Shine.Detail",0.0,50.0,1.0);
nuxDefExprToggle("TC_Shine.SourceAffect");
nuiDefSlider("TC_Shine.Radius",1.0,100.0,1.0);
nuxDefExprToggle("TC_Shine.ReduceFlickering");
nuiDefSlider("TC_Shine.Phase",0.0,360.0,1.0);
nuxDefExprToggle("TC_Shine.UseLoop");
nuiDefSlider("TC_Shine.Revolutions",1,10,1);
nuiPushControlGroup("TC_Shine.Shimmer");
	nuiGroupControl("TC_Shine.Amount");
	nuiGroupControl("TC_Shine.Detail");
	nuiGroupControl("TC_Shine.SourceAffect");
	nuiGroupControl("TC_Shine.Radius");
	nuiGroupControl("TC_Shine.ReduceFlickering");
	nuiGroupControl("TC_Shine.Phase");
	nuiGroupControl("TC_Shine.UseLoop");
	nuiGroupControl("TC_Shine.Revolutions");
nuiPopControlGroup();

nuiDefSlider("TC_Shine.BoostLight",0.0,10.0,0.1);

nuxDefMultiChoice("TC_Shine.Colorize", "None|One Color|3 Color Gradient|5 Color Gradient|Fire|Mars|Chemistry|DeepSea|Electric|Spirit|Aura|Heaven|Romance|Magic|USA|Rastafari|Enlightenment|Radioaktiv|IR Vision|Lysergic|Rainbow|RGB|Technicolor|Chess|Pastell|Desert Sun");
nuxDefMultiChoice("TC_Shine.BaseOn", "Lightness|Luminance|Alpha|Alpha Edges|Red|Green|Blue");
nuiPushControlGroup("TC_Shine.Highlight");
    nuiGroupControl("TC_Shine.rHighlight");
    nuiGroupControl("TC_Shine.gHighlight");
    nuiGroupControl("TC_Shine.bHighlight");
nuiPopControlGroup();
nuiPushControlWidget("TC_Shine.Highlight", nuiConnectColorPCtrl());
nuiPushControlGroup("TC_Shine.Mid High");
    nuiGroupControl("TC_Shine.rMidHigh");
    nuiGroupControl("TC_Shine.gMidHigh");
    nuiGroupControl("TC_Shine.bMidHigh");
nuiPopControlGroup();
nuiPushControlWidget("TC_Shine.Mid High", nuiConnectColorPCtrl());
nuiPushControlGroup("TC_Shine.Mid Tones");
    nuiGroupControl("TC_Shine.rMidTones");
    nuiGroupControl("TC_Shine.gMidTones");
    nuiGroupControl("TC_Shine.bMidTones");
nuiPopControlGroup();
nuiPushControlWidget("TC_Shine.Mid Tones", nuiConnectColorPCtrl());
nuiPushControlGroup("TC_Shine.Mid Low");
    nuiGroupControl("TC_Shine.rMidLow");
    nuiGroupControl("TC_Shine.gMidLow");
    nuiGroupControl("TC_Shine.bMidLow");
nuiPopControlGroup();
nuiPushControlWidget("TC_Shine.Mid Low", nuiConnectColorPCtrl());
nuiPushControlGroup("TC_Shine.Shadows");
    nuiGroupControl("TC_Shine.rShadows");
    nuiGroupControl("TC_Shine.gShadows");
    nuiGroupControl("TC_Shine.bShadows");
nuiPopControlGroup();
nuiPushControlWidget("TC_Shine.Shadows", nuiConnectColorPCtrl());
nuiDefSlider("TC_Shine.EdgeThickness",1,10,1);
nuiPushControlGroup("TC_Shine.Color");
	nuiGroupControl("TC_Shine.Colorize");
	nuiGroupControl("TC_Shine.BaseOn");
	nuiGroupControl("TC_Shine.Highlight");
	nuiGroupControl("TC_Shine.Mid High");
	nuiGroupControl("TC_Shine.Mid Tones");
	nuiGroupControl("TC_Shine.Mid Low");
	nuiGroupControl("TC_Shine.Shadows");
	nuiGroupControl("TC_Shine.EdgeThickness");
nuiPopControlGroup();

nuiDefSlider("TC_Shine.SourceOpacity",0.0,100.0,1.0);
nuiDefSlider("TC_Shine.ShineOpacity",0.0,100.0,1.0);
nuxDefMultiChoice("TC_Shine.TransferMode", "Normal|Add|Multiply|Screen|Overlay|SoftLight|HardLight|Darken|Lighten|Difference|Hue|Saturation|Color|Luminosity|ColorDodge|ColorBurn|Exclusion");

//----------------------------------------------------------------

nuxDefMultiChoice("TC_StarGlow.InputChannel","Lightness|Luminance|Alpha|Red|Green|Blue");

nuiDefSlider("TC_StarGlow.Threshold",0.0,255.0,1.0);
nuiDefSlider("TC_StarGlow.ThresholdSoft",0.0,100.0,1.0);
nuxDefExprToggle("TC_StarGlow.UseMask");
nuiDefSlider("TC_StarGlow.MaskRadius",0.0,300.0,1.0);
nuiDefSlider("TC_StarGlow.MaskFeather",0.0,10.0,1.0);
nuiDefSlider("TC_StarGlow.MaskPosX",0.0,width,1.0);
nuiDefSlider("TC_StarGlow.MaskPosY",0.0,height,1.0);
nuiPushControlGroup("TC_StarGlow.Pre Process");
	nuiGroupControl("TC_StarGlow.Threshold");
	nuiGroupControl("TC_StarGlow.ThresholdSoft");
	nuiGroupControl("TC_StarGlow.UseMask");
	nuiGroupControl("TC_StarGlow.MaskRadius");
	nuiGroupControl("TC_StarGlow.MaskFeather");
	nuiGroupControl("TC_StarGlow.MaskPosX");
	nuiGroupControl("TC_StarGlow.MaskPosY");
nuiPopControlGroup();


nuiDefSlider("TC_StarGlow.StreakLength",0.0,300.0,1.0);
nuiDefSlider("TC_StarGlow.BoostLight",0.0,10.0,1.0);

nuiDefSlider("TC_StarGlow.UpLength",0.0,2.0,0.1);
nuiDefSlider("TC_StarGlow.DownLength",0.0,2.0,0.1);
nuiDefSlider("TC_StarGlow.LeftLength",0.0,2.0,0.1);
nuiDefSlider("TC_StarGlow.RightLength",0.0,2.0,0.1);
nuiDefSlider("TC_StarGlow.UpLeftLength",0.0,2.0,0.1);
nuiDefSlider("TC_StarGlow.UpRightLength",0.0,2.0,0.1);
nuiDefSlider("TC_StarGlow.DownLeftLength",0.0,2.0,0.1);
nuiDefSlider("TC_StarGlow.DownRightLength",0.0,2.0,0.1);
nuiPushControlGroup("TC_StarGlow.Individual Lengths");
	nuiGroupControl("TC_StarGlow.UpLength");
	nuiGroupControl("TC_StarGlow.DownLength");
	nuiGroupControl("TC_StarGlow.LeftLength");
	nuiGroupControl("TC_StarGlow.RightLength");
	nuiGroupControl("TC_StarGlow.UpLeftLength");
	nuiGroupControl("TC_StarGlow.UpRightLength");
	nuiGroupControl("TC_StarGlow.DownLeftLength");
	nuiGroupControl("TC_StarGlow.DownRightLength");
nuiPopControlGroup();

nuxDefMultiChoice("TC_StarGlow.UpColor","ColorMap A|ColorMap B|ColorMap C");
nuxDefMultiChoice("TC_StarGlow.DownColor","ColorMap A|ColorMap B|ColorMap C");
nuxDefMultiChoice("TC_StarGlow.LeftColor","ColorMap A|ColorMap B|ColorMap C");
nuxDefMultiChoice("TC_StarGlow.RightColor","ColorMap A|ColorMap B|ColorMap C");
nuxDefMultiChoice("TC_StarGlow.UpLeftColor","ColorMap A|ColorMap B|ColorMap C");
nuxDefMultiChoice("TC_StarGlow.UpRightColor","ColorMap A|ColorMap B|ColorMap C");
nuxDefMultiChoice("TC_StarGlow.DownLeftColor","ColorMap A|ColorMap B|ColorMap C");
nuxDefMultiChoice("TC_StarGlow.DownRightColor","ColorMap A|ColorMap B|ColorMap C");
nuiPushControlGroup("TC_StarGlow.Individual Colors");
	nuiGroupControl("TC_StarGlow.UpColor");
	nuiGroupControl("TC_StarGlow.DownColor");
	nuiGroupControl("TC_StarGlow.LeftColor");
	nuiGroupControl("TC_StarGlow.RightColor");
	nuiGroupControl("TC_StarGlow.UpLeftColor");
	nuiGroupControl("TC_StarGlow.UpRightColor");
	nuiGroupControl("TC_StarGlow.DownLeftColor");
	nuiGroupControl("TC_StarGlow.DownRightColor");
nuiPopControlGroup();

nuxDefMultiChoice("TC_StarGlow.ColorMapAType", "One Color|3 Color Gradient|5 Color Gradient|Fire|Mars|Chemistry|DeepSea|Electric|Spirit|Aura|Heaven|Romance|Magic|USA|Rastafari|Enlightenment|Radioaktiv|IR Vision|Lysergic|Rainbow|RGB|Technicolor|Chess|Pastell|Desert Sun");
nuiPushControlGroup("TC_StarGlow.HighlightA");
    nuiGroupControl("TC_StarGlow.rHighlightA");
    nuiGroupControl("TC_StarGlow.gHighlightA");
    nuiGroupControl("TC_StarGlow.bHighlightA");
nuiPopControlGroup();
nuiPushControlWidget("TC_StarGlow.HighlightA", nuiConnectColorPCtrl());
nuiPushControlGroup("TC_StarGlow.Mid HighA");
    nuiGroupControl("TC_StarGlow.rMidHighA");
    nuiGroupControl("TC_StarGlow.gMidHighA");
    nuiGroupControl("TC_StarGlow.bMidHighA");
nuiPopControlGroup();
nuiPushControlWidget("TC_StarGlow.Mid HighA", nuiConnectColorPCtrl());
nuiPushControlGroup("TC_StarGlow.Mid TonesA");
    nuiGroupControl("TC_StarGlow.rMidTonesA");
    nuiGroupControl("TC_StarGlow.gMidTonesA");
    nuiGroupControl("TC_StarGlow.bMidTonesA");
nuiPopControlGroup();
nuiPushControlWidget("TC_StarGlow.Mid TonesA", nuiConnectColorPCtrl());
nuiPushControlGroup("TC_StarGlow.Mid LowA");
    nuiGroupControl("TC_StarGlow.rMidLowA");
    nuiGroupControl("TC_StarGlow.gMidLowA");
    nuiGroupControl("TC_StarGlow.bMidLowA");
nuiPopControlGroup();
nuiPushControlWidget("TC_StarGlow.Mid LowA", nuiConnectColorPCtrl());
nuiPushControlGroup("TC_StarGlow.ShadowsA");
    nuiGroupControl("TC_StarGlow.rShadowsA");
    nuiGroupControl("TC_StarGlow.gShadowsA");
    nuiGroupControl("TC_StarGlow.bShadowsA");
nuiPopControlGroup();
nuiPushControlWidget("TC_StarGlow.ShadowsA", nuiConnectColorPCtrl());
nuiPushControlGroup("TC_StarGlow.ColorMap A");
	nuiGroupControl("TC_StarGlow.ColorMapAType");
	nuiGroupControl("TC_StarGlow.HighlightA");
	nuiGroupControl("TC_StarGlow.Mid HighA");
	nuiGroupControl("TC_StarGlow.Mid TonesA");
	nuiGroupControl("TC_StarGlow.Mid LowA");
	nuiGroupControl("TC_StarGlow.ShadowsA");
nuiPopControlGroup();

nuxDefMultiChoice("TC_StarGlow.ColorMapBType", "One Color|3 Color Gradient|5 Color Gradient|Fire|Mars|Chemistry|DeepSea|Electric|Spirit|Aura|Heaven|Romance|Magic|USA|Rastafari|Enlightenment|Radioaktiv|IR Vision|Lysergic|Rainbow|RGB|Technicolor|Chess|Pastell|Desert Sun");
nuiPushControlGroup("TC_StarGlow.HighlightB");
    nuiGroupControl("TC_StarGlow.rHighlightB");
    nuiGroupControl("TC_StarGlow.gHighlightB");
    nuiGroupControl("TC_StarGlow.bHighlightB");
nuiPopControlGroup();
nuiPushControlWidget("TC_StarGlow.HighlightB", nuiConnectColorPCtrl());
nuiPushControlGroup("TC_StarGlow.Mid HighB");
    nuiGroupControl("TC_StarGlow.rMidHighB");
    nuiGroupControl("TC_StarGlow.gMidHighB");
    nuiGroupControl("TC_StarGlow.bMidHighB");
nuiPopControlGroup();
nuiPushControlWidget("TC_StarGlow.Mid HighB", nuiConnectColorPCtrl());
nuiPushControlGroup("TC_StarGlow.Mid TonesB");
    nuiGroupControl("TC_StarGlow.rMidTonesB");
    nuiGroupControl("TC_StarGlow.gMidTonesB");
    nuiGroupControl("TC_StarGlow.bMidTonesB");
nuiPopControlGroup();
nuiPushControlWidget("TC_StarGlow.Mid TonesB", nuiConnectColorPCtrl());
nuiPushControlGroup("TC_StarGlow.Mid LowB");
    nuiGroupControl("TC_StarGlow.rMidLowB");
    nuiGroupControl("TC_StarGlow.gMidLowB");
    nuiGroupControl("TC_StarGlow.bMidLowB");
nuiPopControlGroup();
nuiPushControlWidget("TC_StarGlow.Mid LowB", nuiConnectColorPCtrl());
nuiPushControlGroup("TC_StarGlow.ShadowsB");
    nuiGroupControl("TC_StarGlow.rShadowsB");
    nuiGroupControl("TC_StarGlow.gShadowsB");
    nuiGroupControl("TC_StarGlow.bShadowsB");
nuiPopControlGroup();
nuiPushControlWidget("TC_StarGlow.ShadowsB", nuiConnectColorPCtrl());
nuiPushControlGroup("TC_StarGlow.ColorMap B");
	nuiGroupControl("TC_StarGlow.ColorMapBType");
	nuiGroupControl("TC_StarGlow.HighlightB");
	nuiGroupControl("TC_StarGlow.Mid HighB");
	nuiGroupControl("TC_StarGlow.Mid TonesB");
	nuiGroupControl("TC_StarGlow.Mid LowB");
	nuiGroupControl("TC_StarGlow.ShadowsB");
nuiPopControlGroup();

nuxDefMultiChoice("TC_StarGlow.ColorMapCType", "One Color|3 Color Gradient|5 Color Gradient|Fire|Mars|Chemistry|DeepSea|Electric|Spirit|Aura|Heaven|Romance|Magic|USA|Rastafari|Enlightenment|Radioaktiv|IR Vision|Lysergic|Rainbow|RGB|Technicolor|Chess|Pastell|Desert Sun");
nuiPushControlGroup("TC_StarGlow.HighlightC");
    nuiGroupControl("TC_StarGlow.rHighlightC");
    nuiGroupControl("TC_StarGlow.gHighlightC");
    nuiGroupControl("TC_StarGlow.bHighlightC");
nuiPopControlGroup();
nuiPushControlWidget("TC_StarGlow.HighlightC", nuiConnectColorPCtrl());
nuiPushControlGroup("TC_StarGlow.Mid HighC");
    nuiGroupControl("TC_StarGlow.rMidHighC");
    nuiGroupControl("TC_StarGlow.gMidHighC");
    nuiGroupControl("TC_StarGlow.bMidHighC");
nuiPopControlGroup();
nuiPushControlWidget("TC_StarGlow.Mid HighC", nuiConnectColorPCtrl());
nuiPushControlGroup("TC_StarGlow.Mid TonesC");
    nuiGroupControl("TC_StarGlow.rMidTonesC");
    nuiGroupControl("TC_StarGlow.gMidTonesC");
    nuiGroupControl("TC_StarGlow.bMidTonesC");
nuiPopControlGroup();
nuiPushControlWidget("TC_StarGlow.Mid TonesC", nuiConnectColorPCtrl());
nuiPushControlGroup("TC_StarGlow.Mid LowC");
    nuiGroupControl("TC_StarGlow.rMidLowC");
    nuiGroupControl("TC_StarGlow.gMidLowC");
    nuiGroupControl("TC_StarGlow.bMidLowC");
nuiPopControlGroup();
nuiPushControlWidget("TC_StarGlow.Mid LowC", nuiConnectColorPCtrl());
nuiPushControlGroup("TC_StarGlow.ShadowsC");
    nuiGroupControl("TC_StarGlow.rShadowsC");
    nuiGroupControl("TC_StarGlow.gShadowsC");
    nuiGroupControl("TC_StarGlow.bShadowsC");
nuiPopControlGroup();
nuiPushControlWidget("TC_StarGlow.ShadowsC", nuiConnectColorPCtrl());
nuiPushControlGroup("TC_StarGlow.ColorMap C");
	nuiGroupControl("TC_StarGlow.ColorMapCType");
	nuiGroupControl("TC_StarGlow.HighlightC");
	nuiGroupControl("TC_StarGlow.Mid HighC");
	nuiGroupControl("TC_StarGlow.Mid TonesC");
	nuiGroupControl("TC_StarGlow.Mid LowC");
	nuiGroupControl("TC_StarGlow.ShadowsC");
nuiPopControlGroup();

nuiDefSlider("TC_StarGlow.Amount",0.0,400.0,1.0);
nuiDefSlider("TC_StarGlow.Detail",0.0,50.0,1.0);
nuiDefSlider("TC_StarGlow.Phase",0.0,360.0,1.0);
nuxDefExprToggle("TC_StarGlow.UseLoop");
nuiDefSlider("TC_StarGlow.Revolutions",1,10,1);
nuiPushControlGroup("TC_StarGlow.Shimmer");
	nuiGroupControl("TC_StarGlow.Amount");
	nuiGroupControl("TC_StarGlow.Detail");
	nuiGroupControl("TC_StarGlow.Phase");
	nuiGroupControl("TC_StarGlow.UseLoop");
	nuiGroupControl("TC_StarGlow.Revolutions");
nuiPopControlGroup();

nuiDefSlider("TC_StarGlow.SourceOpacity",0.0,100.0,1.0);
nuiDefSlider("TC_StarGlow.StarglowOpacity",0.0,100.0,1.0);
nuxDefMultiChoice("TC_StarGlow.TransferMode", "Normal|Add|Multiply|Screen|Overlay|SoftLight|HardLight|Darken|Lighten|Difference|Hue|Saturation|Color|Luminosity|ColorDodge|ColorBurn|Exclusion");

//----------------------------------------------------------------

nuiDefSlider("DF_FeedBack.CenterX",0.0,width,1.0);
nuiDefSlider("DF_FeedBack.CenterY",0.0,height,1.0);
nuiAddPointOsc("DF_FeedBack.Center"); 
nuiPushControlGroup("Center");
	nuiGroupControl("DF_FeedBack.CenterX");
 	nuiGroupControl("DF_FeedBack.CenterY");
nuiPopControlGroup();
nuiDefSlider("DF_FeedBack.Velocity",0.0,100.0,1.0);
nuiDefSlider("DF_FeedBack.Opacity",0.0,100.0,1.0);
nuiDefSlider("DF_FeedBack.Iterations",1,50,1);
nuxDefExprToggle("DF_FeedBack.Flip");
nuxDefExprToggle("DF_FeedBack.Reverse");
nuxDefExprToggle("DF_FeedBack.UseMatte");

//----------------------------------------------------------------

nuiDefSlider("AE_CheckerBoard.xCenter",0.0,width,1.0);
nuiDefSlider("AE_CheckerBoard.yCenter",0.0,height,1.0);
nuiAddPointOsc("AE_CheckerBoard.Center"); 
nuiPushControlGroup("Center");
	nuiGroupControl("AE_CheckerBoard.xCenter");
 	nuiGroupControl("AE_CheckerBoard.yCenter");
nuiPopControlGroup(); 
nuxDefMultiChoice("AE_CheckerBoard.SizeFrom", "Corner Point|Width Sliders|Width & Height Sliders");
nuiDefSlider("AE_CheckerBoard.xCorner",0.0,width,1.0);
nuiDefSlider("AE_CheckerBoard.yCorner",0.0,height,1.0);
nuiAddPointOsc("AE_CheckerBoard.Corner"); 
nuiPushControlGroup("Corner");
	nuiGroupControl("AE_CheckerBoard.xCorner");
 	nuiGroupControl("AE_CheckerBoard.yCorner");
nuiPopControlGroup(); 
nuiDefSlider("AE_CheckerBoard.CheckerWidth",2.0,200.0,1.0);
nuiDefSlider("AE_CheckerBoard.CheckerHeight",2.0,200.0,1.0);
nuiDefSlider("AE_CheckerBoard.FeatherWidth",0.0,20.0,1.0);
nuiDefSlider("AE_CheckerBoard.FeatherHeight",0.0,20.0,1.0);
nuiPushControlGroup("AE_CheckerBoard.Feather");
    nuiGroupControl("AE_CheckerBoard.FeatherWidth");
    nuiGroupControl("AE_CheckerBoard.FeatherHeight");
nuiPopControlGroup();
nuiPushControlGroup("AE_CheckerBoard.Checker Color");
    nuiGroupControl("AE_CheckerBoard.rColor");
    nuiGroupControl("AE_CheckerBoard.gColor");
    nuiGroupControl("AE_CheckerBoard.bColor");
nuiPopControlGroup();
nuiPushControlWidget("AE_CheckerBoard.Checker Color", nuiConnectColorPCtrl());
nuiDefSlider("AE_CheckerBoard.Opacity",0.0,100.0,1.0);
nuxDefMultiChoice("AE_CheckerBoard.BlendingMode", "None|Over|Add|Mult|Screen|Overlay|SoftLight|HardLight|Darken|Lighten|Difference|Hue|Saturation|Color|Luminosity|ColorDodge|ColorBurn|Exclusion");

//----------------------------------------------------------------

nuiDefSlider("AE_EyeDropFill.SampleX",0.0,width,1.0);
nuiDefSlider("AE_EyeDropFill.SampleX",0.0,height,1.0);
nuiAddPointOsc("AE_EyeDropFill.Sample"); 
nuiPushControlGroup("Sample");
	nuiGroupControl("AE_EyeDropFill.SampleX");
 	nuiGroupControl("AE_EyeDropFill.SampleY");
nuiPopControlGroup(); 
nuxDefExprToggle("AE_EyeDropFill.MaintainAlpha");
nuiDefSlider("AE_EyeDropFill.Blend",0.0,100.0,1.0);

//----------------------------------------------------------------

nuxDefMultiChoice("AE_Magnify.Shape", "Circle|Square");
nuiDefSlider("AE_Magnify.CenterX",0.0,width,1.0);
nuiDefSlider("AE_Magnify.CenterY",0.0,height,1.0);
nuiAddPointOsc("AE_Magnify.Center"); 
nuiPushControlGroup("Center");
	nuiGroupControl("AE_Magnify.CenterX");
 	nuiGroupControl("AE_Magnify.CenterY");
nuiPopControlGroup(); 
nuiDefSlider("AE_Magnify.Magnification",100.0,600.0,1.0);
nuxDefMultiChoice("AE_Magnify.Link", "None|Size To Magnification|Size & Feather To Magnification");
nuiDefSlider("AE_Magnify.Size",10.0,600.0,1.0);
nuiDefSlider("AE_Magnify.Feather",0.0,50.0,1.0);
nuiDefSlider("AE_Magnify.Opacity",0.0,100.0,1.0);
nuxDefMultiChoice("AE_Magnify.BlendingMode", "None|Over|Add|Mult|Screen|Overlay|SoftLight|HardLight|Darken|Lighten|Difference|Hue|Saturation|Color|Luminosity|ColorDodge|ColorBurn|Exclusion");

//----------------------------------------------------------------

nuiDefSlider("AE_ChangeColor.HueTransform",-180.0,180.0,1.0);
nuiDefSlider("AE_ChangeColor.LightnessTransform",-100.0,100.0,1.0);
nuiDefSlider("AE_ChangeColor.SaturationTransform",-100.0,100.0,1.0);
nuiPushControlGroup("AE_ChangeColor.Matching Color");
    nuiGroupControl("AE_ChangeColor.rColor");
    nuiGroupControl("AE_ChangeColor.gColor");
    nuiGroupControl("AE_ChangeColor.bColor");
nuiPopControlGroup();
nuiPushControlWidget("AE_ChangeColor.Matching Color", nuiConnectColorPCtrl());
nuiDefSlider("AE_ChangeColor.MatchingTolerance",0.0,100.0,1.0);
nuiDefSlider("AE_ChangeColor.MatchingSoftness",0.0,100.0,1.0);
nuxDefExprToggle("AE_ChangeColor.InvertColorCorrectionMask");

//----------------------------------------------------------------

nuiPushControlGroup("AE_ChangeToColor.From");
    nuiGroupControl("AE_ChangeToColor.rFrom");
    nuiGroupControl("AE_ChangeToColor.gFrom");
    nuiGroupControl("AE_ChangeToColor.bFrom");
nuiPopControlGroup();
nuiPushControlWidget("AE_ChangeToColor.From", nuiConnectColorPCtrl());
nuiPushControlGroup("AE_ChangeToColor.To");
    nuiGroupControl("AE_ChangeToColor.rTo");
    nuiGroupControl("AE_ChangeToColor.gTo");
    nuiGroupControl("AE_ChangeToColor.bTo");
nuiPopControlGroup();
nuiPushControlWidget("AE_ChangeToColor.To", nuiConnectColorPCtrl());
nuxDefMultiChoice("AE_ChangeToColor.ChangeChannels", "Hue|Hue & Lightness|Hue & Saturation|Hue, Lightness & Saturation");
nuxDefMultiChoice("AE_ChangeToColor.ChangeBy", "Setting To Color|Transforming To Color");
nuiDefSlider("AE_ChangeToColor.HueTolerance",0.0,100.0,1.0);
nuiDefSlider("AE_ChangeToColor.LightnessTolerance",0.0,100.0,1.0);
nuiDefSlider("AE_ChangeToColor.SaturationTolerance",0.0,100.0,1.0);
nuiDefSlider("AE_ChangeToColor.Softness",0.0,100.0,1.0);
nuxDefExprToggle("AE_ChangeToColor.ViewCorrectionMatte");

//----------------------------------------------------------------

nuiDefSlider("AE_Circle.xCenter",0.0,width,1.0);
nuiDefSlider("AE_Circle.yCenter",0.0,height,1.0);
nuiAddPointOsc("AE_EyeDropFill.Center"); 
nuiPushControlGroup("Center");
	nuiGroupControl("AE_Circle.xCenter");
 	nuiGroupControl("AE_Circle.yCenter");
nuiPopControlGroup(); 
nuxDefMultiChoice("AE_Circle.Edge", "None|Edge Radius|Thickness|Thickness * Radius|Thickness&Feather * Radius");
nuiDefSlider("AE_Circle.Radius",0.0,600.0,1.0);
nuiDefSlider("AE_Circle.Thickness",0.0,600.0,1.0);
nuiDefSlider("AE_Circle.FeatherOuterEdge",0.0,100.0,1.0);
nuiDefSlider("AE_Circle.FeatherInnerEdge",0.0,100.0,1.0);
nuxDefExprToggle("AE_Circle.InvertCircle");
nuiPushControlGroup("AE_Circle.Color");
    nuiGroupControl("AE_Circle.rColor");
    nuiGroupControl("AE_Circle.gColor");
    nuiGroupControl("AE_Circle.bColor");
nuiPopControlGroup();
nuiPushControlWidget("AE_Circle.Color", nuiConnectColorPCtrl());
nuiDefSlider("AE_Circle.Opacity",0.0,100.0,1.0);
nuxDefMultiChoice("AE_Circle.BlendingMode", "None|Over|Add|Mult|Screen|Overlay|SoftLight|HardLight|Darken|Lighten|Difference|Hue|Saturation|Color|Luminosity|ColorDodge|ColorBurn|Exclusion");

//----------------------------------------------------------------

nuiDefSlider("AE_Explode.XTiles",3,100,1);
nuiDefSlider("AE_Explode.YTiles",3,100,1);
nuiDefSlider("AE_Explode.StartFrame",1,1000,1);
nuiDefSlider("AE_Explode.Gravity",1.0,100.0,0.1);
nuiDefSlider("AE_Explode.RotationSpeed",1.0,100.0,0.1);

//----------------------------------------------------------------

nuiDefSlider("SB_Cube.XPos",-1000.0,1000.0,1.0);
nuiDefSlider("SB_Cube.YPos",-1000.0,1000.0,1.0);
nuiDefSlider("SB_Cube.ZPos",-1000.0,1000.0,1.0);
nuiPushControlGroup("SB_Cube.Position");
	nuiGroupControl("SB_Cube.XPos");
	nuiGroupControl("SB_Cube.YPos");
	nuiGroupControl("SB_Cube.ZPos");
nuiPopControlGroup();

nuiDefSlider("SB_Cube.XAngle",0.0,360.0,1.0);
nuiDefSlider("SB_Cube.YAngle",0.0,360.0,1.0);
nuiDefSlider("SB_Cube.ZAngle",0.0,360.0,1.0);
nuiPushControlGroup("SB_Cube.Rotation");
	nuiGroupControl("SB_Cube.XAngle");
	nuiGroupControl("SB_Cube.YAngle");
	nuiGroupControl("SB_Cube.ZAngle");
nuiPopControlGroup();

nuiDefSlider("SB_Cube.Scale",1.0,100.0,1.0);
nuiPushControlGroup("SB_Cube.Transformation");
	nuiGroupControl("SB_Cube.Position");
	nuiGroupControl("SB_Cube.Rotation");
	nuiGroupControl("SB_Cube.Scale");
nuiPopControlGroup();

nuiPushControlGroup("SB_Cube.Color");
    nuiGroupControl("SB_Cube.RColor");
    nuiGroupControl("SB_Cube.GColor");
    nuiGroupControl("SB_Cube.BColor");
nuiPopControlGroup();
nuiPushControlWidget("SB_Cube.Color", nuiConnectColorPCtrl());

nuiDefSlider("SB_Cube.XLightVector",-1.0,1.0,0.1);
nuiDefSlider("SB_Cube.YLightVector",-1.0,1.0,0.1);
nuiDefSlider("SB_Cube.ZLightVector",-1.0,1.0,0.1);
nuiPushControlGroup("SB_Cube.LightVector");
	nuiGroupControl("SB_Cube.XLightVector");
	nuiGroupControl("SB_Cube.YLightVector");
	nuiGroupControl("SB_Cube.ZLightVector");
nuiPopControlGroup();

//----------------------------------------------------------------

nuiDefSlider("SB_Sphere.XPos",-1000.0,1000.0,1.0);
nuiDefSlider("SB_Sphere.YPos",-1000.0,1000.0,1.0);
nuiDefSlider("SB_Sphere.ZPos",-1000.0,1000.0,1.0);
nuiPushControlGroup("SB_Sphere.Position");
	nuiGroupControl("SB_Sphere.XPos");
	nuiGroupControl("SB_Sphere.YPos");
	nuiGroupControl("SB_Sphere.ZPos");
nuiPopControlGroup();

nuiDefSlider("SB_Sphere.XAngle",0.0,360.0,1.0);
nuiDefSlider("SB_Sphere.YAngle",0.0,360.0,1.0);
nuiDefSlider("SB_Sphere.ZAngle",0.0,360.0,1.0);
nuiPushControlGroup("SB_Sphere.Rotation");
	nuiGroupControl("SB_Sphere.XAngle");
	nuiGroupControl("SB_Sphere.YAngle");
	nuiGroupControl("SB_Sphere.ZAngle");
nuiPopControlGroup();

nuiDefSlider("SB_Sphere.radius",1.0,100.0,1.0);
nuiDefSlider("SB_Sphere.XSubdivisions",2,100,1);
nuiDefSlider("SB_Sphere.YSubdivisions",2,100,1);
nuiPushControlGroup("SB_Sphere.Transformation");
	nuiGroupControl("SB_Sphere.Position");
	nuiGroupControl("SB_Sphere.Rotation");
	nuiGroupControl("SB_Sphere.radius");
	nuiGroupControl("SB_Sphere.XSubdivisions");
	nuiGroupControl("SB_Sphere.YSubdivisions");
nuiPopControlGroup();

nuiPushControlGroup("SB_Sphere.Color");
    nuiGroupControl("SB_Sphere.RColor");
    nuiGroupControl("SB_Sphere.GColor");
    nuiGroupControl("SB_Sphere.BColor");
nuiPopControlGroup();
nuiPushControlWidget("SB_Sphere.Color", nuiConnectColorPCtrl());

nuiDefSlider("SB_Sphere.XLightVector",-1.0,1.0,0.1);
nuiDefSlider("SB_Sphere.YLightVector",-1.0,1.0,0.1);
nuiDefSlider("SB_Sphere.ZLightVector",-1.0,1.0,0.1);
nuiPushControlGroup("SB_Sphere.LightVector");
	nuiGroupControl("SB_Sphere.XLightVector");
	nuiGroupControl("SB_Sphere.YLightVector");
	nuiGroupControl("SB_Sphere.ZLightVector");
nuiPopControlGroup();

//----------------------------------------------------------------

nuiDefSlider("SB_Cylinder.XPos",-1000.0,1000.0,1.0);
nuiDefSlider("SB_Cylinder.YPos",-1000.0,1000.0,1.0);
nuiDefSlider("SB_Cylinder.ZPos",-1000.0,1000.0,1.0);
nuiPushControlGroup("SB_Cylinder.Position");
	nuiGroupControl("SB_Cylinder.XPos");
	nuiGroupControl("SB_Cylinder.YPos");
	nuiGroupControl("SB_Cylinder.ZPos");
nuiPopControlGroup();

nuiDefSlider("SB_Cylinder.XAngle",0.0,360.0,1.0);
nuiDefSlider("SB_Cylinder.YAngle",0.0,360.0,1.0);
nuiDefSlider("SB_Cylinder.ZAngle",0.0,360.0,1.0);
nuiPushControlGroup("SB_Cylinder.Rotation");
	nuiGroupControl("SB_Cylinder.XAngle");
	nuiGroupControl("SB_Cylinder.YAngle");
	nuiGroupControl("SB_Cylinder.ZAngle");
nuiPopControlGroup();

nuiDefSlider("SB_Cylinder.radius",1.0,100.0,1.0);
nuiDefSlider("SB_Cylinder.length",1.0,100.0,1.0);
nuiDefSlider("SB_Cylinder.Subdivisions",2,100,1);
nuiPushControlGroup("SB_Cylinder.Transformation");
	nuiGroupControl("SB_Cylinder.Position");
	nuiGroupControl("SB_Cylinder.Rotation");
	nuiGroupControl("SB_Cylinder.radius");
	nuiGroupControl("SB_Cylinder.length");
	nuiGroupControl("SB_Cylinder.Subdivisions");
nuiPopControlGroup();

nuiPushControlGroup("SB_Cylinder.Color");
    nuiGroupControl("SB_Cylinder.RColor");
    nuiGroupControl("SB_Cylinder.GColor");
    nuiGroupControl("SB_Cylinder.BColor");
nuiPopControlGroup();
nuiPushControlWidget("SB_Cylinder.Color", nuiConnectColorPCtrl());

nuiDefSlider("SB_Cylinder.XLightVector",-1.0,1.0,0.1);
nuiDefSlider("SB_Cylinder.YLightVector",-1.0,1.0,0.1);
nuiDefSlider("SB_Cylinder.ZLightVector",-1.0,1.0,0.1);
nuiPushControlGroup("SB_Cylinder.LightVector");
	nuiGroupControl("SB_Cylinder.XLightVector");
	nuiGroupControl("SB_Cylinder.YLightVector");
	nuiGroupControl("SB_Cylinder.ZLightVector");
nuiPopControlGroup();


//----------------------------------------------------------------
