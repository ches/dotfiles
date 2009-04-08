nuiPushToolBox("ProFlares");
	nuiToolBoxItem("Flare35mm",Flare35mm(0,0));
	nuiToolBoxItem("Flare50300mm",Flare50300mm(0,0));
	nuiToolBoxItem("Flare105mm",Flare105mm(0,0));
	nuiToolBoxItem("BrightFlare",BrightFlare(0,0));
	nuiToolBoxItem("HotFlare",HotFlare(0,0));
	nuiToolBoxItem("LaserFlare",LaserFlare(0,0));
	nuiToolBoxItem("LaserFlare2",LaserFlare2(0,0));
	nuiToolBoxItem("RainbowFlare",RainbowFlare(0,0));
	nuiToolBoxItem("RayStarFlare",RayStarFlare(0,0));
	nuiToolBoxItem("RockyFlare",RockyFlare(0,0));
	nuiToolBoxItem("SimpleFlare",SimpleFlare(0,0));
	nuiToolBoxItem("SixStar",SixStar(0,0));
	nuiToolBoxItem("SpaceBlue",SpaceBlue(0,0));
	nuiToolBoxItem("SpaceSun",SpaceSun(0,0));
	nuiToolBoxItem("SunDown",SunDown(0,0));
nuiPopToolBox();

//----------------------------------------------------------------

nuiDefSlider("SimpleFlare.PosX",0.0,width,1.0);
nuiDefSlider("SimpleFlare.PosY",0.0,height,1.0);
nuiAddPointOsc("SimpleFlare.Pos");
nuiPushControlGroup("Position");
	nuiGroupControl("SimpleFlare.PosX");
 	nuiGroupControl("SimpleFlare.PosY");
nuiPopControlGroup();
nuxDefExprToggle("SimpleFlare.UseAlpha");
nuxDefMultiChoice("SimpleFlare.TransferMode", "Normal|Add|Multiply|Screen|Overlay|SoftLight|HardLight|Darken|Lighten|Difference|Hue|Saturation|Color|Luminosity|ColorDodge|ColorBurn|Exclusion");
nuiDefSlider("SimpleFlare.Blend",0.0,100.0,1.0);
nuxDefExprToggle("SimpleFlare.RotationEnable");
nuiDefSlider("SimpleFlare.Brightness",0.0,4.0,0.1);
nuiDefSlider("SimpleFlare.Quality",0.1,1.0,0.1);
nuxDefRadioBtnCtrl("SimpleFlare.Ratio", 1, 1, 0, "0|ProFlares/ux/radio/Normal", "1|ProFlares/ux/radio/Anamorph");

nuiPushControlGroup("SimpleFlare.Color");
    nuiGroupControl("SimpleFlare.rColor");
    nuiGroupControl("SimpleFlare.gColor");
    nuiGroupControl("SimpleFlare.bColor");
nuiPopControlGroup();
nuiPushControlWidget("SimpleFlare.Color", nuiConnectColorPCtrl());

nuxDefExprToggle("SimpleFlare.HaloEnable");
nuiDefSlider("SimpleFlare.HaloScale",0.0,2.0,0.1);
nuiDefSlider("SimpleFlare.HaloOpacity",0.0,1.0,0.1);
nuiPushControlGroup("SimpleFlare.Halo");
	nuiGroupControl("SimpleFlare.HaloEnable");
	nuiGroupControl("SimpleFlare.HaloScale");
	nuiGroupControl("SimpleFlare.HaloOpacity");
nuiPopControlGroup();

nuxDefExprToggle("SimpleFlare.RaysEnable");
nuiDefSlider("SimpleFlare.RaysScale",0.0,2.0,0.1);
nuiDefSlider("SimpleFlare.RaysOpacity",0.0,1.0,0.1);
nuiDefSlider("SimpleFlare.NbRays",0,100,1);
nuiDefSlider("SimpleFlare.RaysBlur",0.0,25.0,1.0);
nuiPushControlGroup("SimpleFlare.Rays");
	nuiGroupControl("SimpleFlare.RaysEnable");
	nuiGroupControl("SimpleFlare.NbRays");
	nuiGroupControl("SimpleFlare.RaysScale");
	nuiGroupControl("SimpleFlare.RaysOpacity");
	nuiGroupControl("SimpleFlare.RaysBlur");
nuiPopControlGroup();

nuxDefExprToggle("SimpleFlare.ReflectionsEnable");
nuiDefSlider("SimpleFlare.ReflectionsScale",0.0,2.0,0.1);
nuiDefSlider("SimpleFlare.ReflectionsOpacity",0.0,1.0,0.1);
nuiPushControlGroup("SimpleFlare.Reflections");
	nuiGroupControl("SimpleFlare.ReflectionsEnable");
	nuiGroupControl("SimpleFlare.ReflectionsScale");
	nuiGroupControl("SimpleFlare.ReflectionsOpacity");
nuiPopControlGroup();

nuxDefExprToggle("SimpleFlare.RedReflectionEnable");
nuiDefSlider("SimpleFlare.RedReflectionScale",0.0,2.0,0.1);
nuiDefSlider("SimpleFlare.RedReflectionOpacity",0.0,1.0,0.1);
nuiPushControlGroup("SimpleFlare.RedReflection");
	nuiGroupControl("SimpleFlare.RedReflectionEnable");
	nuiGroupControl("SimpleFlare.RedReflectionScale");
	nuiGroupControl("SimpleFlare.RedReflectionOpacity");
nuiPopControlGroup();

//----------------------------------------------------------------

nuiDefSlider("RayStarFlare.PosX",0.0,width,1.0);
nuiDefSlider("RayStarFlare.PosY",0.0,height,1.0);
nuiAddPointOsc("RayStarFlare.Pos");
nuiPushControlGroup("Position");
	nuiGroupControl("RayStarFlare.PosX");
 	nuiGroupControl("RayStarFlare.PosY");
nuiPopControlGroup();
nuiDefSlider("RayStarFlare.PivotX",0,width,1.0);
nuiDefSlider("RayStarFlare.PivotY",0,height,1.0);
nuxDefExprToggle("RayStarFlare.UseAlpha");
nuxDefMultiChoice("RayStarFlare.TransferMode", "Normal|Add|Multiply|Screen|Overlay|SoftLight|HardLight|Darken|Lighten|Difference|Hue|Saturation|Color|Luminosity|ColorDodge|ColorBurn|Exclusion");
nuiDefSlider("RayStarFlare.Blend",0.0,100.0,1.0);
nuiDefSlider("RayStarFlare.Brightness",0.0,4.0,0.1);
nuiDefSlider("RayStarFlare.Quality",0.1,1.0,0.1);
nuxDefRadioBtnCtrl("RayStarFlare.Ratio", 1, 1, 0, "0|ProFlares/ux/radio/Normal", "1|ProFlares/ux/radio/Anamorph");

nuiPushControlGroup("RayStarFlare.Color");
    nuiGroupControl("RayStarFlare.rColor");
    nuiGroupControl("RayStarFlare.gColor");
    nuiGroupControl("RayStarFlare.bColor");
nuiPopControlGroup();
nuiPushControlWidget("RayStarFlare.Color", nuiConnectColorPCtrl());

nuxDefExprToggle("RayStarFlare.MainGlowEnable");
nuiDefSlider("RayStarFlare.MainGlowScale",0.0,2.0,0.1);
nuiDefSlider("RayStarFlare.MainGlowOpacity",0.0,1.0,0.1);
nuiDefSlider("RayStarFlare.MainGlowBlur",0.0,10.0,0.1);
nuiPushControlGroup("RayStarFlare.Main Glow");
	nuiGroupControl("RayStarFlare.MainGlowEnable");
	nuiGroupControl("RayStarFlare.MainGlowScale");
	nuiGroupControl("RayStarFlare.MainGlowOpacity");
	nuiGroupControl("RayStarFlare.MainGlowBlur");
nuiPopControlGroup();

nuxDefExprToggle("RayStarFlare.BrownReflectionEnable");
nuiDefSlider("RayStarFlare.BrownReflectionScale",0.0,2.0,0.1);
nuiDefSlider("RayStarFlare.BrownReflectionOpacity",0.0,1.0,0.1);
nuiDefSlider("RayStarFlare.BrownReflectionBlur",0.0,100.0,1.0);
nuiPushControlGroup("RayStarFlare.Brown Reflection");
	nuiGroupControl("RayStarFlare.BrownReflectionEnable");
	nuiGroupControl("RayStarFlare.BrownReflectionScale");
	nuiGroupControl("RayStarFlare.BrownReflectionOpacity");
	nuiGroupControl("RayStarFlare.BrownReflectionBlur");
nuiPopControlGroup();

nuxDefExprToggle("RayStarFlare.RedReflectionEnable");
nuiDefSlider("RayStarFlare.RedReflectionScale",0.0,2.0,0.1);
nuiDefSlider("RayStarFlare.RedReflectionOpacity",0.0,1.0,0.1);
nuiDefSlider("RayStarFlare.RedReflectionBlur",0.0,100.0,1.0);
nuiPushControlGroup("RayStarFlare.Red Reflection");
	nuiGroupControl("RayStarFlare.RedReflectionEnable");
	nuiGroupControl("RayStarFlare.RedReflectionScale");
	nuiGroupControl("RayStarFlare.RedReflectionOpacity");
	nuiGroupControl("RayStarFlare.RedReflectionBlur");
nuiPopControlGroup();

nuxDefExprToggle("RayStarFlare.RaysEnable");
nuiDefSlider("RayStarFlare.RaysScale",0.0,2.0,0.1);
nuiDefSlider("RayStarFlare.RaysOpacity",0.0,1.0,0.1);
nuiPushControlGroup("RayStarFlare.Rays");
	nuiGroupControl("RayStarFlare.RaysEnable");
	nuiGroupControl("RayStarFlare.RaysScale");
	nuiGroupControl("RayStarFlare.RaysOpacity");
nuiPopControlGroup();

nuxDefExprToggle("RayStarFlare.ArcReflectionEnable");
nuiDefSlider("RayStarFlare.ArcReflectionScale",0.0,2.0,0.1);
nuiDefSlider("RayStarFlare.ArcReflectionOpacity",0.0,1.0,0.1);
nuiDefSlider("RayStarFlare.ArcReflectionBlur",0.0,10.0,0.1);
nuiPushControlGroup("RayStarFlare.ArcReflection");
	nuiGroupControl("RayStarFlare.ArcReflectionEnable");
	nuiGroupControl("RayStarFlare.ArcReflectionScale");
	nuiGroupControl("RayStarFlare.ArcReflectionOpacity");
	nuiGroupControl("RayStarFlare.ArcReflectionBlur");
nuiPopControlGroup();



//----------------------------------------------------------------

nuiDefSlider("RainbowFlare.PosX",0.0,width,1.0);
nuiDefSlider("RainbowFlare.PosY",0.0,height,1.0);
nuiAddPointOsc("RainbowFlare.Pos");
nuiPushControlGroup("Position");
	nuiGroupControl("RainbowFlare.PosX");
 	nuiGroupControl("RainbowFlare.PosY");
nuiPopControlGroup();
nuiDefSlider("RainbowFlare.PivotX",0,width,1.0);
nuiDefSlider("RainbowFlare.PivotY",0,height,1.0);
nuxDefExprToggle("RainbowFlare.UseAlpha");
nuxDefMultiChoice("RainbowFlare.TransferMode", "Normal|Add|Multiply|Screen|Overlay|SoftLight|HardLight|Darken|Lighten|Difference|Hue|Saturation|Color|Luminosity|ColorDodge|ColorBurn|Exclusion");
nuiDefSlider("RainbowFlare.Blend",0.0,100.0,1.0);
nuiDefSlider("RainbowFlare.Brightness",0.0,4.0,0.1);
nuxDefRadioBtnCtrl("RainbowFlare.Ratio", 1, 1, 0, "0|ProFlares/ux/radio/Normal", "1|ProFlares/ux/radio/Anamorph");

nuiPushControlGroup("RainbowFlare.Color");
    nuiGroupControl("RainbowFlare.rColor");
    nuiGroupControl("RainbowFlare.gColor");
    nuiGroupControl("RainbowFlare.bColor");
nuiPopControlGroup();
nuiPushControlWidget("RainbowFlare.Color", nuiConnectColorPCtrl());

nuxDefExprToggle("RainbowFlare.HaloEnable");
nuiDefSlider("RainbowFlare.HaloScale",0.0,2.0,0.1);
nuiDefSlider("RainbowFlare.HaloOpacity",0.0,1.0,0.1);
nuiPushControlGroup("RainbowFlare.Halo");
	nuiGroupControl("RainbowFlare.HaloEnable");
	nuiGroupControl("RainbowFlare.HaloScale");
	nuiGroupControl("RainbowFlare.HaloOpacity");
nuiPopControlGroup();

nuxDefExprToggle("RainbowFlare.RaysEnable");
nuiDefSlider("RainbowFlare.RaysScale",0.0,2.0,0.1);
nuiDefSlider("RainbowFlare.RaysOpacity",0.0,1.0,0.1);
nuiDefSlider("RainbowFlare.RaysBlur",0.0,10.0,1.0);
nuiPushControlGroup("RainbowFlare.Rays");
	nuiGroupControl("RainbowFlare.RaysEnable");
	nuiGroupControl("RainbowFlare.RaysScale");
	nuiGroupControl("RainbowFlare.RaysOpacity");
	nuiGroupControl("RainbowFlare.RaysBlur");
nuiPopControlGroup();

nuxDefExprToggle("RainbowFlare.RedRingEnable");
nuiDefSlider("RainbowFlare.RedRingScale",0.0,2.0,0.1);
nuiDefSlider("RainbowFlare.RedRingOpacity",0.0,1.0,0.1);
nuiPushControlGroup("RainbowFlare.Red Ring");
	nuiGroupControl("RainbowFlare.RedRingEnable");
	nuiGroupControl("RainbowFlare.RedRingScale");
	nuiGroupControl("RainbowFlare.RedRingOpacity");
nuiPopControlGroup();

nuxDefExprToggle("RainbowFlare.RainbowRingEnable");
nuiDefSlider("RainbowFlare.RainbowRingOpacity",0.0,1.0,0.1);
nuiDefSlider("RainbowFlare.RainbowRingBlur",0.0,10.0,1.0);
nuiPushControlGroup("RainbowFlare.Rainbow Ring");
	nuiGroupControl("RainbowFlare.RainbowRingEnable");
	nuiGroupControl("RainbowFlare.RainbowRingOpacity");
	nuiGroupControl("RainbowFlare.RainbowRingBlur");
nuiPopControlGroup();

nuxDefExprToggle("RainbowFlare.StreaksEnable");
nuiDefSlider("RainbowFlare.StreaksOpacity",0.0,1.0,0.1);
nuiDefSlider("RainbowFlare.StreaksBlur",0.0,10.0,0.1);
nuiPushControlGroup("RainbowFlare.Streaks");
	nuiGroupControl("RainbowFlare.StreaksEnable");
	nuiGroupControl("RainbowFlare.StreaksOpacity");
	nuiGroupControl("RainbowFlare.StreaksBlur");
nuiPopControlGroup();

nuxDefExprToggle("RainbowFlare.ReflectionsEnable");
nuiDefSlider("RainbowFlare.ReflectionsScale",0.0,2.0,0.1);
nuiDefSlider("RainbowFlare.ReflectionsOpacity",0.0,1.0,0.1);
nuiPushControlGroup("RainbowFlare.Reflections");
	nuiGroupControl("RainbowFlare.ReflectionsEnable");
	nuiGroupControl("RainbowFlare.ReflectionsScale");
	nuiGroupControl("RainbowFlare.ReflectionsOpacity");
nuiPopControlGroup();

nuxDefExprToggle("RainbowFlare.Reflections2Enable");
nuiDefSlider("RainbowFlare.Reflections2Scale",0.0,2.0,0.1);
nuiDefSlider("RainbowFlare.Reflections2Opacity",0.0,1.0,0.1);
nuiPushControlGroup("RainbowFlare.Reflections2");
	nuiGroupControl("RainbowFlare.Reflections2Enable");
	nuiGroupControl("RainbowFlare.Reflections2Scale");
	nuiGroupControl("RainbowFlare.Reflections2Opacity");
nuiPopControlGroup();

//----------------------------------------------------------------

nuiDefSlider("HotFlare.PosX",0.0,width,1.0);
nuiDefSlider("HotFlare.PosY",0.0,height,1.0);
nuiAddPointOsc("HotFlare.Pos");
nuiPushControlGroup("Position");
	nuiGroupControl("HotFlare.PosX");
 	nuiGroupControl("HotFlare.PosY");
nuiPopControlGroup();
nuxDefExprToggle("HotFlare.UseAlpha");
nuxDefMultiChoice("HotFlare.TransferMode", "Normal|Add|Multiply|Screen|Overlay|SoftLight|HardLight|Darken|Lighten|Difference|Hue|Saturation|Color|Luminosity|ColorDodge|ColorBurn|Exclusion");
nuiDefSlider("HotFlare.Blend",0.0,100.0,1.0);
nuiDefSlider("HotFlare.Brightness",0.0,4.0,0.1);
nuxDefRadioBtnCtrl("HotFlare.Ratio", 1, 1, 0, "0|ProFlares/ux/radio/Normal", "1|ProFlares/ux/radio/Anamorph");

nuxDefExprToggle("HotFlare.HaloEnable");
nuiDefSlider("HotFlare.HaloScale",0.0,2.0,0.1);
nuiDefSlider("HotFlare.HaloOpacity",0.0,1.0,0.1);
nuiDefSlider("HotFlare.HaloBlur",0.0,10.0,0.1);
nuiPushControlGroup("HotFlare.Halo");
	nuiGroupControl("HotFlare.HaloEnable");
	nuiGroupControl("HotFlare.HaloScale");
	nuiGroupControl("HotFlare.HaloOpacity");
	nuiGroupControl("HotFlare.HaloBlur");
nuiPopControlGroup();

nuxDefExprToggle("HotFlare.RaysEnable");
nuiDefSlider("HotFlare.RaysScale",0.0,2.0,0.1);
nuiDefSlider("HotFlare.RaysOpacity",0.0,1.0,0.1);
nuiDefSlider("HotFlare.RaysBlur",0.0,10.0,0.1);
nuiPushControlGroup("HotFlare.Rays");
	nuiGroupControl("HotFlare.RaysEnable");
	nuiGroupControl("HotFlare.RaysScale");
	nuiGroupControl("HotFlare.RaysOpacity");
	nuiGroupControl("HotFlare.RaysBlur");
nuiPopControlGroup();

nuxDefExprToggle("HotFlare.Rays2Enable");
nuiDefSlider("HotFlare.Rays2Scale",0.0,2.0,0.1);
nuiDefSlider("HotFlare.Rays2Opacity",0.0,1.0,0.1);
nuiDefSlider("HotFlare.Rays2Blur",0.0,10.0,0.1);
nuiPushControlGroup("HotFlare.Rays2");
	nuiGroupControl("HotFlare.Rays2Enable");
	nuiGroupControl("HotFlare.Rays2Scale");
	nuiGroupControl("HotFlare.Rays2Opacity");
	nuiGroupControl("HotFlare.Rays2Blur");
nuiPopControlGroup();

nuxDefExprToggle("HotFlare.Reflections1Enable");
nuiDefSlider("HotFlare.Reflections1Scale",0.0,2.0,0.1);
nuiDefSlider("HotFlare.Reflections1Opacity",0.0,1.0,0.1);
nuiDefSlider("HotFlare.Reflections1Blur",0.0,10.0,0.1);
nuiPushControlGroup("HotFlare.Reflections1");
	nuiGroupControl("HotFlare.Reflections1Enable");
	nuiGroupControl("HotFlare.Reflections1Scale");
	nuiGroupControl("HotFlare.Reflections1Opacity");
	nuiGroupControl("HotFlare.Reflections1Blur");
nuiPopControlGroup();

nuxDefExprToggle("HotFlare.Reflections2Enable");
nuiDefSlider("HotFlare.Reflections2Scale",0.0,2.0,0.1);
nuiDefSlider("HotFlare.Reflections2Opacity",0.0,1.0,0.1);
nuiDefSlider("HotFlare.Reflections2Blur",0.0,10.0,0.1);
nuiPushControlGroup("HotFlare.Reflections2");
	nuiGroupControl("HotFlare.Reflections2Enable");
	nuiGroupControl("HotFlare.Reflections2Scale");
	nuiGroupControl("HotFlare.Reflections2Opacity");
	nuiGroupControl("HotFlare.Reflections2Blur");
nuiPopControlGroup();

//----------------------------------------------------------------

nuiDefSlider("Flare35mm.PosX",0.0,width,1.0);
nuiDefSlider("Flare35mm.PosY",0.0,height,1.0);
nuiAddPointOsc("Flare35mm.Pos");
nuiPushControlGroup("Position");
	nuiGroupControl("Flare35mm.PosX");
 	nuiGroupControl("Flare35mm.PosY");
nuiPopControlGroup();
nuxDefExprToggle("Flare35mm.UseAlpha");
nuxDefMultiChoice("Flare35mm.TransferMode", "Normal|Add|Multiply|Screen|Overlay|SoftLight|HardLight|Darken|Lighten|Difference|Hue|Saturation|Color|Luminosity|ColorDodge|ColorBurn|Exclusion");
nuiDefSlider("Flare35mm.Blend",0.0,100.0,1.0);
nuiDefSlider("Flare35mm.Brightness",0.0,5.0,0.1);
nuxDefRadioBtnCtrl("Flare35mm.Ratio", 1, 1, 0, "0|ProFlares/ux/radio/Normal", "1|ProFlares/ux/radio/Anamorph");

nuxDefExprToggle("Flare35mm.GlowEnable");
nuiDefSlider("Flare35mm.GlowScale",0.0,2.0,0.1);
nuiDefSlider("Flare35mm.GlowOpacity",0.0,1.0,0.1);
nuiPushControlGroup("Flare35mm.Glow");
	nuiGroupControl("Flare35mm.GlowEnable");
	nuiGroupControl("Flare35mm.GlowScale");
	nuiGroupControl("Flare35mm.GlowOpacity");
nuiPopControlGroup();

nuxDefExprToggle("Flare35mm.OutterRingEnable");
nuiDefSlider("Flare35mm.OutterRingScale",0.0,2.0,0.1);
nuiDefSlider("Flare35mm.OutterRingOpacity",0.0,1.0,0.1);
nuiPushControlGroup("Flare35mm.Outter Ring");
	nuiGroupControl("Flare35mm.OutterRingEnable");
	nuiGroupControl("Flare35mm.OutterRingScale");
	nuiGroupControl("Flare35mm.OutterRingOpacity");
nuiPopControlGroup();

nuxDefExprToggle("Flare35mm.RaysEnable");
nuiDefSlider("Flare35mm.RaysScale",0.0,2.0,0.1);
nuiDefSlider("Flare35mm.RaysOpacity",0.0,1.0,0.1);
nuiDefSlider("Flare35mm.RaysCount",2,25,1);
nuiDefSlider("Flare35mm.RaysWidth",0,30,1);
nuiDefSlider("Flare35mm.RaysAngle",0,360,1);
nuiPushControlGroup("Flare35mm.Rays");
	nuiGroupControl("Flare35mm.RaysEnable");
	nuiGroupControl("Flare35mm.RaysScale");
	nuiGroupControl("Flare35mm.RaysOpacity");
	nuiGroupControl("Flare35mm.RaysCount");
	nuiGroupControl("Flare35mm.RaysWidth");
	nuiGroupControl("Flare35mm.RaysAngle");
nuiPopControlGroup();

nuxDefExprToggle("Flare35mm.Reflections1Enable");
nuiDefSlider("Flare35mm.Reflections1Scale",0.0,2.0,0.1);
nuiDefSlider("Flare35mm.Reflections1Opacity",0.0,1.0,0.1);
nuiPushControlGroup("Flare35mm.Reflections 1");
	nuiGroupControl("Flare35mm.Reflections1Enable");
	nuiGroupControl("Flare35mm.Reflections1Scale");
	nuiGroupControl("Flare35mm.Reflections1Opacity");
nuiPopControlGroup();

nuxDefExprToggle("Flare35mm.Reflections2Enable");
nuiDefSlider("Flare35mm.Reflections2Scale",0.0,2.0,0.1);
nuiDefSlider("Flare35mm.Reflections2Opacity",0.0,1.0,0.1);
nuiPushControlGroup("Flare35mm.Reflections 2");
	nuiGroupControl("Flare35mm.Reflections2Enable");
	nuiGroupControl("Flare35mm.Reflections2Scale");
	nuiGroupControl("Flare35mm.Reflections2Opacity");
nuiPopControlGroup();

nuxDefExprToggle("Flare35mm.Reflections3Enable");
nuiDefSlider("Flare35mm.Reflections3Scale",0.0,2.0,0.1);
nuiDefSlider("Flare35mm.Reflections3Opacity",0.0,1.0,0.1);
nuiPushControlGroup("Flare35mm.Reflections 3");
	nuiGroupControl("Flare35mm.Reflections3Enable");
	nuiGroupControl("Flare35mm.Reflections3Scale");
	nuiGroupControl("Flare35mm.Reflections3Opacity");
nuiPopControlGroup();

nuxDefExprToggle("Flare35mm.StarEnable");
nuiDefSlider("Flare35mm.StarScale",0.0,2.0,0.1);
nuiDefSlider("Flare35mm.StarOpacity",0.0,1.0,0.1);
nuiPushControlGroup("Flare35mm.Star Caustic");
	nuiGroupControl("Flare35mm.StarEnable");
	nuiGroupControl("Flare35mm.StarScale");
	nuiGroupControl("Flare35mm.StarOpacity");
nuiPopControlGroup();

nuiDefSlider("Flare35mm.PerspectiveAngle",0.0,60.0,1.0);

//----------------------------------------------------------------

nuiDefSlider("BrightFlare.PosX",0.0,width,1.0);
nuiDefSlider("BrightFlare.PosY",0.0,height,1.0);
nuiAddPointOsc("BrightFlare.Pos");
nuiPushControlGroup("Position");
	nuiGroupControl("BrightFlare.PosX");
 	nuiGroupControl("BrightFlare.PosY");
nuiPopControlGroup();
nuxDefExprToggle("BrightFlare.UseAlpha");
nuxDefMultiChoice("BrightFlare.TransferMode", "Normal|Add|Multiply|Screen|Overlay|SoftLight|HardLight|Darken|Lighten|Difference|Hue|Saturation|Color|Luminosity|ColorDodge|ColorBurn|Exclusion");
nuiDefSlider("BrightFlare.Blend",0.0,100.0,1.0);
nuiDefSlider("BrightFlare.Brightness",0.0,4.0,0.1);
nuiDefSlider("BrightFlare.Quality",0.1,1.0,0.1);
nuxDefRadioBtnCtrl("BrightFlare.Ratio", 1, 1, 0, "0|ProFlares/ux/radio/Normal", "1|ProFlares/ux/radio/Anamorph");

nuiPushControlGroup("BrightFlare.Color");
    nuiGroupControl("BrightFlare.rColor");
    nuiGroupControl("BrightFlare.gColor");
    nuiGroupControl("BrightFlare.bColor");
nuiPopControlGroup();
nuiPushControlWidget("BrightFlare.Color", nuiConnectColorPCtrl());

nuxDefExprToggle("BrightFlare.Halo1Enable");
nuiDefSlider("BrightFlare.Halo1Scale",0.0,2.0,0.1);
nuiDefSlider("BrightFlare.Halo1Opacity",0.0,1.0,0.1);
nuiPushControlGroup("BrightFlare.Halo 1");
	nuiGroupControl("BrightFlare.Halo1Enable");
	nuiGroupControl("BrightFlare.Halo1Scale");
	nuiGroupControl("BrightFlare.Halo1Opacity");
nuiPopControlGroup();

nuxDefExprToggle("BrightFlare.RaysEnable");
nuiDefSlider("BrightFlare.RaysScale",0.0,2.0,0.1);
nuiDefSlider("BrightFlare.RaysOpacity",0.0,1.0,0.1);
nuiDefSlider("BrightFlare.RaysBlur",0.0,100.0,1.0);
nuiPushControlGroup("BrightFlare.Rays");
	nuiGroupControl("BrightFlare.RaysEnable");
	nuiGroupControl("BrightFlare.RaysScale");
	nuiGroupControl("BrightFlare.RaysOpacity");
	nuiGroupControl("BrightFlare.RaysBlur");
nuiPopControlGroup();

nuxDefExprToggle("BrightFlare.Reflections1Enable");
nuiDefSlider("BrightFlare.Reflections1Scale",0.0,2.0,0.1);
nuiDefSlider("BrightFlare.Reflections1Opacity",0.0,1.0,0.1);
nuiDefSlider("BrightFlare.Reflections1Blur",0.0,10.0,0.1);
nuiPushControlGroup("BrightFlare.Reflections 1");
	nuiGroupControl("BrightFlare.Reflections1Enable");
	nuiGroupControl("BrightFlare.Reflections1Scale");
	nuiGroupControl("BrightFlare.Reflections1Opacity");
	nuiGroupControl("BrightFlare.Reflections1Blur");
nuiPopControlGroup();

nuxDefExprToggle("BrightFlare.Reflections2Enable");
nuiDefSlider("BrightFlare.Reflections2Scale",0.0,2.0,0.1);
nuiDefSlider("BrightFlare.Reflections2Opacity",0.0,1.0,0.1);
nuiPushControlGroup("BrightFlare.Reflections 2");
	nuiGroupControl("BrightFlare.Reflections2Enable");
	nuiGroupControl("BrightFlare.Reflections2Scale");
	nuiGroupControl("BrightFlare.Reflections2Opacity");
nuiPopControlGroup();

//----------------------------------------------------------------

nuiDefSlider("LaserFlare.PosX",0.0,width,1.0);
nuiDefSlider("LaserFlare.PosY",0.0,height,1.0);
nuiAddPointOsc("LaserFlare.Pos");
nuiPushControlGroup("Position");
	nuiGroupControl("LaserFlare.PosX");
 	nuiGroupControl("LaserFlare.PosY");
nuiPopControlGroup();
nuxDefExprToggle("LaserFlare.UseAlpha");
nuxDefMultiChoice("LaserFlare.TransferMode", "Normal|Add|Multiply|Screen|Overlay|SoftLight|HardLight|Darken|Lighten|Difference|Hue|Saturation|Color|Luminosity|ColorDodge|ColorBurn|Exclusion");
nuiDefSlider("LaserFlare.Blend",0.0,100.0,1.0);
nuiDefSlider("LaserFlare.Brightness",0.0,4.0,0.1);
nuiDefSlider("LaserFlare.Quality",0.1,1.0,0.1);
nuxDefRadioBtnCtrl("LaserFlare.Ratio", 1, 1, 0, "0|ProFlares/ux/radio/Normal", "1|ProFlares/ux/radio/Anamorph");

nuiPushControlGroup("LaserFlare.Color");
    nuiGroupControl("LaserFlare.rColor");
    nuiGroupControl("LaserFlare.gColor");
    nuiGroupControl("LaserFlare.bColor");
nuiPopControlGroup();
nuiPushControlWidget("LaserFlare.Color", nuiConnectColorPCtrl());

nuxDefExprToggle("LaserFlare.HaloEnable");
nuiDefSlider("LaserFlare.HaloScale",0.0,2.0,0.1);
nuiDefSlider("LaserFlare.HaloOpacity",0.0,1.0,0.1);
nuiPushControlGroup("LaserFlare.Halo");
	nuiGroupControl("LaserFlare.HaloEnable");
	nuiGroupControl("LaserFlare.HaloScale");
	nuiGroupControl("LaserFlare.HaloOpacity");
nuiPopControlGroup();

nuxDefExprToggle("LaserFlare.RaysEnable");
nuiDefSlider("LaserFlare.RaysOpacity",0.0,1.0,0.1);
nuiDefSlider("LaserFlare.RaysBlur",0.0,100.0,1.0);
nuiPushControlGroup("LaserFlare.Rays");
	nuiGroupControl("LaserFlare.RaysEnable");
	nuiGroupControl("LaserFlare.RaysOpacity");
	nuiGroupControl("LaserFlare.RaysBlur");
nuiPopControlGroup();

nuxDefExprToggle("LaserFlare.ArcEnable");
nuiDefSlider("LaserFlare.ArcScale",0.0,2.0,0.1);
nuiDefSlider("LaserFlare.ArcOpacity",0.0,1.0,0.1);
nuiDefSlider("LaserFlare.ArcBlur",0.0,100.0,1.0);
nuiPushControlGroup("LaserFlare.Arc");
	nuiGroupControl("LaserFlare.ArcEnable");
	nuiGroupControl("LaserFlare.ArcScale");
	nuiGroupControl("LaserFlare.ArcOpacity");
	nuiGroupControl("LaserFlare.ArcBlur");
nuiPopControlGroup();

//----------------------------------------------------------------

nuiDefSlider("LaserFlare2.PosX",0.0,width,1.0);
nuiDefSlider("LaserFlare2.PosY",0.0,height,1.0);
nuiAddPointOsc("LaserFlare2.Pos");
nuiPushControlGroup("Position");
	nuiGroupControl("LaserFlare2.PosX");
 	nuiGroupControl("LaserFlare2.PosY");
nuiPopControlGroup();

nuxDefExprToggle("LaserFlare2.UseAlpha");
nuxDefMultiChoice("LaserFlare2.TransferMode", "Normal|Add|Multiply|Screen|Overlay|SoftLight|HardLight|Darken|Lighten|Difference|Hue|Saturation|Color|Luminosity|ColorDodge|ColorBurn|Exclusion");
nuiDefSlider("LaserFlare2.Blend",0.0,100.0,1.0);
nuiDefSlider("LaserFlare2.Brightness",0.0,5.0,0.1);
nuiDefSlider("LaserFlare2.Quality",0.1,1.0,0.1);
nuxDefRadioBtnCtrl("LaserFlare2.Ratio", 1, 1, 0, "0|ProFlares/ux/radio/Normal", "1|ProFlares/ux/radio/Anamorph");

nuiPushControlGroup("LaserFlare2.Color");
    nuiGroupControl("LaserFlare2.rColor");
    nuiGroupControl("LaserFlare2.gColor");
    nuiGroupControl("LaserFlare2.bColor");
nuiPopControlGroup();
nuiPushControlWidget("LaserFlare2.Color", nuiConnectColorPCtrl());

nuiPushControlGroup("LaserFlare2.Fan Color");
    nuiGroupControl("LaserFlare2.rFanColor");
    nuiGroupControl("LaserFlare2.gFanColor");
    nuiGroupControl("LaserFlare2.bFanColor");
nuiPopControlGroup();
nuiPushControlWidget("LaserFlare2.Fan Color", nuiConnectColorPCtrl());

nuxDefExprToggle("LaserFlare2.HaloEnable");
nuiDefSlider("LaserFlare2.HaloScale",0.0,2.0,0.1);
nuiDefSlider("LaserFlare2.HaloOpacity",0.0,1.0,0.1);
nuiPushControlGroup("LaserFlare2.Halo");
	nuiGroupControl("LaserFlare2.HaloEnable");
	nuiGroupControl("LaserFlare2.HaloScale");
	nuiGroupControl("LaserFlare2.HaloOpacity");
nuiPopControlGroup();

nuxDefExprToggle("LaserFlare2.RaysEnable");
nuiDefSlider("LaserFlare2.RaysScale",0.0,2.0,0.1);
nuiDefSlider("LaserFlare2.RaysOpacity",0.0,1.0,0.1);
nuiDefSlider("LaserFlare2.RaysBlur",0.0,100.0,1.0);
nuiPushControlGroup("LaserFlare2.Rays");
	nuiGroupControl("LaserFlare2.RaysEnable");
	nuiGroupControl("LaserFlare2.RaysScale");
	nuiGroupControl("LaserFlare2.RaysOpacity");
	nuiGroupControl("LaserFlare2.RaysBlur");
nuiPopControlGroup();

nuxDefExprToggle("LaserFlare2.FanEnable");
nuiDefSlider("LaserFlare2.FanScale",0.0,2.0,0.1);
nuiDefSlider("LaserFlare2.FanOpacity",0.0,1.0,0.1);
nuiDefSlider("LaserFlare2.FanBlur",0.0,100.0,1.0);
nuiPushControlGroup("LaserFlare2.Fan");
	nuiGroupControl("LaserFlare2.FanEnable");
	nuiGroupControl("LaserFlare2.FanScale");
	nuiGroupControl("LaserFlare2.FanOpacity");
	nuiGroupControl("LaserFlare2.FanBlur");
nuiPopControlGroup();

nuxDefExprToggle("LaserFlare2.HorizLineEnable");
nuiDefSlider("LaserFlare2.HorizLineScale",0.0,2.0,0.1);
nuiDefSlider("LaserFlare2.HorizLineOpacity",0.0,1.0,0.1);
nuiPushControlGroup("LaserFlare2.Horizontal Line");
	nuiGroupControl("LaserFlare2.HorizLineEnable");
	nuiGroupControl("LaserFlare2.HorizLineScale");
	nuiGroupControl("LaserFlare2.HorizLineOpacity");
nuiPopControlGroup();

nuxDefExprToggle("LaserFlare2.ReflectionsEnable");
nuiDefSlider("LaserFlare2.ReflectionsScale",0.0,2.0,0.1);
nuiDefSlider("LaserFlare2.ReflectionsOpacity",0.0,1.0,0.1);
nuiDefSlider("LaserFlare2.ReflectionsBlur",0.0,100.0,1.0);
nuiPushControlGroup("LaserFlare2.Reflections");
	nuiGroupControl("LaserFlare2.ReflectionsEnable");
	nuiGroupControl("LaserFlare2.ReflectionsScale");
	nuiGroupControl("LaserFlare2.ReflectionsOpacity");
	nuiGroupControl("LaserFlare2.ReflectionsBlur");
nuiPopControlGroup();

nuxDefExprToggle("LaserFlare2.BackLensEnable");
nuiDefSlider("LaserFlare2.BackLensScale",0.0,2.0,0.1);
nuiDefSlider("LaserFlare2.BackLensOpacity",0.0,1.0,0.1);
nuiPushControlGroup("LaserFlare2.Back Lens");
	nuiGroupControl("LaserFlare2.BackLensEnable");
	nuiGroupControl("LaserFlare2.BackLensScale");
	nuiGroupControl("LaserFlare2.BackLensOpacity");
nuiPopControlGroup();

//----------------------------------------------------------------

nuiDefSlider("SpaceSun.PosX",0.0,width,1.0);
nuiDefSlider("SpaceSun.PosY",0.0,height,1.0);
nuiAddPointOsc("SpaceSun.Pos");
nuiPushControlGroup("Position");
	nuiGroupControl("SpaceSun.PosX");
 	nuiGroupControl("SpaceSun.PosY");
nuiPopControlGroup();
nuiAddPointOsc("SpaceSun.Pivot");
nuiPushControlGroup("Pivot");
	nuiGroupControl("SpaceSun.xPos");
 	nuiGroupControl("SpaceSun.yPos");
nuiPopControlGroup();
nuiDefSlider("SpaceSun.Angle",0.0,360.0,1.0);
nuxDefExprToggle("SpaceSun.UseAlpha");
nuxDefMultiChoice("SpaceSun.TransferMode", "Normal|Add|Multiply|Screen|Overlay|SoftLight|HardLight|Darken|Lighten|Difference|Hue|Saturation|Color|Luminosity|ColorDodge|ColorBurn|Exclusion");
nuiDefSlider("SpaceSun.Blend",0.0,100.0,1.0);
nuiDefSlider("SpaceSun.Brightness",0.0,5.0,0.1);
nuiDefSlider("SpaceSun.Quality",0.1,1.0,0.1);
nuxDefRadioBtnCtrl("SpaceSun.Ratio", 1, 1, 0, "0|ProFlares/ux/radio/Normal", "1|ProFlares/ux/radio/Anamorph");

nuiPushControlGroup("SpaceSun.Color");
    nuiGroupControl("SpaceSun.rColor");
    nuiGroupControl("SpaceSun.gColor");
    nuiGroupControl("SpaceSun.bColor");
nuiPopControlGroup();
nuiPushControlWidget("SpaceSun.Color", nuiConnectColorPCtrl());

nuxDefExprToggle("SpaceSun.CenterEnable");
nuiDefSlider("SpaceSun.CenterScale",0.0,2.0,0.1);
nuiDefSlider("SpaceSun.CenterOpacity",0.0,1.0,0.1);
nuiPushControlGroup("SpaceSun.Center");
	nuiGroupControl("SpaceSun.CenterEnable");
	nuiGroupControl("SpaceSun.CenterScale");
	nuiGroupControl("SpaceSun.CenterOpacity");
nuiPopControlGroup();

nuxDefExprToggle("SpaceSun.RaysEnable");
nuiDefSlider("SpaceSun.RaysScale",0.0,2.0,0.1);
nuiDefSlider("SpaceSun.RaysOpacity",0.0,1.0,0.1);
nuiPushControlGroup("SpaceSun.Rays");
	nuiGroupControl("SpaceSun.RaysEnable");
	nuiGroupControl("SpaceSun.RaysScale");
	nuiGroupControl("SpaceSun.RaysOpacity");
nuiPopControlGroup();

nuxDefExprToggle("SpaceSun.BlueStreaksEnable");
nuiDefSlider("SpaceSun.BlueStreaksScale",0.0,2.0,0.1);
nuiDefSlider("SpaceSun.BlueStreaksOpacity",0.0,1.0,0.1);
nuiPushControlGroup("SpaceSun.Blue Streaks");
	nuiGroupControl("SpaceSun.BlueStreaksEnable");
	nuiGroupControl("SpaceSun.BlueStreaksScale");
	nuiGroupControl("SpaceSun.BlueStreaksOpacity");
nuiPopControlGroup();

nuxDefExprToggle("SpaceSun.ReflectionsEnable");
nuiDefSlider("SpaceSun.ReflectionsScale",0.0,2.0,0.1);
nuiDefSlider("SpaceSun.ReflectionsOpacity",0.0,1.0,0.1);
nuiPushControlGroup("SpaceSun.Reflections");
	nuiGroupControl("SpaceSun.ReflectionsEnable");
	nuiGroupControl("SpaceSun.ReflectionsScale");
	nuiGroupControl("SpaceSun.ReflectionsOpacity");
nuiPopControlGroup();

nuxDefExprToggle("SpaceSun.Reflections2Enable");
nuiDefSlider("SpaceSun.Reflections2Scale",0.0,2.0,0.1);
nuiDefSlider("SpaceSun.Reflections2Opacity",0.0,1.0,0.1);
nuiPushControlGroup("SpaceSun.Reflections2");
	nuiGroupControl("SpaceSun.Reflections2Enable");
	nuiGroupControl("SpaceSun.Reflections2Scale");
	nuiGroupControl("SpaceSun.Reflections2Opacity");
nuiPopControlGroup();

//----------------------------------------------------------------

nuiDefSlider("SunDown.PosX",0.0,width,1.0);
nuiDefSlider("SunDown.PosY",0.0,height,1.0);
nuiAddPointOsc("SunDown.Pos");
nuiPushControlGroup("Position");
	nuiGroupControl("SunDown.PosX");
 	nuiGroupControl("SunDown.PosY");
nuiPopControlGroup();
nuxDefExprToggle("SunDown.UseAlpha");
nuxDefMultiChoice("SunDown.TransferMode", "Normal|Add|Multiply|Screen|Overlay|SoftLight|HardLight|Darken|Lighten|Difference|Hue|Saturation|Color|Luminosity|ColorDodge|ColorBurn|Exclusion");
nuiDefSlider("SunDown.Brightness",0.0,5.0,0.1);
nuiDefSlider("SunDown.Blend",0.0,100.0,1.0);
nuxDefRadioBtnCtrl("SunDown.Ratio", 1, 1, 0, "0|ProFlares/ux/radio/Normal", "1|ProFlares/ux/radio/Anamorph");

nuxDefExprToggle("SunDown.CenterEnable");
nuiDefSlider("SunDown.CenterScale",0.0,2.0,0.1);
nuiDefSlider("SunDown.CenterOpacity",0.0,1.0,0.1);
nuiPushControlGroup("SunDown.Center");
	nuiGroupControl("SunDown.CenterEnable");
	nuiGroupControl("SunDown.CenterScale");
	nuiGroupControl("SunDown.CenterOpacity");
nuiPopControlGroup();

nuxDefExprToggle("SunDown.GlowEnable");
nuiDefSlider("SunDown.GlowScale",0.0,2.0,0.1);
nuiDefSlider("SunDown.GlowOpacity",0.0,1.0,0.1);
nuiPushControlGroup("SunDown.Glow");
	nuiGroupControl("SunDown.GlowEnable");
	nuiGroupControl("SunDown.GlowScale");
	nuiGroupControl("SunDown.GlowOpacity");
nuiPopControlGroup();

nuxDefExprToggle("SunDown.ReflectionsEnable");
nuiDefSlider("SunDown.ReflectionsScale",0.0,2.0,0.1);
nuiDefSlider("SunDown.ReflectionsOpacity",0.0,1.0,0.1);
nuiPushControlGroup("SunDown.Reflections");
	nuiGroupControl("SunDown.ReflectionsEnable");
	nuiGroupControl("SunDown.ReflectionsScale");
	nuiGroupControl("SunDown.ReflectionsOpacity");
nuiPopControlGroup();

//----------------------------------------------------------------

nuiDefSlider("SixStar.PosX",0.0,width,1.0);
nuiDefSlider("SixStar.PosY",0.0,height,1.0);
nuiAddPointOsc("SixStar.Pos");
nuiPushControlGroup("Position");
	nuiGroupControl("SixStar.PosX");
 	nuiGroupControl("SixStar.PosY");
nuiPopControlGroup();
nuxDefExprToggle("SixStar.UseAlpha");
nuxDefMultiChoice("SixStar.TransferMode", "Normal|Add|Multiply|Screen|Overlay|SoftLight|HardLight|Darken|Lighten|Difference|Hue|Saturation|Color|Luminosity|ColorDodge|ColorBurn|Exclusion");
nuiDefSlider("SixStar.Brightness",0.0,5.0,0.1);
nuiDefSlider("SixStar.Blend",0.0,100.0,1.0);
nuxDefRadioBtnCtrl("SixStar.Ratio", 1, 1, 0, "0|ProFlares/ux/radio/Normal", "1|ProFlares/ux/radio/Anamorph");

nuxDefExprToggle("SixStar.CenterEnable");
nuiDefSlider("SixStar.CenterScale",0.0,2.0,0.1);
nuiDefSlider("SixStar.CenterOpacity",0.0,1.0,0.1);
nuiPushControlGroup("SixStar.Center");
	nuiGroupControl("SixStar.CenterEnable");
	nuiGroupControl("SixStar.CenterScale");
	nuiGroupControl("SixStar.CenterOpacity");
nuiPopControlGroup();

nuxDefExprToggle("SixStar.GlowEnable");
nuiDefSlider("SixStar.GlowScale",0.0,2.0,0.1);
nuiDefSlider("SixStar.GlowOpacity",0.0,1.0,0.1);
nuiPushControlGroup("SixStar.Glow");
	nuiGroupControl("SixStar.GlowEnable");
	nuiGroupControl("SixStar.GlowScale");
	nuiGroupControl("SixStar.GlowOpacity");
nuiPopControlGroup();

nuxDefExprToggle("SixStar.ReflectionsEnable");
nuiDefSlider("SixStar.ReflectionsScale",0.0,2.0,0.1);
nuiDefSlider("SixStar.ReflectionsOpacity",0.0,1.0,0.1);
nuiPushControlGroup("SixStar.Reflections");
	nuiGroupControl("SixStar.ReflectionsEnable");
	nuiGroupControl("SixStar.ReflectionsScale");
	nuiGroupControl("SixStar.ReflectionsOpacity");
nuiPopControlGroup();

//----------------------------------------------------------------

nuiDefSlider("SpaceBlue.PosX",0.0,width,1.0);
nuiDefSlider("SpaceBlue.PosY",0.0,height,1.0);
nuiAddPointOsc("SpaceBlue.Pos");
nuiPushControlGroup("Position");
	nuiGroupControl("SpaceBlue.PosX");
 	nuiGroupControl("SpaceBlue.PosY");
nuiPopControlGroup();
nuxDefExprToggle("SpaceBlue.UseAlpha");
nuxDefMultiChoice("SpaceBlue.TransferMode", "Normal|Add|Multiply|Screen|Overlay|SoftLight|HardLight|Darken|Lighten|Difference|Hue|Saturation|Color|Luminosity|ColorDodge|ColorBurn|Exclusion");
nuiDefSlider("SpaceBlue.Brightness",0.0,5.0,0.1);
nuiDefSlider("SpaceBlue.Blend",0.0,100.0,1.0);
nuxDefRadioBtnCtrl("SpaceBlue.Ratio", 1, 1, 0, "0|ProFlares/ux/radio/Normal", "1|ProFlares/ux/radio/Anamorph");

nuxDefExprToggle("SpaceBlue.CenterEnable");
nuiDefSlider("SpaceBlue.CenterScale",0.0,2.0,0.1);
nuiDefSlider("SpaceBlue.CenterOpacity",0.0,1.0,0.1);
nuiPushControlGroup("SpaceBlue.Center");
	nuiGroupControl("SpaceBlue.CenterEnable");
	nuiGroupControl("SpaceBlue.CenterScale");
	nuiGroupControl("SpaceBlue.CenterOpacity");
nuiPopControlGroup();

nuxDefExprToggle("SpaceBlue.GlowEnable");
nuiDefSlider("SpaceBlue.GlowScale",0.0,2.0,0.1);
nuiDefSlider("SpaceBlue.GlowOpacity",0.0,1.0,0.1);
nuiPushControlGroup("SpaceBlue.Glow");
	nuiGroupControl("SpaceBlue.GlowEnable");
	nuiGroupControl("SpaceBlue.GlowScale");
	nuiGroupControl("SpaceBlue.GlowOpacity");
nuiPopControlGroup();

nuxDefExprToggle("SpaceBlue.BlueLineEnable");
nuiDefSlider("SpaceBlue.BlueLineScale",0.0,2.0,0.1);
nuiDefSlider("SpaceBlue.BlueLineOpacity",0.0,1.0,0.1);
nuiPushControlGroup("SpaceBlue.Blue Line");
	nuiGroupControl("SpaceBlue.BlueLineEnable");
	nuiGroupControl("SpaceBlue.BlueLineScale");
	nuiGroupControl("SpaceBlue.BlueLineOpacity");
nuiPopControlGroup();

nuxDefExprToggle("SpaceBlue.ReflectionsEnable");
nuiDefSlider("SpaceBlue.ReflectionsScale",0.0,2.0,0.1);
nuiDefSlider("SpaceBlue.ReflectionsOpacity",0.0,1.0,0.1);
nuiPushControlGroup("SpaceBlue.Reflections");
	nuiGroupControl("SpaceBlue.ReflectionsEnable");
	nuiGroupControl("SpaceBlue.ReflectionsScale");
	nuiGroupControl("SpaceBlue.ReflectionsOpacity");
nuiPopControlGroup();

//----------------------------------------------------------------

nuiDefSlider("RockyFlare.PosX",0.0,width,1.0);
nuiDefSlider("RockyFlare.PosY",0.0,height,1.0);
nuiAddPointOsc("RockyFlare.Pos");
nuiPushControlGroup("Position");
	nuiGroupControl("RockyFlare.PosX");
 	nuiGroupControl("RockyFlare.PosY");
nuiPopControlGroup();
nuxDefExprToggle("RockyFlare.UseAlpha");
nuxDefMultiChoice("RockyFlare.TransferMode", "Normal|Add|Multiply|Screen|Overlay|SoftLight|HardLight|Darken|Lighten|Difference|Hue|Saturation|Color|Luminosity|ColorDodge|ColorBurn|Exclusion");
nuiDefSlider("RockyFlare.Brightness",0.0,5.0,0.1);
nuiDefSlider("RockyFlare.Blend",0.0,100.0,1.0);
nuxDefRadioBtnCtrl("RockyFlare.Ratio", 1, 1, 0, "0|ProFlares/ux/radio/Normal", "1|ProFlares/ux/radio/Anamorph");

nuiPushControlGroup("RockyFlare.Color");
    nuiGroupControl("RockyFlare.rColor");
    nuiGroupControl("RockyFlare.gColor");
    nuiGroupControl("RockyFlare.bColor");
nuiPopControlGroup();
nuiPushControlWidget("RockyFlare.Color", nuiConnectColorPCtrl());

nuxDefExprToggle("RockyFlare.HaloEnable");
nuiDefSlider("RockyFlare.HaloScale",0.0,2.0,0.1);
nuiDefSlider("RockyFlare.HaloOpacity",0.0,1.0,0.1);
nuiPushControlGroup("RockyFlare.Halo");
	nuiGroupControl("RockyFlare.HaloEnable");
	nuiGroupControl("RockyFlare.HaloScale");
	nuiGroupControl("RockyFlare.HaloOpacity");
nuiPopControlGroup();

nuxDefExprToggle("RockyFlare.RaysEnable");
nuiDefSlider("RockyFlare.RaysScale",0.0,2.0,0.1);
nuiDefSlider("RockyFlare.RaysOpacity",0.0,1.0,0.1);
nuiDefSlider("RockyFlare.RaysBlur",0.0,100.0,1.0);
nuiPushControlGroup("RockyFlare.Rays");
	nuiGroupControl("RockyFlare.RaysEnable");
	nuiGroupControl("RockyFlare.RaysScale");
	nuiGroupControl("RockyFlare.RaysOpacity");
	nuiGroupControl("RockyFlare.RaysBlur");
nuiPopControlGroup();

nuxDefExprToggle("RockyFlare.ReflectionsEnable");
nuiDefSlider("RockyFlare.ReflectionsScale",0.0,2.0,0.1);
nuiDefSlider("RockyFlare.ReflectionsOpacity",0.0,1.0,0.1);
nuiPushControlGroup("RockyFlare.Reflections");
	nuiGroupControl("RockyFlare.ReflectionsEnable");
	nuiGroupControl("RockyFlare.ReflectionsScale");
	nuiGroupControl("RockyFlare.ReflectionsOpacity");
nuiPopControlGroup();

//----------------------------------------------------------------

nuiDefSlider("Flare50300mm.PosX",0.0,width,1.0);
nuiDefSlider("Flare50300mm.PosY",0.0,height,1.0);
nuiAddPointOsc("Flare50300mm.Pos");
nuiPushControlGroup("Position");
	nuiGroupControl("Flare50300mm.PosX");
 	nuiGroupControl("Flare50300mm.PosY");
nuiPopControlGroup();
nuxDefExprToggle("Flare50300mm.UseAlpha");
nuxDefMultiChoice("Flare50300mm.TransferMode", "Normal|Add|Multiply|Screen|Overlay|SoftLight|HardLight|Darken|Lighten|Difference|Hue|Saturation|Color|Luminosity|ColorDodge|ColorBurn|Exclusion");
nuiDefSlider("Flare50300mm.Brightness",0.0,5.0,0.1);
nuiDefSlider("Flare50300mm.Blend",0.0,100.0,1.0);
nuxDefRadioBtnCtrl("Flare50300mm.Ratio", 1, 1, 0, "0|ProFlares/ux/radio/Normal", "1|ProFlares/ux/radio/Anamorph");

nuxDefExprToggle("Flare50300mm.HaloEnable");
nuiDefSlider("Flare50300mm.HaloScale",0.0,2.0,0.1);
nuiDefSlider("Flare50300mm.HaloOpacity",0.0,1.0,0.1);
nuiPushControlGroup("Flare50300mm.Halo");
	nuiGroupControl("Flare50300mm.HaloEnable");
	nuiGroupControl("Flare50300mm.HaloScale");
	nuiGroupControl("Flare50300mm.HaloOpacity");
nuiPopControlGroup();

nuxDefExprToggle("Flare50300mm.RaysEnable");
nuiDefSlider("Flare50300mm.NbRays",0,200,1);
nuiDefSlider("Flare50300mm.RaysScale",0.0,2.0,0.1);
nuiDefSlider("Flare50300mm.RaysOpacity",0.0,1.0,0.1);
nuiDefSlider("Flare50300mm.RaysBlur",0.0,100.0,1.0);
nuiPushControlGroup("Flare50300mm.Rays");
	nuiGroupControl("Flare50300mm.RaysEnable");
	nuiGroupControl("Flare50300mm.NbRays");
	nuiGroupControl("Flare50300mm.RaysScale");
	nuiGroupControl("Flare50300mm.RaysOpacity");
	nuiGroupControl("Flare50300mm.RaysBlur");
nuiPopControlGroup();

nuxDefExprToggle("Flare50300mm.ReflectionsEnable");
nuiDefSlider("Flare50300mm.ReflectionsScale",0.0,2.0,0.1);
nuiDefSlider("Flare50300mm.ReflectionsOpacity",0.0,1.0,0.1);
nuiDefSlider("Flare50300mm.ReflectionsBlur",0.0,100.0,1.0);
nuiPushControlGroup("Flare50300mm.Reflections");
	nuiGroupControl("Flare50300mm.ReflectionsEnable");
	nuiGroupControl("Flare50300mm.ReflectionsScale");
	nuiGroupControl("Flare50300mm.ReflectionsOpacity");
	nuiGroupControl("Flare50300mm.ReflectionsBlur");
nuiPopControlGroup();

//----------------------------------------------------------------

nuiDefSlider("Flare105mm.PosX",0.0,width,1.0);
nuiDefSlider("Flare105mm.PosY",0.0,height,1.0);
nuiAddPointOsc("Flare105mm.Pos");
nuiPushControlGroup("Position");
	nuiGroupControl("Flare105mm.PosX");
 	nuiGroupControl("Flare105mm.PosY");
nuiPopControlGroup();
nuxDefExprToggle("Flare105mm.UseAlpha");
nuxDefMultiChoice("Flare105mm.TransferMode", "Normal|Add|Multiply|Screen|Overlay|SoftLight|HardLight|Darken|Lighten|Difference|Hue|Saturation|Color|Luminosity|ColorDodge|ColorBurn|Exclusion");
nuiDefSlider("Flare105mm.Brightness",0.0,5.0,0.1);
nuiDefSlider("Flare105mm.Blend",0.0,100.0,1.0);
nuxDefRadioBtnCtrl("Flare105mm.Ratio", 1, 1, 0, "0|ProFlares/ux/radio/Normal", "1|ProFlares/ux/radio/Anamorph");

nuxDefExprToggle("Flare105mm.HaloEnable");
nuiDefSlider("Flare105mm.HaloScale",0.0,2.0,0.1);
nuiDefSlider("Flare105mm.HaloOpacity",0.0,1.0,0.1);
nuiPushControlGroup("Flare105mm.Halo");
	nuiGroupControl("Flare105mm.HaloEnable");
	nuiGroupControl("Flare105mm.HaloScale");
	nuiGroupControl("Flare105mm.HaloOpacity");
nuiPopControlGroup();

nuxDefExprToggle("Flare105mm.RaysEnable");
nuiDefSlider("Flare105mm.NbRays",0,200,1);
nuiDefSlider("Flare105mm.RaysScale",0.0,2.0,0.1);
nuiDefSlider("Flare105mm.RaysOpacity",0.0,1.0,0.1);
nuiDefSlider("Flare105mm.RaysBlur",0.0,100.0,1.0);
nuiPushControlGroup("Flare105mm.Rays");
	nuiGroupControl("Flare105mm.RaysEnable");
	nuiGroupControl("Flare105mm.NbRays");
	nuiGroupControl("Flare105mm.RaysScale");
	nuiGroupControl("Flare105mm.RaysOpacity");
	nuiGroupControl("Flare105mm.RaysBlur");
nuiPopControlGroup();

nuxDefExprToggle("Flare105mm.ReflectionsEnable");
nuiDefSlider("Flare105mm.ReflectionsScale",0.0,2.0,0.1);
nuiDefSlider("Flare105mm.ReflectionsOpacity",0.0,1.0,0.1);
nuiDefSlider("Flare105mm.ReflectionsBlur",0.0,100.0,1.0);
nuiPushControlGroup("Flare105mm.Reflections");
	nuiGroupControl("Flare105mm.ReflectionsEnable");
	nuiGroupControl("Flare105mm.ReflectionsScale");
	nuiGroupControl("Flare105mm.ReflectionsOpacity");
	nuiGroupControl("Flare105mm.ReflectionsBlur");
nuiPopControlGroup();




