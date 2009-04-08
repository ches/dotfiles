nuiPushMenu("Tools");
    nuiPushToolBox("Other");
        nuiToolBoxItem("NormalLight3Dv4",NormalLight3Dv4());
    nuiPopToolBox();
nuiPopMenu();

nuiPushControlGroup("Light");
	nuiGroupControl("NormalLight3Dv4.xCenter");
	nuiGroupControl("NormalLight3Dv4.yCenter");
	nuiGroupControl("NormalLight3Dv4.zCenter");
	nuiGroupControl("NormalLight3Dv4.falloffRadius");
	nuiGroupControl("NormalLight3Dv4.FalloffEffect");
	nuiGroupControl("NormalLight3Dv4.falloff");

nuiPopControlGroup();
nuiPushControlGroup("Material");
	nuiGroupControl("NormalLight3Dv4.CompColor");
	nuiGroupControl("NormalLight3Dv4.EmitDiffuse");
	nuiPushControlGroup("DiffuseColor");
	 	nuiGroupControl("NormalLight3Dv4.DiffuseRed");
		nuiGroupControl("NormalLight3Dv4.DiffuseGreen");
		nuiGroupControl("NormalLight3Dv4.DiffuseBlue");
	nuiPopControlGroup();
	nuiPushControlWidget("DiffuseColor",nuiConnectColorTriplet(kRGBToggle,kCurrentColor,1));

	nuiGroupControl("NormalLight3Dv4.EmitSpecular");
	nuiPushControlGroup("SpecularColor");
		nuiGroupControl("NormalLight3Dv4.SpecRed");		
		nuiGroupControl("NormalLight3Dv4.SpecGreen");
		nuiGroupControl("NormalLight3Dv4.SpecBlue");
	nuiPopControlGroup();
	nuiPushControlWidget("SpecularColor",nuiConnectColorTriplet(kRGBToggle,kCurrentColor,1));

	nuiGroupControl("NormalLight3Dv4.Specularity");
nuiPopControlGroup();

nuiPushControlGroup("Renderer");
	nuiGroupControl("NormalLight3Dv4.NormalType");
	nuiGroupControl("NormalLight3Dv4.UseZDepth");
	nuiGroupControl("NormalLight3Dv4.AlphaSource");
	nuiGroupControl("NormalLight3Dv4.SceneDepth");
	nuiGroupControl("NormalLight3Dv4.LightQuality");
	nuiGroupControl("NormalLight3Dv4.aspectRatio");
nuiPopControlGroup();


nuiDefSlider("NormalLight3Dv4.outBytes",0,1,4);
nuiDefSlider("NormalLight3Dv4.xCenter",0,width,1);
nuiDefSlider("NormalLight3Dv4.yCenter",0,height,1);
nuiDefSlider("NormalLight3Dv4.zCenter",-width,width,1);
nuiDefSlider("NormalLight3Dv4.SpecularRolloff",0,0.1323,0);
nuiDefSlider("NormalLight3Dv4.aspectRatio",0,2,0);
nuiDefSlider("NormalLight3Dv4.falloffRadius",0,width,0);
nuiDefSlider("NormalLight3Dv4.LightQuality",0,1,0);
nuiDefSlider("NormalLight3Dv4.FalloffEffect",0,1,0);
nuxDefExprToggle("NormalLight3Dv4.EmitSpecular");
nuxDefExprToggle("NormalLight3Dv4.EmitDiffuse");
nuxDefExprToggle("NormalLight3Dv4.CompColor");
nuxDefRadioBtnCtrl( "NormalLight3Dv4.NormalType",1,4,0, "1|ux/NormalLight3Dv4.NormalExpA", "2|ux/NormalLight3Dv4.NormalExpB");
nuxDefExprToggle("NormalLight3Dv4.UseZDepth");
nuxDefRadioBtnCtrl("NormalLight3Dv4.AlphaSource", 0, 1, 1, "1|Normal", "2|ZDepth", "3|Color", "4|None");
nuiDefSlider("NormalLight3Dv4.DiffuseRed",0,3,0);
nuiDefSlider("NormalLight3Dv4.DiffuseGreen",0,3,0);
nuiDefSlider("NormalLight3Dv4.DiffuseBlue",0,3,0);
nuiDefSlider("NormalLight3Dv4.Specular Strength",0,3,0);
nuiDefSlider("NormalLight3Dv4.SpecularRed",0,3,0);
nuiDefSlider("NormalLight3Dv4.SpecularGreen",0,3,0);
nuiDefSlider("NormalLight3Dv4.SpecularBlue",0,3,0);