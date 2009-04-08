/*
ParanoiaFX | VolumetricLight 1.1
(c) 2006, Stefan Thamm
kontakt@paranoiafx.com
*/

nuiPushMenu("Tools");
    nuiPushToolBox("ParanoiaFX");
        nuiToolBoxItem("VolumetricLight",VolumetricLight());
    nuiPopToolBox();
nuiPopMenu();

nuiPushControlGroup("LightCenter");
	nuiGroupControl("VolumetricLight.LightCenterX");
 	nuiGroupControl("VolumetricLight.LightCenterY");
nuiPopControlGroup();
nuiAddPointOsc("VolumetricLight.LightCenter");
nuiDefSlider("VolumetricLight.LightCenterX",0,width,0);
nuiDefSlider("VolumetricLight.LightCenterY",0,height,0);
nuiDefSlider("VolumetricLight.RaysLength",0,2,0);
nuiDefSlider("VolumetricLight.red",0,3,0);
nuiDefSlider("VolumetricLight.green",0,3,0);
nuiDefSlider("VolumetricLight.blue",0,3,0);
nuiDefSlider("VolumetricLight.RaysBrightness",0,5,0);
nuiDefSlider("VolumetricLight.RaysSmoothness",0,200,0.1);
nuiDefSlider("VolumetricLight.EdgeStrength",0,10,0.0001);
nuiDefSlider("VolumetricLight.EdgeThreshold",0,1,0.0001);
nuiDefSlider("VolumetricLight.QualityLevel",1,3,0);
nuxDefMultiChoice("VolumetricLight.Show", "result|edges");