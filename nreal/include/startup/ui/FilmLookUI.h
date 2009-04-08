// FilmLook v1.0.1
// by Christoph Hasche
// Copyright 2008

// www.drago-design.com


nuiPushMenu("Tools");
    nuiPushToolBox("User");
        nuiToolBoxItem("FilmLook",FilmLook());
    nuiPopToolBox();
nuiPopMenu();

nuxDefTextCtrl("FilmLook.nfo",1);

nuiDefSlider("FilmLook.exposure",0,3,0);
nuiDefSlider("FilmLook.highlightValue",0,0.9,0);
nuiDefSlider("FilmLook.gammaValue",0,0.9,0);
nuiDefSlider("FilmLook.saturation",0,3,0);
nuiDefSlider("FilmLook.highlights",0,5,0);
nuiDefSlider("FilmLook.gamma",0,4,0);
nuiDefSlider("FilmLook.contrast",0,10,0);
nuiDefSlider("FilmLook.contrastCenter",0,1,0);
nuiDefSlider("FilmLook.contrastSoften",0,1,0);
nuiDefSlider("FilmLook.contrastR",0,10,0);
nuiDefSlider("FilmLook.compressR_Hi",0,1,0);
nuiDefSlider("FilmLook.rHi",0,1,0);
nuiDefSlider("FilmLook.rLo",0,1,0);
nuiDefSlider("FilmLook.gammaR",0,4,0);
nuiDefSlider("FilmLook.contrastG",0,10,0);
nuiDefSlider("FilmLook.compressG_Hi",0,1,0);
nuiDefSlider("FilmLook.gLo",0,1,0);
nuiDefSlider("FilmLook.gammaG",0,4,0);
nuiDefSlider("FilmLook.contrastB",0,10,0);
nuiDefSlider("FilmLook.gammaB",0,4,0);
nuiDefSlider("FilmLook.bHi",0,1,0);
nuiDefSlider("FilmLook.bLo",0,1,0);
nuiDefSlider("FilmLook.overallSaturation",0,3,0);

nuiPushControlGroup("FilmLook.exposure");
		nuiGroupControl("FilmLook.highlightValue");
		nuiGroupControl("FilmLook.gammaValue");
nuiPopControlGroup();

nuiPushControlGroup("FilmLook.channelR");
		nuiGroupControl("FilmLook.contrastR");
		nuiGroupControl("FilmLook.compressR_Hi");
		nuiGroupControl("FilmLook.compressR_Lo");
		nuiGroupControl("FilmLook.gammaR");
nuiPopControlGroup();

nuiPushControlGroup("FilmLook.channelG");
		nuiGroupControl("FilmLook.contrastG");
		nuiGroupControl("FilmLook.compressG_Hi");
		nuiGroupControl("FilmLook.compressG_Lo");
		nuiGroupControl("FilmLook.gammaG");
nuiPopControlGroup();

nuiPushControlGroup("FilmLook.channelB");
		nuiGroupControl("FilmLook.contrastB");
		nuiGroupControl("FilmLook.compressB_Hi");
		nuiGroupControl("FilmLook.compressB_Lo");
		nuiGroupControl("FilmLook.gammaB");
nuiPopControlGroup();