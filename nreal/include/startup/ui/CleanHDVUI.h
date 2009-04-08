nuiPushMenu("Tools");
    nuiPushToolBox("User");
        nuiToolBoxItem("@CleanHDV",CleanHDV());
    nuiPopToolBox();
nuiPopMenu();

nuiDefSlider("CleanHDV.BlurX",0,200,0.1);
nuiDefSlider("CleanHDV.BlurY",0,200,0.1);
nuiDefSlider("CleanHDV.analyseGrain",0,1,0);
nuiDefControlAlias("CleanHDV.analyseGrain","analyse");
nuiDefSlider("CleanHDV.temporalDegrain",0,1,0);
nuiDefControlAlias("CleanHDV.temporalDegrain","temporal");
nuiDefSlider("CleanHDV.showRemovedGrain",0,1,0);
nuiDefControlAlias("CleanHDV.showRemovedGrain","showRemovedGrain");
nuiDefSlider("CleanHDV.medianThreshold",0,1,0);
nuiDefSlider("CleanHDV.highsLoVal",0,1,0);
nuiDefSlider("CleanHDV.highsHiVal",0,1,0);
nuiDefSlider("CleanHDV.highsLoSmooth",0,1,0);
nuiDefSlider("CleanHDV.highsHiSmooth",0,1,0);
nuiDefSlider("CleanHDV.lowsLoVal",0,1,0);
nuiDefSlider("CleanHDV.lowsHiVal",0,1,0);
nuiDefSlider("CleanHDV.lowsLoSmooth",0,1,0);
nuiDefSlider("CleanHDV.lowsHiSmooth",0,1,0);

