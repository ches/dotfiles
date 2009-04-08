nuiPushMenu("Tools");
    nuiPushToolBox("User");
        nuiToolBoxItem("Glow",Glow());
    nuiPopToolBox();
nuiPopMenu();

nuiDefSlider("Glow.glowSize",0,200,0.1);
nuiDefSlider("Glow.glowBrightness",0,5,0);
nuiDefSlider("Glow.glowLoVal",0,1,0);
nuiDefSlider("Glow.glowHiVal",0,1,0);

