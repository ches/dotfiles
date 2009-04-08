nuiPushMenu("Tools");
    nuiPushToolBox("PxF");
        nuiToolBoxItem("PxF_ScreenClean",PxF_ScreenClean());
    nuiPopToolBox();
nuiPopMenu();

nuiDefSlider("PxF_ScreenClean.ScreenColorR",0,1,0);
nuiDefSlider("PxF_ScreenClean.ScreenColorG",0,1,0);
nuiDefSlider("PxF_ScreenClean.ScreenColorB",0,1,0);

nuiPushControlGroup("PxF_ScreenClean.ScreenColor");
    nuiGroupControl("PxF_ScreenClean.ScreenColorR");
    nuiGroupControl("PxF_ScreenClean.ScreenColorG");
    nuiGroupControl("PxF_ScreenClean.ScreenColorB");
nuiPopControlGroup();
nuiPushControlWidget("PxF_ScreenClean.ScreenColor", nuiConnectColorTriplet(kRGBToggle,kAverageColor,1));

nuxDefMultiChoice("PxF_ScreenClean.ScreenType","red|green|blue");

nuxDefTextCtrl("PxF_ScreenClean.version", 1);