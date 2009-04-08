nuiPushMenu("Tools");
    nuiPushToolBox("PxF");
        nuiToolBoxItem("PxF_Shape",PxF_Shape());
    nuiPopToolBox();
nuiPopMenu();

nuiDefSlider("PxF_Shape.nPoints",3,360,1);
nuiDefSlider("PxF_Shape.radius",0,1000,1);
nuiDefSlider("PxF_Shape.rotation",0,360,1);
nuiDefSlider("PxF_Shape.centerX",0,720,1);
nuiDefSlider("PxF_Shape.centerY",0,486,1);

nuxDefTextCtrl("PxF_Shape.version", 1);
