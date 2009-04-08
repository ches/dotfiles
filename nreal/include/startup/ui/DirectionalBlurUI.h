nuiPushMenu("Tools");
    nuiPushToolBox("ParanoiaFX");
        nuiToolBoxItem("DirectionalBlur",DirectionalBlur());
    nuiPopToolBox();
nuiPopMenu();

nuiDefSlider("DirectionalBlur.angle",-360,360,0.1);
nuiDefSlider("DirectionalBlur.Pixels",0,200,0.1);
nuiDefSlider("DirectionalBlur.spread",0,1,0);
nuiDefControlAlias("DirectionalBlur.spread","spread");
nuiDefControlAlias("DirectionalBlur.xFilter","xFilter");
nuiDefControlAlias("DirectionalBlur.yFilter","yFilter");

