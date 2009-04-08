nuiPushMenu("Tools");
    nuiPushToolBox("User");
        nuiToolBoxItem("DLightwrap",DLightwrap());
    nuiPopToolBox();
nuiPopMenu();

nuxDefRadioBtnCtrl(
		"DLightwrap.ViewMode",1,4,0,
		"0|ux/alphaedge",
		"1|ux/background",
		"2|ux/over",
		"3|ux/lightwraped"
			);

nuiDefSlider("DLightwrap.Edge_Gain",0,3,0);
nuiDefSlider("DLightwrap.InnerEdge_Blur",0,200,0.1);
nuiDefSlider("DLightwrap.OuterEdge_Blur",0,200,0.1);
nuiDefSlider("DLightwrap.OuterEdge_Gain",0,3,0);
nuiDefSlider("DLightwrap.Overall_Gain",0,3,0);
nuiDefSlider("DLightwrap.Overall_Blur",0,200,0.1);
nuiDefSlider("DLightwrap.ShadowWrap_Mix",0,100,0.1);
nuiDefSlider("DLightwrap.Background_Intensity",0,6,-1);
nuiDefSlider("DLightwrap.lightSpread",0,200,0.1);

nuiPushControlGroup("Foreground Edge Controls");
	nuiGroupControl("Edge_Gain");
	nuiGroupControl("InnerEdge_Blur");
	nuiGroupControl("OuterEdge_Blur");
	nuiGroupControl("OuterEdge_Gain");
	nuiGroupControl("Overall_Gain");
	nuiGroupControl("Overall_Blur");
nuiPopControlGroup();

nuiPushControlGroup("Background Control");
	nuiGroupControl("Background_Intensity");
	nuiGroupControl("lightSpread");
nuiPopControlGroup();

nuxDefToggle("DLightwrap.clipMode","ux/background.nri|ux/background.focus.nri", "ux/foreground.nri|ux/foreground.focus.nri");
nuxDefExprToggle("DLightwrap.preMultiply");

nuxDefToggle("DLightwrap.MatteOutput", "ux/lw_fg.nri|ux/lw_fg.focus.nri", "ux/lwfg_bg.nri|ux/lwfg_bg.focus.nri", "ux/n_over.nri|ux/n_over.focus.nri", "ux/lwbg_fg.nri|ux/lwbg_fg.focus.nri");