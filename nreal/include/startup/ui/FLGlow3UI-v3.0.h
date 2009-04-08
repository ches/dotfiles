nuiPushMenu("Tools");
    nuiPushToolBox("User");
        nuiToolBoxItem("FLGlow3",FLGlow3());
    nuiPopToolBox();
nuiPopMenu();

nuiPushControlGroup("Burn Settings");
	     nuiGroupControl("FLGlow3.BurnFade");
           nuiGroupControl("FLGlow3.Burn_Lo");
           nuiGroupControl("FLGlow3.Burn_Hi");
nuiPopControlGroup();

nuxDefRadioBtnCtrl( 
"MatteChannel", 
1, 1, 0, 
"N|ux/none", 
"R|ux/red", 
"G|ux/green",
"B|ux/blue",
"A|ux/alpha" 
); 


nuiDefSlider("FLGlow3.Spread",0,200,0.1);
nuiDefSlider("FLGlow3.GlowFade",0,2,0);
nuiDefSlider("FLGlow3.Glow_Lo",0,2,0);
nuiDefSlider("FLGlow3.Glow_Hi",0,2,0);
nuxDefExprToggle("FLGlow3.Burn");
nuiDefSlider("FLGlow3.BurnFade",0,1,0);
nuiDefSlider("FLGlow3.Burn_Lo",0,2,0);
nuiDefSlider("FLGlow3.Burn_Hi",0,2,0);
nuiDefSlider("FLGlow3.red",0,3,0);
nuiDefSlider("FLGlow3.green",0,3,0);
nuiDefSlider("FLGlow3.blue",0,3,0);
nuiDefSlider("FLGlow3.Aspect",0,2,0);
