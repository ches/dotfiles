nuiPushMenu("Tools");
    nuiPushToolBox("Image");
        nuiToolBoxItem("FractalNoise",FractalNoise());
    nuiPopToolBox();
nuiPopMenu();

nuiDefTweakerCol("FractalNoise",1," FractalNoise",-5,300);
nuiDefTweakerCol("FractalNoise",2," ",-5,200);
nuiDefTweakerCol("FractalNoise",3," ",-5,170);
nuiDefTweakerCol("FractalNoise",4," Mult",-5,400);

nuxDefMultiChoice("FractalNoise.Quality", "Best|High|Normal|Low|Poor");
nuxDefMultiChoice("FractalNoise.Type", "noise3d|fnoise3d|turbulence3d");
nuxDefExprToggle("FractalNoise.Colored");

nuiDefPCtrlLoc("FractalNoise.bytes",1);
nuiDefPCtrlLoc("FractalNoise.Quality",1);
nuiDefPCtrlLoc("FractalNoise.Type",1);
nuiDefPCtrlLoc("FractalNoise.Colored",1);

nuiDefPCtrlLoc("FractalNoise.Width",1);
nuiDefPCtrlLoc("FractalNoise.Height",1);
nuiDefPCtrlLoc("FractalNoise.ColorShift",1);
nuiDefPCtrlLoc("FractalNoise.xSize",2);
nuiDefPCtrlLoc("FractalNoise.ySize",2);
nuiDefPCtrlLoc("FractalNoise.xIteration",2);
nuiDefPCtrlLoc("FractalNoise.yIteration",2);
nuiDefPCtrlLoc("FractalNoise.Phase",3);
nuiDefPCtrlLoc("FractalNoise.Speed",3);

nuiDefSlider("FractalNoise.ColorShift",-5,5);
nuiDefSlider("FractalNoise.Width",0,1);
nuiDefSlider("FractalNoise.Height",0,1);
nuiDefSlider("FractalNoise.Quality",0,100);
nuiDefSlider("FractalNoise.xSize",0,100);
nuiDefSlider("FractalNoise.ySize",0,100);
nuiDefSlider("FractalNoise.xIteration",0,100);
nuiDefSlider("FractalNoise.yIteration",0,100);
nuiDefSlider("FractalNoise.Speed",0,10);