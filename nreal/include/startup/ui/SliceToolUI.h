nuiPushMenu("Tools");
    nuiPushToolBox("User");
        nuiToolBoxItem("SliceTool",SliceTool());
    nuiPopToolBox();
nuiPopMenu();

nuxDefRadioBtnOCtrl("SliceTool.colorChannel", 1, 1, 0, "0|ux/color", "1|ux/red", "2|ux/green", "3|ux/blue");
nuxDefRadioBtnOCtrl("SliceTool.backgroundType", 1, 1, 0, "0|ux/wallpaper", "1|ux/lined", "2|ux/clear");
nuxDefRadioBtnOCtrl("SliceTool.backgroundColor", 1, 1, 0, "0|ux/radio/radio_black", "1|ux/radio/radio_white");
nuiDefSlider("SliceTool.graphSoftness", 0, 100, 1);
nuxDefRadioBtnOCtrl("SliceTool.graphPosition", 1, 1, 0, "0|ux/radio/radio_right", "1|ux/radio/radio_top", "2|ux/over", "3|ux/external");
nuxDefRadioBtnOCtrl("SliceTool.sliceColor", 1, 1, 0, "0|ux/inverse", "1|ux/red", "2|ux/green", "3|ux/blue");
nuiDefSlider("SliceTool.lineThickness", 1, 5, 1);
nuxDefMultiChoice("SliceTool.point1Name", "A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z");
nuxDefMultiChoice("SliceTool.point2Name", "A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z");
nuiDefSlider("SliceTool.point1X", 0, In1.width, 1);
nuiDefSlider("SliceTool.point1Y", 0, In1.height, 1);
nuiDefSlider("SliceTool.point2X", 0, In1.width, 1);
nuiDefSlider("SliceTool.point2Y", 0, In1.height, 1);
nuiAddPointOsc("SliceTool.point1");
nuiAddPointOsc("SliceTool.point2");

nuiPushControlGroup("SliceTool.setup");
nuiGroupControl("SliceTool.graphPosition");
nuiGroupControl("SliceTool.sliceColor");
nuiGroupControl("SliceTool.lineThickness");
nuiGroupControl("SliceTool.point1Name");
nuiGroupControl("SliceTool.point2Name");
nuiPopControlGroup();

nuiPushControlGroup("SliceTool.pointPositions");
nuiGroupControl("SliceTool.point1X");
nuiGroupControl("SliceTool.point1Y");
nuiGroupControl("SliceTool.point2X");
nuiGroupControl("SliceTool.point2Y");
nuiPopControlGroup();