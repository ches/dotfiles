nuiPushMenu("Tools");
    nuiPushToolBox("User");
        nuiToolBoxItem("MatteMonitor",MatteMonitor());
    nuiPopToolBox();
nuiPopMenu();

nuxDefToggle("MatteMonitor.alphaOnly");
nuiDefSlider("MatteMonitor.lowBorder", 0, 0.1, 0.001);
nuiDefSlider("MatteMonitor.highBorder", 0.9, 1, 0.001);

nuiPushControlGroup("MatteMonitor.setup");
	nuiGroupControl("MatteMonitor.lowValue");
	nuiGroupControl("MatteMonitor.highValue");
	nuiGroupControl("MatteMonitor.lowBorder");
	nuiGroupControl("MatteMonitor.highBorder");
nuiPopControlGroup();