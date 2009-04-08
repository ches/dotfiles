nuiPushToolBox("Layer");
	nuiToolBoxItem("AE_Layers", AE_Layers(0,0,0,"Over"));
	/*
	nuiToolBoxItem("AE_Layer_Dissolve", AE_Layer_Dissolve(0,0));
	nuiToolBoxItem("AE_Layer_Darken", AE_Layer_Darken(0,0));
	nuiToolBoxItem("AE_Layer_ColorBurn", AE_Layer_ColorBurn(0,0));
	nuiToolBoxItem("AE_Layer_Lighten", AE_Layer_Lighten(0,0));
	nuiToolBoxItem("AE_Layer_ColorDodge", AE_Layer_ColorDodge(0,0));
	nuiToolBoxItem("AE_Layer_Overlay", AE_Layer_Overlay(0,0));
	nuiToolBoxItem("AE_Layer_Softlight", AE_Layer_Softlight(0,0));
	nuiToolBoxItem("AE_Layer_HardLight", AE_Layer_HardLight(0,0));
	nuiToolBoxItem("AE_Layer_LinearLight", AE_Layer_LinearLight(0,0));
	nuiToolBoxItem("AE_Layer_IntenseLight", AE_Layer_IntenseLight(0,0));
	nuiToolBoxItem("AE_Layer_Hue", AE_Layer_Hue(0,0));
	nuiToolBoxItem("AE_Layer_Saturation", AE_Layer_Saturation(0,0));
	nuiToolBoxItem("AE_Layer_Color", AE_Layer_Color(0,0));
	nuiToolBoxItem("AE_Layer_Luminosity", AE_Layer_Luminosity(0,0));
	nuiToolBoxItem("AE_Layer_ModelAlpha", AE_Layer_ModelAlpha(0,0,0));
	nuiToolBoxItem("AE_Layer_LuminanceModel", AE_Layer_LuminanceModel(0,0));
	nuiToolBoxItem("AE_Layer_NotAlpha", AE_Layer_NotAlpha(0,0,0));
	nuiToolBoxItem("AE_Layer_NotLuminance", AE_Layer_NotLuminance(0,0));
	nuiToolBoxItem("AE_Layer_Freeze", AE_Layer_Freeze(0,0));
	nuiToolBoxItem("AE_Layer_Heat", AE_Layer_Heat(0,0));
	nuiToolBoxItem("AE_Layer_Substractive", AE_Layer_Substractive(0,0));
	nuiToolBoxItem("AE_Layer_Interpolation", AE_Layer_Interpolation(0,0));
	*/
nuiPopToolBox();

nuxDefMultiChoice("AE_Layers.mode", "Over|Dissolve|Darken|Mult|ColorBurn|Add|Lighten|Screen|ColorDodge|Overlay|SoftLight|HardLight|LinearLight|IntenseLight|Sub|Xor|Hue|Saturation|Color|Luminosity|ModelAlpha|LuminanceModel|NotAlpha|NotLuminance|AddAlpha|Premult|Freeze|Heat|Substractive|Interpolation");
