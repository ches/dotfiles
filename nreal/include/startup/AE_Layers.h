image AE_Layers(image Foreground, image Background, image BackgroundColor,string mode)
{
	if (mode == "Over")
		return AE_Layer_Over(Foreground, Background);
	else if (mode == "Normal")
		return AE_Layer_Over(Foreground, Background);
	else if (mode == "Mult")
		return AE_Layer_Mult(Foreground, Background);
	else if (mode == "Add")
		return AE_Layer_Add(Foreground, Background);
	else if (mode == "Screen")
		return AE_Layer_Screen(Foreground, Background);
	else if (mode == "Sub")
		return AE_Layer_Sub(Foreground, Background);
	else if (mode == "Xor")
		return AE_Layer_Xor(Foreground, Background);
	else if (mode == "AddAlpha")
		return AE_Layer_AddAlpha(Foreground, Background);
	else if (mode == "Premult")
		return AE_Layer_Premult(Foreground, Background);
	else if (mode == "Dissolve")
		return AE_Layer_Dissolve(Foreground, Background);
	else if (mode == "Darken")
		return AE_Layer_Darken(Foreground, Background);
	else if (mode == "ColorBurn")
		return AE_Layer_ColorBurn(Foreground, Background);
	else if (mode == "Lighten")
		return AE_Layer_Lighten(Foreground, Background);
	else if (mode == "ColorDodge")
		return AE_Layer_ColorDodge(Foreground, Background);
	else if (mode == "Overlay")
		return AE_Layer_Overlay(Foreground, Background);
	else if (mode == "SoftLight")
		return AE_Layer_Softlight(Foreground, Background);
	else if (mode == "HardLight")
		return AE_Layer_HardLight(Foreground, Background);
	else if (mode == "LinearLight")
		return AE_Layer_LinearLight(Foreground, Background);
	else if (mode == "IntenseLight")
		return AE_Layer_IntenseLight(Foreground, Background);
	else if (mode == "Hue")
		return AE_Layer_Hue(Foreground, Background);
	else if (mode == "Saturation")
		return AE_Layer_Saturation(Foreground, Background);
	else if (mode == "Color")
		return AE_Layer_Color(Foreground, Background);
	else if (mode == "Luminosity")
		return AE_Layer_Luminosity(Foreground, Background);
	else if (mode == "ModelAlpha")
		return AE_Layer_ModelAlpha(Foreground, Background, BackgroundColor);
	else if (mode == "LuminanceMode")
		return AE_Layer_LuminanceModel(Foreground, Background);
	else if (mode == "NotAlpha")
		return AE_Layer_NotAlpha(Foreground, Background, BackgroundColor);
	else if (mode == "NotLuminance")
		return AE_Layer_NotLuminance(Foreground, Background);
	else if (mode == "Freeze")
		return AE_Layer_Freeze(Foreground, Background);
	else if (mode == "Heat")
		return AE_Layer_Heat(Foreground, Background);
	else if (mode == "Substractive")
		return AE_Layer_Substractive(Foreground, Background);
	else if (mode == "Interpolation")
		return AE_Layer_Interpolation(Foreground, Background);
	return Background;
}

image AE_Layer_Over(image	Foreground,image Background)
{
	return Over(Foreground,Background);	
}

image AE_Layer_Mult(image	Foreground,image Background)
{
	return IMult(Foreground,Background,1,100,0);	
}

image AE_Layer_Add(image	Foreground,image Background)
{
	return IAdd(Foreground,Background,1,100);	
}

image AE_Layer_Screen(image	Foreground,image Background)
{
	return Screen(Foreground,Background,1);	
}

image AE_Layer_Sub(image	Foreground,image Background)
{
	return ISubA(Foreground,Background,1,100);	
}

image AE_Layer_Xor(image	Foreground,image Background)
{
	return Xor(Foreground,Background,1,0);	
}

image AE_Layer_AddAlpha(image	Foreground,image Background)
{
	return Over(Foreground,Background,1,0,1);	
}

image AE_Layer_Premult(image	Foreground,image Background)
{
	return Over(Foreground,Background,1,1,0);	
}

image AE_Layer_Saturation(image	Foreground,image Background)
{
	ColorSpace1 = ColorSpace(Foreground, "rgb", "hls", 0.3, 0.59, 0.11);
	ColorSpace2 = ColorSpace(Background, "rgb", "hls", 0.3, 0.59, 0.11);
	Copy1 = Copy(ColorSpace1, ColorSpace2, 1, "rg");
	ColorSpace3 = ColorSpace(Copy1, "hls", "rgb", 0.3, 0.59, 0.11);
	Over1 = Over(ColorSpace3, Background, 1, 0, 0);
	return Over1;
}

image AE_Layer_Color(image Foreground,image Background)
{
	ColorSpace1 = ColorSpace(Foreground, "rgb", "hls", 0.3, 0.59, 0.11);
	ColorSpace2 = ColorSpace(Background, "rgb", "hls", 0.3, 0.59, 0.11);
	Copy1 = Copy(ColorSpace1, ColorSpace2, 1, "g");
	ColorSpace3 = ColorSpace(Copy1, "hls", "rgb", 0.3, 0.59, 0.11);
	Over1 = Over(ColorSpace3, Background, 1, 0, 0);
	return Over1;
}

image AE_Layer_Luminosity(image Foreground,image Background)
{
	ColorSpace1 = ColorSpace(Foreground, "rgb", "hls", 0.3, 0.59, 0.11);
	ColorSpace2 = ColorSpace(Background, "rgb", "hls", 0.3, 0.59, 0.11);
	Copy1 = Copy(ColorSpace1, ColorSpace2, 1, "rb");
	ColorSpace3 = ColorSpace(Copy1, "hls", "rgb", 0.3, 0.59, 0.11);
	Over1 = Over(ColorSpace3, Background, 1, 0, 0);
	return Over1;
}

image AE_Layer_Hue(image Foreground,image Background)
{
	ColorSpace1 = ColorSpace(Background, "rgb", "hls", 0.3, 0.59, 0.11);
	ColorSpace2 = ColorSpace(Foreground, "rgb", "hls", 0.3, 0.59, 0.11);
	Copy1 = Copy(ColorSpace1, ColorSpace2, 1, "r");
	ColorSpace3 = ColorSpace(Copy1, "hls", "rgb", 0.3, 0.59, 0.11);
	return ColorSpace3;
}

image AE_Layer_Lighten(image Foreground,image Background)
{
	Max1 = Max(Foreground,Background,1,100);
	return Max1;
}

image AE_Layer_Darken(image Foreground,image Background)
{
	Min1 = Min(Foreground,Background,1,100);
	return Min1;
}

image AE_Layer_Dissolve(image Foreground,image Background)
{
	Rand1 = Rand(Background.width, Background.height, 1, 0.4, time);
	LayerX1 = LayerX(Rand1, Foreground, r2, g2, b2, a2-a*(1-a2), z);
	Over1 = Over(LayerX1, Background, 1, 0, 0);
	return Over1;
}

image AE_Layer_ModelAlpha(image Foreground,image Background,image BackgroundColor)
{
	LayerX1 = LayerX(Foreground, Background, r2, g2, b2, a*a2, z);
	Over1 = Over(LayerX1, BackgroundColor, 1, 1, 0);
	return Over1;
}

image AE_Layer_LuminanceModel(image Foreground,image Background)
{
	ColorSpace1 = ColorSpace(Foreground, "rgb", "hls", 0.3, 0.59, 0.11);
	LayerX1 = LayerX(ColorSpace1, Background, g*r2, g*g2, g*b2, g*a2, z);
	return LayerX1;
}

image AE_Layer_NotAlpha(image Foreground,image Background,image BackgroundColor)
{
	LayerX1 = LayerX(Foreground, Background, r2, g2, b2, (1-a)*a2, z);
	Over1 = Over(LayerX1, BackgroundColor, 1, 1, 0);
	return Over1;
}

image AE_Layer_NotLuminance(image Foreground,image Background)
{
	ColorSpace1 = ColorSpace(Foreground, "rgb", "hls", 0.3, 0.59, 0.11);
	LayerX1 = LayerX(ColorSpace1, Background, (1.0-g)*r2, (1.0-g)*g2, (1.0-g)*b2, (1.0-g)*a2, z);
	return LayerX1;
}

image AE_Layer_Overlay(image Foreground,image Background)
{
	LayerX1 = LayerX(	Foreground, Background,
				r2<0.5?2*r*r2:1.0-2.0*(1-r)*(1-r2), 
				g2<0.5?2*g*g2:1.0-2.0*(1-g)*(1-g2), 
				b2<0.5?2*b*b2:1.0-2.0*(1-b)*(1-b2), 
				a2, z);
	return LayerX1;
}

image AE_Layer_ColorBurn(image Foreground,image Background)
{
	LayerX1 = LayerX(	Foreground, Background, 
				r==0?0:1.0-(1-r2)/r, 
				g==0?0:1.0-(1-g2)/g, 
				b==0?0:1.0-(1-b2)/b, 
				a2, z);
	return LayerX1;
}


image AE_Layer_ColorDodge(image Foreground,image Background)
{
	LayerX1 = LayerX(	Foreground, Background, 
				r==1?1:r2/(1-r), 
				g==1?1:g2/(1-g), 
				b==1?1:b2/(1-b), 
    				a2, z);
    	return LayerX1;
}

image AE_Layer_HardLight(image Foreground,image Background)
{
	LayerX1 = LayerX(	Foreground, Background,
				r<0.5?2*r*r2:1.0-2.0*(1-r)*(1-r2), 
				g<0.5?2*g*g2:1.0-2.0*(1-g)*(1-g2), 
				b<0.5?2*b*b2:1.0-2.0*(1-b)*(1-b2), 
				a2, z);
	return LayerX1;
}

image AE_Layer_Softlight(image Foreground,image Background)
{
	LayerX1 = LayerX(	Foreground, Background, 
				2*r*r2+r2*r2-2*r2*r2*r, 
				2*g*g2+g2*g2-2*g2*g2*g, 
	    			2*b*b2+b2*b2-2*b2*b2*b, 
	    			a2, z);
	return LayerX1;
}

image AE_Layer_LinearLight(image Foreground,image Background)
{
	// Lumiere vive
	LayerX1 = LayerX(	Foreground, Background, 
				r2==1?1:r*r/(1-r2), 
				g2==1?1:g*g/(1-g2), 
    				b2==1?1:b*b/(1-b2), 
    				a, z);
    	return LayerX1;
}

image AE_Layer_Freeze(image Foreground,image Background)
{
	LayerX1 = LayerX(	Foreground, Background, 
				r==0?0:1.0-(1.0-r2)*(1.0-r2)/r, 
				g==0?0:1.0-(1.0-g2)*(1.0-g2)/g, 
    				b==0?0:1.0-(1.0-b2)*(1.0-b2)/b, 
    				a, z);
    	return LayerX1;
}

image AE_Layer_Heat(image Foreground,image Background)
{
	LayerX1 = LayerX(	Foreground, Background, 
				r2==0?0:1.0-(1.0-r)*(1.0-r)/r2, 
				g2==0?0:1.0-(1.0-g)*(1.0-g)/g2, 
    				b2==0?0:1.0-(1.0-b)*(1.0-b)/b2, 
    				a, z);
    	return LayerX1;
}

image AE_Layer_Substractive(image Foreground,image Background)
{
	LayerX1 = LayerX(	Foreground, Background, 
				r+r2-1, 
				g+g2-1, 
    				b+b2-1, 
    				a, z);
    	return LayerX1;
}

image AE_Layer_Interpolation(image Foreground,image Background)
{
	LayerX1 = LayerX(	Foreground, Background, 
				0.5-0.25*cos(M_PI*r2)-0.25*cos(M_PI*r), 
				0.5-0.25*cos(M_PI*g2)-0.25*cos(M_PI*g), 
    				0.5-0.25*cos(M_PI*b2)-0.25*cos(M_PI*b), 
    				a, z);
    	return LayerX1;
}

image AE_Layer_IntenseLight(image Foreground,image Background)
{
	LayerX1 = LayerX(	Foreground, Background, 
				2*r+r2-1, 
				2*g+g2-1, 
    				2*b+b2-1, 
    				a, z);
    	return LayerX1;
}
