image NormalLight3Dv4(
image Normal_In=0,
image ZDepth_In=0,
image Color_In=0,
int outBytes = bytes,
float xCenter=width * .75,
float yCenter=height * .75,
float zCenter=200,
float falloffRadius=width,
float falloff=0.5,
float FalloffEffect=0.5,
int CompColor=0,
int EmitDiffuse=1,
float DiffuseRed=1,
float DiffuseGreen=1,
float DiffuseBlue=1,
int EmitSpecular=0,
float SpecRed=1,
float SpecGreen=1,
float SpecBlue=1,
float Specularity=0.05,
float LightQuality=0.5,
int NormalType=1,
int UseZDepth=0,
int AlphaSource=1,
float SceneDepth=width * 2,
float aspectRatio=defaultAspect
)
{

    Color1 = Color(Normal_In.width, Normal_In.height, 1, LightPos.CompColor, LightPos.CompColor, LightPos.CompColor, 1, 0);
    Color2 = Color(Normal_In.width, Normal_In.height, 1, 0, 0, 0, 1, 0);
    ColorIn = Reorder(Color_In, "");
    LightPos = RGrad(Normal_In.width, Normal_In.height, 4, xCenter, yCenter, aspectRatio, 0, falloffRadius, falloff, 1, 1, (falloffRadius - fabs(zCenter))/falloffRadius, 1, zCenter, 0, 0, 0, 0, 0, float LightQuality = LightQuality, float Falloff = FalloffEffect, int EmitSpecular = EmitSpecular, int EmitDiffuse = EmitDiffuse, float SceneDepth = SceneDepth, int UseZDepth = UseZDepth, int CompColor = CompColor);
    Normal_In = Reorder(Normal_In, "");
    Ramp_Make_X_and_Z_vector_values2 = Ramp(Normal_In.width * LightQuality, Normal_In.height * LightQuality, 4, 1, 0.5, LightPos.xCenter * LightQuality, 0, LightPos.zCenter * LightQuality, 1, 0, (LightPos.xCenter * LightQuality) - width, 0, LightPos.zCenter, 1, 0, float LightQuality = LightPos.LightQuality);
    Ramp_Make_Y_vector_values3 = Ramp(Normal_In.width * LightQuality, Normal_In.height * LightQuality, 4, 0, 0.5, 0, LightPos.yCenter * LightQuality, 0, 1, 0, 0, (LightPos.yCenter * LightQuality) - height, 0, 1, 0, float LightQuality = LightPos.LightQuality);
    ZDepthIn = Reorder(ZDepth_In, "");

    Bytes3 = Bytes(Normal_In, 4);
	Expand1 = Expand(Bytes3, 0.5, rLo, rLo, 0, 1, rHi, rHi, 1);
	NormalType1 = Select(NormalType, Expand1, Bytes3);

    IAdd5 = IAdd(Ramp_Make_X_and_Z_vector_values2, Ramp_Make_Y_vector_values3, 1, 100);
    Resize1 = Resize(LightPos, width * LightPos.LightQuality, height * LightPos.LightQuality, "default", 0);
    Resize2 = Resize(ZDepthIn, width * LightPos.LightQuality, height * LightPos.LightQuality, "default", 0);

    Reorder1 = Reorder(Resize2, "00za");
    Mult1 = Mult(Reorder1, 0, 0, LightPos.SceneDepth, 1, 1);
	ColorX_Adjust_Normals_for_Specular = ColorX(NormalType1, r* cos( -atan2(r,b) ) - b * sin( -atan2(r,b) ), g* cos( -atan2(g,b) ) - b * sin( -atan2(g,b) ), g * sin( -atan2(g,b) ) + (r * sin( -atan2(r,b) ) + b * cos( -atan2(r,b) )) * cos( -atan2(g,b) ), a, z);
    ISub1 = ISub(Ramp_Make_X_and_Z_vector_values2, Mult1, 1, 100);
    IAdd6 = IAdd(ISub1, Ramp_Make_Y_vector_values3, 1, 100);
    Select_3D_or_2D_surface = Select(LightPos.UseZDepth + 1, IAdd5, IAdd6, 0);

	NormalizeA = ColorX(Select_3D_or_2D_surface, sqrt(r*r+g*g+b*b), g, b, a, z);
	NormalizeB = Reorder(NormalizeA, "rrrr");
    NormalizeC = Monochrome(NormalizeB, 1, 1, 1);
    NormalizeD = IDiv(Select_3D_or_2D_surface, NormalizeC, 1, 100, 1);

    IMult6 = IMult(NormalizeD, Resize1, 1, LightPos.Falloff * 100, 0);
    IMult8 = IMult(NormalizeD, Resize1, 1, LightPos.Falloff * 30, 0);
    Resize3 = Resize(IMult6, Normal_In.width, Normal_In.height, "default", 0);
    Resize5 = Resize(IMult8, Normal_In.width, Normal_In.height, "default", 0);
    IMult_Dot_Product6 = IMult(ColorX_Adjust_Normals_for_Specular, Resize5, 1, 100, 0);
    IMult_Dot_Product7 = IMult(NormalType1, Resize3, 1, 100, 0);
    Monochrome_Dot_Product3 = Monochrome(IMult_Dot_Product7, 1, 1, 1);
    Monochrome__Dot_Product6 = Monochrome(IMult_Dot_Product6, 1, 1, 1);
    Mult_Dot_Product5 = Mult(Monochrome_Dot_Product3, 3, 3, 3, 1, 1);
    Mult_Dot_Product8 = Mult(Monochrome__Dot_Product6, 3, 3, 3, 1, 1);
    Gamma_Specular = Gamma(Mult_Dot_Product8, Specularity, rGamma, rGamma, 1);
    Mult_Light_Color1 = Mult(Mult_Dot_Product5, DiffuseRed, DiffuseGreen, DiffuseBlue, 1, 1);
    Threshold2 = Threshold(Mult_Light_Color1, 0, red, red, 0, 0, 0);
    Mult_Specular_Color1 = Mult(Gamma_Specular, SpecRed, SpecGreen, SpecBlue, 1, 1);
    Select_EMIT_DIFFUSE = Select(LightPos.EmitDiffuse + 1, Color1, Threshold2, 0);
    IMult1 = IMult(ColorIn, Select_EMIT_DIFFUSE, 1, 100, 0);
    Threshold1 = Threshold(Mult_Specular_Color1, 0, red, red, 0, 0, 0);
    Select1 = Select(1, Threshold1, Color2, 0);
    Select_COMP_COLOR = Select(LightPos.CompColor + 1, Select_EMIT_DIFFUSE, IMult1, 0);
    IAdd1 = IAdd(Select_COMP_COLOR, Select1, 0, 100);
    Select_EMIT_SPEC = Select(LightPos.EmitSpecular + 1, Select_COMP_COLOR, IAdd1, 0);

	CompressNormal1 = Compress(Normal_In, 0.5, rLo, rLo, 0, 1, rHi, rHi, 1);
	SelectNormalType2 = Select(NormalType, Normal_In, CompressNormal1);
    Monochrome_Clean_Up = Monochrome(SelectNormalType2, 1, 1, 1);
    Mult_Clean_Up = Mult(Monochrome_Clean_Up, 3, 3, 3, 1, 1);
    IMult2 = IMult(Select_EMIT_SPEC, Mult_Clean_Up, 1, 100, 0);

	if(AlphaSource != 4){
		AlphaSelect = Select(AlphaSource, Normal_In, ZDepth_In, Color_In);
		SwitchMatte3 = SwitchMatte(IMult2, AlphaSelect, 1, "A", 1, 0);
		Bytes4 = Bytes(SwitchMatte3, outBytes);
	}
	else{
		Bytes4 = Bytes(IMult2, outBytes);
	}
    OUTPUT = Reorder(Bytes4, "");

    return OUTPUT;
}

