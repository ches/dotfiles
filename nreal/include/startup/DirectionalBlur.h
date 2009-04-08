image DirectionalBlur(
image In=0,
float angle = 0,
float xCenter = width/2,
float yCenter = height/2,
float Pixels=0,
int spread=1,
const char *xFilter="gauss",
const char *yFilter=":xFilter",
const char *channels="rgba"
)
{
    preRotate = Rotate(In, -dirRotate.angle, defaultAspect, width/2, 
        height/2, 0, 0.5, 0);
    xBlur = Blur(preRotate, Pixels, 0, spread, xFilter, yFilter, 
        channels);
    dirRotate = Rotate(xBlur, angle, defaultAspect, width/2, 
        height/2, 0, 0.5, 0);
    
    return dirRotate;
}

