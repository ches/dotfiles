/* PxF_Shape 1.0 - (c) 2007 - PixelFudger
   Contact: Xavier Bourque - xbourque@gmail.com

Description:
	Quick shape generator using Shake's NGLRender function.
	
*/

image PxF_Shape(
int	width=		GetDefaultWidth(),
int	height=		GetDefaultHeight(),
int	nPoints=	3,
float radius = 70,
float rotation = 0,
float centerX= width / 2.0,
float centerY= height / 2.0,
const char *version = "PxF_Shape 1.0 - (c) 2007 - Xavier Bourque - www.pixelfudger.com"
)
{
    Render1 =  NGLRender(
	width,
	height,
	1,
	"
		
		
				
			nglPushMatrix();
				
				nglBegin(NGL_POLYGON);
				nglPointSize(3.0);
				nglColor3f(1,1,1);
				for(int i=0;i<=nPoints;i++) {
			        float j = (360.0 / nPoints) * i;
				    nglVertex2f( ((cosd(j + rotation) * radius) + centerX) , ((sind(j + rotation) * radius) + centerY) );
				
			    }
				
				nglEnd();
			nglPopMatrix();
	"
    );
	
	return Render1;
}



