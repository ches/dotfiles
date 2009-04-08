image MatteMonitor

(
image In=0,
int alphaOnly=1,
float lowValue=0.3,
float highValue=0.7,
float lowBorder=0.001,
float highBorder=0.999
)

{
Lookup = Lookup(In, alphaOnly==0?LinearV(x,1,0@0,lowValue@lowBorder,highValue@highBorder,1@1):JSplineV(x,1,0@0,1@1), alphaOnly==0?LinearV(x,1,0@0,lowValue@lowBorder,highValue@highBorder,1@1):JSplineV(x,1,0@0,1@1), alphaOnly==0?LinearV(x,1,0@0,lowValue@lowBorder,highValue@highBorder,1@1):JSplineV(x,1,0@0,1@1), LinearV(x,1,0@0,lowValue@lowBorder,highValue@highBorder,1@1));
return Lookup;
}