import nuke
import random

def randomGray(min = .1, max = .3):
    '''
    return a random darkish gray as hex code to assign to tile and gl colours
    '''
    v = random.random() * min + (max-min)
    return int('%02x%02x%02x%02x' % (v*255,v*255,v*255,v*255),16)

def autoBackdrop( padding=50, top = 40, fontSize = 40, text ='' ):
    '''
    auto fitting backdrop node intended for use as onCreate callback
    '''
    bd = nuke.nodes.BackdropNode()
    nodes = nuke.selectedNodes()
    bd.setSelected( False )
    minX = [ n.xpos()  for n in nodes]
    maxX = [ n.xpos() + n.screenWidth() for n in nodes]
    minY = [ n.ypos() for n in nodes]
    maxY = [ n.ypos() + n.screenHeight()  for n in nodes]
    minX.sort(), minY.sort(), maxX.sort(), maxY.sort()

    x, y = minX[0], minY[0]
    r, t = maxX[-1], maxY[-1]
    bd.setXYpos( x-padding, y-padding-top )
    bd['bdwidth'].setValue( r-x + 2*padding )
    bd['bdheight'].setValue( t-y +2*padding+top )
    bd['note_font_size'].setValue( fontSize )
    bd['tile_color'].setValue( randomGray() )

