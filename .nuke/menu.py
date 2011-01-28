import myTools

#
# Gizmos and UI
#
nodes_toolbar = nuke.toolbar("Nodes")
my_tools_menu = nodes_toolbar.addMenu('My Tools')
my_tools_menu.addCommand('SliceTool', 'nuke.createNode("SliceTool")')
my_tools_menu.addCommand('AutoBackdrop', 'myTools.autoBackdrop()', 'alt+b')

#RGS-TRAPCODE-001
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

# The Particular V2 Gizmo
import ParticularV2_3DScript
tcMenu = nodes_toolbar.addMenu("Trapcode")
tcMenu.addCommand("Particular 3D Gizmo", "ParticularV2_3DScript.pv2_createGizmo(nuke)")

#create callbacks
ParticularV2_3DScript.pv2_createCallbacks()

#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
