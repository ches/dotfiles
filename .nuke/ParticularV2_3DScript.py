#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////// 

#creates callbacks on startup
def pv2_createCallbacks():
	import nuke
	nuke.addKnobChanged(pv2_UIKnobChanged, nodeClass = 'Group')
	
	
#removes callbacks
def pv2_removeCallbacks():
	import nuke
	nuke.removeKnobChanged(pv2_UIKnobChanged, nodeClass = 'Group') 
	
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////	
	
#function to create gizmo on menu button click/tab menu select
def pv2_createGizmo(nuke):
 	#create 2 scene nodes and select
	epScene = nuke.createNode("Scene")
	epScene.setName("Emitters")
	epScene.setXpos(0)
	epScene.setYpos(-100)

	#now create gizmo to have it auto connect
	nuke.nodePaste("~/.nuke/Particular3DGroup.nk")
	nuke.extractSelected()
	gizmo = nuke.selectedNode()
	gizmo.setXpos(0)
	gizmo.setYpos(0)
	gizmo.setInput(1,epScene)
	nuke.show(gizmo)
	
	#add version check
	if nuke.NUKE_VERSION_MINOR < 1 :
		#change cam values to work with translate values only
		gizmoParticularPath = "%s.Particular v2_1" % gizmo.name()
		camExpressionX = "p_cam.translate.x * parent.pv2_camXMultiplier"
		camExpressionY = "p_cam.translate.y * parent.pv2_camYMultiplier"
		camExpressionZ = "p_cam.translate.z"
		
		print "Camera cannot be parented to Axis node"
		nuke.toNode(gizmoParticularPath)['CameraPos'].setExpression(camExpressionX, 0)
		nuke.toNode(gizmoParticularPath)['CameraPos'].setExpression(camExpressionY, 1)	
		nuke.toNode(gizmoParticularPath)['CameraPos'].setExpression(camExpressionZ, 2)			
		
	
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#runs on gizmo knob changed callback to decide action taken	
def pv2_UIKnobChanged():
	import nuke
	knobName = nuke.thisKnob().name()
	if knobName in ['pv2_getCameraList','pv2_cameraList','pv2_refreshEmitterList','pv2_addSelectedObj','pv2_emitterList', 'pv2_lockCardToDropDown'] :
		workingNode = nuke.thisNode()
		workingNodeName = nuke.thisNode().name()
			
		if knobName == 'pv2_getCameraList' :
			pv2_getCamerasPress(knobName, workingNode, workingNodeName)
			
		elif knobName == 'pv2_cameraList' :
			pv2_setCam(knobName, workingNode, workingNodeName)
			
		elif knobName == 'pv2_refreshEmitterList':
			pv2_emitterListRefresh(knobName, workingNode, workingNodeName)
			
		elif knobName == 'pv2_addSelectedObj':
			pv2_addSelectedNodeToEmitters(knobName, workingNode, workingNodeName)
			
		elif knobName == 'pv2_emitterList':
			pv2_emitterObjSelect(knobName, workingNode, workingNodeName)
			
		elif knobName == 'pv2_lockCardToDropDown':
			pv2_cardLockTo(knobName, workingNode, workingNodeName)
			
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////// 

#function that runs when Get Cameras is pressed
def pv2_getCamerasPress(knob, workingNode, workingNodeName):
	import nuke
	
	#get value
	currentCam = nuke.toNode(workingNodeName)['pv2_cameraList'].value()
	
	#get all cameras in scene
	camNames = pv2_getAllCams()

	#sort alphabetically
	camNames.sort()
	
	#add None to front
	camNames.insert(0, "None")

	#update list values
	nuke.toNode(workingNodeName)['pv2_cameraList'].setValues(camNames)
	
	#check old value still exists
	if currentCam in camNames :
		nuke.toNode(workingNodeName)['pv2_cameraList'].setValue(currentCam)
	else :
		nuke.toNode(workingNodeName)['pv2_cameraList'].setValue("None")
		pv2_clearPCamExpressions(knob, workingNode, workingNodeName)
		
		
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////// 

#function returns all cams
def pv2_getAllCams():
	import nuke
	rootGrp = nuke.root()
	
	try:
		allNodes = nuke.allNodes(group=rootGrp)
	except :
		allNodes = nuke.allNodes()
		
	allCams = []
	for each in allNodes:
		if each.Class() == "Camera2" :
			if each.name() != "p_cam" :
				allCams.append(each.name()[0:20])
				
	return allCams
	
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////// 

#function runs to clear expressions on p_cam
def pv2_clearPCamExpressions(knob, workingNode, workingNodeName):
	import nuke

	pcamName = "%s.p_cam" % workingNodeName
	pCamNode = nuke.toNode(pcamName)
	pCamNode['translate'].clearAnimated()
	pCamNode['rotate'].clearAnimated()
	pCamNode['focal'].clearAnimated()
	pCamNode['haperture'].clearAnimated()
	pCamNode['vaperture'].clearAnimated()
	
	
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////// 

#function that sets current cam
def pv2_setCam(knob, workingNode, workingNodeName):
	import nuke
	
	gizmoCamPath = "%s.p_cam" % workingNodeName
	
	#get selected camera
	successChecker = 0
	try :
		selectedCam = nuke.toNode(workingNodeName)[knob].value()
		successChecker = 1
		
	except :
		successChecker = 0
		
	if selectedCam == "None" or successChecker == 0 :
		#remove expressions on p_cam
		pv2_clearPCamExpressions(knob, workingNode, workingNodeName)
		pass
	
	else : 
	
		#set cam info values
		workingWidth = float(nuke.toNode('root')['format'].value().width())
		workingHeight = float(nuke.toNode('root')['format'].value().height())
		workingAspect = workingWidth / workingHeight
		workingFocal = nuke.toNode(selectedCam)['focal'].value()
		workingHaperture = nuke.toNode(selectedCam)['haperture'].value()
		CamXMultiplier = 1.0/(workingWidth/480)
		CamYMultiplier = CamXMultiplier * workingAspect
		CamZoom = (workingFocal/workingHaperture) * workingWidth
		
		#set UI values
		workingNode['pv2_projectWidth'].setValue(workingWidth)
		workingNode['pv2_projectHeight'].setValue(workingHeight)
		workingNode['pv2_aspect'].setValue(workingAspect)
		workingNode['pv2_camXMultiplier'].setValue(CamXMultiplier)
		workingNode['pv2_camYMultiplier'].setValue(CamYMultiplier)
		workingNode['pv2_camZMultiplier'].setValue(1)
		workingNode['pv2_camZoomSlider'].setValue(CamZoom)
		
		#set expressions on pcam
		camExpressionTX = selectedCam + '.world_matrix.3'
		camExpressionTY = selectedCam + '.world_matrix.7'
		camExpressionTZ = selectedCam + '.world_matrix.11'
		
		camExpressionR = selectedCam + '.rotate'
		camExpressionF = selectedCam + '.focal'
		camExpressionH = selectedCam + '.haperture'
		camExpressionV = selectedCam + '.vaperture'
		
		nuke.toNode(gizmoCamPath)['translate'].setExpression(camExpressionTX,0)
		nuke.toNode(gizmoCamPath)['translate'].setExpression(camExpressionTY,1)
		nuke.toNode(gizmoCamPath)['translate'].setExpression(camExpressionTZ,2)
		
		nuke.toNode(gizmoCamPath)['rotate'].setExpression(camExpressionR)
		nuke.toNode(gizmoCamPath)['focal'].setExpression(camExpressionF)
		nuke.toNode(gizmoCamPath)['haperture'].setExpression(camExpressionH)
		nuke.toNode(gizmoCamPath)['vaperture'].setExpression(camExpressionV)
		
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////// 

#function runs to refresh emitter list
def pv2_emitterListRefresh(knob, workingNode, workingNodeName):
	import nuke
	
	#get existing values
	existingValues = workingNode['pv2_emitterList'].values()
	currentValue = workingNode['pv2_emitterList'].value()
	
	#get all connected
	emittersScene = workingNode.input(1)
	notAttached = []
	notAttachedExists = []
	
	#make sure something is connected 
	if emittersScene : 
	
		#check it's a scene
		if emittersScene.Class() == 'Scene':
			#get all attached nodes
			allAttachedObjs = emittersScene.dependencies()
			allAttached = []
			for each in allAttachedObjs:
				allAttached.append(each.name())
	        	
		#get entries that arent connected and. . 
		for i in existingValues :
			if i not in allAttached:
				notAttached.append(i)
		
		#..confirm they still exist (skip 'None')
		for j in notAttached :
			if nuke.exists(j) == True:
				notAttachedExists.append(i)
		
		#add all existing entries together and sort
		for each in allAttached:
			notAttachedExists.append(each)
			
		notAttachedExists.sort()
		
	notAttachedExists.insert(0, "None")
	
	#repopulate list
	workingNode['pv2_emitterList'].setValues(notAttachedExists)
	
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////// 

#function runs on add selected button press
def pv2_addSelectedNodeToEmitters(knob, workingNode, workingNodeName):
	import nuke
	
	#get variables
	selectedNodes = nuke.toNode('root').selectedNodes()
	existingValues = workingNode['pv2_emitterList'].values()
	nodesToAdd = []
	
	if len(selectedNodes) > 0:
		for each in selectedNodes:
			#find out if it's valid
			validCheck = pv2_hasPositionCheck(each)
			if validCheck == 1:
				#node has transform and we can add it
				#check if they're already in the existing
		
				if each not in existingValues :
					nodesToAdd.append(each.name())
					
	#now update list
	existingValues.pop(0) #remove None
	for each in existingValues:
		nodesToAdd.append(each)
	nodesToAdd.sort()
	
	workingNode['pv2_emitterList'].setValues(nodesToAdd)
	
	#now refresh just in case
	pv2_emitterListRefresh(knob, workingNode, workingNodeName)
	
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
				
#DETERMINES IF NODE HAS POSITION VALUE
def pv2_hasPositionCheck(nodeToCheck):
	try: 
		translateVal = nodeToCheck['translate'].Class()
		if translateVal == "XYZ_Knob":
			return 1
		else:
			return 0
	
	except:
		return 0
		
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#function runs on emitter object select
def pv2_emitterObjSelect(knob, workingNode, workingNodeName):
	import nuke
	
	selectedObj = workingNode['pv2_emitterList'].value()
	rotChecker = 0
	
	if selectedObj == "None" :
		#remove expressions
		workingNode['pv2_proxyPos'].clearAnimated()
		workingNode['pv2_proxyPos'].setValue([0,0,0])
		workingNode['pv2_proxyRot'].clearAnimated()
		workingNode['pv2_proxyRot'].setValue([0,0,0])
	else :
		#set expressions on position
		
		if nuke.NUKE_VERSION_MINOR < 1 :
			#use translate values
			expressionName = selectedObj + '.translate'
			workingNode['pv2_proxyPos'].setExpression(expressionName)
			
		else : 
			#use world_matrix values
			expressionTX = selectedObj + '.world_matrix.3'
			expressionTY = selectedObj + '.world_matrix.7'
			expressionTZ = selectedObj + '.world_matrix.11'
		
			workingNode['pv2_proxyPos'].setExpression(expressionTX, 0)
			workingNode['pv2_proxyPos'].setExpression(expressionTY, 1)
			workingNode['pv2_proxyPos'].setExpression(expressionTZ, 2)
		
		
		#see if it has rotation values
		try:
			nuke.toNode(selectedObj)['rotate'].value()
			rotChecker = 1
		except:
			rotChecker = 0
    	
    	if rotChecker == 1:
    		expression2Name = selectedObj + '.rotate'
    		workingNode['pv2_proxyRot'].setExpression(expression2Name)
    	else :
    		workingNode['pv2_proxyRot'].setExpression("value")
    		
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#function runs on card lock to selection
def pv2_cardLockTo(knob, workingNode, workingNodeName):
	import nuke
	
	transNodeName = "%s.TransformGeo1" % workingNodeName
	transNode = nuke.toNode(transNodeName)
	
	currentMode = workingNode['pv2_lockCardToDropDown'].value()
	
	if currentMode == "None" :
		#set expressions to just normal translates
		xExpression = "parent.pv2_cardOffset.x"
		yExpression = "parent.pv2_cardOffset.y"
		zExpression = "parent.pv2_cardOffset.z"
		transNode['translate'].setExpression(xExpression, 0)
		transNode['translate'].setExpression(yExpression, 1)
		transNode['translate'].setExpression(zExpression, 2)
		
		#clear offset 
		workingNode['pv2_cardOffset'].setValue(0, 2)
		
	elif currentMode == "Emitter": 
		#set expressions to include emitter values translates
		xExpression = "parent.pv2_cardOffset.x + parent.pv2_proxyPos.x"
		yExpression = "parent.pv2_cardOffset.y + parent.pv2_proxyPos.y"
		zExpression = "parent.pv2_cardOffset.z + parent.pv2_proxyPos.z"
		transNode['translate'].setExpression(xExpression, 0)
		transNode['translate'].setExpression(yExpression, 1)
		transNode['translate'].setExpression(zExpression, 2)
		
		#clear offset 
		workingNode['pv2_cardOffset'].setValue(0, 2)
		
	elif currentMode == "Camera": 
		#lock transform geo values to p_cam values
		pcamName = "%s.p_cam" % workingNodeName
		pCamNode = nuke.toNode(pcamName)
		
		xExpression = "p_cam.translate.x + parent.pv2_cardOffset.x"
		yExpression = "p_cam.translate.y + parent.pv2_cardOffset.y"
		zExpression = "p_cam.translate.z + parent.pv2_cardOffset.z"
		transNode['translate'].setExpression(xExpression, 0)
		transNode['translate'].setExpression(yExpression, 1)
		transNode['translate'].setExpression(zExpression, 2)
		
		#set offset in Gizmo UI
		workingNode['pv2_cardOffset'].setValue(-0.75, 2)
	