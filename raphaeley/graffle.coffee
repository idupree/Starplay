

global = {}
tau = global.tau = 6.28318530717958647692528676655900576839433879875021
modulo = global.modulo = (num, mod) ->
	result = num % mod
	result += mod if result < 0
	result
global.turtleFn =
	clone: (mods = {}) ->
		baby = _.extend(global.newTurtle(), @, mods)
		global.turtles.push baby
		baby
	#...maybe have turtle-sets like jquery-sets ?
	forward: (dist = 1) ->
		@x = modulo (@x + dist * Math.cos @heading), global.patches.width
		@y = modulo (@y + dist * Math.sin @heading), global.patches.height
		@
	rotateLeft: (amount = tau / 4) ->
		@heading = modulo (@heading + amount), tau
		@
	rotateRight: (amount = tau / 4) ->
		@heading = modulo (@heading - amount), tau
		@

global.turtleDaemons = {}

global.patchFn = {}
global.patchDaemons = {}

global.newTurtle = (->
	Turtle = ->
	Turtle.prototype = global.turtleFn
	-> new Turtle()
	)()
global.newPatch = (->
	Patch = ->
	Patch.prototype = global.patchFn
	-> new Patch()
	)()
global.time = 0 #time not turn because turn sounds like rotation


simATurn = (global) ->
	for own fnName, condition of global.turtleDaemons
		fn = global.turtleFn[fnName]
		for turtle in global.turtles
			if condition.apply(turtle)
				fn.apply(turtle)
	for own fnName, condition of global.patchDaemons
		fn = global.patchFn
		global.patches.each (patch) ->
			if condition.apply(patch)
				fn.apply(patch)
	for own fnName, fn of global.globalDaemons
		fn()
	global.time += 1
	return

#The rest of the code is UI stuff

renderToCanvas = (global, canvas) ->
	ctx = canvas.getContext('2d')
	width = canvas.width
	height = canvas.height
	canvas.width = canvas.width # clear the canvas

	ctx.save()
#	ctx.scale(canvas.width / grid.width, canvas.height / grid.height)
	ctx.scale(canvas.width / global.patches.width, canvas.height / global.patches.height)

	global.patches.each (patch, x, y) ->
		ctx.fillStyle = if typeof patch.color == 'function' then patch.color() else patch.color
		ctx.fillRect(x, y, 0.95, 0.95)
	
	for turtle in global.turtles
		ctx.fillStyle = if typeof turtle.color == 'function' then turtle.color() else turtle.color
		ctx.save()
		ctx.translate(turtle.x, turtle.y)
		ctx.rotate(turtle.heading)
		ctx.beginPath()
		ctx.moveTo(0.4, 0)
		ctx.lineTo(-0.4, -0.3)		
		ctx.lineTo(-0.4, 0.3)
		ctx.fill()
		ctx.restore()

	ctx.restore()


eachTurn = (global, canvas) ->
	simATurn global
	renderToCanvas global, canvas
	$('#turn').text(global.time)
	$('#turtles').text(global.turtles.length)

#TODO use http://ace.ajax.org/ for code editor/syntax hilight etc.
compileCodeOnPage = ->
	try
		$('#error').text('')
		codeInCoffee = $('.script').text()
		codeInJS = CoffeeScript.compile codeInCoffee, bare: true
			#bare because "new Function()" will make it non-bare anyway
		$('#compileDebug').text(codeInJS)
		code = new Function("global", codeInJS)
		$('#compileDebug').text(code+"")
		fns = code global
		global.initState = fns.initState
		global.initDaemons = fns.initDaemons
		return true
	catch error
		console.log error, error.message, error.stack if console and console.log
		$('#error').text("Error " + error.message)
		return false

startRunning = (global, canvas) ->
	global.isRunning = true
	if not global.runningTimer
		go = ->
			eachTurn global, canvas
			global.runningTimer = setTimeout(go, 250)
		go()

stopRunning = ->
	global.isRunning = false
	if global.runningTimer
		clearTimeout global.runningTimer
		global.runningTimer = null

$ ->
	canvas = $('#gameCanvas')[0]
	canvas.width = 600
	canvas.height = 600
	compileCodeOnPage()
	global.initState()
	global.initDaemons()
	#TODO Should this code clear turtles/patches/turtleFn/etc? or rely on their code to do it or.
	$('#restart').click ->
		global.initState()
		global.initDaemons()
		startRunning global, canvas # ?
	$('#reload').click ->
		if compileCodeOnPage()
			global.initState()
			global.initDaemons()
			startRunning global, canvas # ?
	$('#redaemon').click ->
		if compileCodeOnPage()
			global.initDaemons()
	$('#pause_resume').click ->
		if global.isRunning then stopRunning() else startRunning global, canvas
	startRunning global, canvas

