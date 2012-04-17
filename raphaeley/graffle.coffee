

# Utility

tau = 6.28318530717958647692528676655900576839433879875021
modulo = (num, mod) ->
  result = num % mod
  result += mod if result < 0
  result

# Simulation impl

sim = {}

sim.turtleFn =
  clone: (mods = {}) ->
    baby = sim.newTurtle(@, mods)
    sim.turtles.push baby
    baby
  #...maybe have turtle-sets like jquery-sets ?
  forward: (dist = 1) ->
    @x = modulo (@x + dist * Math.cos @heading), sim.patches.width
    @y = modulo (@y + dist * Math.sin @heading), sim.patches.height
    @
  rotateLeft: (amount = tau / 4) ->
    @heading = modulo (@heading + amount), tau
    @
  rotateRight: (amount = tau / 4) ->
    @heading = modulo (@heading - amount), tau
    @

sim.turtleDaemons = {}

sim.patchFn = {}
sim.patchDaemons = {}

sim.newTurtle = (->
  Turtle = ->
  Turtle.prototype = sim.turtleFn
  return (attrs...) -> _.extend(new Turtle(), attrs...)
  )()
sim.newPatch = (->
  Patch = ->
  Patch.prototype = sim.patchFn
  return (attrs...) -> _.extend(new Patch(), attrs...)
  )()
sim.time = 0 #time not turn because turn sounds like rotation


simATurn = (sim) ->
  for own fnName, condition of sim.turtleDaemons
    fn = sim.turtleFn[fnName]
    for turtle in sim.turtles
      if condition.apply(turtle)
        fn.apply(turtle)
  for own fnName, condition of sim.patchDaemons
    fn = sim.patchFn
    sim.patches.each (patch) ->
      if condition.apply(patch)
        fn.apply(patch)
  for own fnName, fn of sim.globalDaemons
    fn()
  sim.time += 1
  return

#The rest of the code is UI stuff

guiState =
  canvas: null
  isRunning: false
  runningTimer: null

renderToCanvas = ->
  canvas = guiState.canvas
  ctx = canvas.getContext('2d')
  width = canvas.width
  height = canvas.height
  canvas.width = canvas.width # clear the canvas

  ctx.save()
#  ctx.scale(canvas.width / grid.width, canvas.height / grid.height)
  ctx.scale(canvas.width / sim.patches.width, canvas.height / sim.patches.height)

  sim.patches.each (patch, x, y) ->
    ctx.fillStyle = if typeof patch.color == 'function' then patch.color() else patch.color
    ctx.fillRect(x, y, 0.95, 0.95)
  
  for turtle in sim.turtles
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


eachTurn = ->
  simATurn sim
  renderToCanvas()
  $('#turn').text(sim.time)
  $('#turtles').text(sim.turtles.length)

#TODO use http://ace.ajax.org/ for code editor/syntax hilight etc.
compileCodeOnPage = ->
  try
    $('#error').text('')
    codeInCoffee = $('.script').text()
    codeInJS = CoffeeScript.compile codeInCoffee, bare: true
      #bare because "new Function()" will make it non-bare anyway
    console.log(codeInJS) if console and console.log
    code = new Function("sim", "tau", "modulo", codeInJS)
    fns = code sim, tau, modulo
    sim.initState = fns.initState
    sim.initDaemons = fns.initDaemons
    return true
  catch error
    console.log error, error.message, error.stack if console and console.log
    $('#error').text("Error " + error.message)
    return false

startRunning = ->
  guiState.isRunning = true
  if not guiState.runningTimer
    go = ->
      eachTurn()
      guiState.runningTimer = setTimeout(go, 250)
    go()

stopRunning = ->
  guiState.isRunning = false
  if guiState.runningTimer
    clearTimeout guiState.runningTimer
    guiState.runningTimer = null

$ ->
  canvas = guiState.canvas = $('#gameCanvas')[0]
  canvas.width = 600
  canvas.height = 600
  compileCodeOnPage()
  sim.initState()
  sim.initDaemons()
  #TODO Should this code clear turtles/patches/turtleFn/etc? or rely on their code to do it or.
  $('#restart').click ->
    sim.initState()
    sim.initDaemons()
    startRunning() # ?
  $('#reload').click ->
    if compileCodeOnPage()
      sim.initState()
      sim.initDaemons()
      startRunning() # ?
  $('#redaemon').click ->
    if compileCodeOnPage()
      sim.initDaemons()
  $('#pause_resume').click ->
    if guiState.isRunning then stopRunning() else startRunning()
  startRunning()

