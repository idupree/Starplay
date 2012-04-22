

# Utility

tau = 6.28318530717958647692528676655900576839433879875021
modulo = (num, mod) ->
  result = num % mod
  result += mod if result < 0
  result

rand = {
  # Returns an integer in [min, max)
  intInHalfOpenRange: (min, max) ->
    Math.floor(Math.random() * (max - min)) + min

  # Returns an integer in [min, max]
  intInClosedRange: (min, max) ->
    Math.floor(Math.random() * (max+1 - min)) + min

  arrayIndex: (arr) ->
    rand.intInHalfOpenRange(0, arr.length)

  arrayMember: (arr) ->
    arr[rand.intInHalfOpenRange(0, arr.length)]

  # average case O(1) for mostly-okay arrays (worst case O(n) if really unlucky),
  # up to likely O(n) for mostly-not-okay arrays.
  # Returns null if no item meets the predicate. (Or should it throw?)
  okayArrayMember: (arr, predicate) ->
    if arr.length == 0 then return null

    member = rand.arrayMember arr
    if predicate member then return member

    randomTries = Math.floor(arr.length / 10)
    for _ignored in [0..randomTries]
      member = rand.arrayMember arr
      if predicate member then return member

    okayArr = _.filter arr, predicate
    if okayArr.length != 0
    then return rand.arrayMember okayArr
    else return null
  #i dislike infinite loops, so don't do this:
  #  loop
  #    member = randArrayMember arr
  #    if predicate member then return member
  }


# Simulation impl

sim = {}

sim.turtleFn =
  clone: (mods = {}) ->
    baby = sim.newTurtle(@, mods)
    sim.turtles.push baby
    baby
  die: ->
    # TODO use a data structure for turtles that has O(1) delete
    sim.turtles = _.without(sim.turtles, @)
    null
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

sim.setAllPatches = (fn, width = 20, height = 20) ->
  sim.patches = ( ( sim.newPatch(fn(x,y), {x: x, y: y}) \
                  for y in [0...height]) for x in [0...width])
  sim.patches.width = width
  sim.patches.height = height
  sim.patches.each = (callback) ->
    for col, x in @
      for patch, y in col
        callback(patch, x, y)

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


generateWordNotIn = (notInObj) ->
  rand.okayArrayMember window.StarPlay.words,
      (word) -> not _.has notInObj, word

# This has a name (identifier-style(?) string),
# implementation (CoffeeScript text evaluating to a value, possibly of function type),
# and activation (CoffeeScript text evaluating to a function returning boolean, or nothing)
class TurtleFn extends Backbone.Model
  initialize: ->
    #if?
    @set name: generateWordNotIn sim.turtleFn if not @get('name')?
    @set implementation: '-> ' if not @get('implementation')?
    @set activation: null if not @get('activation')?

class TurtleFnList extends Backbone.Collection
  model: TurtleFn
  localStorage: new Store('StarPlay-TurtleFnList')


class TurtleFnView extends Backbone.View
  #tagName: 'li'
  #className: 'turtle-fn-view'
  make: -> @$domTemplate.clone()[0]
  $domTemplate: $ """
    <li
      ><span class="turtle-fn-name" contentEditable="true"></span
      ><span class="turtle-fn-implementation" contentEditable="true"></span
      ><span class="turtle-fn-activation" contentEditable="true"></span
    ></li>
    """
  events:
    'blur .turtle-fn-implementation': 'recompile'
    #'click .turtle-fn-delete': 'remove' #??maybe? perhaps deleting the name (or impl?) & it asks if you want to delete.
    'blur .turtle-fn-name': 'rename'
    'blur .turtle-fn-activation': 'reactivate'
  initialize: ->
    @render()
    #@model.on 'change', @recompile, @
    #@model.on 'destroy',
  recompile: ->
  rename: ->
  reactivate: ->
  render: ->
    @$('.turtle-fn-name').text @model.get 'name'
    @$('.turtle-fn-implementation').text @model.get 'implementation'
    @$('.turtle-fn-activation').text @model.get 'activation'
    @
    
  #later worry about codemirror

# name text turns red while it's the same as another? and has a popup or?'

thisPageTurtleFnList = new TurtleFnList

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
  thisPageTurtleFnList.on 'add', (model) ->
    $('#turtleFns').append new TurtleFnView(model: model).el

  newTurtleDaemon = (name, implementation, activation) ->
    thisPageTurtleFnList.create name: name, implementation: implementation, activation: activation
  newTurtleDaemon 'speed', '-> @forward 1', "-> @type == 'bullet'"
  newTurtleDaemon 'activateGun', "-> @clone type: 'bullet', color: 'red'", "-> @type == 'crazy' and sim.time % 8 == 0"
  
  window.StarPlay.wordsAjaxRequest.done -> $('#testplus').click -> thisPageTurtleFnList.create()
  window.StarPlay.wordsAjaxRequest.fail -> $('#testplus').hide()
  startRunning()
