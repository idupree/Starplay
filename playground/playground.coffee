

# Utility

tau = 6.28318530717958647692528676655900576839433879875021
modulo = (num, mod) ->
  result = num % mod
  result += mod if result < 0
  result

window.console ?= {}
window.console.log ?= ->

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

coffeeenv = sim: sim, tau: tau, modulo: modulo

#hack debug help
window.StarPlay.sim = sim
sim.fn = {}
sim.fn.turtle =
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


sim.fn.patch = {}
sim.fn.world =
  # transferAmountFn(patch1, patch2) must return the numerical amount
  # of propName transferred from patch1 to patch2 in a turn;
  # actually fn(patch1, patch2) - fn(patch2, patch1) is transferred.
  diffuse4: (propName, transferAmountFn) ->
    sim.patches.each (patch, x, y) ->
      patch['delta:'+propName] = 0
      if not _.isFinite patch[propName] then patch.grass = 0
    sim.patches.each (patch1, x, y) ->
      for patch2 in [ sim.patches[modulo x+1, sim.patches.width ][y] ,
                      sim.patches[x][modulo y+1, sim.patches.height] ]
        deltaHere = transferAmountFn(patch2, patch1) - transferAmountFn(patch1, patch2)
        patch1['delta:'+propName] += deltaHere
        patch2['delta:'+propName] -= deltaHere
    sim.patches.each (patch, x, y) ->
      patch[propName] += patch['delta:'+propName]

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
  Turtle.prototype = sim.fn.turtle
  return (attrs...) -> _.extend(new Turtle(), attrs...)
  )()
sim.newPatch = (->
  Patch = ->
  Patch.prototype = sim.fn.patch
  return (attrs...) -> _.extend(new Patch(), attrs...)
  )()
sim.time = 0 #time not turn because turn sounds like rotation


simATurn = (sim) ->
  onDynamicUserCodeError = (error, type, fnName) ->
    console.log error.message #?
    #TODO: fix more-UI-related model code in the simulation:
    #TODO: and fix the O(n) in fn.turtle.length behavior:
    thisPageTurtleFnList.where(type: type, name: fnName)[0].set error: error
  for own fnName, fn of sim.fn.turtle
    condition = fn.activation
    if condition?
      for turtle in sim.turtles
        try
          if condition.apply(turtle)
            fn.apply(turtle)
        catch error
          onDynamicUserCodeError error, 'turtle', fnName
  for own fnName, fn of sim.fn.patch
    condition = fn.activation
    if condition?
      sim.patches.each (patch) ->
        try
          if condition.apply(patch)
            fn.apply(patch)
        catch error
          onDynamicUserCodeError error, 'patch', fnName
  for own fnName, fn of sim.fn.world
    condition = fn.activation
    if condition?
      try
        if condition.apply(turtle)
          fn.apply(sim.fn.world)
      catch error
        onDynamicUserCodeError error, 'world', fnName
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
    try
      ctx.fillStyle = _.result patch, 'color'
    catch error
      return #TODO report the error to user
    ctx.fillRect(x, y, 0.95, 0.95)
  
  for turtle in sim.turtles
    try
      ctx.fillStyle = _.result turtle, 'color'
    catch error
      return #TODO report the error to user
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


# Env is an object { name: value ... } where the names
# are put into the script's environment (by making them
# be function-arguments).
#
# thisVal defaults to undefined; it specifies the value of 'this'
# in the top-level script environment.
#
# If the script returns a value, this function returns that value.
evalScriptInEnv = (scriptText, env, thisVal) ->
  # To get values, map from keys to make certain it's the same number
  # of items in the same order (even if _.values might do that).
  keys = _.keys env;
  values = _.map keys, (key) -> env[key]
  fn = Function.apply null, keys.concat scriptText
  return fn.apply thisVal, values

#compileValue = (coffeescript, env, thisVal) ->
#  scriptAsCoffeeFunction = ('->\n'+coffeescript).replace(/\n/, '\n ')
#  CoffeeScript.compile(, {bare:true})
coffeeeval = (coffeescript, env, thisVal) ->
  # because top-level doesn't make the last line a 'return' in normal coffee
  try
    readyCoffeeScript = ('return (->\n'+coffeescript).replace(/\n/, '\n ')+'\n).call(this)'
    js = '"use strict";' + CoffeeScript.compile readyCoffeeScript, bare: true
  catch error
    # Adjust for the extra line I have to put at the beginning of the script.
    # Also, if it's a one-liner, don't bother with a line number at all.
    error.message = error.message.replace /\ on line ([0-9]+)/, (_all, line) ->
      if /\n/.test coffeescript then ' on line '+(line - 1) else ""
    throw error
  return evalScriptInEnv js, env, thisVal


#TODO use http://ace.ajax.org/ for code editor/syntax hilight etc.

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


generateWordNotIns = (notInObjs) ->
  rand.okayArrayMember window.StarPlay.words,
      (word) -> _.all notInObjs, (obj) -> not _.has obj, word

# This has a name (identifier-style(?) string),
# implementation (CoffeeScript text evaluating to a value, possibly of function type),
# and activation (CoffeeScript text evaluating to a function returning boolean, or nothing)
class TurtleFn extends Backbone.Model
  setIfNot: (props, setOptions) ->
    for key, val of props
      if not @get(key)?
        obj = {}
        obj[key] = val
        @set obj, setOptions
  setIfNotF: (props, setOptions) ->
    for key, valf of props
      if not @get(key)?
        obj = {}
        obj[key] = valf()
        @set obj, setOptions
  initialize: ->
    @setIfNotF
      type: -> 'turtle'
      name: -> generateWordNotIns [sim.fn.turtle, sim.fn.patch, sim.fn.world]
      implementation: -> '-> '
      activation: -> '-> '
      error: -> null
    @on 'change:type change:name change:implementation change:activation', @updateSimCode, @
    @updateSimCode()
  updateSimCode: ->
    delete sim.fn[@previous 'type'][@previous 'name']
    try
      fn = sim.fn[@get 'type'][@get 'name'] = coffeeeval @get('implementation'), coffeeenv
      fn.type = @get 'type'
      fn.activation = coffeeeval @get('activation'), coffeeenv if @get('activation')?
      @set 'error': null
    catch error
      @set 'error': error.message
      console.log @get('name'), error, error.message, error.stack

class TurtleFnList extends Backbone.Collection
  model: TurtleFn
  localStorage: new Store('StarPlay-TurtleFnList')


class TurtleFnView extends Backbone.View
  #tagName: 'li'
  #className: 'turtle-fn-view'
  make: -> @$domTemplate.clone()[0]
  $domTemplate: $ """
    <li
      ><div class="turtle-fn-menu"
        ><img alt="turtle" tabindex="0" class="turtle-fn-type" src="turtle23x23.png" width="23" height="23"
        /><div
          ><a href="javascript:;" class="fn-become-turtle"
            ><img alt="be turtle" src="turtle23x23.png" width="23" height="23" /></a
          ><a href="javascript:;" class="fn-become-patch"
            ><img alt="be patch" src="patch23x23.png" width="23" height="23" /></a
          ><a href="javascript:;" class="fn-become-world"
            ><img alt="be world" src="world23x23.png" width="23" height="23" /></a
          ><a href="javascript:;" class="fn-delete"
            >Delete</a
        ></div
      ></div
      ><span class="turtle-fn-name" contentEditable="true"></span
      ><span class="turtle-fn-implementation" contentEditable="true"></span
      ><span class="turtle-fn-activation" contentEditable="true"></span
      ><output class="error"></output
    ></li>
    """
  events:
    'focus div.turtle-fn-menu *': -> @$('.turtle-fn-menu').addClass 'menuOpen'
    'blur div.turtle-fn-menu *': -> @$('.turtle-fn-menu').removeClass 'menuOpen'
    #'click .turtle-fn-delete': 'remove' #??maybe? perhaps deleting the name (or impl?) & it asks if you want to delete.
    'blur .turtle-fn-name': 'rename'
    'blur .turtle-fn-implementation': 'recompile'
    'blur .turtle-fn-activation': 'reactivate'
    'click .fn-become-turtle': -> @model.set 'type': 'turtle'
    'click .fn-become-patch': -> @model.set 'type': 'patch'
    'click .fn-become-world': -> @model.set 'type': 'world'
  initialize: ->
    @render()
    @model.on 'change:type', =>
      type = @model.get 'type'
      @$('.turtle-fn-type').attr('alt': type, 'src': type+'23x23.png')
    @model.on 'change:error', =>
      @$('.error').text (@model.get('error') || '')
      @$el.toggleClass 'hasError', (@model.get 'error')?
    #@model.on 'change', @recompile, @
    #@model.on 'destroy',
  #no consistency/compilability checking yet
  #red background? ability to reset to previous? undoes?
  #possibly check that it compiles? also doesn't throw exceptions?? lintz? in real time while typing?
  rename: -> @model.set 'name', @$('.turtle-fn-name').text()
  recompile: -> @model.set 'implementation', @$('.turtle-fn-implementation').text()
  reactivate: -> @model.set 'activation', @$('.turtle-fn-activation').text()
  render: ->
    @$('.turtle-fn-name').text @model.get 'name'
    @$('.turtle-fn-implementation').text @model.get 'implementation'
    @$('.turtle-fn-activation').text @model.get 'activation'
    @$('.error').text (@model.get('error') || '')
    type = @model.get 'type'
    @$('.turtle-fn-type').attr('alt': type, 'src': type+'23x23.png')
    @
    
  #later worry about codemirror

# cf http://www.chris-granger.com/2012/02/26/connecting-to-your-creation/ which i saw a few days after starting this project

# name text turns red while it's the same as another? and has a popup or?'

thisPageTurtleFnList = new TurtleFnList

runInitScript = ->
  sim.turtles = []
  delete sim.patches
  try
    coffeeeval $('.initScript').text(), coffeeenv
    return true
  catch error
    #TODO put error somewhere
    return false

$ ->
  canvas = guiState.canvas = $('#gameCanvas')[0]
  canvas.width = 600
  canvas.height = 600
  $('#restart').click ->
    if runInitScript() then startRunning() else stopRunning()
  $('#pause_resume').click ->
    if guiState.isRunning then stopRunning() else startRunning()
  thisPageTurtleFnList.on 'add', (model) ->
    $('#turtleFns').append new TurtleFnView(model: model).el

  newFn = (type, name, implementation, activation) ->
    thisPageTurtleFnList.create type: type, name: name, implementation: implementation, activation: activation
  newFn 'turtle', 'speed', '-> @forward 1', "-> @type == 'bullet'"
  newFn 'turtle', 'activateGun', "-> @clone type: 'bullet', color: 'red'", "-> @type == 'crazy' and sim.time % 8 == 0"
  newFn 'turtle', 'wobble', """
    ->
      @rotateLeft tau / 16 * (Math.random() - 0.5)
      @forward 0.25""",
    """-> @type == 'crazy'"""
  newFn 'turtle', 'patchHere', "-> sim.patches[Math.floor(@x)][Math.floor(@y)]"
  newFn 'turtle', 'layGrass', "-> @patchHere().grass += 5", "-> true"
  
  newFn 'patch', 'decayGrass', "-> @grass *= 0.99", "-> true"
  newFn 'world', 'diffuseGrass', """
    -> @diffuse4 'grass', (patch1) -> patch1.grass / 10 / (4+1)
    """, "-> true"
  
  window.StarPlay.wordsAjaxRequest.done -> $('#testplus').click -> thisPageTurtleFnList.create()
  window.StarPlay.wordsAjaxRequest.fail -> $('#testplus').hide()
  runInitScript()
  startRunning()
