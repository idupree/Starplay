#//backbonejs

###
`
(function() {


function prettyConnectingPathBetweenShapes(obj1, obj2) {
    var bb1 = obj1.getBBox(),
        bb2 = obj2.getBBox(),
        p = [{x: bb1.x + bb1.width / 2, y: bb1.y - 1},
        {x: bb1.x + bb1.width / 2, y: bb1.y + bb1.height + 1},
        {x: bb1.x - 1, y: bb1.y + bb1.height / 2},
        {x: bb1.x + bb1.width + 1, y: bb1.y + bb1.height / 2},
        {x: bb2.x + bb2.width / 2, y: bb2.y - 1},
        {x: bb2.x + bb2.width / 2, y: bb2.y + bb2.height + 1},
        {x: bb2.x - 1, y: bb2.y + bb2.height / 2},
        {x: bb2.x + bb2.width + 1, y: bb2.y + bb2.height / 2}],
        d = {}, dis = [];
    for (var i = 0; i < 4; i++) {
        for (var j = 4; j < 8; j++) {
            var dx = Math.abs(p[i].x - p[j].x),
                dy = Math.abs(p[i].y - p[j].y);
            if ((i == j - 4) || (((i != 3 && j != 6) || p[i].x < p[j].x) && ((i != 2 && j != 7) || p[i].x > p[j].x) && ((i != 0 && j != 5) || p[i].y > p[j].y) && ((i != 1 && j != 4) || p[i].y < p[j].y))) {
                dis.push(dx + dy);
                d[dis[dis.length - 1]] = [i, j];
            }
        }
    }
    if (dis.length == 0) {
        var res = [0, 4];
    } else {
        res = d[Math.min.apply(Math, dis)];
    }
    var x1 = p[res[0]].x,
        y1 = p[res[0]].y,
        x4 = p[res[1]].x,
        y4 = p[res[1]].y;
    dx = Math.max(Math.abs(x1 - x4) / 2, 10);
    dy = Math.max(Math.abs(y1 - y4) / 2, 10);
    var x2 = [x1, x1, x1 - dx, x1 + dx][res[0]].toFixed(3),
        y2 = [y1 - dy, y1 + dy, y1, y1][res[0]].toFixed(3),
        x3 = [0, 0, 0, 0, x4, x4, x4 - dx, x4 + dx][res[1]].toFixed(3),
        y3 = [0, 0, 0, 0, y1 + dy, y1 - dy, y4, y4][res[1]].toFixed(3);
    var path = ["M", x1.toFixed(3), y1.toFixed(3), "C", x2, y2, x3, y3, x4.toFixed(3), y4.toFixed(3)].join(",");
    return path;
}


Raphael.fn.connection = function (obj1, obj2, fgcolor, bgcolor) {
    var precomputed;
    if (obj1.fgline && obj1.from && obj1.to) {
        precomputed = obj1;
        obj1 = precomputed.from;
        obj2 = precomputed.to;
    }
    var path = prettyConnectingPathBetweenShapes(obj1, obj2);
    if (precomputed) {
        precomputed.bgline && precomputed.bgline.attr({path: path});
        precomputed.fgline.attr({path: path});
    } else {
        fgcolor = typeof fgcolor == "string" ? fgcolor : "#000";
        return {
            bgline: bgcolor && bgcolor.split && this.path(path).attr({
                        stroke: bgcolor.split("|")[0],
                        fill: "none",
                        "stroke-width": bgcolor.split("|")[1] || 3}),
            fgline: this.path(path).attr({stroke: fgcolor, fill: "none"}),
            from: obj1,
            to: obj2
        };
    }
};

var el;
window.onload = function () {
    var dragger = function () {
        this.ox = this.type == "rect" ? this.attr("x") : this.attr("cx");
        this.oy = this.type == "rect" ? this.attr("y") : this.attr("cy");
        this.animate({"fill-opacity": .2}, 500);
    },
        move = function (dx, dy) {
            var att = this.type == "rect" ? {x: this.ox + dx, y: this.oy + dy} : {cx: this.ox + dx, cy: this.oy + dy};
            this.attr(att);
            for (var i = connections.length; i--;) {
                r.connection(connections[i]);
            }
            r.safari();
        },
        up = function () {
            this.animate({"fill-opacity": 0}, 500);
        },
        r = Raphael("holder", 640, 480),
        connections = [],
        shapes = [  r.ellipse(190, 100, 30, 20),
                    r.rect(290, 80, 60, 40, 10),
                    r.rect(290, 180, 60, 40, 2),
                    r.ellipse(450, 100, 20, 20)
                ];
    for (var i = 0, ii = shapes.length; i < ii; i++) {
        var color = Raphael.getColor();
        shapes[i].attr({fill: color, stroke: color, "fill-opacity": 0, "stroke-width": 2, cursor: "move"});
        shapes[i].drag(move, dragger, up);
    }
    connections.push(r.connection(shapes[0], shapes[1], "#fff"));
    connections.push(r.connection(shapes[1], shapes[2], "#fff", "#fff|5"));
    connections.push(r.connection(shapes[1], shapes[3], "#000", "#fff"));
};

}());
`

# I'll use regular JS data structures for now, and full re-rendering?,
# so it's not multiple-things-to-learn-at-once.
# but aah. relational databases sound so much better.
`
//faux relational tables
{
	nodes = {
		1: { ..., edges = [1, 7]},
		2: { contacts = {
				controlActionFlow: true, //for anything that has side effects. Can I make reading not a side effect.
				//Also, if any of the parameters can only be computed from side effects
				//(is that a reasonable possibility?) then they can also limit it.

				//or, look functional. and sequence using "then" if necessary. uh.
				parameters: 2,
				result: true //may contain control flow and/or a value. If control flow
					//not required to get here, then the result is always there,
					//and/or lazily evaluated as needed by something with control flow
					//if computation time is a restricted thing in some meaningful way.
					//What about multiple results. What about complex data structures;
					//  lisp "list" (variable number of arguments),
			}
		},
	...
	},
	edges = {
		1: { head: { node: 1, contact: "result"}, tail: { node: 2, contact: "parameter1"}, type: "value" }
	}
}



// so
// trickies in conversion so far.
// result may point multiple places:: requires a (let ). which is imperative-sequencing in lisp i think also.
// but in lisp that means it needs a name. interesting.
// Parameters presumably can only have one input :) ..any other combining mechanism needs to be a named/node thingy.
// control-action-flow could reasonably come from multiple places:
//   translation is to defn this rather than just put it after something directly.
// it could come from "every turn/tick" object, or sth. hmm.

// could use a clojure-subset just because it is less-ugly version of lisp basic syntax, and also
// has proper fn/data namespace like scheme

// is it practical to make an IDE in a text field instead/additionally? like the etoys one
// only with actual syntax. and auto indentation and coloring that makes it easy to follow.
//   jsfiddle.net uses iframe with <body contenteditable="true">, so let's copy that.
// I bet when you edit it it fixes up the syntax coloring for you ASAP
// parens are really easy to match/color/background/border/indent. even if it's three different kinds of parens i guess.
// drag&drop: ?? -- how do you select something to pick up, and spots to put it in?

//anyway i think we need this textfield with it updated in parallel with a graphical repr ?

//lua let/do doesn't need nesting(indentation)complication...hmm.
//ohh, remember that idea for dummy variable names that were amusing random words.

//so i really need an at least basic simulation, for playing with this to be interesting.
//hm.
//robots that move around the 2d space and have paint for patches..i'll make it like starlogo
//and can sense what's under them & that's all. or, 3-8 direction neighbors too.
//and can move forward and turn left
//(or it could be that diversity/Occupy program. heh. but directionality is easier to program.)

//heck i can write that haha for my demo... and uh have turtle programs be JS at first
//borrow rendering from the occupy code, and add a rotatable/colorable turtle PNG? er does
//colorable work for that? nah, use a polygon instead.

`
###

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

	#http://jng.imagine27.com/articles/2011-10-02-171602_overtone.html
###
#eh, scrap "vars.", live on the edge:P
turtles = [{vars:{},x:double,y:double,color:{r:,g:,b:or,csscolor},heading:0..tau}]
patches = [[{vars:{},color:c}]]
global = {vars:{},turn:0} #e.g. configuration sliders

turtleProgram ['a', 'activate gun'], -> @clone type: 'bullet'
turtleProgram ['s', 'speed', daemon], -> if @type == 'bullet' then @forward 1
  ##but look, the 'if' should only apply as an if for daemon activation!
  ## Manually, anyone should be able to speed (although..maybe filter if
  ## you have a lot of things selected? hmm.)
  ##A turtle program should be able to call another turtle program.
  ##   Or itself on the next frame ?
  ##  global.a   global.speed    global['activate gun']  global.activate_gun
  ##                             global.activateGun  #silly auto camel casing
turtleProgram ['s', 'speed', -> @type == 'bullet'], -> @forward 1
turtleProgram ['s', 'speed', -> true], -> @forward 1
  ##individual programs click toggle daemonicness ?
  ##closures as a turtle's program ?
  #...the 1-character name should not really be a programming name, the longer name should
global.activateGun = turtleProgram ['a'], -> @clone type: 'bullet'
  #i'll just not have keyboard shortcuts at first.
global.activateGun = turtleProgram -> @clone type: 'bullet'
global.turtlePrograms.activateGun = -> @clone type: 'bullet'
  #i suppose it can infer from the code whether it is turtle and/or patch program?? and/or global?
  #global.turtlePrograms can contain 'clone' and 'forward' too? or inherit from that, rather
  #HEH HEH HEH and turtles inherit from turtlePrograms, so they can call these with @ .
  #It doesn't work for mutable data, but functions are rather immutable.
global.turtleFn.activateGun = -> @clone type: 'bullet'
global.turtleFn.speed = these 'bullet', -> @forward 1
  #setting attributes of the function. which daemon will check.
#or we could make it more object hier archy... so that bulletyFn contains
#functions that only daemonize for bullets?  this seems kinda... clever:( ...though.
#we need separate daemon list.
global.turtleFn.activateGun = -> @clone type: 'bullet'
global.turtleFn.speed = -> @forward 1
global.turtleDaemons = speed: -> @type == 'bullet'
prototype hier?: global > turtleFn > each turtle. nah, ppl trying to set 'turn' and failing.
just turtleFn > each turtle, and also 'global' in scope. and 'turn' maybz..but it's not that important'
###

#and do the single characters do it for all turtles? the 'selected' ones?
#oh hahaha the ones with @selected == true ?
#that sounds pretty useful actually. possible to programmatically choose them.
#click to toggle individuals, drag to select a swath?  select all/none is an
#easy program. so is checking x/y but can i display the coordinate system easily.
#numbers shown every 5 on the bottom and right maybe.
#visually distinguish selected ones with an outline. Does clone preserve selectedness?
#all start selected;
#Long-names are _buttons_ you can _click_ to do the thing, shown in the sidebar.
#Anything in global...vars not functions, if is the right type e.g. number is slider
#(is there an html5 slider? or it's a textfield in basic browsers)
#OOOH :P
#code w/ syntax coloring (does jsfiddle share that code publicly?) below, in a
#box, or next to each button!(& exportable to a box) - changes take instant
#   effect if syntactical? maybe? what if typo or changing a number with multiple
#   keystrokes...  PAUSE / RUN button for daemon things.

#"StarCoffee" / "StarClojure" : "This is a total ripoff of StarLogo, with love."

#patchProgram #i won't have any for now. but can look into starlogo book for them.
#->@type == 'bullet', -> @forward 1
#patchProgram
##daemons = {'a':[hasvar monster,()->spinincircles]}
##turtlePrograms = [(turtle)=>]  #[function(){}]
#programs / daemons!
#@clone().type='bullet'

#action? like player types a key and?
#A turtle you are currently controlling? like our vision. So each turtle/patch
# program has a name/ a letter ?  autofiring programs for each turtle ?
#Are the shortcuts per-turtle , or global, or something in between clever ness.
#Right now, global looks easier??

