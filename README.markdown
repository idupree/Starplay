
# playground/playground.html #

http://www.idupree.com/starplay/

This is a simulation somewhat like StarLogo
(cf. Mitchel Resnick, _Turtles, Termites, and Traffic Jams:
Explorations in Massively Parallel Microworlds_)

with "turtles" - active agents that move around the world using
simple scripts,

and "patches" - a grid of locations that can hold state (and also
do things if they want to).

The world wraps on the side and top/bottom (it's a torus).

The code I'm writing and playing with is in
`playground/index.html` and `playground/playground.coffee`.

This runs completely in-browser; presently you program it in
CoffeeScript.  There's an example starting script already visible
in the browser that you can click on and edit.  There are buttons
in the top-right to re-compile and run the script; it will give you
an error message if you have a syntax error.
(Currently, the line number it gives you is not very usable,
and various other things are probably tedious.  It is not
yet self-documenting.)

Tested in: current Firefox and Chromium.


## CoffeeScript intro for programmers ##

It compiles to JavaScript and tries to have as straightforward a
correspondence between the CoffeeScript and the JavaScript
in order to aid debugging and make people more comfortable with
using CoffeeScript.
Indentation matters.  The last line of a function implicitly
has a `return` prefixed to it if there isn't one there.
`@` means `this`, and `@thing` means `this.thing`.
`(a, b) -> stuff` creates a function.  Zero-argument functions
may have the argument tuple omitted (`-> stuff`).
Function-call argument parens, object-construction curly-brackets,
etc., may be omitted in some cases.
An entire CoffeeScript is wrapped in `(function{...}())`, and `var`
is implicit; thus it's impossible to write to global variables
except by saying things like `window.foo = "bar"`.

http://coffeescript.org/


## Credits ##

Inspiration from StarLogo and many people.

Libraries
Backbone.js, Underscore.js, jQuery, Raphaël, CoffeeScript
(most of which I don't use currently)

Code started from Raphael demo "graffle"
(though currently has nothing to do with it)
http://raphaeljs.com/graffle.html
