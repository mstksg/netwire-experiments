Netwire Experiments
===================

Experiments with netwire and FRP in general.

Will hopefully post some detailed writeups at [http://jle.im].

Experiment.Planets
------------------

An N-body simulation with gravity.  Simulates the planets rotating around our
solar system; uses verlet integration for motion.  Every planet attracts every
other planet, just like in real life.

To test the input event mechanisms, there is a UFO rotating just outside of
Mars's orbit that exerts a gigantic pull when the mouse is held down.

Experiment.Room
---------------

Attempting to apply the same physics engine to a ball bouncing in a room.  Was
not too interesting so did not go past just making the ball bounce a few
times and plotting the results using gnuplot.

Experiment.Archers
------------------

Initial attempt at a battlefield-like simulation, where a bunch of archers are
spawned on different teams and they all try to fight eachother until the last
surviving team wins.  Basically got my feet wet with dynamic wires and
interactions.

Experiment.Battlefield
----------------------

Like archers, but also with swordsmen, axemen, longbowmen, horsemen, horse
archers.  Basically a medieval battle simulator.  Added battlefield-like
"Bases" that spawn more per team, and teams race to take over eachothers'
bases in Battlefield/TF2 style.  Bases have walls to simulate defender's
advantage but they will be taken out later and replaced with terrain.

This really pushed the limits of the abstractions I developed for Archers, in
terms of communication and dynamic wires.

Next step would be

* Player interaction
* game engine: intelligent AI & terrain
* moving either everything to ghcjs, or making a ghcjs frontend with a native
  haskell server backend.

But this will probably be forked to a separate project before that happens.

Render
------

All four experiments are rendered with a generic rendering backend --- i've
written backends for SDL, GLUT, and GNUPlot.  HTML5/Canvas coming next, or
Server/HTML5 Client.  Basically was forced to do this because I could not get
SDL-gfx to compile on Windows, but could not get GLUT to stream over VNC.  So
used SDL on my server and GLUT on my windows.  An HTML5 version would make
this all unecessary, but it was fun writing a modular backend.

So far it's all 2D rendering, but the actual game logics are all 3D so it
would just be a matter of adding support for 3D rendering and 3D would be good
to go.
*



