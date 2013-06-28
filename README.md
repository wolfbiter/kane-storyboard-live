# kane-storyboard-live

software which uses overtone and clojure to create a "storyboard" live djing mode for the launchpad

## Usage

1. plug in your launchpad
2. cd into project directory, run lein repl
3. copy and paste the contents of src/beatn_live/core.clj into the repl
 - this is a hack i know, but on my computer overtone takes long enough to load that lein repl terminates its initialization
4. assuming everything worked, your launchpad should have LEDs on.
pressing any of the 8x8 grid buttons will start the sequencer. the arrow buttons on the right
are the different "actions" (read: functions) you can select, and pressing any of the grid buttons which the sequencer
is running will "schedule" the selected action to trigger on that beat. atm the first six arrow buttons
(from top to bottom) are loaded with a variety of uplifters, downlifters, and explodes.

## License

Copyright © 2013 FIXME

Distributed under the Eclipse Public License, the same as Clojure.
