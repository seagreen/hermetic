# Setup

1. Install [Stack](https://haskellstack.org/).

2. Install the system dependencies (`libGL`, `libGLU`, `freeglut`, maybe others?).

3. `stack install`

# Single player sandbox

Play against a computer opponent that doesn't do anything besides end turn:

`hermetic --sandbox`

# Multiplayer (using the game server)

On the off chance my server is up you can use it.

First settle on an unlikely `ROOM_NAME` with your opponent. If a third party tries to join the same room while the game is in progress it will crash.

Then each player runs:

`hermetic --host relay.ianjeffries.net --room ROOM_NAME`

# Multiplayer (using your own server)

```sh
cd json-relay
stack install
json-relay # Start the server
```

Then proceed as in the previous section, replacing the argument to `--host` with your IP address.
