# Requirements

Linux, unfortunately.

The game worked on previous versions of macOS, but has stopped working on the latest versions. This may have to do with macOS deprecating OpenGL.

# Setup

1. Install [Stack](https://haskellstack.org/).

2. Install the system dependencies (`libGL`, `libGLU`, `freeglut`, maybe others?).

3. `stack install`

# Single player sandbox

Play against a computer opponent that doesn't do anything besides end turn:

`hermetic --sandbox`

# Multiplayer (using the game server)

If my server is up you can use it (replace `ROOM_NAME` with your own random string):
```
$ hermetic --room ROOM_NAME
AddrInfo {addrFlags = [], addrFamily = AF_INET, addrSocketType = Stream, addrProtocol = 6, addrAddress = 178.128.66.50:3000, addrCanonName = Nothing}
Sending message
```

(If the server is down you'll either get an error or `Sending message` won't show up)

Then your opponent runs the same command and a game window will open on each computer.

Make sure you settle on an unlikely `ROOM_NAME` with your opponent. If a third party tries to join the same room while the game is in progress it will crash.

# Multiplayer (using your own server)

```sh
cd json-relay
stack install
json-relay # Start the server
```

Then proceed as in the previous section, but adding `--host ADDRESS` and `--port PORT` to the `hermetic` commands.
