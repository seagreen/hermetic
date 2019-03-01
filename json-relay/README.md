# Summary

A server with rooms. Relays each message from a client to the others in the same room. Upstream of Hermetic and has no game-specific code.

This is a quick-and-dirty project, it's not up to the coding/documentation standards of the main game.

# Deployment

## Goals

+ Run the `json-relay` server in the cloud with as little work as possible.

+ Build it locally because I don't want to pay for a server with enough RAM to compile it.

+ For it to work the server with no linking issues.

## Solution

Stack + Docker + Digital Ocean (or any hosting provider that supports Docker).

Use `scp` to get the Docker container to Digital Ocean, this lets us avoid Docker Hub.

## Setup

### Digital Ocean

Create a Droplet via "Create > One-click apps > Docker".

### Stack

Add the following to `stack.yaml`:

```yaml
image:
  containers:
    - base: "fpco/pid1"
```

## Deploy

First make a container and copy it to the server.

This is the `./deploy` script. Replace `ianjeffries` with your Docker username.

It relies on `./docker-compose.yaml` in this directory, which uses [this](https://hub.docker.com/r/fpco/pid1/) Docker image.

```sh
stack image container
docker save json-relay > /tmp/json-relay.tar
scp ./docker-compose.yaml root@relay.ianjeffries.net:/root
scp /tmp/json-relay.tar root@relay.ianjeffries.net:/root
```

Verify what that created (optional):

```sh
docker-images
docker-compose up
```

Complete deployment:

```sh
ssh root@relay.ianjeffries.net
cat json-relay.tar | docker load
docker-compose up --detach
```

## Misc

(Run these from the server)

View logs:

```sh
docker logs json-relay-container
```

Stop the server:

```sh
docker stop json-relay-container
```

SSH into the container:

```sh
docker exec --interactive --tty json-relay-container /bin/bash
```
