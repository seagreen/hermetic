Droplet created via "Create > One-click apps > Docker".

Make a container and copy it to the server (from the json-relay directory):

```sh
./deploy/deploy
```

Verify what that created (from this directory):

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

View logs:
```sh
ssh root@relay.ianjeffries.net
docker logs json-relay-container
```
