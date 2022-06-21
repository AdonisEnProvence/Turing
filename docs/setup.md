# Project setup

## Build

```bash
rebar3 escriptize
```

## Run CLI

```bash
_build/default/bin/turing run machine-config.json "YOUR_INPUT"
```

## Start server

```bash
_build/default/bin/turing serve
```

## Unit tests

```bash
rebar3 eunit
```

### Generate code coverage reports

```bash
rebar3 cover --verbose
```

You can visualize code coverage, and which lines of code are not tested, by opening `_build/test/cover/index.html`.

## Integrations tests

```bash
./bats-core/bin/bats test/
```

## Dockerfile

Follow those steps to launch a local Docker container with Erlang installed.

```bash
# Build an image defined in the `Dockerfile` of `.` directory and name it `turing`.

# Note: Dockerfile.prod is for production only; it starts the server automatically.
docker build -f Dockerfile.local -t turing .

# Run the image named `turing`, attach local directory to /turing directory in the container, thanks to volumes.
# If we change files on our computer, the changes will be reflected in the container.
# Run the container in detached mode so that we can access the container with a shell when we want.
docker run -d --name turing -v $PWD:/turing -p 8080:8080 turing

# Launch a shell inside the container.
# We will write the same commands as if we were developing directly on our machine.
docker exec -it turing bash

# Remove container
docker rm -f turing
```

## Run the visualizer locally

[See Visualizer setup →](/viz/README.md)
