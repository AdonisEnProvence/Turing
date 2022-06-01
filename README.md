# turing

An escript

## Build

    $ rebar3 escriptize

## Run

    $ _build/default/bin/turing

## Test

    $ rebar3 eunit

### Generate code coverage reports

    $ rebar3 cover --verbose

You can visualize code coverage, and which lines of code are not tested, by opening `_build/test/cover/index.html`.

## Dockerfile

Follow those steps to launch a Docker container with Erlang installed.

```sh
# Build an image defined in the `Dockerfile` of `.` directory and name it `turing`.
docker build -t turing .

# Run the image named `turing`, attach local directory to /turing directory in the container, thanks to volumes.
# If we change files on our computer, the changes will be reflected in the container.
# Run the container in detached mode so that we can access the container with a shell when we want.
docker run -d --name turing -v $PWD:/turing turing

# Launch a shell inside the container.
# We will write the same commands as if we were developing directly on our machine.
docker exec -it turing bash

# Remove container
docker rm -f turing
```

## Bats shell tests

To run shell tests you have to be inside the docker container
Run
```sh
./bats-core/bin/bats test/
```
