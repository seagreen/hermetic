# About

Instructions for using [haskell-code-explorer](https://github.com/alexwl/haskell-code-explorer) to browse this project.

# Use

```sh
$ ./unpack
<lots of output>

$ ./index
<lots of output>

$ ./serve
```

Then go to `localhost:8080`

# Notes

Packages are stored in `./deps` (`./explore/deps` from the top level of the project).

The `stack.yaml` in this directory is copied into each package in `./deps` by `index`.
