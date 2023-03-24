# LasIO

[![CI](https://github.com/visr/LasIO.jl/actions/workflows/CI.yml/badge.svg?branch=master)](https://github.com/visr/LasIO.jl/actions/workflows/CI.yml)

Julia package for reading and writing the LAS lidar format.

This is a pure Julia package for reading and writing ASPRS `.las` files. Currently only LAS versions 1.1 - 1.3 and point formats 0 - 3 are supported. For LAZ support see below.

If the file fits into memory, it can be loaded using

```julia
using FileIO, LasIO
header, points = load("test.las")
```

where `header` is of type `LasHeader`, and, if it is point format 3, `points` is a `Vector{LasPoint3}`. `LasPoint3` is an immutable that directly corresponds to the binary data in the LAS file. Use functions like `xcoord(p::LasPoint, header::LasHeader)` to take out the desired items in the point.

If the file does not fit into memory, it can be memory mapped using

```julia
using FileIO, LasIO
header, points = load("test.las", mmap=true)
```

where `points` is now a memory mapped `PointVector{LasPoint3}` which behaves in the same way as the `Vector{LasPoint3}`, but reads the points on the fly from disk when indexed, not allocating the complete vector beforehand.

See `test/runtests.jl` for other usages.

## LAZ support
We advise to use [LazIO](https://github.com/evetion/LazIO.jl), which works out of the box and is compatible with LasIO.

The compressed LAZ format is supported by LasIO itself, but requires the user to make sure the `laszip` executable can be found in the PATH. LAZ files are piped through `laszip` to provide reading and writing capability. `laszip` is not distributed with this package. One way to get it is to download `LAStools` from https://rapidlasso.com/. The LAStools ZIP file already contains `laszip.exe` for Windows, for Linux or Mac it needs to be compiled first. When this is done this should work just like with LAS:

```julia
using FileIO, LasIO
header, points = load("test.laz")
```

## Handling of Non-Standard Data

The internal data structures aim to represent any data that follows the standard.
They aim to hold all the bytes of a LAS file except for reserved data.

The reader tries to read all data even if it does not strictly adhere to the standard.
If unexpected values are encountered, the reader emits a warning but continues unless it would have to jump backwards in the file (e.g. if the header is larger than the offset to the point data).
Reserved fields are discarded but generate a warning if they contain unexpected data.

The writer should always write files that adhere to the standard.
