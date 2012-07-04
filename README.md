# Pillow

**Pillow** is a lightweight cushion for high-volume key-value streams over TCP.
It launches two TCP servers, one for receiving a constant stream of data from
multiple potential clients (pusher), the other for retrieving the current
snapshot of the dataset (dumper).

The data stream which flows into the pusher is expected to have the following
format:

```
ID;Field1;...;FieldN<LF>
```

For every line, the ID (key) is separated from the rest (value) and stored
in-memory using the erlang term storage (ETS) as `key => value`. If there
already is an entry for the current ID, it is overwritten. This way, it is
possible to retrieve a snapshot of the stream at any time.

This snapshot can be obtained by invoking the dumper.

**Pillow** is pre-alpha and currently has the following constraints:

* If you want to start the pusher and dumper on different ports, you have
to edit the file `ebin/pillow.app`.
* The data is assumed to be streamed in a CSV-like format, currently the
separator is a semicolon (;). This will be configurable in the future.

## Installation

## Usage

TBD.