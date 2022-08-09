This is a simple tool for composing protocols, generating state machines based on
protocol specification, and extracting protocols from existing code.

The protocol type is defined within interleave.erl. This file also contains a few
examples together with the algorithm for protocol composition.

# Building
The tool can be built using rebar3:

    rebar3 compile

# Running

Protocol Composition:

    interleave:interleaveWeak(Protocol1, Protocol2)).

For example:

    interleave:interleaveWeak(interleave:pin(), interleave:tan()).

Code Generation:


    generate:gen(FileName, Protocol).

For example:

    generate:gen(pin, interleave:pin()).


Protocol Extraction:

    extract:protocol(FileName, Path).

For example:

    extract:protocol("pin.erl", ".").
