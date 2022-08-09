Protocol re-engineering artifact

## 1. Getting started guide

This is a tool for composing protocols, generating state machines based on
protocol specification, and extracting protocols from existing code.

The protocol type is defined within interleave.erl. This file also contains a few
examples together with the algorithm for protocol composition.

### Prerequisites
The following software needs to be installed:

- Erlang
- rebar3


### Building
The tool can be built using rebar3:

    rebar3 compile

## 2. Step-by-Step Instructions

### Running
Enter the rebar3 shell:

		rebar3 shell
Protocol Composition:
Strong:

		interleave:interleave(Protocol1, Protocol2).

Weak:

    interleave:interleaveWeak(Protocol1, Protocol2).

Correlating:

		interleave:interleaveCorrelating(Protocol1, Protocol2).

All:

		interleave:interleaveAll(Protocol1, Protocol2).

For example:

    interleave:interleaveWeak(examples:pin(), examples:tan().

Code Generation:


    generate:gen(FileName, Protocol).

For example:

    generate:gen(pin, examples:pin()).


Protocol Extraction:

    extract:protocol(FileName, Path).

For example:

    extract:protocol("pin.erl", ".").

To obtain Table 1 from the paper run:

		examples:table().

## 2. Overview of the claims

Claims supported by the artifact:

	1. Protocol Composition: Strong, Weak, Correlating, All
	2. Protocol Generation
	3. Protocol Extraction

Results presented in Table 1 should be obtained on any machine.
To obtain Table 1 from the paper run:

		examples:table().
