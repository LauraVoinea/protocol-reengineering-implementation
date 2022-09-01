# Build stage 0
FROM erlang:alpine

# Set working directory
WORKDIR /root
RUN pwd
RUN mkdir /root/buildroot
WORKDIR /root/buildroot

#Copy src, include folders and rebar.config
COPY src/ src/
COPY include/ include/
COPY rebar.config .

RUN pwd
RUN ls -la

RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
     apk add --no-cache libstdc++ && \
      apk add --no-cache libgcc

RUN apk update && apk add bash
CMD /bin/bash  
# CMD rebar3 shell